library(shiny)

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "lms.R"), local = TRUE)
source(file.path(repo_root, "R", "measures.R"), local = TRUE)
source(file.path(repo_root, "R", "ref_data.R"), local = TRUE)
source(file.path(repo_root, "R", "charts.R"), local = TRUE)
source(file.path(repo_root, "R", "percentile_score.R"), local = TRUE)

hw_ref <- read_iap_hw_ref(file.path(repo_root, "iap_ht__wt_percentile_ref.csv"))
bmi_ref <- read_iap_bmi_ref(file.path(repo_root, "iap_bmi_pecentiles_5-18.csv"))
wc_ref <- read_iap_wc_ref(file.path(repo_root, "iap_wc_ref.csv"))
bp_ref <- read_iap_bp_ref(file.path(repo_root, "iap_bp_ref.csv"))
all_ref <- rbind(hw_ref, bmi_ref, wc_ref)

measure_fields <- c(
  "age",
  "sex",
  "height",
  "weight",
  "bmi",
  "wc",
  "sbp",
  "dbp",
  "height_percentile"
)

ui <- fluidPage(
  titlePanel("iapgrowthr Shiny App"),
  tabsetPanel(
    tabPanel(
      "Single Record",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "measure",
            "Measure",
            c("height", "weight", "bmi", "wc", "bp")
          ),
          selectInput("sex", "Sex", c("m", "f")),
          numericInput("age", "Age (years)", value = 10, min = 1, max = 18, step = 0.5),
          conditionalPanel(
            condition = "input.measure != 'bp'",
            numericInput(
              "value",
              "Observed value",
              value = 30,
              step = 0.1
            )
          ),
          
          conditionalPanel(
            condition = "input.measure == 'bp'",
            
            numericInput(
              "height",
              "Height (cm)",
              value = 140,
              min = 50,
              max = 220
            ),
            
            numericInput(
              "sbp",
              "Systolic Blood Pressure (SBP)",
              value = 110
            ),
            
            numericInput(
              "dbp",
              "Diastolic Blood Pressure (DBP)",
              value = 70
            )
          ),
          actionButton("calc", "Calculate")
        ),
        mainPanel(
          h4("Estimated result"),
          tableOutput("score_table"),
          h4("Chart"),
          plotOutput("chart", height = "700px")
        )
      )
    ),
    tabPanel(
      "Batch Upload",
      sidebarLayout(
        sidebarPanel(
          fileInput("dataset_file", "Upload CSV", accept = ".csv"),
          helpText("Upload first, then map your file columns to the required fields."),
          uiOutput("column_mapping_ui"),
          actionButton("process_dataset", "Process Dataset"),
          downloadButton("download_dataset", "Download Output")
        ),
        mainPanel(
          h4("Preview"),
          tableOutput("dataset_preview")
        )
      )
    )
  )
)

build_mapping_controls <- function(column_names) {
  choices <- c("-- Ignore --" = "", stats::setNames(column_names, column_names))

  tagList(
    h4("Column Mapping"),
    lapply(measure_fields, function(field) {
      selectInput(
        inputId = paste0("map_", field),
        label = tools::toTitleCase(gsub("_", " ", field)),
        choices = choices,
        selected = guess_mapping(field, column_names)
      )
    })
  )
}

guess_mapping <- function(field, column_names) {
  normalized <- normalize_dataset_names(column_names)
  idx <- which(normalized == field)

  if (length(idx) > 0) {
    return(column_names[[idx[[1]]]])
  }

  ""
}

apply_column_mapping <- function(data, input) {
  out <- data

  for (field in measure_fields) {
    selected <- input[[paste0("map_", field)]]
    if (!is.null(selected) && nzchar(selected) && selected %in% names(out)) {
      names(out)[names(out) == selected] <- field
    }
  }

  out
}

plot_bp_percentile_chart <- function(bp_ref,
                                     sex,
                                     measure = "sbp",
                                     age,
                                     height_pct,
                                     value) {
  
  sex <- tolower(sex)
  
  bp_data <- bp_ref[bp_ref$gender == sex, ]
  
  ht_band <- c(5,10,25,50,75,90,95)
  
  nearest_ht <- ht_band[
    which.min(abs(ht_band - height_pct))
  ]
  
  col_name <- paste0(measure, "_ht", nearest_ht)
  
  p90 <- bp_data[
    bp_data$bp_percentile == 90,
    c("age", col_name)
  ]
  
  p95 <- bp_data[
    bp_data$bp_percentile == 95,
    c("age", col_name)
  ]
  
  p99 <- bp_data[
    bp_data$bp_percentile == 99,
    c("age", col_name)
  ]
  
  names(p90)[2] <- "p90"
  names(p95)[2] <- "p95"
  names(p99)[2] <- "p99"
  
  plot_df <- Reduce(
    merge,
    list(p90, p95, p99)
  )
  
  library(ggplot2)
  
  ggplot(plot_df, aes(age)) +
    
    geom_line(
      aes(y = p90, colour = "90th Percentile"),
      linewidth = 1
    ) +
    
    geom_line(
      aes(y = p95, colour = "95th Percentile"),
      linewidth = 1
    ) +
    
    geom_line(
      aes(y = p99, colour = "99th Percentile"),
      linewidth = 1
    ) +
    
    geom_point(
      data = data.frame(
        age = age,
        bp = value
      ),
      aes(
        x = age,
        y = bp
      ),
      colour = "red",
      size = 4
    ) +
    
    labs(
      title = paste(
        toupper(measure),
        "Percentile Chart"
      ),
      subtitle = paste(
        "Height percentile:",
        nearest_ht
      ),
      x = "Age (years)",
      y = "Blood Pressure (mmHg)",
      colour = "Threshold"
    ) +
    
    theme_minimal()
}
server <- function(input, output, session) {
  score_data <- eventReactive(input$calc, {
    
    if (input$measure == "bp") {
      height_result <- score_percentile_ref(
        ref_data = all_ref,
        measure = "height",
        sex = input$sex,
        age = input$age,
        value = input$height
      )
      
      height_pct <- round(height_result$percentile)
      
      sbp_result <- score_bp_ref(
        bp_ref = bp_ref,
        measure = "sbp",
        sex = input$sex,
        age = input$age,
        height_percentile = height_pct,
        value = input$sbp
      )
      
      dbp_result <- score_bp_ref(
        bp_ref = bp_ref,
        measure = "dbp",
        sex = input$sex,
        age = input$age,
        height_percentile = height_pct,
        value = input$dbp
      )
      
      rbind(sbp_result, dbp_result)
    
      
    } else {
      
      score_percentile_ref(
        ref_data = all_ref,
        measure = input$measure,
        sex = input$sex,
        age = input$age,
        value = input$value
      )
      
    }
    
  }, ignoreInit = FALSE)

  output$score_table <- renderTable({
    
    result <- score_data()
    
    if (input$measure == "bp")  {
      
      result$category <- dplyr::case_when(
        result$percentile < 90 ~ "Normal",
        result$percentile < 95 ~ "Elevated",
        result$percentile < 99 ~ "Stage 1 HTN",
        TRUE ~ "Stage 2 HTN"
      )
      
    }
    
    result
    
  }, digits = 2)
  output$chart <- renderPlot({
    obs <- data.frame(
      measure = input$measure,
      age = input$age,
      value = if (input$measure == "bp") NA else input$value,
      stringsAsFactors = FALSE
    )
    if (input$measure == "bp") {
      height_result <- score_percentile_ref(
        ref_data = all_ref,
        measure = "height",
        sex = input$sex,
        age = input$age,
        value = input$height
      )
      
      height_pct <- round(height_result$percentile)
      
      library(patchwork)
      
      p1 <- plot_bp_percentile_chart(
        bp_ref = bp_ref,
        sex = input$sex,
        measure = "sbp",
        age = input$age,
        height_pct = height_pct,
        value = input$sbp
      )
      
      p2 <- plot_bp_percentile_chart(
        bp_ref = bp_ref,
        sex = input$sex,
        measure = "dbp",
        age = input$age,
        height_pct = height_pct,
        value = input$dbp
      )
      
      print(p1 / p2)   }
    
    if (input$measure %in% c("height", "weight")) {
      plot_iap_height_weight_chart(
        ref_data = all_ref,
        sex = input$sex,
        observations = obs,
        main = sprintf("IAP %s chart", input$measure)
      )
    } else if (input$measure == "bmi") {
      plot_iap_bmi_chart(
        ref_data = all_ref,
        sex = input$sex,
        observations = obs,
        main = "IAP BMI chart"
      )
    } else if (input$measure == "wc") {
      wc_data <- all_ref[all_ref$measure == "wc" & all_ref$sex == input$sex, , drop = FALSE]
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      plot_single_measure(
        ref_data = wc_data,
        observations = obs,
        ylab = "Waist circumference (cm)",
        main = sprintf("IAP %s WC chart", if (input$sex == "m") "Boys" else "Girls")
      )
    }
  })

  uploaded_data <- reactive({
    req(input$dataset_file)
    utils::read.csv(input$dataset_file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  })

  output$column_mapping_ui <- renderUI({
    req(uploaded_data())
    build_mapping_controls(names(uploaded_data()))
  })

  processed_dataset <- eventReactive(input$process_dataset, {
    req(uploaded_data())
    mapped_data <- apply_column_mapping(uploaded_data(), input)
    score_dataset_ref(mapped_data, all_ref, bp_ref = bp_ref)
  })

  output$dataset_preview <- renderTable({
    head(processed_dataset(), 20)
  }, digits = 2)

  output$download_dataset <- downloadHandler(
    filename = function() {
      paste0("iapgrowthr_output_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(processed_dataset(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
