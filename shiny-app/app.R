library(shiny)

repo_root <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "lms.R"), local = TRUE)
source(file.path(repo_root, "R", "measures.R"), local = TRUE)
source(file.path(repo_root, "R", "ref_data.R"), local = TRUE)
source(file.path(repo_root, "R", "charts.R"), local = TRUE)
source(file.path(repo_root, "R", "percentile_score.R"), local = TRUE)

hw_ref <- read_iap_hw_ref(file.path(repo_root, "iap_ht__wt_percentile_ref.csv"))
bmi_ref <- read_iap_bmi_ref(file.path(repo_root, "iap_bmi_pecentiles_5-18.csv"))
wc_ref <- read_iap_wc_ref(file.path(repo_root, "iap_wc_ref.csv"))
all_ref <- rbind(hw_ref, bmi_ref, wc_ref)

ui <- fluidPage(
  titlePanel("iapgrowthr Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("measure", "Measure", c("height", "weight", "bmi", "wc")),
      selectInput("sex", "Sex", c("m", "f")),
      numericInput("age", "Age (years)", value = 10, min = 1, max = 18, step = 0.5),
      numericInput("value", "Observed value", value = 30, step = 0.1),
      actionButton("calc", "Calculate")
    ),
    mainPanel(
      h4("Estimated result"),
      tableOutput("score_table"),
      h4("Chart"),
      plotOutput("chart", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  score_data <- eventReactive(input$calc, {
    score_percentile_ref(
      ref_data = all_ref,
      measure = input$measure,
      sex = input$sex,
      age = input$age,
      value = input$value
    )
  }, ignoreInit = FALSE)

  output$score_table <- renderTable({
    score_data()
  }, digits = 2)

  output$chart <- renderPlot({
    obs <- data.frame(
      measure = input$measure,
      age = input$age,
      value = input$value,
      stringsAsFactors = FALSE
    )

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
    } else {
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
}

shinyApp(ui, server)
