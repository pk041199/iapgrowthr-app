# Shiny app

This app reads the package code directly from the parent repository.

## Local run

From the repository root in R:

```r
shiny::runApp("shiny-app")
```

Or from terminal:

```powershell
Rscript -e "shiny::runApp('shiny-app', host='0.0.0.0', port=8080)"
```

## Deploy

If you deploy only `shiny-app/`, the app will fail because it expects the parent `R/` folder and CSV files.

Deploy the repository contents together, or refactor the app to use an installed package instead of sourcing local files.
