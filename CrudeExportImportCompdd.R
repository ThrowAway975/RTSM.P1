library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr) # Added for pivot_wider
library(shinycssloaders)
library(scales) # For currency and percentage formatting

# Change the file path to where your CSV file is stored
filePath <- "C:\\Users\\Y\\Desktop\\Files\\MBA\\RTSM\\CrudeOilImportAndPetroleumProductImportExportByOilCompanies.csv"

oil_data <- read_csv(filePath, show_col_types = FALSE) # Added show_col_types = FALSE to quiet column specification message

# Adjust your 'Month Year' to 'date' conversion accordingly
oil_data$date <- as.Date(paste0(oil_data$Year, "-", oil_data$Month, "-01"), format="%Y-%B-%d")

ui <- dashboardPage(
  dashboardHeader(title = "Crude Oil and Petroleum Products Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datasets", tabName = "datasets", icon = icon("table")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Analysis", tabName = "analysis", icon = icon("search")),
      menuItem("Statistics", tabName = "stats", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets",
              fluidRow(
                box(title = "Dataset Overview", status = "primary", solidHeader = TRUE,
                    dataTableOutput("datasetTable") %>% withSpinner())
              )
      ),
      tabItem(tabName = "visualizations",
              fluidRow(
                box(title = "Trade Type Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("tradePieChart") %>% withSpinner()),
                box(title = "Monthly Import/Export Value in USD", status = "info", solidHeader = TRUE,
                    plotlyOutput("monthlyValueBarChart") %>% withSpinner())
              ),
              fluidRow(
                box(title = "Product Volume Heatmap", width = 12,
                    plotlyOutput("productHeatmap") %>% withSpinner())
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Quantity and Value Over Time", status = "danger", solidHeader = TRUE,
                    plotlyOutput("timeSeries") %>% withSpinner())
              ),
              fluidRow(
                box(title = "Filter by Product", status = "primary", solidHeader = TRUE,
                    selectInput("productFilter", "Choose a product:", choices = unique(oil_data$PRODUCTS)),
                    plotlyOutput("filteredTimeSeries") %>% withSpinner())
              )
      ),
      tabItem(tabName = "stats",
              fluidRow(
                box(title = "Summary Statistics", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("summaryStats") %>% withSpinner())
              )
      )
    )
  )
)

server <- function(input, output) {

  output$datasetTable <- renderDataTable({
    oil_data
  })

  output$tradePieChart <- renderPlotly({
    data_pie <- oil_data %>%
      count(TRADE) %>%
      mutate(perc = n / sum(n))

    plot_ly(data_pie, labels = ~TRADE, values = ~perc, type = 'pie', textinfo = 'label+percent',
            marker = list(colors = RColorBrewer::brewer.pal(4, "Set3"))) %>%
      layout(title = 'Trade Type Distribution')
  })

  output$monthlyValueBarChart <- renderPlotly({
    data_bar <- oil_data %>%
      group_by(date) %>%
      summarise(TotalValueUSD = sum(`Value.in.Dollars..Million.US.dollar.`)) %>%
      arrange(desc(date))

    plot_ly(data_bar, x = ~date, y = ~TotalValueUSD, type = 'bar', marker = list(color = 'rgba(76, 175, 80, 0.6)')) %>%
      layout(title = 'Monthly Import/Export Value in USD',
             xaxis = list(title = "Date"),
             yaxis = list(title = "Value in Million USD"))
  })

  output$productHeatmap <- renderPlotly({
    data_heatmap <- oil_data %>%
      count(Month, PRODUCTS, name = "Volume") %>%
      pivot_wider(names_from = PRODUCTS, values_from = Volume, values_fill = list(Volume = 0)) # Updated

    plot_ly(data_heatmap, z = ~`CRUDE OIL`, x = ~Month, type = "heatmap", colors = colorRamp(c("blue", "red"))) %>%
      layout(title = "Volume Heatmap by Product and Month")
  })



  output$summaryStats <- renderPrint({
    summary(oil_data %>% select(`Quantity (000 Metric Tonnes)`, `Value in Rupees (Crore)`, `Value in Dollars (Million US dollar)`))
  })

}

shinyApp(ui, server)
