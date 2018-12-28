library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(shinyjs)

# Data Read ---------------------------------------------------------------
country_code <- read_csv("data/country_code.csv") %>% 
  rename(country = "COUNTRY", code = "CODE")

parameters <- c("CPI Price" = "cpi",
                "GDP" = "gdp",
                "Industrial Production" = "indust",
                "Exports" = "exports",
                "Imports" = "imports")

files <- c("cpi", "gdp", "indust", "imports", "exports")

for (i in seq_along(files)) {
  full_path <- paste0("data/", files[i], ".xlsx")
  if (file.exists(full_path)) {
    assign(files[i], read_excel(full_path))
    assign(files[i], get(files[i]) %>% 
             rename(year = "X__1") %>% 
             slice(2:n()) %>%
             gather(-year, key = "country", value = "value") %>% 
             inner_join(country_code, by = "country") %>% 
             select(year, country, value, code) %>% 
             mutate(value = as.numeric(value)))
  }
}

# UI definition -----------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  shinyUI(navbarPage
          ("Global Economy Monitor",
            # Tab 1 -------------------------------------------------------------------
            tabPanel("Global Summary",
                     column(4, wellPanel( 
                       helpText("Control the graph and table data"),
                       # Choose Parameter --------------------------------------------------------
                       selectInput('parameter', 'Choose parameter to view', choices = NULL),
                       br(),
                       # Radio Buttons -----------------------------------------------------------
                       fluidRow(column(7, 
                                       radioButtons("input_type",
                                                    "Choose Input",
                                                    c("Average" = "avg_wise",
                                                      "Year-Wise" = "year_wise"), 
                                                    selected = "avg_wise"))),
                       br(),
                       # Year Input --------------------------------------------------------------
                       sliderInput("avg_wise_range", "Choose year range for average",
                                   min = 0, max = 0, 
                                   value = c(0,0),
                                   sep = "", step = 1),
                       selectInput('year_wise_year', 'Choose year', choices = NULL)),
                       helpText("The World Bank's Development Prospects Group conducts in-depth analysis of key global 
                                macroeconomic developments and their impact on World Bank member countries.", br(),
                                "Find more information in the Background section")),
                     # Output ------------------------------------------------------------------
                     mainPanel(
                       textOutput("map_title_1"),
                       plotlyOutput("map_plot"),
                       htmlOutput("summary_info"),
                       br(),
                       DT::dataTableOutput("overall_table"))),
            # Tab 2 -------------------------------------------------------------------
            tabPanel("Per-Country",
                     column(4, wellPanel( 
                       helpText("View the statistics over the years"),
                       selectInput("country", "Choose Country",
                                   multiple = TRUE, choices = NULL),
                       helpText("Data may not be available for all countries"),
                       checkboxGroupInput("params", label = "Choose parameters to plot",
                                          choices = parameters, selected = "cpi"))),
                     # Output ------------------------------------------------------------------
                     mainPanel(
                       plotlyOutput("country_plot_cpi"), br(),
                       plotlyOutput("country_plot_gdp"), br(),
                       plotlyOutput("country_plot_indust"), br(),
                       plotlyOutput("country_plot_exports"), br(),
                       plotlyOutput("country_plot_imports"))),
            tabPanel("Background", 
                     h3("Global Economic Monitoring"),
                     p("The dataset is maintained by the World Bank and is available from",
                       a("Kaggle.", href = "https://www.kaggle.com/theworldbank/global-economic-monitor")),
                     br(),
                     p("The following economic parameters are available for 140 countries, between the years 1989 to 2018."),
                     tags$ul(
                       tags$li((a("Consumer Price Index", href = "https://www.investopedia.com/terms/c/consumerpriceindex.asp")), 
                               "- A measure of inflation - Percent increase in living expense year over year."), 
                       tags$li((a("Gross Domestic Product", href = "https://www.investopedia.com/terms/g/gdp.asp")), 
                               "- The monetary value of a country's economic output."), 
                       tags$li((a("Industrial Production", href = "https://www.investopedia.com/university/releases/productioncapacity.asp")), 
                               "- A economic indicator measuring real output in the industrial manufacturing."), 
                       tags$li((a("Imports Merchandise", href = "https://datacatalog.worldbank.org/imports-merchandise-customs-price-us-seas-adj")), 
                               "- TT=he price index of Merchandise (goods) imports, cost, insurance and freight basis (c.i.f.)."), 
                       tags$li((a("Exports Merchandise", href = "https://datacatalog.worldbank.org/exports-merchandise-customs-price-us-seas-adj")), 
                               "- TThe price index of Merchandise (goods) exports free on board (f.o.b.).")
                     ),
                     p("This dashboard was made by Harish M.", br(),
                       "The code is available on", a("GitHub.", href = "https://github.com/mr-hn/economyMonitor"))
            ) 
          )))

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Pre-fill Inputs ---------------------------------------------------------
  input_country <- reactive({
    input_country <- bind_rows(cpi,
                               gdp,
                               imports,
                               exports,
                               indust) %>%
      distinct(country) %>% arrange(country)
  })
  input_years <- reactive({
    input_years <- cpi %>%
      distinct(year) %>% arrange(year)
  })
  observe({
    updateSelectInput(session = session, 
                      inputId = "country", 
                      choices = input_country()$country,
                      selected = c("United States", "United Kingdom"))
  })
  observe({
    updateSelectInput(session = session, 
                      inputId = "parameter", 
                      choices = parameters)
  })
  observe({
    if (input$input_type == "year_wise") {
      updateSelectInput(session = session, 
                        inputId = "year_wise_year", 
                        choices = input_years()$year)
    } else {
      updateSelectInput(session = session, 
                        inputId = "year_wise_year", 
                        choices = " ")
    }
  })
  observe({
    if (input$input_type == "avg_wise") {
      updateSliderInput(session = session, 
                        inputId = "avg_wise_range", 
                        min = input_years()$year[1], 
                        max = input_years()$year[nrow(input_years())],
                        value = c(input_years()$year[1],
                                  input_years()$year[nrow(input_years())]))
    } else {
      updateSliderInput(session = session, 
                        inputId = "avg_wise_range", 
                        min = 0, max = 0,
                        value = c(0,0))
    }
  })
  
  # Hide Plots Dynamically --------------------------------------------------
  observeEvent(input$params, {
    for (i in seq_along(parameters)) {
      plot_name <- paste0("country_plot_", parameters[i])
      if (parameters[i] %in% input$params) {
        show(plot_name) }
      else{
        hide(plot_name)
      }}
  })
  
  # Output Titles -----------------------------------------------------------
  output$map_title_1 <- renderText({
    if (input$parameter == "cpi") {
      paste("CPI Price")
    } else if (input$parameter == "gdp") {
      paste("GDP - Market Prices, in millions of US Dollars")
    } else if (input$parameter == "indust") {
      paste("Industrial Production - constant 2010, in US Dollars")
    } else if (input$parameter == "imports") {
      paste("Imports - Merchandise, Customs, Price, in million US Dollars")
    } else if (input$parameter == "exports") {
      paste("Exports - Merchandise, Customs, Price, in million US Dollars")
    }
  })
  
  # Map Plot ----------------------------------------------------------------
  output$map_plot <- renderPlotly({
    
    req(input$parameter)
    data <- get(input$parameter)
    
    # Get year/year-range based on parameter
    if (input$input_type == "avg_wise") {
      begin_year <- as.numeric(input$avg_wise_range[1])
      end_year <- as.numeric(input$avg_wise_range[2])
    } else if (input$input_type == "year_wise") {
      begin_year <- as.numeric(input$year_wise_year)
      end_year <- as.numeric(input$year_wise_year)
    }
    
    # Plotly settings
    l <- list(color = toRGB("grey"), width = 0.5)
    g <- list(showframe = FALSE,
              showcoastlines = FALSE,
              projection = list(type = 'Mercator'))
    
    # Filter data
    data %>% 
      filter(year <= end_year & year >= begin_year) %>%
      group_by(country, code) %>% 
      summarize(Average = round(mean(value, na.rm = TRUE),2)) %>% 
      # Plotly map chart --------------------------------------------------------
    plot_geo() %>%
      add_trace(
        z = ~Average, color = ~Average, colors = 'Blues',
        text = ~country, locations = ~code, marker = list(line = l)
      ) %>% 
      colorbar(title = "Scale") %>%
      layout(geo = g)
  })
  
  # Summary Stats -----------------------------------------------------------
  output$summary_info <- renderUI({
    
    req(input$parameter)
    data <- get(input$parameter)
    
    # Get year/year-range based on parameter
    if (input$input_type == "avg_wise") {
      begin_year <- as.numeric(input$avg_wise_range[1])
      end_year <- as.numeric(input$avg_wise_range[2])
      
      data <- data %>% 
        filter(year <= end_year & year >= begin_year) %>%
        group_by(country, code) %>% 
        summarize(Average = round(mean(value, na.rm = TRUE),2)) %>% 
        arrange(desc(Average))
      
      paste("The country with the highest average between the years",
            begin_year, "and", end_year,
            "is", data$country[1], "with a", names(parameters[which(parameters == input$parameter)]),
            "of", data$Average[1])
      
    } else if (input$input_type == "year_wise") {
      begin_year <- as.numeric(input$year_wise_year[1])
      
      data <- data %>% 
        filter(year == begin_year) %>%
        group_by(country, code) %>% 
        summarize(Average = round(mean(value, na.rm = TRUE),2)) %>% 
        arrange(desc(Average))
      
      paste("The country with the highest",
            names(parameters[which(parameters == input$parameter)]),
            "for the year", begin_year, "is", data$country[1], 
            "with a value of", data$Average[1])
    }
  })
  
  # Map Table ---------------------------------------------------------------
  output$overall_table <- DT::renderDataTable({
    
    req(input$parameter)
    data <- get(input$parameter)
    
    # Get year/year-range based on parameter
    if (input$input_type == "avg_wise") {
      begin_year <- as.numeric(input$avg_wise_range[1])
      end_year <- as.numeric(input$avg_wise_range[2])
    } else if (input$input_type == "year_wise") {
      begin_year <- as.numeric(input$year_wise_year)
      end_year <- as.numeric(input$year_wise_year)
    }
    
    data %>%
      filter(year <= end_year & year >= begin_year) %>%
      group_by(country) %>%
      summarize(Stat = round(mean(value, na.rm = TRUE),2)) %>%
      rename(Country = "country") %>% 
      filter(Stat >= 0) %>% 
      DT::datatable(options = list(pageLength = 10),
                    rownames = FALSE)
  })
  
  # Tab 2 Plots -------------------------------------------------------------
  output$country_plot_cpi <- renderPlotly({
    req(input$country, input$params)
    if (parameters[1] %in% input$params) {
      data <- cpi %>% filter(country == input$country)
      if (nrow(data) != 0) {
        data %>% plot_ly(x = ~year, y = ~value, color = ~country,
                         type = 'scatter', mode = 'lines+markers') %>% 
          layout(xaxis = list(title = "Year"),
                 yaxis = list(title = "CPI"),
                 title = "Time Series plot of CPI")
      } }
  })
  
  output$country_plot_gdp <- renderPlotly({
    req(input$country, input$params)
    if (parameters[2] %in% input$params) {
      data <- gdp %>% filter(country == input$country)
      if (nrow(data) != 0) {
        data %>% plot_ly(x = ~year, y = ~value, color = ~country,
                         type = 'scatter', mode = 'lines+markers') %>% 
          layout(xaxis = list(title = "Year"),
                 yaxis = list(title = "GDP"),
                 title = "Time Series plot of GDP")
      }}
  })
  
  output$country_plot_indust <- renderPlotly({
    req(input$country, input$params)
    if (parameters[3] %in% input$params) {
      data <- indust %>% filter(country == input$country)
      if (nrow(data) != 0) {
        data %>% plot_ly(x = ~year, y = ~value, color = ~country,
                         type = 'scatter', mode = 'lines+markers') %>% 
          layout(xaxis = list(title = "Year"),
                 yaxis = list(title = "Industrial Production"),
                 title = "Time Series plot of Industrial Production")
      }}
  })
  
  output$country_plot_exports <- renderPlotly({
    req(input$country, input$params)
    if (parameters[4] %in% input$params) {
      data <- exports %>% filter(country == input$country)
      if (nrow(data) != 0) {
        data %>% plot_ly(x = ~year, y = ~value, color = ~country,
                         type = 'scatter', mode = 'lines+markers') %>% 
          layout(xaxis = list(title = "Year"),
                 yaxis = list(title = "Exports"),
                 title = "Time Series plot of Exports")
      }}
  })
  
  output$country_plot_imports <- renderPlotly({
    req(input$country, input$params)
    if (parameters[5] %in% input$params) {
      data <- imports %>% filter(country == input$country)
      if (nrow(data) != 0) {
        data %>% plot_ly(x = ~year, y = ~value, color = ~country,
                         type = 'scatter', mode = 'lines+markers') %>% 
          layout(xaxis = list(title = "Year"),
                 yaxis = list(title = "Imports"),
                 title = "Time Series plot of Imports")
      }}
  })
}

shinyApp(ui = ui, server = server)