# Load libraries
library(shiny)
library(plotly)
library(dplyr)
library(countrycode)
library(viridis)  # Add this line to load the viridis package
library(shinythemes)

# Read the data
covid_data <- read.csv("owid-covid-data.csv", stringsAsFactors = FALSE)

# Filter out ambiguous location names
valid_locations <- covid_data$location[countrycode(covid_data$location, "country.name", "iso3c", warn = FALSE) != ""]
covid_data <- covid_data[covid_data$location %in% valid_locations, ]
# Define asia_countries outside of filtered_data3 reactive expression
asia_countries <- unique(covid_data$location[covid_data$continent == "Asia"])
# Define UI
ui <- navbarPage(
  "COVID-19 Dashboard",
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(
      HTML("
        # .navbar-default { background-color: #1679AB; }
        # .navbar-default .navbar-nav > li > a { color: white; }
        .card { 
          box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1); 
          padding: 15px;
          margin-bottom: 15px;
          background-color: #ffffff;
        }
      ")
    )
  ),
  tabPanel("Global Overview",
           tabsetPanel(
             tabPanel("Global Map",
                      br(),
                      br(),
                      fluidRow(
                        sidebarPanel(
                          selectInput("age_filter1", "Filter by Age:",
                                      choices = c("Median Age", "Age 65+", "Age 70+"), selected = "Median Age"),
                          selectInput("sex_filter1", "Filter by Sex:",
                                      choices = c("Male", "Female"), selected = "Male"),
                          selectInput("metric_filter1", "Select Metric:",
                                      choices = c("Total Cases", "Total Deaths", "ICU admissions", "Hospital admissions",
                                                  "Total vaccinations", "Stringency Index"), selected = "Total Cases")
                        ),
                        mainPanel(
                          div(
                            class = "card",
                            plotlyOutput("world_map")
                          )
                        )
                      )
             ),
             tabPanel("Continent-wise Overview",
                      br(),
                      br(),
                      fluidRow(
                        sidebarPanel(
                          selectInput("age_filter2", "Filter by Age (for all plots):",
                                      choices = c("Median Age", "Age 65+", "Age 70+"), selected = "Median Age"),
                          selectInput("sex_filter2", "Filter by Sex (for all plots):",
                                      choices = c("Male", "Female"), selected = "Male"),
                          selectInput("metric_filter2", "Select Metric (for all plots):",
                                      choices = c("Total Cases", "Total Deaths", "ICU admissions", "Hospital admissions",
                                                  "Total vaccinations", "Stringency Index"), selected = "Total Cases"),
                          dateRangeInput("date_range2", "Select Date Range:",
                                         start = "2020-01-01", end = "2024-12-31",
                                         min = "2020-01-01", max = "2024-12-31")
                        ),
                        mainPanel(
                          div(
                            class = "card",
                            plotlyOutput("line_chart")
                          )
                        )
                      ),
                      br(),
                      fluidRow(
                        column(6, div(class = "card", plotlyOutput("bar_chart"))),
                        column(6, div(class = "card", plotlyOutput("pie_charts")))
                      )
             )
           )
  ),
  tabPanel("Asia Trends",
           br(),
           br(),
           fluidRow(
             sidebarPanel(
               uiOutput("country_selector"),  # Adding UI for country selection
               selectInput("age_filter3", "Filter by Age (for all plots):",
                           choices = c("Median Age", "Age 65+", "Age 70+"), selected = "Median Age"),
               selectInput("sex_filter3", "Filter by Sex (for all plots):",
                           choices = c("Male", "Female"), selected = "Male"),
               selectInput("metric_filter3", "Select Metric (for all plots):",
                           choices = c("Total Cases", "Total Deaths", 
                                       "Total vaccinations", "Stringency Index"), selected = "Total Cases")
             ),
             mainPanel(
               div(
                 class = "card",
                 plotlyOutput("bar_chart3")
               )
             )
           ),
           br(),
           
           fluidRow(
             column(6),
             column(6, div(class="card",plotlyOutput("pie_charts3")))
           )
           
  ),
  tabPanel("India Insights",
           br(),
           br(),
           fluidRow(
             sidebarPanel(
               selectInput("x_axis_feature", "X-axis Feature:",
                           choices = c("Total Cases", "Total Deaths",
                                       "Total vaccinations", "Stringency Index"), selected = "Total Cases"),
               selectInput("y_axis_feature", "Y-axis Feature:",
                           choices = c("Total Cases", "Total Deaths",
                                       "Total vaccinations", "Stringency Index"), selected = "Total Deaths"),
               selectInput("shape_feature", "Pointer Shape:", choices = c("Circle", "Square", "X"), selected = "Circle"),
               selectInput("color_feature", "Pointer Color:", choices = c("Red", "Green", "Blue"), selected = "Red"),
               sliderInput("size_feature", "Pointer size:", min = 1, max = 20, value = 10),
             ),
             mainPanel(
               div(
                 class = "card",
                 plotlyOutput("india_scatter_plot")
               )
             )
           )
  )
)

# Define server logic
server <- function(input, output) {
  # Filter data based on user inputs
  filtered_data1 <- reactive({
    data <- covid_data  # Replace with your actual data
    
    # Filter by metric
    if (input$metric_filter1 == "Total Cases") {
      data$total_cases <- data$total_cases
    } else if (input$metric_filter1 == "Total Deaths") {
      data$total_cases <- data$total_deaths
    } else if (input$metric_filter1 == "ICU admissions") {
      data$total_cases <- data$icu_patients
    } else if (input$metric_filter1 == "Hospital admissions") {
      data$total_cases <- data$hosp_patients
    } else if (input$metric_filter1 == "Total vaccinations") {
      data$total_cases <- data$total_vaccinations                        
    } else if (input$metric_filter1 == "Stringency Index") {
      data$total_cases <- data$stringency_index                        
    } 
    # Filter by age
    if (input$age_filter1 == "Median Age") {
      data$total_cases <- data$total_cases * mean(data$median_age, na.rm = TRUE) /100
    } else if (input$age_filter1 == "Age 65+") {
      data$total_cases <- data$total_cases * mean(data$aged_65_older, na.rm = TRUE) /100
    } else if (input$age_filter1 == "Age 70+") {
      data$total_cases <- data$total_cases * mean(data$aged_70_older, na.rm = TRUE) /100
    }
    
    # Filter by sex
    if (input$sex_filter1 == "Male") {
      data$total_cases <- data$total_cases * mean(data$male_smokers, na.rm = TRUE) /100
    } else if (input$sex_filter1 == "Female") {
      data$total_cases <- data$total_cases * mean(data$female_smokers, na.rm = TRUE) /100
    }
    
    
    
    # Remove "World" data
    data <- data[data$location != "World" & data$continent != "", ]
    data
  })
  
  
  filtered_data2 <- reactive({
    data <- covid_data  # Replace with your actual data
    # Filter by metric
    if (input$metric_filter2 == "Total Cases") {
      data$total_cases <- data$total_cases
    } else if (input$metric_filter2 == "Total Deaths") {
      data$total_cases <- data$total_deaths
    } else if (input$metric_filter2 == "ICU admissions") {
      data$total_cases <- data$icu_patients
    } else if (input$metric_filter2 == "Hospital admissions") {
      data$total_cases <- data$hosp_patients
    } else if (input$metric_filter2 == "Total vaccinations") {
      data$total_cases <- data$total_vaccinations                        
    } else if (input$metric_filter2 == "Stringency Index") {
      data$total_cases <- data$stringency_index                        
    } 
    
    # Filter by age
    if (input$age_filter2 == "Median Age") {
      data$total_cases <- data$total_cases * mean(data$median_age, na.rm = TRUE) /100
    } else if (input$age_filter2 == "Age 65+") {
      data$total_cases <- data$total_cases * mean(data$aged_65_older, na.rm = TRUE) /100
    } else if (input$age_filter2 == "Age 70+") {
      data$total_cases <- data$total_cases * mean(data$aged_70_older, na.rm = TRUE) /100
    }
    
    # Filter by sex
    if (input$sex_filter2 == "Male") {
      data$total_cases <- data$total_cases * mean(data$male_smokers, na.rm = TRUE) /100
    } else if (input$sex_filter2 == "Female") {
      data$total_cases <- data$total_cases * mean(data$female_smokers, na.rm = TRUE) /100
    }
    
    
    
    # Filter by date range
    if (!is.null(input$date_range2)) {
      start_date <- input$date_range2[1]
      end_date <- input$date_range2[2]
      data <- data[data$date >= start_date & data$date <= end_date, ]
    }
    
    # Remove "World" data
    data <- data[data$location != "World" & data$continent != "", ]
    data
  })
  
  # Aggregate total cases by continent for bar chart
  total_cases_by_continent <- reactive({
    data <- filtered_data2()
    
    # Aggregate total cases by continent
    total_cases <- data %>%
      group_by(continent) %>%
      summarise(total_cases = mean(total_cases, na.rm = TRUE), .groups="drop")
    
    total_cases
  })
  
  ########################################## TAB2 ######################################################
  # Create bar chart
  output$bar_chart <- renderPlotly({
    # Plot bar chart
    data <- total_cases_by_continent()
    
    # Replace continent labels
    data$continent <- ifelse(data$continent == "North America", "N. America",
                             ifelse(data$continent == "South America", "S. America", data$continent))
    
    plot_title <- paste(input$metric_filter2, " by Continent")   
    
    p <- plot_ly(data, x = ~continent, y = ~total_cases, type = "bar", marker = list(color = "blue"), width = 0.8) %>%
      layout(
        title = plot_title,
        xaxis = list(title = "Continent"),
        yaxis = list(title = input$metric_filter2),
        dragmode = "zoom",
        showlegend = FALSE
      )
    
    p
  })
  
  # Create line chart with date range filter
  output$line_chart <- renderPlotly({
    data <- filtered_data2()
    data$continent <- ifelse(data$continent == "North America", "N. America",
                             ifelse(data$continent == "South America", "S. America", data$continent))
    # Group data by continent and date
    grouped_data <- data %>%
      group_by(continent, date) %>%
      summarise(total_cases = mean(total_cases, na.rm = TRUE), .groups = 'drop')
    # Plot line chart
    plot_title <- paste(input$metric_filter2, " by Continent")  
    plot_ly(data = grouped_data, x = ~date, y = ~total_cases, color = ~continent, type = "scatter", mode = "lines") %>%
      layout(
        dragmode = "zoom",
        showlegend = TRUE,
        legend = list(orientation = "h",x = 0.5, y = -0.2),  # Adjust legend position
        title = plot_title,
        xaxis = list(
          title = "Date",
          tickmode = "array",  # Specify tick mode as array
          tickvals = c(min(grouped_data$date), max(grouped_data$date)),  # Set tick values to starting and ending dates
          ticktext = c(as.character(min(grouped_data$date)), as.character(max(grouped_data$date))),  # Set tick labels as strings of starting and ending dates
          range = c(min(grouped_data$date), max(grouped_data$date))
        ),
        yaxis = list(title = input$metric_filter2)  # Adjust y-axis title standoff
      )
  })
  
  # Create pie charts
  output$pie_charts <- renderPlotly({
    data <- filtered_data2()
    
    # Aggregate total cases by continent
    total_cases_by_continent <- data %>%
      group_by(continent) %>%
      summarise(total_cases = sum(total_cases, na.rm = TRUE))
    # print(total_cases_by_continent)
    # Filter data based on selected metric
    values <- total_cases_by_continent$total_cases
    # Create the pie chart
    title <- paste("Total", input$metric_filter2, "by Continent")
    pie <- plot_ly(labels = total_cases_by_continent$continent, values = values, type = "pie",
                   textinfo = "label+percent", marker = list(colors = viridis(6))) %>%
      layout(title = title)
    
    pie
  })
  
  ######################################## TAB1 #########################################
  # Create world map
  output$world_map <- renderPlotly({
    # Create data_world
    data <- filtered_data1()
    data_world <- data %>%
      group_by(location) %>%
      summarise(
        continent = first(continent),
        date = first(date),
        total_cases = mean(total_cases, na.rm = TRUE),
        total_deaths = mean(total_deaths, na.rm = TRUE),
        icu_patients = mean(icu_patients, na.rm = TRUE),
        hosp_patients = mean(hosp_patients, na.rm = TRUE),
        total_vaccinations = mean(total_vaccinations, na.rm = TRUE),
        stringency_index = mean(stringency_index, na.rm = TRUE),
        median_age = mean(median_age, na.rm = TRUE),
        aged_65_older = mean(aged_65_older, na.rm = TRUE),
        aged_70_older = mean(aged_70_older, na.rm = TRUE),
        male_smokers = mean(male_smokers, na.rm = TRUE),
        female_smokers = mean(female_smokers, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Get latitude and longitude for countries
    country_lat_lon <- data.frame(
      country = unique(covid_data$location),
      latitude = countrycode(unique(covid_data$location), "country.name", "iso3c", warn = FALSE),
      longitude = countrycode(unique(covid_data$location), "country.name", "iso3c", warn = FALSE)
    )
    
    # Merge data_world with country_lat_lon
    merged_data <- merge(data_world, country_lat_lon, by.x = "location", by.y = "country", all.x = TRUE)
    
    merged_data <- merged_data[!is.na(merged_data$total_cases), ]
    # Plot a world map
    fig <- plot_ly(
      data = merged_data,
      type = 'choropleth',
      locations = ~latitude,
      z = ~total_cases,
      text = ~paste(location, "<br>",input$metric_filter1, total_cases),
      colorscale = 'Viridis',
      reversescale = TRUE,
      colorbar = list(title = input$metric_filter1)
    ) %>%
      layout(
        title = 'Total COVID-19 Cases Across the World',
        geo = list(
          outline = list(color = 'black', width = 1)
          
        )
      )
    
    fig
  })
  
  ############################## for Asia #########################################
  
  # Filter data based on user inputs
  filtered_data3 <- reactive({
    data <- covid_data  # Replace with your actual data
    # Filter by metric
    if (input$metric_filter3 == "Total Cases") {
      data$total_cases <- data$total_cases
    } else if (input$metric_filter3 == "Total Deaths") {
      data$total_cases <- data$total_deaths
    } else if (input$metric_filter3 == "Total vaccinations") {
      data$total_cases <- data$total_vaccinations                        
    } else if (input$metric_filter3 == "Stringency Index") {
      data$total_cases <- data$stringency_index                        
    } 
    
    # Filter by age
    if (input$age_filter3 == "Median Age") {
      data$total_cases <- data$total_cases * mean(data$median_age, na.rm = TRUE) /100
    } else if (input$age_filter3 == "Age 65+") {
      data$total_cases <- data$total_cases * mean(data$aged_65_older, na.rm = TRUE) /100
    } else if (input$age_filter3 == "Age 70+") {
      data$total_cases <- data$total_cases * mean(data$aged_70_older, na.rm = TRUE) /100
    }
    
    # Filter by sex
    if (input$sex_filter3 == "Male") {
      data$total_cases <- data$total_cases * mean(data$male_smokers, na.rm = TRUE) /100
    } else if (input$sex_filter3 == "Female") {
      data$total_cases <- data$total_cases * mean(data$female_smokers, na.rm = TRUE) /100
    }
    
    
    
    # Remove "World" data
    data <- data[data$location != "World" & data$continent != "", ]
    
    # Step 1: Create a list of all countries in Asia
    # asia_countries <- unique(data$location[data$continent == "Asia"])
    
    # Step 2: Create a new dataset with information of countries in Asia
    asia_data <- data[data$location %in% asia_countries, ]
    
    # Step 3: Reduce repeated countries to 1 and take average of numerical attributes
    asia_data <- asia_data %>%
      group_by(location) %>%
      summarise(
        continent = first(continent),
        date = first(date),
        total_cases = mean(total_cases, na.rm = TRUE),
        total_deaths = mean(total_deaths, na.rm = TRUE),
        icu_patients = mean(icu_patients, na.rm = TRUE),
        hosp_patients = mean(hosp_patients, na.rm = TRUE),
        total_vaccinations = mean(total_vaccinations, na.rm = TRUE),
        stringency_index = mean(stringency_index, na.rm = TRUE),
        median_age = first(median_age),
        aged_65_older = first(aged_65_older),
        aged_70_older = first(aged_70_older),
        male_smokers = first(male_smokers),
        female_smokers = first(female_smokers),
        .groups = 'drop'
      )
    
    asia_data
  })
  
  # Add a multiple select options box for countries in Asia
  output$country_selector <- renderUI({
    selectInput("countries_select3", "Select Countries:",
                choices = asia_countries, selected = "India", multiple = TRUE)
  })
  
  # Create bar chart for Tab 3
  output$bar_chart3 <- renderPlotly({
    data <- filtered_data3()
    
    # Filter data for selected countries
    selected_countries <- input$countries_select3
    if (!"India" %in% selected_countries) {
      selected_countries <- c("India", selected_countries)
    }
    data <- data[data$location %in% selected_countries, ]
    
    # Plot bar chart
    t <- paste(input$metric_filter3, "by Country")
    plot_ly(data, x = ~location, y = ~total_cases, type = "bar", marker = list(color = viridis(length(selected_countries)))) %>%
      layout(title = t,
             xaxis = list(title = "Country"),
             yaxis = list(title = input$metric_filter3))
  })
  
  
  # Create pie charts for Tab 3
  output$pie_charts3 <- renderPlotly({
    data <- filtered_data3()
    
    # Filter data for selected countries
    selected_countries <- input$countries_select3
    if (!"India" %in% selected_countries) {
      selected_countries <- c("India", selected_countries)
    }
    data <- data[data$location %in% selected_countries, ]
    
    # Plot pie chart
    t <- paste(input$metric_filter3, "by Country") 
    plot_ly(data, labels = ~location, values = ~total_cases, type = "pie") %>%
      layout(title = t)
  })
  
  ################################## for India ############################################
  # Filter data for India based on selected features
  filtered_data_india <- reactive({
    data <- covid_data  # Replace with your actual data
    # Filter data for India
    india_data <- data[data$location == "India", ]
    
    
    
    # Filter by selected features for x-axis, y-axis, color, size, and shape
    # india_data <- india_data[, c("date", input$x_axis_feature, input$y_axis_feature, input$color_feature, input$size_feature, input$shape_feature)]
    india_data
  })
  
  # Create scatter plot for India
  output$india_scatter_plot <- renderPlotly({
    data <- filtered_data_india()
    # Plot scatter plot for India
    shape_feature <- switch(input$shape_feature, "Circle" = "circle", "Square" = "square", "X" = "x")
    color_feature <- input$color_feature
    size_feature <- input$size_feature
    # print(input$x_axis_feature)
    # print(input$y_axis_feature)
    
    #set value for x and y axis features
    #x axis
    if (input$x_axis_feature == "Total Cases") {
      x_axis <- data$total_cases
    } else if (input$x_axis_feature == "Total Deaths") {
      x_axis <- data$total_deaths
    } else if (input$x_axis_feature == "Total vaccinations") {
      x_axis <- data$total_vaccinations                        
    } else if (input$x_axis_feature == "Stringency Index") {
      x_axis <- data$stringency_index                        
    } 
    #y axis
    if (input$y_axis_feature == "Total Deaths") {
      y_axis <- data$total_deaths
    } else if (input$y_axis_feature == "Total Cases") {
      y_axis <- data$total_cases
    } else if (input$y_axis_feature == "Total vaccinations") {
      y_axis <- data$total_vaccinations                        
    } else if (input$y_axis_feature == "Stringency Index") {
      y_axis <- data$stringency_index                        
    } 
    
    plot_title <- paste("India Scatter Plot:", input$x_axis_feature, "vs", input$y_axis_feature)
    p <- plot_ly(data, x = x_axis, y = y_axis, type = "scatter",mode = "markers", size = I(size_feature),
                 marker = list(symbol = shape_feature, color=color_feature,line = list(color = color_feature, width = 1))) %>%
      layout(title = plot_title,
             xaxis = list(title = input$x_axis_feature),
             yaxis = list(title = input$y_axis_feature))
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)