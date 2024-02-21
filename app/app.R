# Installing packages
packages <- c("shiny", "shinydashboard", "leaflet", "DT", "dplyr", "readr", 
              "ggplot2", "lubridate", "tidyr", "plotly", "sf", "shinyWidgets", 
              "maps", "ggmap", "magrittr", "mapview", "leafsync", "tidyverse")

# Loading packages
for(package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  
  library(package, character.only = TRUE)
}

# Load merged dataset
data <- read_csv("../data/Support_Efficiency_Shiny.csv")

################################################################################
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Disaster Assistance Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Incident Type Overview", tabName = "mapView", icon = icon("globe")),
      menuItem("Renter/Owner Support", tabName = "dataTab", icon = icon("table")),
      menuItem("Historical Overview by ZIP Code", tabName = "locationSelection", icon = icon("list")),
      menuItem("More Info", tabName = "moreInfo", icon = icon("book")),
      menuItem("About Us", tabName = "aboutUs", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Map View Tab
      tabItem(tabName = "mapView",
              fluidPage(
                titlePanel("Disaster Assistance by Incident Type"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("incidentType", "Choose an incident type:", choices = unique(data$`Incident Type`),
                                selectize = FALSE, width = '250px')
                  ),
                  mainPanel(
                    leafletOutput("map", height = "800px")
                  )
                )
              )
      ),
      # About Us Tab
      tabItem(tabName = "aboutUs",
              h2("About Us"),
              p("Welcome to the Disaster Assistance Dashboard, a project meticulously crafted by Zichen Zhao, with the mission to enhance transparency and understanding of disaster assistance across the United States. Our endeavor seeks to dissect and present the intricate dynamics of disaster impact and FEMA's assistance to both homeowners and renters."),
              p("Our Mission: We are committed to empowering individuals, communities, and policymakers with actionable insights into the complexities of disaster support mechanisms. This dashboard is our tool to illuminate the efficiency of disaster support across various regions, aiming to pinpoint areas where FEMA's assistance has been most effective and identify those that require further attention and resources."),
              p("Background: The backbone of our project is a team of dedicated data scientists, developers, and disaster management experts. United by a common goal, we leverage cutting-edge technology and a rigorous analytical approach to address and elucidate critical societal challenges posed by natural disasters."),
              p("Our Process: The creation of this dashboard involved an extensive data cleaning and merging process, utilizing datasets from FEMA's Open Data platform and enhancing them with geographical data through Google's Geocoding API. This meticulous preparation has allowed us to develop the 'Support Efficiency' metric, a novel measure designed to evaluate the balance between FEMA's allocated assistance and the actual needs of disaster-affected populations."),
              p("Contact Us: We welcome your inquiries, suggestions, or feedback. Please reach out at xxxxxx@columbia.edu. Your input is invaluable to us as we continually strive to enhance the functionality and impact of our dashboard."),
              p("Appreciation: Thank you for visiting our dashboard. It is our firm belief that through collective effort and shared knowledge, we can significantly improve disaster response strategies and ultimately make a meaningful difference in the recovery processes of affected communities.")
      ),
      # More Info Tab
      tabItem(tabName = "moreInfo",
              h2("More Information"),
              p("Our Disaster Assistance Dashboard is designed to provide an in-depth analysis of disaster impact and assistance across the United States, focusing on both owners and renters affected by disasters. This project aims to elucidate the specifics of disaster occurrences, the timing of these events, and the extent of FEMA's assistance in different regions."),
              p("Project Goal: The primary objective of this project is to extract and analyze data regarding the conditions of disaster-stricken homeowners and renters, including their corresponding areas, the nature and timing of disasters, and the amount of assistance provided by FEMA. By computing the 'Support Efficiency' metric, which represents the difference between the funds allocated by FEMA and the actual financial needs of the affected individuals, we aim to reveal the surplus of FEMA funds remaining post-disaster. This surplus can be instrumental for future disaster response planning, allowing local governments and investors to allocate resources more effectively."),
              p("Data Sources: Our comprehensive dataset is derived from FEMA's Open Data, incorporating detailed records from 'FemaWebDisasterDeclarations.csv', 'HousingAssistanceOwners.csv', and 'HousingAssistanceRenters.csv'. The integration of these datasets, enhanced with geographical coordinates via Google's Geocoding API, forms the basis of our 'Support Efficiency' analysis."),
              p("Data Preparation: We undertook a meticulous data cleaning and merging process to ensure the integrity and utility of our dataset. This preparatory work allows us to precisely calculate support efficiency metrics, thereby shedding light on the effectiveness of disaster support distribution."),
              p("Insights: The dashboard facilitates a nuanced understanding of how disaster assistance is distributed among affected populations, highlighting areas where the allocation of FEMA funds has been particularly effective or areas where improvements are needed. By identifying regions with significant support efficiency surplus, stakeholders can strategize on the optimal use of resources for enhancing disaster resilience."),
              p("Features:"),
              tags$ul(
                tags$li("Interactive Maps: Explore the geographic distribution of disaster assistance."),
                tags$li("Data Tables: Delve into specific data points by selecting various criteria."),
                tags$li("Historical Analysis: Track changes in support efficiency over time."),
                tags$li("Custom Metrics: Utilize our 'Support Efficiency' metric to assess FEMA's financial impact."),
                tags$li("Visualization Tools: Engage with dynamic and static visualizations for comprehensive insights.")
              ),
              p("Designed for researchers, policymakers, local governments, and investors, this dashboard empowers stakeholders with actionable data to inform disaster response strategies and investment decisions.")
      ),
      # Data Tab
      tabItem(tabName = "dataTab", 
              fluidPage(
                titlePanel("Data Details"),
                leafletOutput("interactiveMap", height = "500px"), # Map output for interactive map
                DTOutput("dataDetails") # Table output for showing details
              )
      ),
      # New tab for location selection
      tabItem(tabName = "locationSelection",
              fluidRow(
                column(3,
                       selectInput("selectedState", "Select a state:", choices = unique(data$`State Name`))
                ),
                column(3,
                       selectInput("selectedCounty", "Select a county:", choices = NULL)
                ),
                column(3,
                       selectInput("selectedCity", "Select a city:", choices = NULL)
                ),
                column(3,
                       selectInput("selectedZipCode", "Select a ZIP code:", choices = NULL)
                )
              ),
              fluidRow(
                column(12,
                       DTOutput("locationDataTable")
                )
              ),
              fluidRow(
                column(12,
                       plotlyOutput("supportEfficiencyBarChart")
                )
              )
      )
    )
  )
)

################################################################################
# Define server
server <- function(input, output, session) {
  # Server for Incident Type Overview
  # Render the map based on incident type selection
  output$map <- renderLeaflet({
    filteredData <- data %>% filter(`Incident Type` == input$incidentType)
    
    # Calculate color based on Support Efficiency
    pal <- colorNumeric(palette = c("red", "green"), domain = filteredData$Support_Efficiency)
    
    leaflet(filteredData) %>%
      addTiles() %>%
      addCircleMarkers(~lon, ~lat, popup = ~paste("State Name:", `State Name`, "<br>",
                                                  "City:", City, "<br>",
                                                  "County:", County, "<br>",
                                                  "Support Efficiency:", Support_Efficiency),
                       fillColor = ~pal(Support_Efficiency), color = ~pal(Support_Efficiency), fillOpacity = 0.8, stroke = TRUE) %>%
      addLegend("bottomright", pal = pal, values = ~Support_Efficiency,
                title = "Support Efficiency", labFormat = labelFormat(suffix = ""),
                opacity = 1)
  })
  
  # Server for Renter/Owner Support
  # Render the interactive map for the "Data" tab with dynamic coloring
  output$interactiveMap <- renderLeaflet({
    pal <- colorNumeric(palette = c("red", "green"), domain = data$Support_Efficiency)
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~paste("Zip Code:", `Zip Code`, "<br>", "County:", County, "<br>", "State:", `State Name`, "<br>", "City:", City),
                       fillColor = ~pal(Support_Efficiency), color = ~pal(Support_Efficiency), fillOpacity = 0.8, stroke = TRUE,
                       layerId = ~paste(`State Name`, County, City, `Zip Code`)) %>%
      addLegend("bottomright", pal = pal, values = ~Support_Efficiency,
                title = "Support Efficiency", labFormat = labelFormat(suffix = ""),
                opacity = 1)
  })
  
  # Render the table showing detailed data with selected columns only
  output$dataDetails <- renderDT({
    req(input$interactiveMap_marker_click)
    # Interactive table and map function within Renter/Owner Support
    clickedMarker <- input$interactiveMap_marker_click$id
    selectedData <- data %>%
      filter(paste(`State Name`, County, City, `Zip Code`) == clickedMarker) %>%
      select(`Incident Begin Date`, `Incident End Date`, `Incident Type`, `Support_Efficiency`, 
             `Total Damage`, `Total Approved IHP Amount of Owner`, `Repair/Replace Amount of Owner`, 
             `Rental Amount of Owner`, `Other Needs Amount of Owner`, 
             `Total Approved IHP Amount of Renter`, `Repair/Replace Amount of Renter`, 
             `Rental Amount of Renter`, `Other Needs Amount of Renter`, 
             `Total_Support_Provided`, `Total_IHP_Approved_Amount`) 
    
    datatable(selectedData, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  #Server for Historical Map Review
  # Update county selection based on the selected state
  observe({
    county_choices <- data %>% filter(`State Name` == input$selectedState) %>% pull(County)
    updateSelectInput(session, "selectedCounty", choices = unique(county_choices))
  })
  
  # Update city selection based on the selected state and county
  observe({
    city_choices <- data %>% filter(`State Name` == input$selectedState, County == input$selectedCounty) %>% pull(City)
    updateSelectInput(session, "selectedCity", choices = unique(city_choices))
  })
  
  # Update ZIP code selection based on the selected state, county, and city
  observe({
    zip_code_choices <- data %>% filter(`State Name` == input$selectedState, County == input$selectedCounty, City == input$selectedCity) %>% pull(`Zip Code`)
    updateSelectInput(session, "selectedZipCode", choices = unique(zip_code_choices))
  })
  
  # Render the table for location selection
  output$locationDataTable <- renderDT({
    req(input$selectedZipCode)
    selectedData <- data %>% filter(`State Name` == input$selectedState, 
                                    County == input$selectedCounty, 
                                    City == input$selectedCity,
                                    `Zip Code` == input$selectedZipCode) %>%
      mutate(Support_Efficiency = log(Support_Efficiency)) # Apply logarithm transformation to Support Efficiency
    
    datatable(selectedData, options = list(scrollX = TRUE, autoWidth = TRUE))
  })
  
  # Render the bar chart for support efficiency by incident begin date
  output$supportEfficiencyBarChart <- renderPlotly({
    req(input$selectedZipCode)
    selectedData <- data %>% filter(`State Name` == input$selectedState, 
                                    County == input$selectedCounty, 
                                    City == input$selectedCity,
                                    `Zip Code` == input$selectedZipCode)
    
    p <- ggplot(selectedData, aes(x = `Incident Begin Date`, y = log(Support_Efficiency), fill = `Incident Type`)) + # Apply logarithm transformation to Support Efficiency
      geom_bar(stat = "identity") +
      labs(x = "Incident Begin Date", y = "Log Support Efficiency", fill = "Incident Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

################################################################################
# Run the application 
shinyApp(ui = ui, server = server)