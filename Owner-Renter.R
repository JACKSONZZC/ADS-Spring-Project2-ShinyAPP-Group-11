# Install library for data loading
install.packages("tidyverse")
install.packages("shiny")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("shinydashboard")
install.packages("tidyr")
install.packages("leaflet")
install.packages("plotly")
install.packages("sf")
install.packages("readr")
install.packages("shinyWidgets")
install.packages("maps")
install.packages("ggmap")
library(tidyverse)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyr)
library(plotly)
library(sf)
library(readr)
library(leaflet)
library(lubridate)
library(shinyWidgets)
library(maps)
library(ggmap)

# Choose working directory and load csv file
merge <- read_csv("merged_data.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Disaster Impact and FEMA Support Heatmap"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stateInput", "Select a State", choices = unique(merge$State)),
      sliderInput("damageInput", "Select Damage Range", min = min(merge$TotalDamage),
                  max = max(merge$TotalDamage), value = c(min(merge$TotalDamage), max(merge$TotalDamage))),
      selectInput("countyInput", "Select a County", choices = NULL),
      sliderInput("femaSupportInput", "Select FEMA Support Range",
                  min = 0,
                  max = 1000,
                  value = c(0,10000))
    ),
    mainPanel(
      leafletOutput("heatmap")
    )
  )
)




# Define Server
server <- function(input, output){
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      senView()
  })
}

# Generate ShinyAPP
shinyAPP(ui = ui, server = server)


