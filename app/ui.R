# TEST UI MERGE
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
    leafletOutput("left_map",width="100%",height=1200),
                                             leafletOutput("right_map",width="100%",height=1200))),
                        #control panel on the left
                        absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                      top = 200, left = 50, right = "auto", bottom = "auto", width = 250, height = "auto",
                                      tags$h4('Citi Bike Activity Comparison'), 
                                      tags$br(),
                                      tags$h5('Pre-covid(Left) Right(Right)'), 
                                      prettyRadioButtons(
                                                      inputId = "adjust_score",
                                                      label = "Score List:", 
                                                      choices = c("start_cnt", 
                                                                  "end_cnt", 
                                                                  "day_diff_absolute",
                                                                  "day_diff_percentage"),
                                                      inline = TRUE, 
                                                      status = "danger",
                                                      fill = TRUE
                                                        ),
                                      awesomeRadio("adjust_time", 
                                                   label="Time",
                                                    choices =c("Overall",
                                                               "Weekday", 
                                                               "Weekend"), 
                                                    selected = "Overall",
                                                    status = "warning"),
                                      # selectInput('adjust_weather',
                                      #             label = 'Adjust for Weather',
                                      #             choices = c('Yes','No'), 
                                      #             selected = 'Yes'
                                      #             ),
                                      style = "opacity: 0.80"
                                      
                                ), #Panel Control - Closing
                            ) #Maps - Div closing
                        ) #tabPanel maps closing
   


    ) #navbarPage closing  
) #Shiny UI closing    
