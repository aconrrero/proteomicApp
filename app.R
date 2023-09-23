##############################
#Welcome to my Proteomic App!!
#If you need to contact me, 
#please reach me out at: 
#agustinaconrrero@gmail.com
##############################

#APP: 

# Verify that the required packages are installed. If not, install them automatically
packages = c("plotly","shiny","shinythemes","tidyverse")
packageCheck <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

#Load libraries and custom functions
library(plotly)
library(shiny)
library(shinythemes)
library(tidyverse)
source("functions.R")

# Define UI 
ui <- navbarPage("ProteomicApp",
           #TAB FILTER&DISCOVER              
           tabPanel("Filter&Discover",fluidPage(theme = shinytheme("united")),
                    tags$head(
                      tags$style(HTML(".vertical-line {border-right: 1px solid black;
                                      height: 100%;}.center-plot {text-align: center;}"))),
            pageWithSidebar(
              headerPanel(""),
              #Filtering Sidebar Panel 
              sidebarPanel(width = 4,
                           HTML("<b style='font-size: 20px;'>1) Choose the variables:</b>"),
                           selectInput('variableX', 'Variable X:',
                                       c("Mascot_Score","SSM","UPM","SSM_used_for_quantification",
                                         "Vehicle_control_.n.1.","Vehicle_control_.n.2.",
                                         "Binding_of_NBF_to_fresh_beads_.n.1.","Binding_of_NBF_to_fresh_beads_.n.2.",
                                         "Hill","IC50","MS1_intensity","Kdapp"), selected= "SSM"),
                           selectInput('variableY', 'Variable Y:',
                                       c("Mascot_Score","SSM","UPM","SSM_used_for_quantification",
                                         "Vehicle_control_.n.1.","Vehicle_control_.n.2.",
                                         "Binding_of_NBF_to_fresh_beads_.n.1.","Binding_of_NBF_to_fresh_beads_.n.2.",
                                         "Hill","IC50","MS1_intensity","Kdapp"), selected= "Mascot_Score"),
                           HTML("<br><br> <b style='font-size: 20px;'>2) Filter the data:</b>"),
                           sliderInput("mascot", "Mascot Score:", min = 30, max = 9655, value = c(30,8000)),
                           sliderInput("ssm", "SSM:", min = 2, max = 443, value = c(2,440)),
                           sliderInput("upm", "UPM:", min = 2, max = 195, value = c(4,180)),
                           sliderInput("hill", "Hill:", min = 0.42, max = 2.5, value = c(0,2.5)),
                           checkboxGroupInput(inputId = "proteinType",
                                              label = 'Protein Type:', 
                                              choices = c("AXXXX" = "A", "BXXXX" = "B",
                                                        "OXXXX"="O","PXXXX"="P","QXXXX"="Q"), 
                                              selected = c("A", "B", "O", "P", "Q"),inline=TRUE),
                           HTML("<br><br>"),
                           submitButton("Update changes")),
              #Main Plot
              mainPanel(
                column(10, align="center", plotlyOutput("plotSelectedData", width = 1000, height=700),
                       HTML("<br><br>"),
                       p("Hover the mouse over the data points on the graph to view the UniProt ID and 
                         protein descriptions. Use the zoom function by drawing a rectangle with the 
                         mouse to get a closer look. The legends are interactive; click on the experiments 
                         to show or hide the data points. Double-click on a data point to isolate it.",
                         style = "font-size:18px"))))),
           #TAB COMPARE&UNCOVER  
           tabPanel("Compare&Uncover",
                    fluidRow(column(4, align="center", h1("")),
                             column(4, align="center", h3("NorAds")),
                             column(4, align="center", h3("Solanocapsine")),),
                    fluidRow(
                      column(4, align= "justify", style = "margin-top: 5em;",
                             HTML("<p style='font-size: 20px; '> Compare NorAds and Solanocapsin values 
                                  interactively. Hover over data points to get detailed information. 
                                  Dive into the 3D graph: rotate and zoom in for a closer look. Simplify 
                                  your view by deactivating some legends in the line charts. Additionally,
                                  capture visualizations using the download option, accessible through
                                  the camera icon.</p>")),
                      column(4, align="center", plotlyOutput("plotHeatmaplyNorAds",width = 600, height=300)),
                      column(4, align="center", plotlyOutput("plotHeatmaplySolan",width = 600, height=300))),
                    HTML("<br><br><br>"),
                    fluidRow(
                      column(4, align="center", plotlyOutput("plotLineCharthlyMetrics",width = 600, height=350)),
                      column(4, align="center", plotlyOutput("plotLineCharthlyNorAds",width = 600, height=350)),
                      column(4, align="center", plotlyOutput("plotLineCharthlySolan",width = 600, height=350))
                    )),
           #TAB ABOUT
           tabPanel("About",p("The data were extracted from Guantay et al., 'Deoxycytidine kinase (dCK) 
                              inhibition is synthetic lethal with BRCA2 deficiency' published in Drug Resistance
                              Updates, Volumen 67, 2023, 100932, ISSN 1368-7646 ", 
                              a("https://doi.org/10.1016/j.drup.2023.100932", 
                                href="https://www.sciencedirect.com/science/article/pii/S1368764623000158", 
                                target="_blank"), ".  ", style = "font-size:25px"),
                    
                    hr(), 
                    p("The first dataset in the 'Filter&Discover' tab consists of 14,812 observations and 23 
                      variables, including numerical metrics such as Mascot Score and categorical attributes like 
                      Experiment_ID and Protein Description. In the 'Compare&Uncover' tab, the second dataset has 
                      96 observations and 12 variables, enabling the comparison of Solanocapsine with NorAds at 
                      different temperatures. No modifications were made due to limited familiarity with the domain,
                      presenting the data in its original state.",style = "font-size:25px;"),
                    hr(),
                    p("This innovative proteomics visualization app draws inspiration from the mentioned research 
                    but aims to become an essential and customizable tool for understanding complex data, paving 
                    the way for future advancements, particularly in the field of scientific research.",
                    style = "font-size:25px;")),
           
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

  
  #Upload Data1
  data <- reactive({
    read.csv("1-s2.0-S1368764623000158-mmc5_1.csv")
  })
  
  #Upload Data2
  data2 <- reactive({ 
    data2 <- read.csv("1-s2.0-S1368764623000158-mmc5_2.csv", check.names = F)
    data2 <- pivot_longer(data2, cols = c("0.4","2.0","10","50"),
                          names_to = "Concentration", values_to = "Values")
    data2$Concentration <- factor(data2$Concentration, levels = c("0.4", "2.0", "10", "50"))
    data2
  })
  
  #Filter NorAdS
  data2NorAds<- reactive({
    data2NorAds = data2()[data2()$Compound == "NorAdS",]
    data2NorAds
  })
  #Filter Solanocapsine
  data2Solan<- reactive({ 
    data2Solan= data2()[data2()$Compound == "Solanocapsine",]
    data2Solan
  })
  
  #Filter Data Interactivelly
  selectedData <-  reactive({
    data() %>%
      filter(Mascot_Score >= input$mascot[1]) %>%
      filter(Mascot_Score <= input$mascot[2]) %>%
      filter(SSM >= input$ssm[1])  %>%
      filter(SSM <= input$ssm[2]) %>%
      filter(UPM >= input$upm[1])  %>%
      filter(UPM <= input$upm[2]) %>%  
      filter(ProteinType %in% input$proteinType)
  })
  
  #Generate color pallete for plots
  custom_colors <- reactive({
    colors= c("#333333", "#666666", "#999999", "#CCCCCC", "#FFD700", "#FFA500", "#FF4500","red")
    colors
  })
  
  #Generate Output Big Scatter Plot with Filtered Data
  output$plotSelectedData <- renderPlotly({
    plot_ly(selectedData(), x = ~get(input$variableX), y = ~get(input$variableY),
            color = ~Experiment_ID, type = 'scatter', mode= "markers",
            text = ~paste(Representative.UniProt_ID, ": ", Protein_description)) %>%
            layout(xaxis = list(title = input$variableX,
                          range = c(0, max(selectedData()[[input$variableX]]))),
             yaxis = list(title = input$variableY,
                          range = c(0, max(selectedData()[[input$variableY]]))),
             legend = list(showlegend = TRUE, 
                           font = list(size = 15),
                           title = list(text = "Experiment\n")))
  })
  
  output$plotHeatmaplyNorAds <- renderPlotly({
    hover_text <- matrix(paste(" Exp ID:", data2NorAds()$`Experiment ID`, "<br>",
                               "Concentration: ", data2NorAds()$Concentration, "uM <br>",
                               "Temperature: ", data2NorAds()$Temperature, "<br>",
                               "Value: ", data2NorAds()$Values), ncol = 1)
    heatmap= getHeatmaply(data = data2NorAds(), customColors=custom_colors(), 
                          hoverText = hover_text, title = "[NorAds]")
    heatmap
  })
  
  output$plotLineCharthlyNorAds <- renderPlotly({
   hover_text <- matrix(paste(" Exp ID:", data2NorAds()$`Experiment ID`, "<br>",
                               "Concentration: ", data2NorAds()$Concentration, "uM <br>",
                               "Temperature: ", data2NorAds()$Temperature, "<br>",
                               "Value: ", data2NorAds()$Values), ncol = 1)
    linechart= getLineChartly(data=data2NorAds(), customColors = custom_colors(), 
                              hoverText = hover_text, title = "[NorAds]")
    linechart
    
  })
  
  
  output$plotHeatmaplySolan <- renderPlotly({
    hover_text <- matrix(paste(" Exp ID:", data2Solan()$`Experiment ID`, "<br>",
                               "Concentration: ", data2Solan()$Concentration, "uM <br>",
                               "Temperature: ", data2Solan()$Temperature, "<br>",
                               "Value: ", data2Solan()$Values), ncol = 1)
    heatmap= getHeatmaply(data = data2Solan(), customColors=custom_colors(), 
                          hoverText = hover_text, title = "[Solanocapsine]")
    heatmap
    
  })
  
  output$plotLineCharthlySolan <- renderPlotly({
    hover_text <- matrix(paste(" Exp ID:", data2Solan()$`Experiment ID`, "<br>",
                               "Concentration: ", data2Solan()$Concentration, "uM <br>",
                               "Temperature: ", data2Solan()$Temperature, "<br>",
                               "Value: ", data2Solan()$Values), ncol = 1)
    
    linechart= getLineChartly(data=data2Solan(), customColors = custom_colors(), 
                              hoverText = hover_text, title = "[Solanocapsine]")
    linechart
  })
  
  output$plotLineCharthlyMetrics <- renderPlotly({  
    hover_text <- paste("Exp ID:", data2()$`Experiment ID`, "<br>",
                        "Concentration: ", data2()$Concentration, "uM <br>",
                        "Temperature: ", data2()$Temperature, "<br>",
                        "Value: ", data2()$Values)
    threeDplot= getThreeDplotly(data =data2(), hoverText = hover_text )
    threeDplot
  })
}

# Run the application and cross your fingers 
shinyApp(ui = ui, server = server)
