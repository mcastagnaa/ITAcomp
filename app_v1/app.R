## TO DO
### 

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyauthr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(DT)
library(tidyquant)
library(janitor)
library(ggcorrplot)
library(Microsoft365R)
library(shinyWidgets)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)

### SETUP ######################################
source("datamanagement.R")
source("f_getACchart.R")

################################################

ui <- fluidPage(
  theme=shinytheme("lumen"),
  #htmlwidgets::getDependency('sparkline'),
  #useShinyjs(),
  
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  # login section
  shinyauthr::loginUI(id = "login"),
  
  fluidRow(
    column(2,
           br(), 
           br(),
           div(img(src = "logo.png", width = "100")),
           br(), 
           br(), 
           style="text-align: center;"),
    column(10,
           titlePanel("Italian competitors dashboard"),
           column(2,
                  selectInput("refDate",
                              "As of:",
                              unique(dataset$DateLabel[order(dataset$Date, decreasing = T)]),
                              multiple = F,
                              selected = max(dataset$Date))),
           column(10,
                  h5("v. 0.01", style="text-align:right; color:red"))
           )),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput("refAM",
                  "Reference AM:",
                  unique(dataset$AM),
                  multiple = F,
                  selected = "Mediolanum"),
      selectInput("compAM",
                  "Competitors AM:",
                  setdiff(unique(dataset$AM), "Mediolanum"),
                  multiple = T,
                  selected = setdiff(unique(dataset$AM),"Mediolanum")),
      br(),
      radioButtons("MGF", 
                   "MGF role:",
                   c("Mediolanum" = "medio",
                     "Competitor" = "comp"),
                   selected = "comp"),
      br(),
      sliderInput("AUM", 
                  "AUM greater than (€mn):",
                  min = 0, 
                  max = 2000, 
                  value = 50),
      width = 2),
    mainPanel = mainPanel(
      fluidRow(div(),
               br()),
      tabsetPanel(
        type = "tabs",
        tabPanel("Asset Class level",
                 br(),
                 div(plotOutput("mainStats")),
                 br(),
                 div(plotOutput("ACdistEvo"))),
        tabPanel("Peer Group level",
                 br()),
        tabPanel("Fund level",
                 br()),
        tabPanel("Dataset",
                 br(),
                 div(textOutput("refAM")),
                 div(textOutput("compAM")),
                 div(textOutput("MGF")),
                 div(textOutput("AUM")),
                 div(textOutput("refDate")),
                 br(),
                 tableOutput("rawdata"))
      ), 
      width = 8))
)

server <- function(input, output, session) {
  
  # credentials <- shinyauthr::loginServer(
  #   id = "login",
  #   data = user_base,
  #   user_col = user,
  #   pwd_col = password,
  #   sodium_hashed = TRUE
  # )
  
  observeEvent(input$refAM, {
    updateSelectInput(session,
                      inputId = "compAM",
                      choices = c(setdiff(unique(dataset$AM), input$refAM)),
                      selected = c(setdiff(unique(dataset$AM), input$refAM)))
    if(input$refAM != "MGF"){
      enable("compAM")
      enable("MGF")
    } else {
      updateRadioButtons(session, "MGF", selected = "comp")
      disable("MGF")
    }})
  
  observeEvent(input$MGF, {
    if(input$MGF == "medio"){
      if(input$refAM == "MGF") {
        updateSelectInput(session, "refAM", selected = "Mediolanum")
        updateSelectInput(session,
                          inputId = "compAM",
                          choices = setdiff(unique(dataset$AM), c(input$refAM, "MGF")),
                          selected = setdiff(unique(dataset$AM), c(input$refAM, "MGF")))
      } else {
        updateSelectInput(session,
                          inputId = "compAM",
                          choices = setdiff(unique(dataset$AM), c(input$refAM, "MGF")),
                          selected = setdiff(unique(dataset$AM), c(input$refAM, "MGF")))
      }
    } else {
      updateSelectInput(session,
                        inputId = "compAM",
                        choices = setdiff(unique(dataset$AM), c(input$refAM)),
                        selected = setdiff(unique(dataset$AM), c(input$refAM)))
    }
    })
  
  # test output for input values
  output$refAM <- renderText(input$refAM)
  output$compAM <- renderText(input$compAM)
  output$AUM <- renderText(input$AUM)
  output$MGF <- renderText(input$MGF)
  output$refDate <- renderText(input$refDate)
  
  selectedData <- reactive({
    req(input$refAM)
    req(input$compAM)
    req(input$MGF)
    req(input$AUM)
    req(input$refDate)
    
    refDate <- unique(dataset$Date[dataset$DateLabel == input$refDate])
    
    return(dataset %>% 
      mutate(AM == ifelse(input$MGF == "medio" & AM == "MGF", "Mediolanum", AM)) %>%
      filter(AM %in% c(input$refAM, input$compAM),
             AUM >= input$AUM,
             Date <= refDate) %>%
        select(-DateLabel))
    })
  
  fullStats <- reactive({
    QuartHalves <- selectedData() %>%
      pivot_longer(-c(Date, AM, ISIN, Fondo, PeerGroup, AssetClass, AUM), names_to = "Frame", values_to = "Percentile") %>%
      filter(!is.na(Percentile)) %>%
      mutate(Quartile = cut(Percentile, breaks = c(0, 25, 50, 75, 100), right = T, labels = c("Q1", "Q2", "Q3", "Q4")),
             Halves = cut(Percentile, breaks = c(0, 50, 100), right = T, labels = c("H1", "H2")),
             Frame = factor(Frame, levels = c("Net22J", "NetYtD", "Net1y", "Net3y", "Net5y",
                                              "Grs22J", "GrsYtD", "Grs1y", "Grs3y", "Grs5y"))) 
             
             return(rbind(QuartHalves %>%
                            group_by(Date, Frame, AM, Group = Quartile) %>%
                            summarise(No = n()) %>%
                            mutate(Perc = No/sum(No)),
                          QuartHalves %>%
                            group_by(Date, Frame, AM, Group = Halves) %>%
                            summarise(No = n()) %>%
                            mutate(Perc = No/sum(No))) %>%
                      mutate(Group = factor(Group, levels = c("Q1", "Q2", "H1", "Q3", "Q4", "H2"))) %>%
                      #complete(Group, fill= list(No = 0, Perc = 0)) %>%
                      arrange(Date, Frame, AM, Group))
  })

  output$rawdata <- renderTable(fullStats())
  
  output$mainStats <- renderPlot(f_getACchart(fullStats(), 
                                              AUMinput = input$AUM,
                                              refAM = input$refAM, 
                                              compAM = input$compAM,
                                              MGF = (input$MGF == "medio")))
}

shinyApp(ui = ui, server = server)
