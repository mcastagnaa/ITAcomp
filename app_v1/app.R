## TO DO
### 

library(shiny)
library(shinyjs)
library(shinythemes)
#library(shinyauthr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(ggrepel)
#library(DT)
#library(tidyquant)
#library(janitor)
#library(shinyWidgets)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)
options(knitr.kable.NA = '')

### SETUP ######################################
source("datamanagement.R")
source("f_getACchart.R")
source("f_getACchart_ACGroup.R")
source("f_getMainDistChart.R")
source("f_getPGchart_PGGroup.R")
source("f_getPGDistChart.R")

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
                  h5("v. 0.11", style="text-align:right; color:red"))
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
        tabPanel("Main level",
                 br(),
                 
                 selectInput("selFrame1",
                             "Select frames:",
                             c("Net22J", "NetYtD", "Net1y", "Net3y", "Net5y",
                               "Grs22J", "GrsYtD", "Grs1y", "Grs3y", "Grs5y"),
                             multiple = T,
                             selected = c("Net22J", "Grs22J")),
                 div(plotOutput("mainDist", height = "700px")),
                 br(),
                 fluidRow(
                   column(6,
                          selectInput("selAC1",
                                      "Select Asset Classes:",
                                      c("Fixed Income", "Equity", "MultiAsset"),
                                      multiple = T,
                                      selected = c("Fixed Income", "Equity", "MultiAsset"))),
                   column(6,
                          selectInput("selGroup1",
                                      "Select groups:",
                                      c("Q1", "Q2", "H1", "Q3", "Q4", "H2"),
                                      multiple = T,
                                      selected = c("H1", "Q3", "Q4")))
                 ),
                 div(plotOutput("mainStats", height = "600px")),
                 br()),
        tabPanel("Asset Class level",
                 br(),
                 selectInput("selFrame3",
                             "Select Period:",
                             c("Net22J", "NetYtD", "Net1y", "Net3y", "Net5y",
                               "Grs22J", "GrsYtD", "Grs1y", "Grs3y", "Grs5y"),
                             multiple = F,
                             selected = "Net22J"),
                 div(plotOutput("ACmain", height = "600px")),
                 div(tableOutput("fullStatsAC"))),
        tabPanel("Peer Group level",
                 br(),
                 fluidRow(column(4,
                                 selectInput("selAC2",
                                             "Select Asset Class:",
                                             c("Fixed Income", "Equity", "MultiAsset"),
                                             multiple = F,
                                             selected = "Fixed Income")),
                          column(4,
                                 selectInput("selFrame2",
                                             "Select Period:",
                                             c("Net22J", "NetYtD", "Net1y", "Net3y", "Net5y",
                                               "Grs22J", "GrsYtD", "Grs1y", "Grs3y", "Grs5y"),
                                             multiple = F,
                                             selected = "Net22J")),
                          column(4,
                                 selectInput("selPG",
                                             "Select Peer Groups:",
                                             unique(dataset$PeerGroup[dataset$AssetClass == "Fixed Income"]),
                                             multiple = T,
                                             selected = unique(dataset$PeerGroup[dataset$AssetClass == "Fixed Income"])[1])),
                          ),
                 div(plotOutput("PGmain", height = "600px")),
                 div(plotOutput("PGevo", height = "600px")),
                 div(tableOutput("fullStatsPG"))),
        tabPanel("Fund level",
                 br(),
                 div(tableOutput("rawdata"))),
        tabPanel("Dataset",
                 br(),
                 div(),
                 br())
      ), 
      width = 8))
)

server <- function(input, output, session) {
  
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
  
  observeEvent(input$selAC2, {
    PGchoices <- fullStatsPG() %>%
      ungroup() %>%
      filter(AssetClass == input$selAC2) %>%
      select(PeerGroup) %>%
      distinct() %>%
      pull(PeerGroup)
    
    updateSelectInput(session,
                      inputId = "selPG",
                      choices = PGchoices,
                      selected = PGchoices[1])
  })
  
  fundsSet <- reactive({
    dataset %>%
      filter(PeerGroup %in% input$selPG) %>%
      select(Date, AM, ISIN, Fondo, PeerGroup, input$selFrame2) %>%
      pivot_wider(names_from = "Date", values_from = input$selFrame2)
  
    })
  
  
  # test output for input values
  #output$refAM <- renderText(input$refAM)
  #output$compAM <- renderText(input$compAM)
  #output$AUM <- renderText(input$AUM)
  #output$MGF <- renderText(input$MGF)
  #output$refDate <- renderText(input$refDate)
  
  refDate <- reactive({
    unique(dataset$Date[dataset$DateLabel == input$refDate])
  })
  
  selectedData <- reactive({

    return(dataset %>% 
      mutate(AM = ifelse(input$MGF == "medio" & AM == "MGF", "Mediolanum", AM)) %>%
      filter(AM %in% c(input$refAM, input$compAM),
             AUM >= input$AUM,
             Date <= refDate()) %>%
        select(-DateLabel))
    })
  
  quartHalves <- reactive({
    return(
      selectedData() %>%
        pivot_longer(-c(Date, AM, ISIN, Fondo, PeerGroup, AssetClass, AUM), names_to = "Frame", values_to = "Percentile") %>%
        filter(!is.na(Percentile),
               AssetClass %in% input$selAC1) %>%
        mutate(Quartile = cut(Percentile, breaks = c(0, 25, 50, 75, 100), right = T, labels = c("Q1", "Q2", "Q3", "Q4")),
               Halves = cut(Percentile, breaks = c(0, 50, 100), right = T, labels = c("H1", "H2")),
               Frame = factor(Frame, levels = c("Net22J", "NetYtD", "Net1y", "Net3y", "Net5y",
                                                "Grs22J", "GrsYtD", "Grs1y", "Grs3y", "Grs5y")))
    )
  })
  
  fullStats <- reactive({
    
    return(rbind(quartHalves() %>%
                   group_by(Date, Frame, AM, Group = Quartile) %>%
                   summarise(No = n()) %>%
                   mutate(Perc = No/sum(No)),
                 quartHalves() %>%
                   group_by(Date, Frame, AM, Group = Halves) %>%
                   summarise(No = n()) %>%
                   mutate(Perc = No/sum(No))) %>%
             mutate(Group = factor(Group, levels = c("Q1", "Q2", "H1", "Q3", "Q4", "H2"))) %>%
             arrange(Date, Frame, AM, Group))
  })

  fullStatsPG <- reactive({
    return(
      rbind(quartHalves() %>%
                         group_by(Date, Frame, AssetClass, PeerGroup, AM, Group = Quartile) %>%
                         summarise(No = n()) %>%
                         mutate(Perc = No/sum(No)),
                       quartHalves() %>%
                         group_by(Date, Frame, AssetClass, PeerGroup, AM, Group = Halves) %>%
                         summarise(No = n()) %>%
                         mutate(Perc = No/sum(No))) %>%
        mutate(Group = factor(Group, levels = c("Q1", "Q2", "H1", "Q3", "Q4", "H2"))) %>%
        filter(AssetClass == input$selAC2,
               Frame == input$selFrame2) %>%
        group_by(Frame, AssetClass, PeerGroup) %>%
        mutate(NoAM = n_distinct(AM)) %>%
        filter(NoAM > 1) %>%
        select(-NoAM) %>%
        arrange(AssetClass, PeerGroup, Frame, AM, Group)
    )
  })
  
  fullStatsAC <- reactive({
    return(
      rbind(quartHalves() %>%
              group_by(Date, Frame, AssetClass, AM, Group = Quartile) %>%
              summarise(No = n()) %>%
              mutate(Perc = No/sum(No)),
            quartHalves() %>%
              group_by(Date, Frame, AssetClass, AM, Group = Halves) %>%
              summarise(No = n()) %>%
              mutate(Perc = No/sum(No))) %>%
        mutate(Group = factor(Group, levels = c("Q1", "Q2", "H1", "Q3", "Q4", "H2"))) %>%
        arrange(Date, AssetClass, Frame, AM, Group) %>%
        ungroup()
    )
  })
  
  output$fullStatsAC <- function() {
    
    tbl <- fullStatsAC() %>%
      mutate(Label = paste(No, scales::label_percent(accuracy = 0.1)(Perc), sep = "/")) %>%
      select(-c(No, Perc)) %>%
      pivot_wider(names_from = Group, values_from = Label, names_sort = T) %>%
      filter(Date == refDate(), 
             Frame %in% input$selFrame1) %>%
      select(-c(Date)) %>%
      relocate(AssetClass, .before = Frame) 
    
    nrows <- nrow(tbl)
    
    tbl %>%
      kable(align = "r", caption = "Table by Asset Class") %>%
      kable_styling(latex_option = c("hold_position"), font_size = 11, full_width = F) %>%
      column_spec(1:2, bold = T) %>%
      collapse_rows(1:2, valign = "top") %>%
      row_spec(1:nrows, extra_css = "padding: 1px")
  }
    
  output$fullStatsPG <- function() {
    
    PGtable <- fullStatsPG() %>%
      filter(Date == refDate()) %>%
      mutate(Label = paste(No, scales::label_percent()(Perc), sep = "/")) %>%
      select(-c(No, Perc)) %>%
      pivot_wider(names_from = Group, values_from = Label, names_sort = T) %>%
      ungroup() 
      #filter(AssetClass == "Fixed Income", Date == "2022-11-01", Frame == "Net22J") %>%
      #select(-c(AssetClass, Date, Frame))
    
    nrows <- nrow(PGtable)
    
    PGtable %>%
      select(-c(AssetClass, Date, Frame)) %>%
      kable(striped = T, condensed = T, align = "r", caption = "Table by Peer Group", linesep = "") %>%
      kable_styling(font_size = 11, full_width = F) %>%
      column_spec(1, bold = T) %>%
      collapse_rows(1, valign = "top") %>%
      row_spec(1:nrows, extra_css = "padding: 1px")
  }
  
  distRefComp <- reactive({
    return(
      selectedData() %>%
        pivot_longer(-c(Date, AM, ISIN, Fondo, PeerGroup, AssetClass, AUM), names_to = "Frame", values_to = "Percentile") %>%
        filter(Frame %in% input$selFrame1) %>%
        select(AssetClass, PeerGroup, Date, AM, Frame, Percentile) %>%
        mutate(Side = ifelse(AM == input$refAM, "Main", "Comp")) %>%
        group_by(AssetClass, PeerGroup, Date, Side, Frame) %>%
        summarise(AvgRank = mean(Percentile, na.rm = T),
                  Count = n()) %>%
        pivot_wider(names_from = Side, values_from = c(AvgRank, Count)) %>%
        mutate(Dist = AvgRank_Comp-AvgRank_Main,
               Frame = factor(Frame, levels = c("Net22J", "NetYtD", "Net1y", "Net3y", "Net5y",
                                                "Grs22J", "GrsYtD", "Grs1y", "Grs3y", "Grs5y"))) %>%
        filter(!is.na(Dist)) %>%
        select(-c(AvgRank_Main, AvgRank_Comp))
    )
  })
  
  subTitChart <- reactive({
    return(
      paste0("Filters: AUM greater than €", input$AUM, "mn, MGF is ", 
             case_when((input$MGF == "medio") ~ "in Mediolanum",
                       !("MGF" %in% c(input$refAM, input$compAM)) ~ "excluded",
                       TRUE ~ "a competitor"))
    )
    
  })
  
  output$mainDist <- renderPlot(f_getMainDistChart(distRefComp(), 
                                                   Frames = input$selFrame1,
                                                   refAMFilter = input$refAM, 
                                                   AUMFilter = input$AUM))
  
  output$rawdata <- renderTable(fundsSet())
  
  output$mainStats <- renderPlot(f_getACchart(fullStats(), 
                                              AUMinput = input$AUM,
                                              refAM = input$refAM,
                                              sbtle = subTitChart(),
                                              ChartGroup = input$selGroup1,
                                              ChartFrame = input$selFrame1,
                                              ChartAC = input$selAC1))
  
  output$ACmain <- renderPlot(f_getACchart_ACGroup(fullStats = fullStatsAC(),
                                                   sbtle = subTitChart(),
                                                   selFrame = input$selFrame3,
                                                   selGroups = input$selGroup1,
                                                   refAM = input$refAM))
  
  output$PGmain <- renderPlot(f_getPGchart_PGGroup(fullStats = fullStatsPG(),
                                                   sbtle = subTitChart(),
                                                   selFrame = input$selFrame2,
                                                   selGroups = input$selGroup1,
                                                   refAM = input$refAM,
                                                   selPG = input$selPG))
  
  output$PGevo <- renderPlot(f_getPGDistChart(distMedioComp = distRefComp(),
                                              selFrame = input$selFrame2,
                                              selPG = input$selPG,
                                              refAM = input$refAM,
                                              sbtle = subTitChart()))
}

shinyApp(ui = ui, server = server)
