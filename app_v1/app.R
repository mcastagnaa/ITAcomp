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
source("f_getMainTable.R")

################################################

ui <- fluidPage(theme=shinytheme("lumen"),
                htmlwidgets::getDependency('sparkline'),
                useShinyjs(),
                
                # logout button
                div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                # login section
                shinyauthr::loginUI(id = "login"),
                
                fluidRow(
                  column(2,
                         br(), br(),
                         div(img(src = "logo.png", width = "100")),
                         br(), br(), style="text-align: center;"),
                  column(10,
                         titlePanel("Italian competitors dashboard"),
                         h5(),
                         hr())
                ),
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    selectInput("filter1",
                                "Asset Class",
                                c("Equity", "Fixed Income", "MultiAsset")),
                    multiple = F,
                    selected = "Fixed Income",
                  width = 2),
                  mainPanel = mainPanel(
                    fluidRow(column(4, dateInput("refDate", "Reference date", value = NA, 
                                                 format = "d-M-yy", width = "150px",
                                                 weekstart = 1)),
                             div(),
                             br()),
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Characteristics",
                               br()),
                      tabPanel("Risk",
                               br()))
                    , width = 8))
)

server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE
  )

  output$MainTable <- renderDataTable({
    #req(credentials()$user_auth)
  })
}

shinyApp(ui = ui, server = server)
