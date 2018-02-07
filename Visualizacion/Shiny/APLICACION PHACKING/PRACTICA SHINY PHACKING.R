library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(MASS)

ui<- dashboardPage(
        dashboardHeader(title="Select your trick"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Subsample",tabName = "Subsample_trick",icon=icon("line_chart")),
                        menuItem("Weighting",tabName = "Weighting_trick",icon=icon("balance-scale")),
                        menuItem("Change Variables",tabName = "Varchange_trick",icon=icon("refresh"))
                )
        ),
        dashboardBody(
                box(plotOutput("grafica1")),
                verbatimTextOutput(outputId="correlacion"),
                verbatimTextOutput(outputId="p-valor"),
                tabItems(
                        tabItem(tabName="Subsample_trick",
                                fluidPage(
                                        mainPanel(
                                                fluidRow(
                                                        column(6,textInput(inputId="Subsample",
                                                                           label="Subsample Size"),
                                                               actionButton(inputId="Run_random", label="random")),
                                                        column(6,plotOutput("grafica_subsample"))
                                                )
                                        )
                                )
                        ),
                        tabItem(tabName="Weighting_trick",
                                fluidPage(
                                        mainPanel(
                                                fluidRow(
                                                        column(6, selectInput(inputId="weight_var",
                                                                              label="weight variable",
                                                                              choices = 1,
                                                                              selected=1)), #meter las variables que vaya a elegir
                                                        column(6, plotOutput("grafica_weighted"))
                                                )
                                        )
                                )
                        ),
                        tabItem(tabName="Varchange_trick",
                                fluidPage(
                                        mainPanel(
                                                fluidRow(
                                                        column(6, selectInput(inputId="new X",
                                                                              label="Select X",
                                                                              choices = 3,
                                                                              selected=3),selectInput(inputId="New Y",
                                                                                                      label="Select Y",
                                                                                                      choices = 2,
                                                                                                      selected=2)),
                                                        column(6, plotOutput("grafica_varchange"))
                                                )
                                        )
                                )
                        )
                ))
        
)

server <- function(input, output){
        
}



shinyApp(ui,server)