require(shiny)

shinyUI(
  fluidPage(
    headerPanel("MaRCS"),
  
    tabsetPanel(type = "tabs", 
                  tabPanel("Setting Parameter", uiOutput("main")), 
                  tabPanel("Real-Generate Grid", uiOutput("grid")), 
                  tabPanel("Dashbroad", tableOutput("dashbroad"))
    )
  )
)