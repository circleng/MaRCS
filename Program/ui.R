require(shiny)
require(shinyjs)
require(shinydashboard)



shinyUI(
  dashboardPage(
    dashboardHeader(title = "MaRCS"),
    dashboardSidebar(
      shinyjs::useShinyjs(),
      sidebarMenu(id = "tabs",
        menuItem("Setting Parameter", tabName = "setpara", icon = icon("gear")),
        menuItem("Simulation Result", tabName = "grid", icon = icon("th")),
        menuItem("System Summary", tabName = "dash", icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "setpara",
           uiOutput("main")
        ),
        tabItem(tabName = "grid",
           uiOutput("grid")    
        ),
        tabItem(tabName = "dash",
           uiOutput("dash")    
        )
      )
    ),
    
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #005D66;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #005D66;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #007984;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #1B2627;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #047F90;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #1B2627;
                              color: #FFFFFF;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #7DADAF;
                              }
                              .btn-default{
                                background-color: #337ab7;
                                color: #f6f6f6;
                              }
                              
                              #progressBox .bg-purple{
                                background-color: #078692!important;
                              }')))
                

  )
)