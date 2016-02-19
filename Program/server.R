require(shiny)
require(ggplot2)

shinyServer(function(input, output) {
    a <- reactiveValues(data = NULL)
    ##read input drop-down
    formulaText <- reactive({
      input$variable
    })
    
    output$caption <- renderText({
      paste("??ҿ?ʴ??ӹǹ",formulaText(),"?????Դ?غѵ??˵?㹪?ǧ??ȡ?Ż?????")
    })
    
    ##output UI [right]
    output$main <- renderUI({
      ##same CSS
      
      ##in summary case
        list(
          
          wellPanel(
            h2("Worker Setting"),
            sliderInput("slider_numworker", label = h3("Number of Worker"), min = 0, max = 100, value = 100),
            sliderInput("slider_norprob", label = h3("Normal-Worker Probability"), min = 0, max = 100, value = 100),
            sliderInput("slider_ratioadver", label = h3("Ratio of Adversary"), min = 0, max = 100, value = 0),
            radioButtons("radio_advertype", label = h3("Adversary-Worker Type"),
                         choices = list("Fixed-Answer Adversary : Normal" = "type1nor", "Fixed-Answer Adversary : Flood" = "type1flood", "Random Adversary"= "type2" ,"Perfect Adversary" = "type3"), 
                         selected = "type1nor")),
          
          wellPanel(
            h2("Problem Setting"),
            radioButtons("radio_problemtype", label = h3("Problem Type"),
                         choices = list("Fixed-Problem : Normal" = "fixnor", "Fixed-Problem : Flood" = "fixflood","Random Problem" = "random", "Real Problem"= "real"), 
                         selected = "fixnor"),
            sliderInput("slider_x", label = h3("Number of X-axe"), min = 1, max = 20, value = 1),
            sliderInput("slider_y", label = h3("Number of Y-axe"), min = 1, max = 20, value = 1),
            radioButtons("radio_level", label = h3("Problem Label-Level"),
                         choices = list("2 Level" = "2level", "3 Level" = "3level"), 
                         selected = "2level")),
          
          wellPanel(
            h2("DataCenter Setting"),
            radioButtons("radio_datafusion", label = h3("Data Fusion"),
                         choices = list("Majority Voting" = "mv", "EM Algorithm" = "em"), 
                         selected = "mv"),
            
            radioButtons("radio_reputation", label = h3("Reputaion Management"),
                         choices = list("None" = "none","Simple Penalty" = "simple", "Soft Penalty" = "soft","Hard Penalty" = "hard"), 
                         selected = "none"),
            
            checkboxInput("checkbox_cut", label = "Do you use cut worker per round?", value = TRUE),
            
            uiOutput("slide1"),
            
            checkboxInput("checkbox_penalty", label = "Do you need to see panlty?", value = TRUE)),
          
          actionButton("action_start", label = "Start Now"),
          actionButton("action_reset", label = "Reset")
          
        )
    })
    
    output$slide1 <- renderUI({
      if(input$checkbox_cut == TRUE){
        
        sliderInput("slider_cutperround", label = h3("Number of Cut Worker Per Round"), min = 0, max = 100, value = 100)
      }
    })
    
    ##start button
    observeEvent(input$action_start, {
      init()
      main()
      go()
    })
    
    ##reset buuton
    observeEvent(input$action_reset, {
      uiOutput("main")
    })
    
    
    output$grid <- renderUI({
      fluidRow(
        column(6,
          plotOutput("plot1")
        ),
        column(6,
          plotOutput("plot2")
        )
      )
    })
    
    output$plot1 <- renderPlot({
      plot.a
    })
    
    output$plot2 <- renderPlot({
      plot.a
    })

    ##run netlogo by R 
    ##start here
    init <- function(){
      library(RNetLogo)
      curDir <- getwd()
      
      nlDir <- "C:/Program Files/NetLogo 5.3/app"
      setwd(nlDir)
      
      nl.path <- getwd()
      NLStart(nl.path)
      
      model.path <- paste(curDir,"marcs2.nlogo",sep = "/")
      NLLoadModel(model.path)
    }
    
    main <- function(){
      #parameter setup
      ##set seperate x,y
      area <<- c(input$slider_x,input$slider_y)
      min.pxcor <<- NLReport("min-pxcor")
      min.pycor <<- NLReport("min-pycor")
      max.pxcor <<- NLReport("max-pxcor")
      max.pycor <<- NLReport("max-pycor")
      
      setup()
    }
    
    setup <- function(){
      ##set parameter
      ##for worker
      NLCommand(paste("set","number-workers", input$slider_numworker )) 
      NLCommand(paste("set","workers-probability", input$slider_norprob )) 
      NLCommand(paste("set","adversaries-rate", input$slider_ratioadver )) 
      if(input$radio_advertype=="type1nor")
      {
        temp_advertype <- "1. fixed : normal"
      }
      else if(input$radio_advertype=="fixflood")
      {
        temp_advertype <- "1. fixed : flood"
      }
      else if(input$radio_advertype=="random")
      {
        temp_advertype <- "2. random"
      }
      else
      {
        temp_advertype <- "3. perfect"
      }
      print(temp_advertype)
      
      NLCommand(paste("set","adversaries-type", shQuote(temp_advertype))) 
      
      
      ##for problem
      if(input$radio_problemtype=="fixnor")
      {
        temp_problemtype="normal"
      }
      else if(input$radio_problemtype=="fixflood")
      {
        temp_problemtype="flood"
      }
      else if(input$radio_problemtype=="random")
      {
        temp_problemtype="random"
      }
      else
      {
        temp_problemtype="real"
      }
      NLCommand(paste("set","problem-label", shQuote(temp_problemtype) )) 
      NLCommand(paste("set","area.x", input$slider_x )) 
      NLCommand(paste("set","area.y", input$slider_y ))
      if(radio_level=="2level")
      {
        temp_level="2level"
      }
      else
      {
        temp_level="3level"
      }
      NLCommand(paste("set","label-level", shQuote(temp_level) )) 
      
      
      ##setup datacenter
      if(input$radio_datafusion=="mv")
      {
        temp_datafusion="Majority Voting"
      }
      else
      {
        temp_datafusion="EM Algorithm"
      }
      NLCommand(paste("set","data-fusion", shQuote(temp_datafusion) )) 
      if(input$"radio_reputation"=="none")
      {
        temp_reputation="None"
      }
      else if(input$"radio_reputation"=="simple")
      {
        temp_reputation="Simple Penalty"
      }
      else if(input$"radio_reputation"=="Soft Penalty")
      {
        temp_reputation="Soft Penalty"
      }
      else
      {
        temp_reputation="Hard Penalty"
      }
      NLCommand(paste("set","reputation-algorithm", shQuote(temp_reputation))) 
      NLCommand(paste("set","use-cut-worker-by-penalty?", input$checkbox_cut )) 
      NLCommand(paste("set","cut-worker-per-round", input$slider_cutperround)) 
      
      
      ##setup penalty
      NLCommand(paste("set","show-penalty?", input$checkbox_penalty )) 
      
      
      ##call setup in netlogo
      NLCommand("setup") 
      setup.patches()
      setup.datacenters()
      setup.workers()
    }
  
    ##call go in netlogo
    go <- function(){
      NLCommand("go") 
      NLCommand("if ticks mod flood-tick = 0 and ticks > 0[setup-flood]")
      NLCommand(paste("if ticks mod flood-tick = 0 and ticks > 0[",setup.border(),"]"))
      NLCommand("toggle-penalty")
      NLCommand("tick")
    }
    
   
})
