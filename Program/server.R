require(shiny)
require(shinydashboard)
require(ggplot2)
require(RNetLogo)
source("result.R")

wd = getwd()

shinyServer(function(input, output, session) {
    values <- reactiveValues(block = 0, percent = 0, counter = 0, vec = c(), vec2 = c(), oldvec = c())
    p <- reactiveValues(correctness = c()
                        , avg.correctness = c()
                        , image = c())
    
    ##output UI [right]
    output$main <-renderUI({
    ##same CSS
    ##in summary case
    
    list(
      fluidRow(
        box(width = 4, 
            title = "Worker Setting",
            height = "490",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
                 sliderInput("slider_numworker", label = "Number of Workers", min = 0, max = 100, value = 100),
                 sliderInput("slider_norprob", label = "Probability of Workers to Answer Correctly", min = 0, max = 100, value = 100),
                 sliderInput("slider_ratioadver", label = "Ratio of Adversarial Workers", min = 0, max = 100, value = 0),
                 radioButtons("radio_advertype", label = "Adversarial Worker Types",
                              choices = list("Fixed-Answer Adversary : Normal" = "type1nor", "Fixed-Answer Adversary : Flood" = "type1flood", "Random Adversary"= "type2" ,"Perfect Adversary" = "type3"), 
                              selected = "type3")
               
        ),
        
        box(width = 4,
            title = "Problem Setting",
            height = "490",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
                 radioButtons("radio_problemtype", label = "Problem Type",
                              choices = list("Fixed-Problem : Normal" = "fixnor", "Fixed-Problem : Flood" = "fixflood","Random Problem" = "random", "Real Problem"= "real"), 
                              selected = "fixnor"),
                 sliderInput("slider_x", label = "X-axis of Area", min = 1, max = 20, value = 1),
                 sliderInput("slider_y", label = "Y-axis of Area", min = 1, max = 20, value = 1),
                 radioButtons("radio_level", label = "Level of Problem Label",
                              choices = list("2 Level" = "2level", "3 Level" = "3level"), 
                              selected = "2level")
        ),
        
        box(width = 4,
            title = "Datacenter Setting",
            height = "490",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
                 radioButtons("radio_datafusion", label = "Data Fusion",
                              choices = list("Majority Voting" = "mv", "EM Algorithm" = "em"), 
                              selected = "mv"),
                 
                 radioButtons("radio_reputation", label = "Reputaion Management",
                              choices = list("None" = "none","Simple Penalty" = "simple", "Soft Penalty" = "soft","Hard Penalty" = "hard"), 
                              selected = "none"),
                 
                 checkboxInput("checkbox_cut", label = "Do you want to cut workers when process?", value = TRUE),
                 
                 uiOutput("slide1"),
                 
                 checkboxInput("checkbox_penalty", label = "Do you want to see penalty?", value = TRUE)) 
      ),
      fluidRow(
        column(12,align = "center",
               actionButton("action_reset", label = "Reset", icon = icon("refresh")),
               actionButton("action_start", label = "Start Now", icon = icon("arrow-right")),
               checkboxInput("viewupdate", label = "View Update?", value = TRUE)
               )
      )
    )
  })
    
    output$slide1 <- renderUI({
      if(input$checkbox_cut == TRUE){
        
        sliderInput("slider_cutperround", label = "Number of Cut Worker Per Round", min = 0, max = 50, value = 5, post = "%")
      }
    })
    
    output$grid <- renderUI({
      list( 
        valueBoxOutput("progressBox"),
        fluidRow(
          tabBox(width = 12, 
                 id = "plottab",   
                 title = "Output Plot",
                 tabPanel("Correctness",
                          fluidRow(       
                            column(6,
                                   plotOutput("plot1")
                            ),
                            column(6,
                                   plotOutput("plot2")
                            )
                          )
                 ),
                 tabPanel("Current View", 
                            fluidRow(align = "center",      
                              column(12,
                                     imageOutput("view")
                              )
                            )),
                 tabPanel("Compare Grid", 
                          fluidRow(align = "center",      
                                   column(6,
                                          plotOutput("gridplot1")
                                   ),
                                   column(6,
                                          plotOutput("gridplot2")
                                   )
                          ))
          )
        )
      )
    })
    
    output$dash <- renderUI({
      fluidRow(  
        tabBox(width = 12, 
               id = "dashtab",   
               title = "Result Plot",
               tabPanel("Data Fusion",
                        fluidRow(       
                          column(6,
                                 plotOutput("plot3")
                          ),
                          column(6,
                                 plotOutput("plot4")
                          )
                       ),
                       fluidRow(       
                         column(6,
                                plotOutput("plot5")
                         ),
                         column(6,
                                plotOutput("plot6")
                         )
                       )
               ),
               tabPanel("Reputation Management",
                        fluidRow(       
                          column(6,
                                 plotOutput("plot7")
                          ),
                          column(6,
                                 plotOutput("plot8")
                          )
                        ),
                        fluidRow(       
                          column(6,
                                 plotOutput("plot9")
                          ),
                          column(6,
                                 plotOutput("plot10")
                          )
                        ),
                        fluidRow(       
                          column(6,
                                 plotOutput("plot11")
                          ),
                          column(6,
                                 plotOutput("plot12")
                          )
                        ),
                        fluidRow(       
                          column(6,
                                 plotOutput("plot13")
                          ),
                          column(6,
                                 plotOutput("plot14")
                          )
                        )
               )
        )
      )
    })
    
    output$progressBox <- renderValueBox({
      if(values$percent != 100){
        valueBox(
          paste0(values$percent, "%"), "Progress",
          color = "purple"
        )
      }
      else{
        valueBox(
          paste0(values$percent, "%"), "Success", icon = icon("check"),
          color = "green"
        )
      }
    })
    
    
   
    output$plot1 <- renderPlot({
      if(length(p$correctness) == 0){
        plot <- ggplot() + geom_blank() 
      }
      else{
        df <- data.frame(p$correctness, 1:length(p$correctness))
        plot <- ggplot(data = df, aes(x = df[,2], y = df[,1])) + geom_line()
      }
        
      plot + xlim(1,48) + ylim(0,100) + 
        ggtitle("Correctness Plot") + xlab("Time") + ylab("Correctness") +
        theme_bw() +
        theme(
          line = element_line(colour = "blue", size = 1.2)
        )
    })
    
    output$plot2 <- renderPlot({
      if(length(p$correctness) == 0){
        plot <- ggplot() + geom_blank() 
      }
      else{
        df <- data.frame(p$avg.correctness, 1:length(p$avg.correctness))
        plot <- ggplot(data = df, aes(x = df[,2], y = df[,1])) + geom_line()
      }
      
      plot + xlim(1,48) + ylim(0,100) + 
        ggtitle("Average Correctness Plot") + xlab("Time") + ylab("Average Correctness") +
        theme_bw()
    })
    
    output$plot3 <- renderPlot({
      test2()
      test3()
      s1()
    })
    
    output$plot4 <- renderPlot({
      test7()
      test10()
      s2()
    })
    
    output$plot5 <- renderPlot({
      test8()
      test11()
      s3()
    })
    
    output$plot6 <- renderPlot({
      test9.1()
      test12()
      s4()
    })
    
    output$plot7 <- renderPlot({
      test5()
    })
    
    output$plot8 <- renderPlot({
      test6()
    })
    
    output$plot9 <- renderPlot({
      test13()
    })
    
    output$plot10 <- renderPlot({
      test14()
    })
    
    output$plot11 <- renderPlot({
      test15()
    })
    
    output$plot12 <- renderPlot({
      test16()
    })
    
    output$plot13 <- renderPlot({
      test17()
    })
    
    output$plot14 <- renderPlot({
      test18()
    })
    
    output$view <- renderImage({
      list(
        src = normalizePath(p$image)
      )
    })
    
    output$gridplot1 <- renderPlot({
      separatemap(p$oldvec)
    })
    
    output$gridplot2 <- renderPlot({
      separatemap(p$vec2)
    })
    
    ##start button
    observeEvent(input$action_start, {
      updateTabItems(session, "tabs", "grid")
      init()
      main()
      values$count <- 0
      values$block <- 1
    })
    
    observeEvent(values$percent, {
      if(values$percent == 100){
        values$block <- 0
      }
    })
    
    ##graph + image
    observe({
      if(input$tabs == "setpara"){
        shinyjs::disable("radio_level")
      }
      
       if(!is.null(input$plottab) & values$block ==  1){
          isolate({
            
            if(input$viewupdate){
              go(1)
            } 
            else{
              NLCommand("no-display")
              go(30)
            }
            
            if(input$viewupdate & input$plottab == "Correctness"){
              if(NLReport("ticks") %% 30 == 1 & NLReport("ticks") != 1){
                p$correctness <- c(p$correctness ,NLReport("correctness"))
                p$avg.correctness <- c(p$avg.correctness,NLReport("sum-correctness / floor(ticks / update-ticks)"))
              }
            } 
            if(!input$viewupdate & input$plottab == "Correctness"){
              p$correctness <- c(p$correctness ,NLReport("correctness"))
              p$avg.correctness <- c(p$avg.correctness,NLReport("sum-correctness / floor(ticks / update-ticks)"))
            }

            
            #Grid
            if(input$plottab == "Compare Grid"){
              p$oldvec <- p$vec
              
              vec <- c()
              vec2 <- c()
              for(i in 1:(area[1]*area[2])){
                vec <- c(vec,NLReport(paste("first [true-label] of patches with [road-num = ", i,"]")))
                vec2 <- c(vec2,NLReport(paste("[generate-label] of datacenter", i)))
              }
              p$vec <- vec
              p$vec2 <- vec2
            }
            
            #View
            toggle(condition = input$viewupdate, selector = "#plottab li a[data-value=View]")
            
            if(input$plottab == "Current View"){
              path <- paste(wd, "images", paste(NLReport("ticks"),"png",sep = "."), sep = "/")
              NLCommand(paste("export-view",shQuote(path)))
              p$image <- path
            }
            
            values$percent <- round(NLReport("ticks") / 1441 * 100,2) 
          })
       }   
      
      if (isolate(values$counter) < 1441){
        invalidateLater(0, session)
      }
    })    
        
    #reset buuton
    observeEvent(input$action_reset, {
        shinyjs::reset("main")
    })

    ##run netlogo by R 
    ##start here
    init <- function(){
      nlDir <- "C:/Program Files (x86)/NetLogo 5.2.0"
      setwd(nlDir)
      
      nl.path <- getwd()
      tryCatch({(NLStart(nl.path, gui = F))
      },warning = function(w) {
        print("NetLogo Already Started")
      }, error = function(e) {
        print("NetLogo Already Started")
      })
      
      model.path <- paste(wd,"marcs.nlogo",sep = "/")
      NLLoadModel(model.path)
    }
    
    setup <- function(){
      p$correctness <- c()
      p$avg.correctness <- c()
      
      ##set parameter
      ##for worker
      NLCommand(paste("set","number-workers", input$slider_numworker )) 
      NLCommand(paste("set","workers-probability", input$slider_norprob )) 
      NLCommand(paste("set","adversaries-rate", input$slider_ratioadver )) 
      if(input$radio_advertype=="type1nor")
      {
        temp_advertype <- "1. fixed : normal"
      }
      else if(input$radio_advertype=="type1flood")
      {
        temp_advertype <- "1. fixed : flood"
      }
      else if(input$radio_advertype=="type2")
      {
        temp_advertype <- "2. random"
      }
      else
      {
        temp_advertype <- "3. perfect"
      }
      
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
      if(input$radio_level=="2level")
      {
        temp_level="2 Levels"
      }
      else
      {
        temp_level="3 Levels"
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
      NLCommand(paste("set","cut-workers-per-round", input$slider_cutperround)) 
      
      
      ##setup penalty
      NLCommand(paste("set","show-penalty?", input$checkbox_penalty )) 
      
      
      ##call setup in netlogo
      NLCommand("setup") 
      NLCommand("go") 
    }
    
    main <- function(){
      #parameter setup
      ##set seperate x,y
      area <<- c(input$slider_x,input$slider_y)
      
      setup()
      
      update.ticks <<- NLReport("update-ticks")
      min.pxcor <<- NLReport("min-pxcor")
      min.pycor <<- NLReport("min-pycor")
      max.pxcor <<- NLReport("max-pxcor")
      max.pycor <<- NLReport("max-pycor")
    }
    ##call go in netlogo
    go <- function(number = 1441){ 
      NLDoCommand(number,"go")
    
      values$counter <-  values$counter + 1
    }
    
    separatemap <- function(vec){
      i <- c()
      j <- c()
      #1 dim -> 2 dim
      for(num in 0:(length(vec)-1)){
        i <- c(i,floor(num / area[1]) + 1)
        j <- c(j,num %% area[1] + 1)
      }
      
      #NA
      vec[is.na(vec)] <- 2
      df <- data.frame(i,j,vec)
      
      ggplot(data=df, aes(i,j)) + geom_tile(aes(fill=vec)) +
        scale_fill_gradient(limits=c(0, 1),low="#0088db", high = "#dddddd", name = 'Floods') +
        geom_text(data=df, aes(i,j, label = vec)) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        xlab('') +
        ylab('') 
    }
})
