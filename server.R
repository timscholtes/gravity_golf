shinyServer(function(input,output){
  
  values <- reactiveValues()
  
  # initialise
  values$n <- 3
  values$parms <- list(x=runif(3,-1,1),
                  y=runif(3,-1,1),
                  m=runif(3,0,1))
  values$hole <- rep(0,4)
  values$angle <- 0
  values$speed <- 1
  values$dt <- 0.5
  # update n, and create observers
  observeEvent(input$planet_n,{
    values$parms <- NULL
    values$n <- input$planet_n
    
    lapply(1:input$planet_n, function(i) {
      
      observeEvent(input[[paste0("planet_",i,"_x")]],
                   {values$parms$x[i] <- input[[paste0("planet_",i,"_x")]]})
      observeEvent(input[[paste0("planet_",i,"_y")]],
                   {values$parms$y[i] <- input[[paste0("planet_",i,"_y")]]})
      observeEvent(input[[paste0("planet_",i,"_m")]],
                   {values$parms$m[i] <- input[[paste0("planet_",i,"_m")]]})
      })
    
    })
  
  # update hole params
  observeEvent(input$hole_x,{
    values$hole[1] <- input$hole_x
  })
  observeEvent(input$hole_y,{
    values$hole[2] <- input$hole_y
  })
  observeEvent(input$hole_m,{
    values$hole[3] <- input$hole_m
  })
  observeEvent(input$hole_r,{
    values$hole[4] <- input$hole_r
  })
  
  # update shot params
  observeEvent(input$angle, {
    values$angle <- input$angle
    print(input$angle)
  })
  observeEvent(input$speed, {
    values$speed <- input$speed
  })
  
  
  
  output$planet_pars <- renderUI({
    lapply(1:values$n, function(i) {
      fluidRow(
        column(width=3,
               sliderInput(inputId=paste0("planet_",i,"_x"),label=paste("Planet ",i," x coord"),
                           min=-4,max=4,value=runif(1,-4,4),step=0.1)
        ),column(width=3,
               sliderInput(inputId=paste0("planet_",i,"_y"),label=paste("Planet ",i," y coord"),
                           min=-4,max=4,value=runif(1,-4,4),step=0.1)
        ),column(width=3,
               sliderInput(inputId=paste0("planet_",i,"_m"),label=paste("Planet ",i," mass"),
                           min=0.1,max=20,value=runif(1,0.1,10),step=0.1)
        ),
        column(width=3,
               selectInput(inputId=paste0("planet_",i,"_name"),label="Pick a name",
                                          choices=char_names,selected=char_names[i])
        ))
    })
  })
  
  observeEvent(input$dt, {
    values$dt <- as.numeric(input$dt)
  })
  

  observeEvent(input$LIFTOFF, {
    print(values$dt)
    print(class(values$dt))
    values$out <- tee_func(values$angle,values$speed,values$parms,values$dt)
    print("fire")
  })
  
  
  grapher <- reactive({
    
    plot(x=-5,y=0,cex=3,col=2,xlim=c(-5.5,5.5),ylim=c(-5,5.5))
    
    if(input$contours) {
      values$pot <- gravity_pot(values$parms)
      contour(x=seq(-5.5,5.5,0.1),y=seq(-5.5,5.5,0.1),values$pot,add=TRUE)
    }
    
    if(!is.null(values$out)) {
      lines(values$out[,3]~values$out[,2])
    }
     
    draw.circle(x=values$hole[1],y=values$hole[2],radius=values$hole[4],col=1)
     for(i in 1:values$n){
       image_points(images[[match(input[[paste0("planet_",i,"_name")]],char_names)]],
                    x=values$parms$x[i],y=values$parms$y[i],cex=(values$parms$m[i]*1)^(1/2)) # set to 1/2 to make planets bigger
     }
  })

  output$graph <- renderPlot({
    grapher()
  })
  
  
})
