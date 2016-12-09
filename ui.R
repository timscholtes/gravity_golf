shinyUI(fluidPage(
  titlePanel("Gravity Golf"),
  
  sidebarLayout(
    sidebarPanel(
      h3("The Planets"),
      sliderInput("planet_n","How many planets do we want?",
                  1,10,value=3),
      uiOutput("planet_pars"),
      h3("The Hole"),
      fluidRow(column(width=6,
                      sliderInput("hole_x","Adjust the hole x-coordinate",
                                  min=-3,max=4,value=2,step=0.1)
                      ),
               column(width=6,
                      sliderInput("hole_y","Adjust the hole y-coordinate",
                                  min=-4,max=4,value=0,step=0.1)
               )),
      fluidRow(column(width=6,
                      sliderInput("hole_r","Adjust the hole radius",
                                  min=0.1,max=0.5,value=0.2,step=0.05)
      ),
              column(width=6,
                   sliderInput("hole_m","Adjust the hole mass",
                               min=-10,max=20,value=0,step=0.1)
      )),
      
      h3("Your Shot"),
      sliderInput("angle","Select your launch angle",
                  min=-90,max=90,value=0,step=0.5),
      sliderInput("speed","Pick your launch speed!",
                  min=1,max=10,value=1,step=0.1),
      selectInput("dt","Pick the timestep",choices = c(0.01,0.1,0.5,1),selected=0.5),
      actionButton("LIFTOFF","LIFT OFF!!"),
      checkboxInput("contours","Display Contours (LOSER)?",value=FALSE)
      #actionButton("pot_button","Plot potential field (because you're shit at golf)?")
      
      ),
    mainPanel(
      plotOutput("graph",height=1000)
    )
  )
))
