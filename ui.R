shinyUI(fluidPage(
  titlePanel("Gravity Golf"),
  
  sidebarLayout(
    sidebarPanel(
      h3("The Planets"),
      sliderInput("planet_n","How many planets do we want?",
                  1,10,value=3),
      uiOutput("planet_pars"),
      h3("The Hole"),
      sliderInput("hole_x","Adjust the hole x-coordinate",min=1,max=3,value=2,step=0.1),
      sliderInput("hole_y","Adjust the hole y-coordinate",min=-2,max=2,value=0,step=0.1),
      sliderInput("hole_r","Adjust the hole radius",min=0.1,max=0.5,value=0.2,step=0.05),
      sliderInput("hole_m","Adjust the hole mass",min=-10,max=10,value=0,step=0.1),
      h3("Your Shot"),
      sliderInput("angle","Select your tee-off angle",
                  min=-90,max=90,value=0,step=0.5),
      sliderInput("speed","Pick your launch speed!",
                  min=0,max=100,value=0,step=1)
      
      ),
    mainPanel("plot goes here")
  )
))
