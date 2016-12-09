library(plotrix)
library(deSolve)
library(jpeg)
library(abind)
library(rasterVis)
library(plotly)
# describe gravity here
gravity = function(t,state,parms) {
  with(as.list(state), {
    n = length(parms$x)
    x_diff = parms$x-x
    y_diff = parms$y-y
    
    net_accel_vectors<- 0.1*c(x_diff,y_diff)/
      (pmax(0.1,((x_diff^2+y_diff^2)^(3/2)))/parms$m)
    
    net_accel_vectors <- c(sum(head(net_accel_vectors,n)),
                           sum(tail(net_accel_vectors,n)))
    
    return(list(c(v0,v1,net_accel_vectors)))
  })
}

gravity_pot <- function(parms) {
  pot <- matrix(rep(0,(5.5*2/0.1+1)^2),nrow=(5.5*2/0.1+1))
  
  for(x in 1:nrow(pot)) {
    for(y in 1:ncol(pot)) {
      
      x_diff=parms$x-((x-56)/56)*5.5
      y_diff=parms$y-((y-56)/56)*5.5
      pot[x,y]<- max(-10,-sum(parms$m/
                                ((x_diff^2+y_diff^2)^(1/2))))
    }
  }
  
  return(pot)
}


tee_func <- function(angle,speed,parms,dt) {
  times=seq(0,100,by=dt)
  vx = speed*cos(angle*pi/180)
  vy = speed*sin(angle*pi/180)
  state <- c(x=-5,y=0,v0=vx,v1=vy)
  print(state)
  out <- ode(y=state,times=times,func=gravity,parms,method="rk4")
  return(out)
}

image_points = function(image, x, y, cex = 1, pos = NULL) {
  if (length(x) != length(y)) {
    stop("length(x)!=length(y): check your data")
  }
  dim.x = dim(image)[2]  #image width
  dim.y = dim(image)[1]  #image height
  if (dim.x == dim.y) {
    # obtian the ratio of width to height or height to width
    ratio.x = ratio.y = 1
  } else if (dim.x < dim.y) {
    ratio.x = dim.x/dim.y
    ratio.y = 1
  } else {
    ratio.x = 1
    ratio.y = dim.y/dim.x
  }
  cex = cex/10  #how large the image should be, divided by 10 so that it matches more closely to plotting points
  pin = par()$pin  #pin provides the width and height of the _active graphic device_
  pin.ratio = pin/max(pin)  #take the ratio
  usr = par()$usr  #usr provides the lower.x, lower.y, upper.x, upper.y values of the plotable region
  
  # combine the active device dimensions, the image dimensions, and the
  # desired output size
  image.size.y = (usr[4] - usr[3]) * pin.ratio[1] * cex
  image.size.x = (usr[2] - usr[1]) * pin.ratio[2] * cex
  for (i in 1:length(x)) {
    # plot each point pos can be NULL (default) or 1, 2, 3, or 4, corresponding
    # to centered (defualt), bottom, left, top, right, respectively.
    if (is.null(pos)) {
      # centered at (x,y), define the bottom/top and left/right boundaries of the
      # image
      x.pos = c(x[i] - (image.size.x * ratio.x)/2, x[i] + (image.size.x * 
                                                             ratio.x)/2)
      y.pos = c(y[i] - (image.size.y * ratio.y)/2, y[i] + (image.size.y * 
                                                             ratio.y)/2)
      
      rasterImage(image, x.pos[1], y.pos[1], x.pos[2], y.pos[2])
    } else if (pos == 1) {
      x.pos = c(x[i] - (image.size.x * ratio.x)/2, x[i] + (image.size.x * 
                                                             ratio.x)/2)
      y.pos = c(y[i] - (image.size.y * ratio.y), y[i])
    } else if (pos == 2) {
      x.pos = c(x[i] - (image.size.x * ratio.x), x[i])
      y.pos = c(y[i] - (image.size.y * ratio.y)/2, y[i] + (image.size.y * 
                                                             ratio.y)/2)
    } else if (pos == 3) {
      x.pos = c(x[i] - (image.size.x * ratio.x)/2, x[i] + (image.size.x * 
                                                             ratio.x)/2)
      y.pos = c(y[i], y[i] + (image.size.y * ratio.y))
    } else if (pos == 4) {
      x.pos = c(x[i], x[i] + (image.size.x * ratio.x))
      y.pos = c(y[i] - (image.size.y * ratio.y)/2, y[i] + (image.size.y * 
                                                             ratio.y)/2)
    }
    
    rasterImage(image, x.pos[1], y.pos[1], x.pos[2], y.pos[2])  #plot image
  }
}

image_files <- list.files("data")

if("Thumbs.db" %in% image_files) {
  image_files <- image_files[-which(image_files=="Thumbs.db")]}

char_names <- gsub(".jpg","",tolower(image_files))

images <- lapply(image_files, function(x) {
  out <- readJPEG(paste0("data/",x))
  r <- out[,,1]+out[,,2]+out[,,3]
  z <- r
  z[,] <- 3
  z[r>0.95*3] <- 0
  
  z <- z/3
  out <- abind(out,z)
  return(out)
})







