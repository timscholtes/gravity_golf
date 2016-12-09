library(deSolve)

# define planets (immovable)
n <- 10
parms <- list(x=runif(n,-1,1),
    y=runif(n,-1,1),
  m=runif(n,0,1))

# define start and end locations
hole <- c(x=runif(1,-1,1),y=runif(1,-1,1),r=0.05)
# tee:
v_size = 0.1
state  <- c(x=runif(1,-1,1),y=runif(1,-1,1),
            v0=runif(1,-1,1)*v_size,v1=runif(1,-1,1)*v_size)

# time step and mass of the ball
dt = 1

# describe gravity here
gravity = function(t,state,parms) {
  with(as.list(state), {
    
    x_diff = parms$x-x
    y_diff = parms$y-y
    
    net_accel_vectors<- 1e-3*c(x_diff,y_diff)/
      (((x_diff^2+y_diff^2)^(3/2))/parms$m)
    
    net_accel_vectors <- c(sum(head(net_accel_vectors,n)),
                           sum(tail(net_accel_vectors,n)))
    
    return(list(c(v0,v1,net_accel_vectors)))
  })
}

# run ode here
times=seq(0,100,by=1)
out <- ode(y=state,times=times,func=gravity,parms,method="rk4")


plot(out[,3]~out[,2],type='l',xlim=c(-2,2),ylim=c(-2,2))
points(state[2]~state[1],cex=3,col=2)
draw.circle(x=hole[1],y=hole[2],radius=hole[3],col=1)
for(i in 1:n){
  draw.circle(x=parms$x[i],y=parms$y[i],radius=(parms$m[i]*1e-2)^(1/3),col=3)
}
