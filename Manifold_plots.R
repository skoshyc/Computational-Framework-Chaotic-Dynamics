#plotting the solution of ODEs
library(deSolve)
library(lazyeval) #load this first to avoid errors
library(ggplot2)
library(plotly)#for the parallel coordinates plot 
#Kot system
parameters= c( D = 0.1, si = 115,mu1 = 0.5, mu2 = 0.2,
               y1 =0.4,y2 =0.6, k1 = 8, k2 = 9, eps = 0.753444, omega =0.455121)
		        
state=c(x= 	4.49844	,y=0.428112,	z=	0.446482   )                   
Kot= function(t, state, parameters){
  with(as.list(c(state,parameters)), {
    A = mu1/D ;
    a = k1/si ;
    B = mu2/D ;
    b = k2/y1/si ;  
    
    dx = 1 + eps*sin(omega*t) - x - A*x*y/( a+x ) ;
    
    dy = (A*x*y)/(a+x) - y -( B*y*z)/( b+y );
    
    dz = (B*y*z)/(b+y) - z ;
    
    return(list(c(dx,dy,dz)))
  })
}              
times <- seq(0, 100, by = 0.01)               
out <- ode(y = state, times = times, func = Kot, parms = parameters)              
plot(out) 
out1=as.data.frame(out)
p=plot_ly(out1, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
        opacity = 1, line = list(width = 6, reverscale = FALSE))
htmlwidgets::saveWidget(as_widget(p),"Kot_parallel_manifold.html")



#####bifurcation plots
library(deSolve)

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(1-x)-(a1*x/(1+b1*x))*y
    dy = (a1*x/(1+b1*x))*y - (a2*y/(1+b2*y))*z-d1*y
    dz = (a2*y/(1+b2*y))*z - d2*z
    return(list(c(dx, dy, dz)))
  })
}

n <- 100 # number of simulations
param.name <- "b2" # choose parameter to perturb
param.seq <- seq(2,3,length = 50) # choose range of parameters

Pars <- c(a1=5, b1=3, a2=0.1, b2=2, d1=0.4, d2=0.01)
Time <- seq(0, 10, length = n)
State <- c(x = .5, y = .3, z= 9.2)

param.index <- which(param.name == names(Pars))
out <- list()
for (i in 1:length(param.seq)){
  out[[i]] <- matrix(0, n, length(State))
}

for (i in 1:length(param.seq)) {
  # set params
  Pars.loop <- Pars
  Pars.loop[param.index] <- param.seq[i]
  # converge
  init <- ode(State, Time, LotVmod, Pars.loop)
  # get converged points
  out[[i]] <- ode(init[n,-1], Time, LotVmod, Pars.loop)[,-1]
}


range.lim <- lapply(out, function(x) apply(x, 2, range))
range.lim <- apply(do.call("rbind", range.lim), 2, range)
plot.variable <- "x" # choose which variable to show
plot(0, 0, pch = "", xlab = param.name, ylab = plot.variable,
     xlim = range(param.seq), ylim = range.lim[,plot.variable])
for (i in 1:length(param.seq)) {
  points(rep(param.seq[i], n), out[[i]][,plot.variable])
}

#for bifurcation ggplot
rm(out.df) #Remove any previous data frames
out.df <- cbind.data.frame(out[[1]][,"x"], cbind(rep(param.seq[1], n)))
colnames(out.df) <- c( "x","b2")
for (i in 2:length(param.seq)) {
  out.df <- rbind.data.frame(out.df, cbind.data.frame(x=out[[i]][,"x"], b2=cbind(rep(param.seq[i], n))))
}

ggplot(out.df, aes(x = b2, y = x)) + geom_point(size = 0.5)





