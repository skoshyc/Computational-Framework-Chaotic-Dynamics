#plotting the solution of ODEs
library(deSolve)
library(lazyeval) #load this first to avoid errors
library(ggplot2)
library(plotly)#for the manifold plot 
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
htmlwidgets::saveWidget(as_widget(p),"insert name of file.html")





