library(lazyeval) #load this first to avoid errors
library(ggplot2)
library(plotly)#for the parallel coordinates plot 
library(tidyverse)
Sys.setenv("plotly_username"="skoshyc")
Sys.setenv("plotly_api_key"="ATQDgAZBUEaxOgwzlWNT")#to publish the plots online.
#Lyapunov exponents
folder_name="Kot/"
folder_name="Lorenz/"
folder_name="Hastings_Powell/"
path1=file.path("insert file path to your COPASI results",folder_name)
setwd(path1)

filename="insert name of result .txt file generated from genetic algorithm in COPASI"
filename_simulated="insert name of .txt file generated from simulated annealing algorithm in COPASI"

data2=read.delim(file=filename,sep = "",header=FALSE) #header=TRUE in the repeat file
data2_simulated=read.delim(file=filename_simulated,sep = "",header=FALSE) 
#when the report is Optimization_copy from Copasi we have to rename the columns and remove the parenthesis 
#and function_evaluations
#The first column is the function evaluation step, second column will be the lyapunov exponent,
#third and last but one column are the parenthesis. The last column is the CPU time.
#The fourth to last but two column are the parameters in the order
#in which they were inputed into Copasi's optimization search.
data2=data2[,-c(1,3,ncol(data2)-1)]
data2_simulated=data2_simulated[,-c(1,3,ncol(data2_simulated)-1)]
#Use the parameters for the relevant system
parameters=c("eps","omega","x0","y0","z0")#Kot
parameters=c("r","b")#Lorenz
parameters=c("b1","b2","x0","y0","z0") #Hastings_Powell
colnames(data2)=c("Lyapunov_exponent",parameters,"Time taken")
colnames(data2_simulated)=c("Lyapunov_exponent",parameters,"Time taken")

#Comparison plot b/w Genetic algorithm and simulated annealing
data_combined=cbind.data.frame(data2_simulated[,2:3],
                               "algorithm" =c(rep("simulated_annealing",nrow(data2_simulated))))
data_combined=rbind.data.frame(data_combined,cbind.data.frame(data2[1:nrow(data2_simulated),2:3],
                                      "algorithm" =   c(rep("genetic_algorithm",nrow(data2_simulated)))))
system_name="Kot"
system_name="Lorenz"
system_name="Hastings_Powell"

comparison_plot=ggplot(data_combined, aes(x=data_combined[,1], y=data_combined[,2])) +
  scale_x_continuous(breaks=seq(0.12,0.27,0.03))+
  scale_y_continuous(breaks=seq(0.5,1,0.1))+
  geom_point(aes(color=algorithm,shape=algorithm))+
  labs(title=paste("Comparison of ",system_name," data_2params",sep=""),x=colnames(data_combined)[1],
       y=colnames(data_combined)[2]
       , caption=paste0("The results from ", nrow(data2_simulated), " runs of 2 optimization algorithms"))
p=ggplotly(comparison_plot)
htmlwidgets::saveWidget(as_widget(p),paste("Comparison_",system_name,"_data_2params_1",".html",sep=""))

#surface plot of parameters and lyapunov exponent
folder_name="Kot/"
folder_name="Hastings_Powell/"
system_number=1 #if it is Hastings_Powell
system_number=2 #if it is Kot
path1=file.path("C:/Users/sherl/Documents/Chaos paper/Copasi",folder_name)
setwd(path1)
filename="name of text file containing the results from Lyapunov exponent calculation and 2 chosen parameters"
data_surface=read.delim(file=filename,sep = "",header=TRUE) #header=TRUE in the repeat file
colnames(data_surface)[1]="Lyapunov_exponent"
lyap_exp=matrix(data_surface$Lyapunov_exponent,nrow=100,ncol=100)
if(system_number==1){
  p=plot_ly(x = unique(data_surface$b1), y = unique(data_surface$b2), z = lyap_exp)%>% add_surface()
  p=p%>%layout(
    scene = list(
      xaxis = list(nticks = 10,title="b1"),
      yaxis=list(nticks=10,title="b2"),
      zaxis = list(nticks = 10,title="Lyapunov")))
  system_name="Hastings_Powell" 
  htmlwidgets::saveWidget(as_widget(p),paste(system_name,"_surface_plot.html",sep=""))
}else if(system_number==2){
  p=plot_ly(x = unique(data_surface$eps), y = unique(data_surface$omega), z=lyap_exp)%>% add_surface()
  p=p%>%layout(
    scene = list(
      xaxis = list(nticks = 10,title="eps"),
      yaxis=list(nticks=10,title="omega"),
      zaxis = list(nticks = 5,title="Lyapunov")))
  system_name="Kot" 
  htmlwidgets::saveWidget(as_widget(p),paste(system_name,"_surface_plot.html",sep=""))
}





#parallel coordinates plot
options(viewer = NULL)

#constraint range is a user-defined range of the parameters you wish to see. 
#Kot system
Lyapunov_range=c((min(data2$Lyapunov_exponent)-1),(max(data2$Lyapunov_exponent)+1))
p=data2%>%plot_ly(type = 'parcoords',
          line = list(color = ~Lyapunov_exponent),
          dimensions = list(
            list(range = Lyapunov_range,
                 label = 'Lyapunov_exponent', values = ~Lyapunov_exponent),
            list(range = c(0,1),
                 constraintrange = c(0.1,0.9),
                 label = 'Eps', values = ~eps),
            list(range = c(0,1),
                 label = 'Omega', values = ~omega),
            list(range = c(0,5),
                 label = 'x0', values = ~x0),
            list(range = c(0,1),
                 label = 'y0', values = ~y0),
            list(range = c(0,1),
                 label = 'z0', values = ~z0)))


#for lorenz system
Lyapunov_range=c((min(data2$Lyapunov_exponent)-1),(max(data2$Lyapunov_exponent)+1))
p=data2%>%plot_ly(type = 'parcoords',
                  line = list(color = ~Lyapunov_exponent),
                  dimensions = list(
                    list(range = Lyapunov_range,
                         label = 'Lyapunov_exponent', values = ~Lyapunov_exponent),
                    list(range = c(log(80),log(110)),
                         constraintrange = c(log(90),log(100)),
                         label = 'ln(r)', values = ~log(r)),
                    list(range = c(2.55,2.75),
                         label = 'b', values = ~b)))


#Hastings_Powell system
Lyapunov_range=c((min(data2$Lyapunov_exponent)-1),(max(data2$Lyapunov_exponent)+1))
p=data2%>%plot_ly(type = 'parcoords',
                  line = list(color = ~Lyapunov_exponent),
                  dimensions = list(
                    list(range = Lyapunov_range,
                         label = 'Lyapunov_exponent', values = ~Lyapunov_exponent),
                    list(range = c(2,4),
                         constraintrange = c(2.5,3.5),
                         label = 'b1', values = ~b1),
                    list(range = c(0,3.5),
                         label = 'b2', values = ~b2),
                    list(range = c(0,1),
                         label = 'x0', values = ~x0),
                    list(range = c(0,1),
                         label = 'y0', values = ~y0),
                    list(range = c(7,11),
                         label = 'z0', values = ~z0)))


print(p)
filename1="insert desired filename"
api_create(p,filename = filename1)


#To save the plot as a web page
system_name="Kot"
system_name="Lorenz"
system_name="Hastings_Powell"
method_name="insert optimization algorithm name"
htmlwidgets::saveWidget(as_widget(p),paste0("Parallel_",system_name,"_",method_name,".html"))
