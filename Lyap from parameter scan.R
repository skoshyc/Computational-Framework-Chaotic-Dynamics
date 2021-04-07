library(lazyeval) #load this first to avoid errors
library(ggplot2)
library(plotly)#for the parallel coordinates plot 
library(tidyverse)
Sys.setenv("plotly_username"="skoshyc")
Sys.setenv("plotly_api_key"="ATQDgAZBUEaxOgwzlWNT")#to publish the plots online.
#Lyapunov exponents
folder_name="Sprott_system_B/"
folder_name="Kot/"
folder_name="Rossler/"
folder_name="Lorenz/"
folder_name="Hastings_Powell/"
path1=file.path("C:/Users/sherl/Documents/Chaos paper/Copasi",folder_name)
setwd(path1)

filename="optimization_genetic_200_repeat.txt"
filename_simulated="Optimization_simulated_annealing_repeat_2params_1.txt"

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
#parameters=c("eps","omega","x0","y0","z0")#Kot
#parameters=c("eps","omega")#Kot
parameters=c("r","b")#Lorenz
#parameters=c("a1","a2","a3","a4","a5")#Sprott_system_B
#parameters=c("c","b") #Rossler system
#parameters=c("b1","b2","x0","y0","z0") #Hastings_Powell
#parameters=c("b1","b2") #Hastings_Powell
colnames(data2)=c("Lyapunov_exponent",parameters,"Time taken")
colnames(data2_simulated)=c("Lyapunov_exponent",parameters,"Time taken")
#keep only the lyapunov exponent which is positive
#data2=data2[which(data2$Lyapunov_exponent>0),]

#differences=data2[,2:(ncol(data2)-1)]-data2_simulated[1:nrow(data2),2:(ncol(data2_simulated)-1)] 
#write.csv(differences,paste(gsub('/','_',folder_name),"Diff between genetic and simulated annealing.csv",""))

#Comparison plot b/w GA and SA
data_combined=cbind.data.frame(data2_simulated[,2:3],
                               "algorithm" =c(rep("simulated_annealing",nrow(data2_simulated))))
data_combined=rbind.data.frame(data_combined,cbind.data.frame(data2[1:nrow(data2_simulated),2:3],
                                      "algorithm" =   c(rep("genetic_algorithm",nrow(data2_simulated)))))
system_name="Rossler"
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
filename="b1_b2_lyapunov_4.txt"
filename="eps_omega_lyapunov_6.txt"
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
  htmlwidgets::saveWidget(as_widget(p),paste(system_name,"_surface_plot_4.html",sep=""))
}else if(system_number==2){
  p=plot_ly(x = unique(data_surface$eps), y = unique(data_surface$omega), z=lyap_exp)%>% add_surface()
  p=p%>%layout(
    scene = list(
      xaxis = list(nticks = 10,title="eps"),
      yaxis=list(nticks=10,title="omega"),
      zaxis = list(nticks = 5,title="Lyapunov")))
  system_name="Kot"
  htmlwidgets::saveWidget(as_widget(p),paste(system_name,"_surface_plot_6.html",sep=""))
}





#parallel coordinates plot
options(viewer = NULL)
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
#Rossler system
Lyapunov_range=c((min(data2$Lyapunov_exponent)-1),(max(data2$Lyapunov_exponent)+1))
p=data2%>%plot_ly(type = 'parcoords',
                  line = list(color = ~Lyapunov_exponent),
                  dimensions = list(
                    list(range = Lyapunov_range,
                         label = 'Lyapunov_exponent', values = ~Lyapunov_exponent),
                    list(range = c(1,16),
                         constraintrange = c(10,16),
                         label = 'c', values = ~c),
                    list(range = c(0,1),
                         label = 'b', values = ~b)))

#for Sprott system B
a1a2a4_range=c(-6,6)
Lyapunov_range=c((min(data2$Lyapunov_exponent)-1),(max(data2$Lyapunov_exponent)+1))
p=data2%>%plot_ly(type = 'parcoords',
                  line = list(color = ~Lyapunov_exponent),
                  dimensions = list(
                    list(range = Lyapunov_range,
                         label = 'Lyapunov_exponent', values = ~Lyapunov_exponent),
                    list(range = a1a2a4_range,
                         constraintrange = c(-5,5),
                         label = 'a1', values = ~a1),
                    list(range = a1a2a4_range,
                         label = 'a2', values = ~a2),
                    list(range = c(-5,1),
                         label = 'a3', values = ~a3),
                    list(range = a1a2a4_range,
                         label = 'a4', values = ~a4),
                    list(range = c(-5,1),
                         label = 'a5', values = ~a5)))
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
filename1="Parallel_Kot_genetic_2000"
filename1="Parallel_Kot_genetic_20000"
api_create(p,filename = filename1)


#To save the plot as a web page
system_name="Rossler"
system_name="Kot"
system_name="Lorenz"
system_name="Hastings_Powell"
method_name="genetic_200"
method_name="levenberg"
htmlwidgets::saveWidget(as_widget(p),paste0("Parallel_",system_name,"_",method_name,".html"))
