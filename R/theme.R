#install and load ggplot2

install.packages("ggplot2")
library(ggplot2)


data(iris)
mod_object <- lm(Petal.Length~Species, data = iris)
print(mod_object)




plot_p <- function()
{ 
  x1<-fitted(mod_object)
  y1<-resid(mod_object)
  y2<-rstandard(mod_object)
  
  df1<-data.frame(x1,y1,y2)
  
  labels_plot_1 <- vector(length=150)
  labels_plot_2 <- vector(length=150)
  labels_plot_1[] <- ""
  labels_plot_2[] <- ""
  labels_plot_1[119]<-'119'
  labels_plot_1[118]<-'118'
  labels_plot_2[99]<-'99'

  
  ggplot(df1,aes(x=x1,y=y1)) + 
    geom_point(shape=21,size=2.5) + 
    labs(title = "Residuals vs Fitted",
         x="Fitted values \n lm(Petal.Length ~ Species)",
         y="Residuals"  )+
    theme(plot.title = element_text(hjust = 0.5, size=12)) +
    geom_text(aes(label=ifelse(labels_plot_1 != FALSE ,as.character(labels_plot_1),'')),hjust=1.5,vjust=0.5) +
    geom_text(aes(label=ifelse(labels_plot_2 != FALSE ,as.character(labels_plot_2),'')),hjust=-0.5,vjust=0.5) +
    theme(panel.background = element_blank(), element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
    stat_summary(fun = median, geom="line", colour="red")+
    geom_hline(yintercept=-0, linetype="dashed", color = "grey")+
    scale_y_continuous(limits=c(-1.5, 1.5), breaks = seq(-1.5, 1.5, by=1))
  
   
  
  
  
  ggplot(df1,aes(x=x1,y=y2)) + 
    geom_point(shape=21) + 
    labs(title = "Residuals vs Fitted",
         x="Fitted values \n lm(Petal.Length ~ Species)",
         y="Residuals"  )+
    theme(plot.title = element_text(hjust = 0.5)) 
  
  
}




?rstandard

?plot

rownames(df1[which.min(df1)])


labels_plot_1 <- vector(length=150)
labels_plot_2 <- vector(length=150)
labels_plot_1[] <- ""
labels_plot_2[] <- ""
labels_plot_1[119]<-'119'
labels_plot_1[118]<-'118'
labels_plot_2[99]<-'99'
labels_plot

rownames(df1[c(99)])
df1[,2]
label_plot <- c("99")



geom_text(aes(label=ifelse(labels_plot != FALSE ,as.character(labels_plot),'')),hjust=0,vjust=0)

ifelse(labels_plot != FALSE ,as.character(labels_plot),'')
