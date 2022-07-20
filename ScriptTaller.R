library(readxl)
Chile <- read_excel("~/GitHub/estadistica/1. ChileTaller.xlsx", 
                    col_types = c("numeric", "text", "numeric", 
                                  "text", "numeric", "text", "numeric", 
                                  "numeric", "text"))
#View(Chile)
Region_= Chile$region
Rtabla=data.frame(table(Region_))
porcentaje=prop.table(Rtabla[,2])
Rtabla2= cbind(Rtabla, porcentaje)
cum_frequencia=cumsum(Rtabla2[,2])
Rtabla3= cbind(Rtabla2, cum_frequencia)
cum_porcentaje=cumsum(Rtabla3[,3])
Rtabla4= cbind(Rtabla3, cum_porcentaje)
Rtabla4


BPregion <- barplot(prop.table(table(Chile$region)),col=c("orange","blue","green","red","purple"),ylim=c(0,0.8),xlim=c(0,11),main="Frecuencias relativas de participaciÃ³n por regiÃ³n",ylab ="Frecuencias Relativas")

Vote_= Chile$vote
Vtabla=data.frame(table(Vote_))
porcentaje=prop.table(Vtabla[,2])
Vtabla2= cbind(Vtabla, porcentaje)
cum_frequencia=cumsum(Vtabla2[,2])
Vtabla3= cbind(Vtabla2, cum_frequencia)
cum_porcentaje=cumsum(Vtabla3[,3])
Vtabla4= cbind(Vtabla3, cum_porcentaje)
Vtabla4

library(dplyr)
sel<-c("N","S")
Chile_NteySur <- Chile %>% filter(region %in% sel)
library(gmodels)
CrossTable(Chile_NteySur$education,Chile_NteySur$vote)



library(readxl)
Nutrition <- read_excel("~/GitHub/estadistica/2. nutrition_elderly.xlsx", 
                        col_types = c("text", "text", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "text", "text", "text", 
                                      "text", "text", "text"))
#View(Nutrition)

#Selección de personas mayores de 79 de género femenino**

Nutrition_2<-subset(Nutrition,age>79 & gender=="2")

Fat_= Nutrition_2$fat
Fat_tabla=data.frame(table(Fat_))
porcentaje=prop.table(Fat_tabla[,2])
Fat_tabla2= cbind(Fat_tabla, porcentaje)
cum_frequencia=cumsum(Fat_tabla2[,2])
Fat_tabla3= cbind(Fat_tabla2, cum_frequencia)
cum_porcentaje=cumsum(Fat_tabla3[,3])
Fat_tabla4= cbind(Fat_tabla3, cum_porcentaje)
Fat_tabla4

pie(Fat_tabla4[,2],labels=Fat_tabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")

BPEdu <- barplot(prop.table(table(Nutrition_2$fat)),col=c("orange","blue","green","purple","yellow","red"), ylim=c(0,0.4),main="Frecuencias relativas de fat",ylab ="Frecuencias Relativas")

Status_= Nutrition_2$status
Status_tabla=data.frame(table(Status_))
porcentaje=prop.table(Status_tabla[,2])
Status_tabla2= cbind(Status_tabla, porcentaje)
cum_frequencia=cumsum(Status_tabla2[,2])
Status_tabla3= cbind(Status_tabla2, cum_frequencia)
cum_porcentaje=cumsum(Status_tabla3[,3])
Status_tabla4= cbind(Status_tabla3, cum_porcentaje)
Status_tabla4

BPStatus <- barplot(prop.table(table(Nutrition_2$status)),col=c("orange","blue","green"), ylim=c(0,1),main="Frecuencias relativas de status",ylab ="Frecuencias Relativas")


hist(Nutrition_2$height,main ="titulo", xlab="Height")

library(agricolae)
h4<-graph.freq(Nutrition_2$height, col="blue", frequency=3)
normal.freq(h4, col="red", lty=4,lwd=2, frequency=3,las=2)

par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h9<-graph.freq(Nutrition_2$height, density=6, col="blue", frequency=3,xlab="height",ylim=c(0,0.1))
h10<-graph.freq(Nutrition_2$height, border=0, frequency=3,ylim=c(0,0.1),xlab="height")
polygon.freq(h10,col="blue", frequency=3)

h0=hist(Chile$population)
par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
#h1<-graph.freq(Chile$population, density=6, col="blue", frequency=3,xlab="population",ylim=c(0,1400))
#h2<-graph.freq(Chile$population, border=0, frequency=3,ylim=c(0,1400),xlab="population")
polygon.freq(h0,col="blue", frequency=3)

library(agricolae)
data(genxenv)
yield <- subset(genxenv$YLD,genxenv$ENV==2)
yield <- round(yield,1)
h<- graph.freq(yield,axes=FALSE, frequency=1, ylab="frequency",col="yellow")
axis(1,h$breaks)

library(agricolae)
par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Chile$population, density=6, col="blue",border="red",ylim=c(0,0.6), frequency=2,xlab="weight")
h2<-graph.freq(Chile$population, border=0,ylim=c(0,0.6), frequency=2,xlab="weight")
polygon.freq(h2,col="blue", frequency=2)
axis(2,seq(0,20,0.1))
# To reproduce histogram.
h1 <- graph.freq(h, col="blue", frequency=2,border="red", density=20,axes=FALSE,
                 xlab="YIELD",ylab="relative")
axis(1,h$breaks)
axis(2,seq(0,.4,0.1))
# summary, only frecuency

par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Chile$age, density=6, col="blue",border="red",ylim=c(0,0.6), frequency=3,xlab="Age")
h2<-graph.freq(Chile$age, border=0,ylim=c(0,0.6), frequency=3,xlab="Age")
polygon.freq(h2,col="blue", frequency=3)

par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Chile$age, density=6, col="blue",border="red",ylim=c(0,500), frequency=1,xlab="Age")
h2<-graph.freq(Chile$age, border=0,ylim=c(0,500), frequency=1)
polygon.freq(h2,col="blue", frequency=1)


par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Chile$population, density=6, col="blue",border="red",ylim=c(0,0.6), frequency=2,xlab="population")
h2<-graph.freq(Chile$population, border=0,ylim=c(0,0.6), frequency=2,xlab="population")
polygon.freq(h2,col="blue", frequency=2)


par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Chile$income, density=6, col="blue",border="red", frequency=2,xlab="Income")
h2<-graph.freq(Chile$income, border=0, frequency=2,xlab="Income")
polygon.freq(h2,col="blue", frequency=2)

h1<-graph.freq(Chile$income, density=6, col="blue",border="red", frequency=2,xlab="Age")

par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Chile$statusquo, density=6, col="blue",border="red", frequency=2,xlab="Statusquo")
h2<-graph.freq(Chile$statusquo, border=0, frequency=2,xlab="Statusquo")
polygon.freq(h2,col="blue", frequency=2)

Fat_= Nutrition_2$fat
Fat_tabla=data.frame(table(Fat_))
porcentaje=prop.table(Fat_tabla[,2])
Fat_tabla2= cbind(Fat_tabla, porcentaje)
cum_frequencia=cumsum(Fat_tabla2[,2])
Fat_tabla3= cbind(Fat_tabla2, cum_frequencia)
cum_porcentaje=cumsum(Fat_tabla3[,3])
Fat_tabla4= cbind(Fat_tabla3, cum_porcentaje)
Fat_tabla4

BPFat <- barplot(prop.table(table(Nutrition_2$fat)),col=c("orange","blue","green","purple","yellow","red"), 
                 ylim=c(0,0.4),main="Frecuencias relativas de fat",ylab ="Frecuencias Relativas", names.arg = c("Butter","Margarine","Peanut Oil","Sunflower Oil","Olive Oil","Mix of vegetables Oil"))


Status_= Nutrition_2$status
Status_tabla=data.frame(table(Status_))
porcentaje=prop.table(Status_tabla[,2])
Status_tabla2= cbind(Status_tabla, porcentaje)
cum_frequencia=cumsum(Status_tabla2[,2])
Status_tabla3= cbind(Status_tabla2, cum_frequencia)
cum_porcentaje=cumsum(Status_tabla3[,3])
Status_tabla4= cbind(Status_tabla3, cum_porcentaje)
Status_tabla4

BPStatus <- barplot(prop.table(table(Nutrition_2$status)),col=c("orange","blue","green"), ylim=c(0,1),main="Frecuencias relativas de status",ylab ="Relativas (%)",names.arg = c("Single","Living With Spouse","Living With Family"))


tablacruzada=CrossTable(Nutrition_2$fat,Nutrition_2$status)



Region_= Chile$region
Rtabla=data.frame(table(Region_))
porcentaje=prop.table(Rtabla[,2])
Rtabla2= cbind(Rtabla, porcentaje)
cum_frequencia=cumsum(Rtabla2[,2])
Rtabla3= cbind(Rtabla2, cum_frequencia)
cum_porcentaje=cumsum(Rtabla3[,3])
Rtabla4= cbind(Rtabla3, cum_porcentaje)
Rtabla4


BPregion <- barplot(prop.table(table(Chile$region)),col=c("orange","blue","green","red","purple"),
                    legend.text=c("Center","Metropolitan","North","South","City of Santiago"),
                    ylim=c(0,0.8),
                    xlim=c(0,11),main="Frecuencias relativas de participación por región",
                    ylab ="Frecuencias Relativas", 
                    names.arg = c("Center","Metropolitan","North","South","City of Santiago")
                    ,cex.names = 0.5,inside = T)

par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Nutrition_2$height, density=6, col="blue", frequency=2,xlab="height",ylab="Relativa (%)",ylim=c(0,0.7))
h2<-graph.freq(Nutrition_2$height, border=0, frequency=2,ylim=c(0,0.7),xlab="height",ylab="Relativa (%)")
polygon.freq(h2,col="blue", frequency=2)

par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Nutrition_2$height, density=6, col="blue", frequency=3,xlab="height")
h2<-graph.freq(Nutrition_2$height, border=0, frequency=3,xlab="height",ylab="Relativa (%)")
polygon.freq(h2,col="blue", frequency=3)
hist(Nutrition_2$height)


par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Nutrition_2$weight, density=6, col="blue", frequency=2,xlab="weight")
h2<-graph.freq(Nutrition_2$weight, border=0, frequency=2,xlab="weight")
polygon.freq(h2,col="blue", frequency=2)

h1<-graph.freq(Nutrition_2$weight, density=6, col="blue", frequency=2,xlab="weight")



library(MASS)
library(ggplot2)


# Gráfico Simple (X,Y)
ggplot(data = Nutrition_2, aes(x = height, y = weight)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersión altura y peso") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# Midiendo el nivel de correlación:
#cor(x = log10(Nutrition_2$height), y = Nutrition_2$weight)
cor(x = Nutrition_2$height, y = Nutrition_2$weight, method = "pearson")

par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<-graph.freq(Chile$age, density=6, col="blue",border="red", frequency=2,xlab="Age",ylim=c(0,0.2))
h2<-graph.freq(Chile$age, border=0, frequency=2,ylim=c(0,0.2))
polygon.freq(h2,col="blue", frequency=2)

