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