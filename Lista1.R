install.packages(c("summarytools","gmodels","dplyr","rfm","lubridate","sqldf"))


library(summarytools)

library(readxl)
Cadastral <- read_excel("MBA/WORKING R/Listas/Cadastral.xlsx")
View(Cadastral)

TabelaFrequencia<-freq(Cadastral$Sexo)
TabelaFrequencia

table(TabelaFrequencia)

table(Cadastral$Sexo)


A<-unique(Cadastral)

install.packages("xlsx")

library(xlsx)

write.xlsx(A,'A.xlsx')

TabelaFrequanciadoA<-freq(A$Sexo)

table(A$Sexo)

table(TabelaFrequanciadoA)

A$data_atual<-Sys.Date()



#exercicio 6
varnumerica <- sapply(A, is.numeric)
varnumerica



#exercicio 7
salariomaximo = A$salario
salariomaximo[which.max(salariomaximo)]

salariominimo = A$salario
salariominimo[which.min(salariominimo)]

install.packages("car")

library(carData)

#exercicio 8
summary(A$salario)
A$faixa_salarial<-cut(A$salario,breaks=c(1574,3000,5000,7000,13500),
                      labels=c("D","C","B","A"))


#Exercicio 9
library(readxl)
Transacional <- read_excel("MBA/WORKING R/Listas/Transacional.xlsx")
View(Transacional)
B<-Transacional
write.xlsx(B,'B.xlsx')
library(readxl)
B <- read_excel("MBA/WORKING R/Listas/B.xlsx")
View(B)


#exercicio 10
AeB<-merge(A,B,by.x = "ID",by.y = "ID", all.x = TRUE)
write.xlsx(AeB,'AeB.xlsx')

library(scales)

AeB <- B <- read_excel("MBA/WORKING R/Listas/AeB.xlsx")
View(AeB)

AeB$Comprometimento_renda<-ValorComprometido<-percent((AeB$ValorEmprestimo/AeB$salario),2)


#Exercicio 12
library(gmodels)
CrossTable(AeB$faixa_salarial,AeB$Sexo,prop.r = TRUE,
           prop.c = FALSE,prop.t = TRUE,prop.chisq = FALSE)


#exercício 13
install.packages("forecast")
library(forecast)



install.packages("plotly")

library(ggplot2)
library(plotly)

ggplot(data = AeB$Comprometimento_renda, aes(x=AeB$salario, y = AeB$ValorEmprestimo))

