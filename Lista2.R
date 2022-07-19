library(readxl)
AeB <- read_excel("MBA/WORKING R/Listas/AeB.xlsx")
View(AeB)

install.packages(c("caret","rpart","ratle"))
install.packages('caret', dependencies = TRUE)

library(car)
library(dplyr)
library(lubridate)
library(summarytools)
library(readxl)
library(DT)
library(scales)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)


##NOVAS VARIÁVEIS
#FaixaSalario
#ComprometimentoRenda
#FaixaComprometimentoRenda
#Idade
#FaixaQuantidadeParcelasPagas
#FaixaQuantidadeParcelas
#FaixaTempoServico
#FaixaValorEmprestimo
#FaixaTempoResidencia

#FaixaSalario
min(AeB$salario)
max(AeB$salario)
AeB$FaixaSalario <- cut(AeB$salario, breaks = c(0,2700,5400,8100,10800,13500))
AeB$FaixaSalario

#ComprometimentoRenda
AeB$ComprometimentoRenda <- AeB$ValorEmprestimo/AeB$salario
AeB$ComprometimentoRenda

#FaixaComprometimentoRenda
min(AeB$ComprometimentoRenda)
max(AeB$ComprometimentoRenda)

AeB$FaixaComprometimentoRenda <- cut (AeB$ComprometimentoRenda, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7))
table(AeB$FaixaComprometimentoRenda)

#Idade
AeB$idade <- year(AeB$data_atual)-year(AeB$DataNascimento)


#FaixaQuantidadeParcelasPagas
min(AeB$QtdaPagas)
max(AeB$QtdaPagas)

AeB$FaixaQuantidadeParcelasPagas <- cut (AeB$QtdaPagas, breaks = c(0,5,10,15))
table(AeB$FaixaQuantidadeParcelasPagas)


#FaixaQuantidadeParcelas
min(AeB$QtdaParcelas)
max(AeB$QtdaParcelas)

AeB$FaixaQuantidadeParcelas <- cut (AeB$QtdaParcelas, breaks = c(0,15,30,45))
table(AeB$FaixaQuantidadeParcelas)


#FaixaTempoServico
min(AeB$TempodeServiço)
max(AeB$TempodeServiço)

AeB$FaixaTempoServico <- cut (AeB$TempodeServiço, breaks = c(60,70,80,90,100))
table(AeB$FaixaTempoServico)


#FaixaValorEmprestimo
min(AeB$ValorEmprestimo)
max(AeB$ValorEmprestimo)

AeB$FaixaValorEmprestimo <- cut (AeB$ValorEmprestimo, breaks = c(0,500,1000,1500,2000))
table(AeB$FaixaValorEmprestimo)


#FaixaTempoResidencia
min(AeB$TempodeResidencia)
max(AeB$TempodeResidencia)
AeB$FaixaTempoResidencia <- cut (AeB$TempodeResidencia, breaks = c(0,10,20,30,40,50))
table(AeB$FaixaTempoResidencia)


names(AeB)


AeB$Sexo <- factor(AeB$Sexo)
AeB$Atraso <- factor(AeB$Atraso)
AeB$default <- factor(AeB$default)


###Árvore de decisão
set.seed(123456);rnorm(20)

treina <- createDataPartition(AeB$ID,p = 0.7,list = FALSE,times = 1)
head(treina)

AmostraTreina <- AeB[treina,]
head(AmostraTreina)

AmostraTeste <-AeB[-treina,]
head(AmostraTeste)

dim(AeB)
dim(AmostraTreina)
dim(AmostraTeste)

table(AeB$default)


##MODELO 1
#VARIÁVEIS = NumerodeFilhos - FaixaValorEmprestimo - FaixaQuantidadeParcelas - FaixaQuantidadeParcelasPagas - FaixaTempoResidencia - Sexo 
arvore1 <- rpart(
  default ~ AmostraTreina$NumerodeFilhos + AmostraTreina$FaixaValorEmprestimo + AmostraTreina$FaixaQuantidadeParcelas 
  + AmostraTreina$FaixaQuantidadeParcelasPagas
  + AmostraTreina$TempodeResidencia + AmostraTreina$Sexo,
  data = AmostraTreina,
  method = "class",
  parms = list(split='gini'),
  minsplit = 2,
  minbucket = 1
)
arvore1
arvore1$variable.importance

fancyRpartPlot(arvore1)


AmostraTreina$prob <- predict(arvore1,newdata = AmostraTreina, type = "prob")
AmostraTreina$prev <- predict(arvore1,newdata = AmostraTreina, type = "class")
Funfou <- table(AmostraTreina$default, AmostraTreina$prev)
FunfouPraGeral <- (Funfou[1]+Funfou[4])/sum(Funfou)
Funfou
FunfouPraGeral
fancyRpartPlot(arvore1)


#Modelo2
#Variáveis: FaixaSalario - FaixaQuantidadeParcelasPagas - ; FaixaValorEmprestimo - FaixaComprometimentoRenda
arvore2 <- rpart(
  default ~ AmostraTreina$FaixaSalario + AmostraTreina$FaixaQuantidadeParcelasPagas 
  + AmostraTreina$FaixaValorEmprestimo + AmostraTreina$FaixaComprometimentoRenda,
  data = AmostraTreina,
  method = "class",
  parms = list(split = 'gini'),
  minsplit = 2,
  minbucket = 1
)
arvore2
arvore2$variable.importance
AmostraTreina$prob <- predict(arvore2,newdata = AmostraTreina, type = "prob")
AmostraTreina$prev <- predict(arvore2,newdata = AmostraTreina, type = "class")
Funfou <- table(AmostraTreina$default, AmostraTreina$prev)
FunfouPraGeral <- (Funfou[1]+Funfou[4])/sum(Funfou)
Funfou
FunfouPraGeral
fancyRpartPlot(arvore2)


#Modelo3
#Variáveis: Atraso – FaixaComprometimentoRenda – FaixaQuantidadeParcelasPagas –  FaixaValorEmprestimo - Sexo
arvore3 <- rpart(
  default ~ AmostraTreina$Atraso + AmostraTreina$FaixaComprometimentoRenda 
  + AmostraTreina$FaixaQuantidadeParcelasPagas + AmostraTreina$FaixaValorEmprestimo
  + AmostraTreina$Sexo,
  data = AmostraTreina,
  method = "class",
  parms = list(split = 'gini'),
  minsplit = 2,
  minbucket = 1,
)
arvore3
arvore3$variable.importance
AmostraTreina$prob <- predict(arvore3,newdata = AmostraTreina, type = "prob")
AmostraTreina$prev <- predict(arvore3,newdata = AmostraTreina, type = "class")
Funfou <- table(AmostraTreina$default, AmostraTreina$prev)
FunfouPraGeral <- (Funfou[1]+Funfou[4])/sum(Funfou)
Funfou
FunfouPraGeral
fancyRpartPlot(arvore3)

#Modelo 4 
#Variáveis: Atraso - FaixaQuantidadeParcelasPagas - FaixaSalario
arvore4 <- rpart(
  default ~ AmostraTreina$Atraso + AmostraTreina$FaixaQuantidadeParcelasPagas
  + AmostraTreina$FaixaSalario,
  data = AmostraTreina,
  method = "class",
  parms = list(split = 'gini'),
  minsplit = 2,
  minbucket = 1
)
arvore4
arvore4$variable.importance
AmostraTreina$prob <- predict(arvore4,newdata = AmostraTreina, type = "prob")
AmostraTreina$prev <- predict(arvore4,newdata = AmostraTreina, type = "class")
Funfou <- table(AmostraTreina$default, AmostraTreina$prev)
FunfouPraGeral <- (Funfou[1]+Funfou[4])/sum(Funfou)
Funfou
FunfouPraGeral
fancyRpartPlot(arvore4)


#Modelo 5
#Variáveis: FaixaQuantidadeParcelas - FaixaSalario - FaixaQuantidadeParcelasPagas - FaixaValorEmprestimo 
arvore5 <- rpart(
  default ~ AmostraTreina$FaixaQuantidadeParcelas + AmostraTreina$FaixaSalario
  + AmostraTreina$FaixaQuantidadeParcelasPagas + AmostraTreina$FaixaValorEmprestimo,
  data = AmostraTreina,
  method = "class",
  parms = list(split = 'gini'),
  minsplit = 2,
  minbucket = 1
)
arvore5
arvore5$variable.importance
AmostraTreina$prob <- predict(arvore5,newdata = AmostraTreina, type = "prob")
AmostraTreina$prev <- predict(arvore5,newdata = AmostraTreina, type = "class")
Funfou <- table(AmostraTreina$default, AmostraTreina$prev)
FunfouPraGeral <- (Funfou[1]+Funfou[4])/sum(Funfou)
Funfou
FunfouPraGeral
fancyRpartPlot(arvore5)

#Matriz de confusao
confusionMatrix(factor(AmostraTreina$default), factor(AmostraTreina$prev))

