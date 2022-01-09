# Carregando pacotes

#Instalando e carregando pacotes

if (!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)

#Selecionando o banco de dados
dados = read.csv2("Banco de dados rentabilidade e divida.csv")
View(dados)
glimpse(dados)

#Criando o modelo
model = lm(roa~endividamento, dados)
summary(model)


#Analise gráfica de pressupostos
par(mfrow=c(2,2)) #Apresentando 04 graficos em um unico plot
plot(model)

par(mfrow=c(1,1))

#Teste de normalidade de Shapiro
shapiro.test(model$residuals)

#Outliers nos resíduos
summary(rstandard(model))

#Teste de homocedasticidade
bptest(model)

#Análise do modelo
summary(model)
options(spien=10)

  
  
  
  
  
  
  
  
  

