# Carregando pacotes

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyer, ggplot2, car, srtatix, lmtest, ggpubr)

# Carregando o banco de dados

dados <- read.csv2("Banco de Dados 11.csv")
View(dados)
glimpse(dados)


# Verificar os pressupostos para a regressão linear simples

## Passo 01: Relação linear entre VARIAVEL DEPENDENTE  e VARIAVEL INDEPENDETE:
### vendas: VD  publicidade: VI

plot(dados$Publicidade,dados$Vendas)

# Passo 02: Construção do modelo
mod <-lm(Vendas ~ Publicidade, dados)
mod

# Analise gráfica
par(mfrow=c(2,2)) # ver em 4 graficos
plot(mod)

# ANALISANDO OS GRÁFICOS

# 1°: linha vermelha proxima a pontilhada >>>> inferência de linearidade (o que queremos)
#     padrão dos pontos retangulares >>>>> inferencia de homocedasticidade / homogeniedade das variancias (o que queremos)
#     padrão dos pontos triangulares >>>>>> inferencia de heterocedastividade


# 2°: distribuição normal >>>> os resíduos em cima da linha pontilhada

# 3° caso exista homocedasticidade >>>> linha vermelha horizontal

# 4°: Há outliers quando os residuos estão abaixo de de -3 e a cima de 3
#     pontos de alavancagem: ponto que deve ser observado pois influenciam a estimação do modelo / ponto fora da linha pontilhada vermelha

par(mfrow=c(1,1)) #visualizar apenas um grafico


# Passo 03: Teste de Shapiro/ normalidade dos resíduos
shapiro.test(mod$residuals)
# h0: distribuição dos dados = normal  p > 0,05 (quero aceitar)
# h1: distribuição dos dados diferente de normal   p < ou = 0,05

# Passo 04: Outliers nos resíduos
summary(rstandard(mod))


# Passo 05: Pressuposto da independencia dos resíduos
durbinWatsonTest(mod)
# Statistic recomendo ser entre 1 e 3
# Valor p-value se > 0,05 aceita h1: a correlação é diferene de 0 (quero aceitar)
#               se < 0,05 aceita h0: a correlação é igual a 

# Passo 06: Teste de homocedasticidade
bptest(mod)
# h0: ha homocedasticidade p > 0,05 (quero aceitar)
# h1: não há homocedasticidade p < ou = a 0,05


#Passo 07: analise do modelo
summary(mod)
options(scipen = 10)

# h0: coeficiente = 0  p > 0,05
# h1: coeficiente diferente de 0 p < ou = a 0,05
# se coeficiente == 0 (aceitando h0), a publicidade não tem impacto nas vendas

#INTERPRETANDO O "ESTIMATE"
# Publicidade: a cada unidade de moeda gasta em publicidade, aumenta 0,1 em venda em média
#(Intercepto): se eu não investir nada em publicidade eu espero uma venda de 125 unidades de moeda

#ANALISANDO o R-squared
# o quanto a variavel independente explica a variavel dependente
# o quanto o modelo explica a variavel dependente

#ANALISANDO ESTATISTICA F
# H0:  p < 0,05: o modelo criado esta de fato melhor que um modelo sem previsor
# H1: p > 0,05: exite uma diferença entre os modelos

#Passo 08: Gráfico de dispersão

ggplot(data=dados, mapping = aes(x= Publicidade, y= Vendas)) +
  geom_point() + geom_smooth(method = "lm", col="red") +
  stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label.., sep="*plain(\",\")~~")), label.x=0, label.y=400) + 
  theme_classic()
  
  
  
  
  
  
  
  
  
  
  

