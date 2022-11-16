#define diretório
setwd("/home/carlos/FCD/BigDataRAzure/projetos/projeto1")
getwd()

library(corrgram)
library(tidyverse)

#verificando a distribuição de frequências da variável target
summary(df1$Energy_consumption)

ggplot(df1, aes(x = Energy_consumption)) +
  geom_histogram(binwidth = 0.5, fill = "darksalmon")

# Consumo de Energia x potenciais variáveis preditoras

#para a geração dos gráficos, vamos criar dois vetores, contendo as variáveis do tipo fator (para os 
#boxplots) e as variáveis numéricas (para os scatterplots).

fator <- sapply(df1, is.factor)

fatores <- colnames(select(df1, which(fator)))
numericas <- colnames(select(df1, which(!fator)))
numericas <- numericas[! numericas %in% c("Energy_consumption")] 

#Boxplots

labels_boxplot = list()

for (n in 1:(length(fatores))) {
  labels_boxplot[[n]] <- paste("Energy Consumption by",fatores[n])    
  }

# Função para criar os boxplots
plot.boxes  <- function(X, label){ 
  ggplot(df1, aes_string(x = X, y = "Energy_consumption", group = X)) + 
    geom_boxplot(fill = "violet") + 
    ggtitle(label) +
    theme(text = element_text(size = 18)) 
}

Map(plot.boxes, fatores, labels_boxplot)


#excluindo objetos que não serão mais utilizados
rm(labels_boxplot)
rm(plot.boxes)
rm(n)


#Scatterplots

labels_scatter = list()

for (n in 1:(length(numericas))) {
  labels_scatter[[n]] <- paste("Energy Consumption by",numericas[n])
  }

plot.scatter <- function(X, label){ 
  ggplot(df1, aes_string(x = X, y = "Energy_consumption")) + 
    geom_point(aes_string(colour = "Energy_consumption"), alpha = 2.0) + 
    scale_colour_gradient(low = "purple", high = "orange") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20)) 
}

Map(plot.scatter, numericas, labels_scatter)


#removendo objetos que não erão mais usados. O índice para filtro de variáveis (fator), ainda será usado.


rm(n)
rm(labels_scatter)
rm(plot.scatter)
rm(numericas)

#verificando a correlação das variáveis preditoras com a variável target.

corrgram(df1,cor.method = "pearson", panel = panel.cor)

#percebe-se que a maioria das variáveis são positivamente correlacionadas com o consumo de energia.
#Algumas tem correlação fraca (Wheelbase e Height). Percebe-se ainda que algumas variáveis preditoras
#tem forte correlação entre si, o que pode #representar problemas de multicolinearidade, que afetam
#negativamente o modelo.





























