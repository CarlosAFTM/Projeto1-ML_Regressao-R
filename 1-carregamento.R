#define diretório
setwd("/home/carlos/FCD/BigDataRAzure/projetos/projeto1")
getwd()

library(haven)
library(tidyverse)


#carrega dataset 
df <- read_spss("FEV-dataset-SPSS.sav")
View(df)
dim(df)
str(df)


#convertendo variáveis do tipo caractere para o tipo fator

df1 <- df %>% 
  mutate_if(is.character, factor)

z <- c("Seats","Doors","Tire_size","DC")
lapply(df1[z], unique)




#As variáveis "Seats","Doors","Tire_size","DC", apesar de numéricas, tem poucos valores. É mais apropriada a 
#classificação no tipo Fator

df1[,z] <- lapply(df1[,z],factor)
str(df1)

rm(z)

#verifica valores NA

colSums(is.na(df)) 
#os modelos sem dados para a variável dependente (Energy Comsuption) não serão 
#úteis para elaboração do modelo preditivo. Optou-se por excluí-los.

df1 <- filter(df1, !is.na(Energy_consumption))

which(is.na(df1), arr.ind = TRUE)
colnames(df1[c(7,22,23)])

#restaram apenas dois modelos de veículos com valores NA, nas variáveis "Brakes", Boot_capacity" e "Acceleration".
#optou-se por excluí-los do dataset

df1 <- na.omit(df1)

any(is.na(df1))
dim(df1)

#para evitar overfitting, optou-se por excluir as variáveis "CAR", "Make" e "Model" do dataset. O objetivo é
#um modelo que preveja o consumo de energia através da análise de dados sobre o veículo, independente de fabricante,
#nome ou fabricante.

df1 <- select(df1,-c("CAR","Make","Model"))
View(df1)

