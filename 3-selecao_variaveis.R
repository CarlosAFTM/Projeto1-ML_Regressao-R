#Definindo o diretório
setwd("/home/carlos/FCD/BigDataRAzure/projetos/projeto1")
getwd()

#Colinearidade
library(caret)
library(car)

#Primeiro vamos criar um modelo de regressão linear para avaliar a multicolinearidade com a função "vif" do pacote car.
#para isso, vamos dividir o dataframe pelas variáveis numéricas.

df1_numeric <- select(df1,-all_of(fatores))
set.seed(123)


#criando modelo de regressão linear para avaliação das variáveis
M <- lm(Energy_consumption ~ .,data=df1_numeric)
summary(M)
vif(M)

#O R2 ajustado foi de 0,9225. Excluindo do modelo a variável "Weight" - valores VIF acima de 5 são problemáticos
#para o modelo

M2 <- lm(Energy_consumption~.-Weight,data=df1_numeric)
summary(M2)
vif(M2)
#O R2 ajustado foi de 0,9248. Houve melhora da colinearidade.
#Excluindo variável "Power" do dataset


M3 <- lm(Energy_consumption~.-Weight -Power,data=df1_numeric)
summary(M3)
vif(M3)
#O R2 ajustado foi de 0,9260. Houve melhora da colinearidade.
#Excluindo variável "Permissible Weight" do dataset


M4 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight,data=df1_numeric)
summary(M4)
vif(M4)
#O R2 ajustado foi de 0,9247. Houve melhora da colinearidade.
#Excluindo variável "Max_speed" do dataset


M5 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight -Max_speed,data=df1_numeric)
summary(M5)
vif(M5)
#O R2 ajustado foi de 0,9263. Houve melhora da colinearidade.
#Excluindo variável "Torque" do dataset


M6 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight -Max_speed -Torque,data=df1_numeric)
summary(M6)
vif(M6)
#O R2 ajustado foi de 0,9282. Houve melhora da colinearidade.
#Excluindo variável "Lenght" do dataset


M7 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight -Max_speed -Torque -Length,data=df1_numeric)
summary(M7)
vif(M7)
#O R2 ajustado foi de 0,9215. Houve melhora da colinearidade.
#Excluindo variável "Price" do dataset


M8 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight -Max_speed -Torque -Length -Price,data=df1_numeric)
summary(M8)
vif(M8)
#O R2 ajustado foi de 0,923. Houve melhora da colinearidade.
#Excluindo variável "Wheelbase" do dataset


M9 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight -Max_speed -Torque -Length -Price
         -Wheelbase,data=df1_numeric)
summary(M9)
vif(M9)
#O R2 ajustado foi de 0,9242. Houve melhora da colinearidade.
#Excluindo variável "Acceleration" do dataset


M10 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight -Max_speed -Torque -Length -Price
         -Wheelbase -Acceleration,data=df1_numeric)
summary(M10)
vif(M10)
#O R2 ajustado foi de 0,9263. Houve melhora da colinearidade.
#Excluindo variável "Boot_capacity" do dataset


M11 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight -Max_speed -Torque -Length -Price
          -Wheelbase -Acceleration -Boot_capacity,data=df1_numeric)
summary(M11)
vif(M11)
#O R2 ajustado foi de 0,928. Houve melhora da colinearidade.
#Excluindo variável "Width" do dataset


M12 <- lm(Energy_consumption~.-Weight -Power -Permissible_weight -Max_speed -Torque -Length -Price
          -Wheelbase -Acceleration -Boot_capacity -Width,data=df1_numeric)
summary(M12)
vif(M12)
#O R2 ajustado foi de 0,9248. Houve melhora da colinearidade.
#Excluindo variável "Height" do dataset


M13 <- lm(Energy_consumption~Battery+Range,data=df1_numeric)
summary(M13)
vif(M13)

#O R2 ajustado foi de 0,9022. Houve melhora da colinearidade, mas também alguma perda de precisão do modelo.
#Como a colinearidade já está resolvida, decidimos continuar com a variável "Heigth" no modelo.

#Vamos criar um segundo dataframe, incluindo as variáveis do tipo fator.

z <- c(fatores,c("Battery","Range","Height","Energy_consumption"))
df2 <- df1 %>%
  select(all_of(z))
View(df2)
rm(z)

#Vamos avaliar o modelo com as variáveis do tipo fator incluídas

M14 <- lm(Energy_consumption ~ .,data=df2)
summary(M14)

#O R2 ajustado foi de 0,981. Vamos agora excluir as variáveis fator menos relevantes para o modelo, começando com "Doors".

M15 <- lm(Energy_consumption ~ . -Doors,data=df2)
summary(M15)

#O R2 ajustado foi de 0,9819. Todas as outras variáveis fator apresentam algum nível de importância. Assim, chegou-se
#ao dataframe final, excluindo a variável "Doors".


df2 <- select(df2, -Doors)
View(df2)
dim(df)
dim(df2)

#Houve redução significativa do número de variáveis preditoras (de 25 para 9), foram resolvidos os problemas de colinearidade
#e não houve perda significativa da capacidade de previsão do modelo. Vamos excluir os objetos que não serão mais utilizados.

rm(df1,df1_numeric,M,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,fator,fatores)




