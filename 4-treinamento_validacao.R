#Definindo o diretório
setwd("/home/carlos/FCD/BigDataRAzure/projetos/projeto1")
getwd()

#Criando Modelos de Regressão linear e Random Forest.

#1º - Dividindo o dataframe em dados de treino e de test

set.seed(123)
training.samples <- df2$Energy_consumption %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df2[training.samples, ]
test.data <- df2[-training.samples, ]

#Modelo 1 - Regressão Linear

m1 <- train(Energy_consumption~., 
            data=train.data, 
            method='lm', 
            metric='RMSE', 
            trControl = trainControl(method = "cv", number = 5)
)
#Em modelos de regressão linear, dataframes com poucas observações e muitas variáveis
#costuma gerar esse tipo de mensagem de erro. Por isso, vamos excluir algumas variáveis menos
#importantes antes de gerar o modelo. 

m1 <- train(Energy_consumption~. -DC -Seats -Tire_size, 
            data=train.data, 
            method='lm', 
            metric='RMSE', 
            trControl = trainControl(method = "cv", number = 5)
)

predictions <- m1 %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$Energy_consumption),
  R2 = R2(predictions, test.data$Energy_consumption)
)

#Modelo 2 - Random Forest

m2 <- train(Energy_consumption~., 
            data=train.data, 
            method='rf', 
            metric='RMSE', 
            trControl = trainControl(method = "cv", number = 5)
)

predictions2 <- m2 %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions2, test.data$Energy_consumption),
  R2 = R2(predictions2, test.data$Energy_consumption)
)

tabelalm <- cbind(pred = predictions,obs = test.data$Energy_consumption)
View(tabelalm)

#Entre os dois modelos, escolhemos o modelo de Regressão Linear.

rm(predictions,predictions2,m1,m2,tabelalm,test.data,train.data,training.samples)
