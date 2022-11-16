setwd("/home/carlos/FCD/BigDataRAzure/projetos/projeto1")
getwd()

library(h2o)
library(ggbeeswarm)

#A 1ª linha é opcional, para indicar em que diretório se encontra a versão de 64 bits do Java,
#exigido pelo H2O.
#Sys.setenv(JAVA_HOME="/usr/lib/jvm/default-java") ##your own path of Java SE intalled
h2o.init()

# O H2O requer que os dados estejam no formato de dataframe do H2O
h2o_frame <- as.h2o(df2)
class(h2o_frame)
head(h2o_frame)
str(h2o_frame)

# Split dos dados em treino e teste

h2o_frame_split <- h2o.splitFrame(h2o_frame, ratios = 0.80, seed = 123)
head(h2o_frame_split)


#esse split do pacote H20 cria uma lista. O 1º elemento da lista é o dataset de treino e o 2º é o dataset de teste.

# Modelo AutoML

#gráfico de barras da variável preditora DC. É necessário fazer o balanceamento das classes, pois algumas
#das classes tem poucas observações.
table(df2$DC)
ggplot(df2, aes(x = DC)) +
  geom_bar(fill = "purple")


#Essa função define o modelo, testando dezenas de modelos diferentes e identificando o melhor.

modelo_automl <- h2o.automl(y = 'Energy_consumption',
                            seed = 123,
                            balance_classes = TRUE,
                            training_frame = h2o_frame_split[[1]],
                            leaderboard_frame = h2o_frame_split[[2]],
                            max_runtime_secs = 60 * 10, 
                            include_algos = c('XGBoost', 'DRF', 'GLM'),
                            sort_metric = "RMSE")

#Para avaliação de modelos, o H2O utiliza como padrão a avaliação cruzada com 5 amostras (folds = 5). 
#Tivemos ainda que definir também um parâmetro para limite de tempo de processamento
#(**max_runtime_secs = 60 \* 10**) e um limite no número de algoritmos de Machine Learning para 
#avaliação **(XGBoost, DRF, GBM)**, devido à limitações de capacidade de processamento de dados e memória. 
#Por fim, decidimos fazer um balanceamento de classes, pois o dataframe final tem apenas 42 observações, 
#o que acarreta falta de dados de algumas classes para elaboração do modelo. A métrica escolhida para avaliação
#dos modelos é RMSE (raiza quadrada do erro médio).

# Extrai o leaderboard
leaderboard_automl <- as.data.frame(modelo_automl@leaderboard)
View(leaderboard_automl)

# Extrai o líder (modelo com melhor performance)
lider_automl <- modelo_automl@leader
lider_automl


#Verificando o desempenho do modelo na previsão dos dados de treino.

predicted <- h2o.predict(lider_automl,h2o_frame_split[[2]]) %>%
  as.data.frame()
df_train <- as.data.frame(h2o_frame_split[[2]])

tabela <- data.frame(
  pred = predicted$predict,
  obs = df_train$Energy_consumption
)
rm(predicted,df_train)

R2(tabela$pred,tabela$obs)
RMSE(tabela$pred,tabela$obs)
View(tabela)
#O modelo AutoML teve desempenho melhor na métrica escolhida para definição dos modelos (RMSE), relativamente
#ao modelo manual (0,68 X 1,09 do modelo de regressão linear). 


# Para o melhor modelo extraímos a contribuição de cada variável para as previsões.
# Os valores extraídos são chamados de valores SHAP
# Usamos os dados de teste

var_contrib <- predict_contributions.H2OModel(lider_automl, h2o_frame_split[[2]])

# Preparando o dataframe
df_var_contrib <- var_contrib %>%
  as.data.frame() %>%
  select(-BiasTerm) %>%
  gather(feature, shap_value) %>%
  group_by(feature) %>%
  mutate(shap_importance = mean(abs(shap_value)), shap_force = mean(shap_value)) %>% 
  ungroup()

# gather é usado para juntar dados de varias colunas numa coluna só

View(df_var_contrib)

# Plot da importância de cada variável para prever a variável alvo
df_var_contrib %>% 
  select(feature, shap_importance) %>%
  distinct() %>% 
  ggplot(aes(x = reorder(feature,shap_importance), y = shap_importance)) +
  geom_col(fill = 'blue') +
  coord_flip() +
  xlab(NULL) +
  ylab("Valor Médio das Métricas SHAP") +
  theme_minimal(base_size = 15)
#coord_flip: barras na vertical
#reorder: ordena as variáveis (feature) por ordem de tamanho das barras de shap_importance

# Desliga o H2O

h2o.shutdown()
Y
