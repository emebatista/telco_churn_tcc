telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
telco <- telco[complete.cases(telco),] 
# se o campo SeniorCitizen for 0, fica YES senao fica NO
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'Yes', 'No'))
telco$Churn <- as.factor(telco$Churn)
telco$customerID = NULL
# trata dados, substituindo alguns conteúdos 
telco <- data.frame(lapply(telco, function(x) {
  gsub("No internet service", "No", x)}))

telco <- data.frame(lapply(telco, function(x) {
  gsub("No phone service", "No", x)}))

#converte campos para numéricos
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco[num_columns] <- sapply(telco[num_columns], as.numeric)

# padroniza conteúdo de campos numéricos
telco_int <- telco[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

#cria uma coluna nova tenure_bin como cópia de tenure
telco <- mutate(telco, tenure_bin = tenure)

# cria as faixas de tempo de permanência como cliente
telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 anos'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 anos'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 anos'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 anos'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 anos'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 anos'

#converte o texto em variável categórica
telco$tenure_bin <- as.factor(telco$tenure_bin)

categoricas = c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,21,22)

for(i in categoricas) { 
  telco[,as.numeric(i)-1] <- as.factor(telco[,as.numeric(i)-1])
}

# usando algoritmo de Random Forest para base TElco
set.seed(123)
# Gera 80% de 1´s e 20% de 2´s para separar as amostras
n <- sample(1:2, # vamos amostrar elementos do conjunto c(1,2)
            size=nrow(telco), # O tamanho da amostragem
            replace=TRUE, # Amostragem com reposição (de c(1,2))
            prob=c(0.8,0.2)) # A probabilidade de ser 1 é 80%, de ser 2 é 20%

######################################
# Dividir amostras de treino e teste #

# Amostra de treino: n==1 (os 80%)
treino <- telco[n==1,]
# Amostra de teste: n==2 (os 20%)
teste <- telco[n==2,]

######################################
# Treinar a Random Forest            #

# Semente aleatória para buscar a reprodutibilidade
set.seed(123)

# Rodar o algoritmo
treino_random_forest <- randomForest::randomForest(
  Churn ~ ., 
  data = treino, 
  ntree = 50,
  mtry = 3, 
  importance = T)

avalia <- function(modelo, nome_modelo="modelo"){
  # Base de treino
  p_treino <- predict(modelo, treino, type='prob') # Probabilidade predita
  c_treino <- predict(modelo, treino)              # Classificação
  
  #Base de teste
  p_teste <- predict(modelo, teste, type='prob')
  c_teste <- predict(modelo, teste)
  
  # Data frame de avaliação (Treino)
  aval_treino <- data.frame(obs=treino$Churn, 
                            pred=c_treino,
                            Y = p_treino[,2],
                            N = 1-p_treino[,2]
  )
  
  # Data frame de avaliação (Teste)
  aval_teste <- data.frame(obs=teste$Churn, 
                           pred=c_teste,
                           Y = p_teste[,2],
                           N = 1-p_teste[,2]
  )
  
  tcs_treino <- caret::twoClassSummary(aval_treino, 
                                       lev=levels(aval_treino$obs))
  tcs_teste <- caret::twoClassSummary(aval_teste, 
                                      lev=levels(aval_teste$obs))
  ##########################
  # Curva ROC              #
  
  CurvaROC <- ggplot2::ggplot(aval_teste, aes(d = obs, m = Y, colour='1')) + 
    plotROC::geom_roc(n.cuts = 0, color="blue") +
    plotROC::geom_roc(data=aval_treino,
                      aes(d = obs, m = Y, colour='1'),
                      n.cuts = 0, color = "red") +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    theme(legend.position = "none") +
    ggtitle(paste("Curva ROC | ", nome_modelo, " | AUC-treino=",
                  percent(tcs_treino[1]),
                  "| AUC_teste = ",
                  percent(tcs_teste[1]))
    )
  
  print('Avaliação base de treino')
  print(tcs_treino)
  print('Avaliação base de teste')
  print(tcs_teste)
  CurvaROC 
}

avalia(treino_random_forest, nome_modelo="Random Forest")

previsto <- predict(treino_random_forest, type = "response", newdata = teste[,-24])

nrow(teste)


cutoff_churn <- factor(ifelse(previsto >=0.50, "Yes", "No"))
ROC_glm <- roc(response = teste$Churn, predictor = as.numeric(previsto))
plot(ROC_glm,      legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
#conf_final <- confusionMatrix(cutoff_churn, churn_real, positive = "Yes")

