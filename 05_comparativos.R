options(repr.plot.width = 10, repr.plot.height = 8)
set.seed(0)

glm.pred <- predict(final_model_glm, type = "response", newdata = teste[,-24])
confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), factor(ifelse(glm.pred >= 0.50 , "Yes", "No")) , positive = "Yes")
MAPE( y_pred = as.numeric(glm.pred) , y_true = as.numeric(teste$Churn) )
Metrics::rmse(as.numeric(teste$Churn), as.numeric(glm.pred) )

#Treinando Decision Tree
treino = telco_final[indices,]
Dtree = rpart(Churn ~., data = treino, method = "class")

#Predicting Decision Tree  
Dtree.pred <- predict(Dtree, type = "class", newdata = teste[,-24])

##ifelse(Dtree.pred[1] >= 1 , "Yes", "No")

confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), factor(ifelse(Dtree.pred >= 1 , "Yes", "No")) , positive = "Yes")
MAPE( y_pred = as.numeric(Dtree.pred) , y_true = as.numeric(teste$Churn) )
Metrics::rmse(as.numeric(teste$Churn), as.numeric(Dtree.pred) )

#Treinando the RandomForest Model
treino = telco_final[indices,]
modelo_random_forest <- randomForest(Churn ~ ., data = treino, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)

#Predicting on the test set and checking the Confusion Matrix.
rf.pred <- predict(modelo_random_forest, newdata=teste[,-24])
confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), factor(ifelse(rf.pred == 1 , "Yes", "No")) , positive = "Yes")
MAPE( y_pred = as.numeric(rf.pred) , y_true = as.numeric(teste$Churn) )
Metrics::rmse(as.numeric(teste$Churn), as.numeric(rf.pred) )

#Checking the variable Importance Plot
varImpPlot(modelo_random_forest)

#Montagem das curvas ROC 

plot(rf.roc, col = "red" , add = TRUE, print.auc.y = 0.85, print.auc = TRUE)
glm.roc <- roc(response = teste$Churn, predictor = as.numeric(glm.pred))
DT.roc  <- roc(response = teste$Churn, predictor = as.numeric(Dtree.pred))
rf.roc  <- roc(response = teste$Churn, predictor = as.numeric(rf.pred))

plot(glm.roc,legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
plot(DT.roc, col = "blue", add = TRUE, print.auc.y = 0.65, print.auc = TRUE)
plot(rf.roc, col = "red" , add = TRUE, print.auc.y = 0.85, print.auc = TRUE)
legend("bottom", c("Random Forest", "Decision Tree", "Logistic"),
       lty = c(1,1), lwd = c(2, 2), col = c("red", "blue", "black"), cex = 0.75)

## Curva ROC - GLM
ggplotly(
  ggroc(glm.roc, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(glm.roc$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((glm.roc$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)


## Curva ROC - Decision Tree
ggplotly(
  ggroc(DT.roc, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(DT.roc$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((DT.roc$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)


## Curva ROC - Random Forest
ggplotly(
  ggroc(rf.roc, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(rf.roc$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((rf.roc$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)
