options(repr.plot.width = 10, repr.plot.height = 8)
set.seed(0)

# Modelo GLM já foi treinado. Rodaremos o predict
glm.pred <- predict(final_model_glm, type = "response", newdata = teste[,-24])
glm.acc  <- confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), 
                             factor(ifelse(glm.pred >= 0.50 , "Yes", "No")), 
                             positive = "Yes")$overall[1]
glm.acc
glm.mape <- MAPE( y_pred = as.numeric(glm.pred) , y_true = as.numeric(teste$Churn) )
glm.rmse <- Metrics::rmse(as.numeric(teste$Churn), as.numeric(glm.pred) )
glm.roc  <- roc(response = teste$Churn, predictor = as.numeric(glm.pred))
glm.gini <- round((glm.roc$auc[1] - 0.5) / 0.5, 3)

#Treinando Decision Tree
treino = telco_final[indices,]
set.seed(0)
Dtree = rpart(Churn ~., data = treino, method = "class")
Dtree.pred <- predict(Dtree, type = "class", newdata = teste[,-24])
Dtree.acc  <- confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), 
                               factor(ifelse(Dtree.pred == 1 , "Yes", "No")) , 
                               positive = "Yes")$overall[1]
Dtree.acc
Dtree.mape <- MAPE( y_pred = as.numeric(Dtree.pred) , y_true = as.numeric(teste$Churn) )
Dtree.rmse <- Metrics::rmse(as.numeric(teste$Churn), as.numeric(Dtree.pred) )
Dtree.roc  <- roc(response = teste$Churn, predictor = as.numeric(Dtree.pred))
Dtree.gini <- round((Dtree.roc$auc[1] - 0.5) / 0.5, 3)

#Treinando the RandomForest Model
treino = telco_final[indices,]
set.seed(0)
rf.model <- randomForest(Churn ~ ., data = treino, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)
rf.pred <- predict(rf.model, newdata=teste[,-24])
rf.acc  <- confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), factor(ifelse(rf.pred == 1 , "Yes", "No")) , positive = "Yes")$overall[1]
rf.acc
rf.mape <- MAPE( y_pred = as.numeric(rf.pred) , y_true = as.numeric(teste$Churn) )
rf.rmse <- Metrics::rmse(as.numeric(teste$Churn), as.numeric(rf.pred) )
rf.roc  <- roc(response = teste$Churn, predictor = as.numeric(rf.pred))
rf.gini <- round((rf.roc$auc[1] - 0.5) / 0.5, 3)

#Plotagem das curvas ROC
plot(glm.roc,legacy.axes = TRUE         , print.auc.y = 1.00, print.auc.x = 1.20, print.auc = TRUE)
plot(rf.roc,    col = "red" , add = TRUE, print.auc.y = 0.80, print.auc.x = 0.20 ,print.auc = TRUE)
plot(Dtree.roc, col = "blue", add = TRUE, print.auc.y = 0.60, print.auc = TRUE)
legend("bottom", c("Random Forest", "Decision Tree", "GLM"),
       lty = c(1,1), lwd = c(1, 2), col = c("red", "blue", "black"), cex = 0.55)

## glm ## 
#acurácia 0.7967
#mape 0.8035
#auc 0.8452
#rmse 1.0703
#gini 0.69

## Dtree
# acurácia 0.7967
# mape 0.1233
# auc 0.6697
# rmse 0.4516
# gini 0.3390

## Random Forrest 
# acurácia 0.7960
# mape 0.1350
# auc 0.6956
# rmse 0.4516
# gini 0.3910




