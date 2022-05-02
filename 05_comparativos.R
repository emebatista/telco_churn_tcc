options(repr.plot.width = 10, repr.plot.height = 8)
set.seed(0)

# Modelo GLM já foi treinado. Rodaremos o predict
glm_pred <- predict(final_model_glm, type = "response", newdata = teste[,-24])
glm_acc  <- confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), 
                             factor(ifelse(glm_pred >= 0.50 , "Yes", "No")), 
                             positive = "Yes")$overall[1]
glm_acc
glm_mape <- MAPE( y_pred = as.numeric(glm_pred),
                  y_true = as.numeric(teste$Churn) )
glm_rmse <- Metrics::rmse(as.numeric(teste$Churn), as.numeric(glm_pred) )
glm_roc  <- roc(response = teste$Churn, predictor = as.numeric(glm_pred))
glm_gini <- round((glm_roc$auc[1] - 0.5) / 0.5, 3)

#Treinando Decision Tree
treino <- telco_final[indices,]
set.seed(0)
dtree <- rpart(Churn ~., data = treino, method = "class")
dtree_pred <- predict(dtree, type = "class", newdata = teste[,-24])
dtree_acc  <- confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), 
                               factor(ifelse(dtree_pred == 1 , "Yes", "No")) , 
                               positive = "Yes")$overall[1]
dtree_acc
dtree_mape <- MAPE( y_pred = as.numeric(dtree_pred),
                    y_true = as.numeric(teste$Churn) )
dtree_rmse <- Metrics::rmse(as.numeric(teste$Churn), as.numeric(dtree_pred) )
dtree_roc  <- roc(response = teste$Churn, predictor = as.numeric(dtree_pred))
dtree_gini <- round((dtree_roc$auc[1] - 0.5) / 0.5, 3)

#Treinando the RandomForest Model
treino = telco_final[indices,]
set.seed(0)
rf_model <- randomForest(Churn ~ ., data = treino, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)
rf_pred <- predict(rf_model, newdata=teste[,-24])
rf_acc  <- confusionMatrix( factor(ifelse(teste$Churn == 1 , "Yes", "No")), factor(ifelse(rf_pred == 1 , "Yes", "No")) , positive = "Yes")$overall[1]
rf_acc
rf_mape <- MAPE( y_pred = as.numeric(rf_pred) , y_true = as.numeric(teste$Churn) )
rf_rmse <- Metrics::rmse(as.numeric(teste$Churn), as.numeric(rf_pred) )
rf_roc  <- roc(response = teste$Churn, predictor = as.numeric(rf_pred))
rf_gini <- round((rf_roc$auc[1] - 0.5) / 0.5, 3)

#Plotagem das curvas ROC
plot(glm_roc,legacy.axes = TRUE         , print.auc.y = 1.00, print.auc.x = 1.20, print.auc = TRUE)
plot(rf_roc,    col = "red" , add = TRUE, print.auc.y = 0.80, print.auc.x = 0.20 ,print.auc = TRUE)
plot(dtree_roc, col = "blue", add = TRUE, print.auc.y = 0.60, print.auc = TRUE)
legend("bottom", c("Random Forest", "Decision Tree", "GLM"),
       lty = c(1,1), lwd = c(1, 2), col = c("red", "blue", "black"), cex = 0.55)

## glm ## 
#acurácia 0.7967
#mape 0.8035
#auc 0.8452
#rmse 1.0703
#gini 0.69

## dtree
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
