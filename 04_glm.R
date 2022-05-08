# Para GLM vamos rodar primeiro o Churn em função de todas as variáveis
# na base de treino
############################################################

#antes de stepwise
set.seed(0)
modelo_glm = glm(Churn ~ ., data = treino, family = "binomial")
#final_model_glm <- step(object = modelo_glm,
#                       k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

# o modelo abaixo é o resultado do stepwise
final_model_glm <- glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
      PhoneService + MultipleLines + InternetService.xFiber.optic + 
      InternetService.xNo + OnlineBackup + DeviceProtection + StreamingTV + 
      StreamingMovies + Contract.xOne.year + Contract.xTwo.year + 
      PaperlessBilling + PaymentMethod.xElectronic.check + tenure_bin.x2.3.anos + 
      tenure_bin.x3.4.anos + tenure_bin.x4.5.anos + tenure_bin.x5.6.anos, 
    family = "binomial", data = treino)

# temos acuracia de 81.08 para cutoff de 0.5
confusionMatrix(table(predict(final_model_glm, type = "response") >= 0.5,
                      treino$Churn == 1)[2:1, 2:1])

# temos acuracia de 78.47 para cutoff de 0.7
confusionMatrix(table(predict(final_model_glm, type = "response") >= 0.7,
                      treino$Churn == 1)[2:1, 2:1])

# temos acuracia de 76.98 para cutoff de 0.3
confusionMatrix(table(predict(final_model_glm, type = "response") >= 0.3,
                      treino$Churn == 1)[2:1, 2:1])

#summary(final_model_glm)
# o Loglik é de -2302.189 com 20 graus de liberdade
logLik(final_model_glm)
vif(final_model_glm)
summary(final_model_glm)

# comparando os LL de antes e depois do stepwise
#lrtest(modelo_glm, final_model_glm)

#export_summs(modelo_glm, final_model_glm, scale = F,
#             digits = 4)

#função prediction do pacote ROCR
#predicoes treino
predicoes_glm <- prediction(predictions = final_model_glm$fitted.values, 
                        labels = treino$Churn) 


##data(cars)
##reg <- lm(log(dist) ~ log(speed), data = cars)
##MAPE(y_pred = exp(reg$fitted.values), y_true = cars$dist)

#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

#função performance do pacote ROCR
dados_curva_roc_glm <- performance(predicoes_glm, measure = "sens") 
#A função peformance(), do pacote ROCR, extrai do objeto 'predicoes' os 
#dados de sensitividade e de especificidade para a plotagem.

#Desejamos os dados da sensitividade e de especificidade. Então, devemos
#digitar os seguintes códigos::

sensitividade <- (performance(predicoes_glm, measure = "sens"))@y.values[[1]] 
especificidade <- (performance(predicoes_glm, measure = "spec"))@y.values[[1]]

#Extraindo os cutoffs:
cutoffs_glm <- dados_curva_roc_glm@x.values[[1]] 

dados_plotagem_glm <- cbind.data.frame(cutoffs_glm, especificidade, sensitividade)

#função roc do pacote pROC
ROC_glm <- roc(response = treino$Churn, 
           predictor = modelo_glm$fitted.values)

ggplotly(
  ggroc(ROC_glm, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC_glm$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC_glm$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)


## COMANDOS ABAIXO PARA CONSTRUIR O GRAFICO COM ACURACIA, ESPECIFICIDADE E SENSIBILIDADE ######################################################################################
previsto <- predict(final_model_glm, type = "response", newdata = treino[,-24])
treino$prob <- previsto

# Using probability cutoff of 50%.
churn_previsto <- factor(ifelse(previsto >= 0.50, "Yes", "No"))
churn_real     <- factor(ifelse(treino$Churn==1 , "Yes", "No"))
table(churn_real,churn_previsto)

###
cutoff_churn <- factor(ifelse(previsto >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, churn_real, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]

options(repr.plot.width = 8, repr.plot.height = 6)

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(previsto >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, churn_real, positive = "Yes")
  accuracy <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuracy))) 
  colnames(out) <- c("sensitividade", "especificidade", "acuracia")
  return(out)
}

# vai de 0.01 até 0.80 o teste de variação dos cutoff 
# este array vai ser o eixo dos cutoffs (x)
s = seq(0.01,0.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Valor",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =0.5,inset = 0.05,
       box.lty=0,cex = 0.8, 
       lwd=c(1,1,1,2),c("Sensitividade","Especificidade","Acurácia"))
abline(v = 0.29, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))

#######################################################################
########################### FAZENDO O TESTE COM O DATASET TESTE #######
options(repr.plot.width = 10, repr.plot.height = 8)
set.seed(0)
# tira a coluna 24 porque ela é o churn
previsto <- predict(final_model_glm, type = "response", newdata = teste[,-24])
ROC_glm <- roc(response = teste$Churn, predictor = as.numeric(previsto))

plot(ROC_glm,      legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)

ggplotly(
  ggroc(ROC_glm, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC_glm$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC_glm$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

###############################################################
# gráfico triplo da base de teste 
###############################################################
previsto <- predict(final_model_glm, type = "response", newdata = teste[, -24])
cutoff_churn <- factor(ifelse(previsto >= 0.50, "Yes", "No"))
churn_previsto <- factor(ifelse(previsto >= 0.50, "Yes", "No"))
churn_real <- factor(ifelse(teste$Churn == 1, "Yes", "No"))
confusionMatrix(cutoff_churn, churn_real, positive = "Yes")

# table(churn_real,churn_previsto)
for(i in 1:100)
{
  #print(s[i])
  #enviado o cutoff como parametro, retornam 3 (sensitividade, especificidade, acurácia)
  OUT[i,] = perform_fn(s[i]) 
} 

plot(s, OUT[,1],xlab="Cutoff",
     ylab="Valor",
     cex.lab=1.5,
     cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =0.5,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitividade","Especificidade","Acurácia"))
abline(v = 0.29, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))


confusionMatrix(cutoff_churn, churn_real, positive = "Yes")




