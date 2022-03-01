
##################################### INICIO DA PREPARACAO DOS DADOS ##################################
#carrega o CSV
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
summary(telco)
## resultado exportado para o TCC

# retira o que tiver missing values
telco <- telco[complete.cases(telco),] 
view(telco)
# se o campo SeniorCitizen for 0, fica YES senao fica NO
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'Yes', 'No'))

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
  telco[,as.numeric(i)] <- as.factor(telco[,as.numeric(i)])
}

glimpse(telco)


#remove quantitativas, pois faremos a dumização delas
telco_cat <- telco[,-c(1,6,19,20)]


#cria variáveis dummy
telco_dummy <- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))

#combina dataset de variáveis numéricas e categóricas dummy
telco_final <- cbind(telco_int,telco_dummy)

########################## FIM DA PREPARACAO DOS DADOS ##################################

# Para GLM vamos rodar primeiro o Churn em função de todas as variáveis
modelo_glm = glm(Churn ~ ., data = telco_final, family = "binomial")
summary(modelo_glm)
logLik(modelo_glm)

confusionMatrix(table(predict(modelo_glm, type = "response") >= 0.5,
                      telco_final$Churn == 1)[2:1, 2:1])

confusionMatrix(table(predict(modelo_glm, type = "response") >= 0.7,
                      telco_final$Churn == 1)[2:1, 2:1])

confusionMatrix(table(predict(modelo_glm, type = "response") >= 0.3,
                      telco_final$Churn == 1)[2:1, 2:1])


########################### Procedimento Stepwise ################################

# para tentar melhorar o modelo glm, vamos rodar o stepwise para um intervalo 
# de confiança de 5%
model_glm_step <- step(object = modelo_glm,
                     k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
final_model <- model_glm_step 
summary(model_glm_step)
logLik(model_glm_step)
vif(model_glm_step)

confusionMatrix(table(predict(model_glm_step, type = "response") >= 0.5,
                      telco_final$Churn == 1)[2:1, 2:1])

confusionMatrix(table(predict(model_glm_step, type = "response") >= 0.7,
                      telco_final$Churn == 1)[2:1, 2:1])

confusionMatrix(table(predict(model_glm_step, type = "response") >= 0.3,
                      telco_final$Churn == 1)[2:1, 2:1])


# comparando os LL
lrtest(modelo_glm, model_glm_step)

export_summs(modelo_glm, model_glm_step, scale = F,
             digits = 4)

#função prediction do pacote ROCR
predicoes_glm <- prediction(predictions = modelo_glm$fitted.values, 
                        labels = telco_final$Churn) 
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
ROC_glm <- roc(response = telco_final$Churn, 
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




##### reavaliar se vai precisar daqui pra baixo.


###################################
pred <- predict(model_glm_step, type = "response", newdata = telco_final[,-24])
summary(pred)
telco_final$prob <- pred

# Using probability cutoff of 50%.

pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(telco_final$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)
###

cutoff_churn <- factor(ifelse(pred >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
##

##

options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.32, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))

#cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cutoff_churn <- factor(ifelse(pred >=0.32, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]

##
set.seed(123)
telco_final$Churn <- as.factor(telco_final$Churn)

indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]
options(repr.plot.width = 10, repr.plot.height = 8)

# tira a coluna 24 porque ela é o churn
pred <- predict(final_model, type = "response", newdata = validation[,-24])

#crio a coluna prob no dataset de validação.
validation$prob <- pred

options(repr.plot.width =10, repr.plot.height = 8)

glm.roc <- roc(response = validation$Churn, predictor = as.numeric(pred))

plot(glm.roc,      legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)

