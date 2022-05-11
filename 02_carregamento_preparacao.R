#################################### INICIO DA PREPARACAO DOS DADOS ##################################
# carrega o CSV da base de dados
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

# retira os missing values
telco <- telco[complete.cases(telco),] 

# se o campo SeniorCitizen for 0, fica YeS senao fica No
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'Yes', 'No'))

# trata dados, substituindo alguns conteúdos 
telco <- data.frame(lapply(telco, function(x) {
  gsub("No internet service", "No", x)}))

telco <- data.frame(lapply(telco, function(x) {
  gsub("No phone service", "No", x)}))

# converte campos para numéricos
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco[num_columns] <- sapply(telco[num_columns], as.numeric)

# padroniza conteúdo de campos numéricos
telco_int <- telco[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

# cria uma coluna nova tenure_bin como cópia de tenure
telco <- mutate(telco, tenure_bin = tenure)

# cria as faixas de tempo de permanência como cliente
telco$tenure_bin[telco$tenure_bin >= 0 & telco$tenure_bin <= 12] <- '0-1 anos'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 anos'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 anos'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 anos'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 anos'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 anos'

# converte o texto em variável categórica
telco$tenure_bin <- as.factor(telco$tenure_bin)

categoricas = c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,21,22)

for(i in categoricas) { 
  telco[,as.numeric(i)] <- as.factor(telco[,as.numeric(i)])
}

# remove quantitativas, pois faremos a dumização das demais
telco_cat <- telco[,-c(1,6,19,20)]

# cria variáveis dummy
telco_dummy <- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))

# combina dataset de variáveis numéricas e categóricas dummy
telco_final <- cbind(telco_int,telco_dummy)

############################################################################
# divide entre treino e teste
############################################################################
set.seed(0)

telco_final$Churn <- as.factor(telco_final$Churn)

for(i in 1:29) { 
  telco_final[,as.numeric(i)] <- as.factor(telco_final[,as.numeric(i)])
}
telco_final$tenure <- as.numeric(telco_final$tenure)
telco_final$MonthlyCharges <- as.numeric(telco_final$MonthlyCharges)
telco_final$TotalCharges <- as.numeric(telco_final$TotalCharges)
telco_final$Churn <- as.factor(telco_final$Churn)



indices = sample.split(telco_final$Churn, SplitRatio = 0.8)
treino = telco_final[indices,]
teste = telco_final[!(indices),]
glimpse(treino)
rm(categoricas)
rm(i)
rm(num_columns)
rm(telco_int)
rm(telco_dummy)
rm(telco_cat)
rm(telco)


