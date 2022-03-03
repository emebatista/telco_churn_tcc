# Instalação e Carregamento dos Pacotes e funções de apoio
#GITHUB_PAT=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
usethis::use_git_config(user.name = "Emerson D Batista", # Seu nome
                        user.email = "emebatista@hotmail.com") # Seu email



pacotes <- c("usethis","MASS","car","cowplot",
             "pROC","ggcorrplot","rgl",
             "e1071","caTools","ROCR","nnet","magick",
             'tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'viridis',    # Escalas 'viridis' para o ggplot2
             'caret',       # Funções úteis para machine learning
             'AMR',
             'randomForest',
             'fastDummies',
             'rattle',
             'xgboost',
             "plotly","reshape2","knitr","kableExtra",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","party","lme4")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


theme1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")

theme2 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(previsto >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, churn_real, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}




# 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
