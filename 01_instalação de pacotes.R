# Instalação e Carregamento dos Pacotes e funções de apoio

usethis::use_git_config(user.name = "Emerson D Batista", # Seu nome
                        user.email = "emebatista@hotmail.com") # Seu email

pacotes <- c("usethis",
             "MASS",
             "car",
             "cowplot",
             "pROC",
             "ggcorrplot",
             "Metrics",
             "rgl",
             "e1071",
             "caTools",
             "ROCR",
             "nnet",
             "magick",
             "tidyverse",  # Pacote básico de datawrangling
             "rpart",      # Biblioteca de árvores
             "rpart.plot", # Conjunto com Rpart, plota a parvore
             "gtools",     # funções auxiliares como quantcut,
             "Rmisc",      # carrega a função sumarySE para a descritiva
             "scales",     # importa paletas de cores
             "viridis",    # Escalas "viridis" para o ggplot2
             "caret",       # Funções úteis para machine learning
             "AMR",
             "randomForest",
             "fastDummies",
             "MLmetrics",
             "rattle",
             "xgboost",
             "MLmetrics",
             "plotly",
             "reshape2",
             "knitr",
             "kableExtra",
             "nlme",
             "lmtest",
             "msm",
             "lmeInfo",
             "jtools",
             "rpart",
             "party")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

rm(pacotes)
github_pat()
