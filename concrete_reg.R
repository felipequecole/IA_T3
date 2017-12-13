library(rpart)
library(rpart.plot)

rm(list = ls())
#rm(data, data_test, data_train)
data <- read.csv('slump_test.data.csv') # leio o dataset
data <- data[,-c(1,9,10)] # deixa apenas uma classe de output
#numeric_check <- sapply(data, is.numeric) # o dataset contem outros
#data <- data[,numeric_check]
#data <- data[, -c(14,15)]
data_train <- data
# pegar aleatoriamente 25 linhas (25% da base)
x <- sample(1:103, 25)
data_test <-  data[x,] # separa como dados de teste
# remove os dados de teste da base de treino
data_train <-  data_train[-x, ] # remove da lista de treino
# constroi o modelo
regression_tree <- rpart(Compressive_Strength ~ Cement + Slag + Fly_ash + Water + SP + Coarse_Aggr + Fine_Aggr, 
                       data = data_train,
                       method = "anova",
                       control = rpart.control(minsplit = 2),
                       parms = list(split = "Information"))
# faz o plot da arvore
plot_dt <- rpart.plot(regression_tree, type=3)


# testa o modelo com a base de testes
y_predicted <- predict(regression_tree, data_test[,-ncol(data_test)], "vector")
aux <- 0
n <- length(y_predicted)

for (i in 1:n){
  aux <- aux + (data_test$Compressive_Strength[i] - y_predicted[i])^2
}
mean_squared_error <- aux / n
print(paste0("Erro minimo quadratico: ", mean_squared_error))
