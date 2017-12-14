library(rpart)
library(rpart.plot)

rm(list = ls())
#rm(data, data_test, data_train)
data <- read.csv('iris.data.csv')
data_train <- data
# pegar aleatoriamente 13 de cada classe para formar a base de testes
x <- c(sample(1:50,13), sample(51:100,13), sample(101:150, 13))
data_test <-  data[x,] # separa como dados de teste
# remove os dados de teste da base de treino
data_train <-  data_train[-x, ] # remove da lista de treino
# constroi o modelo
decision_tree <- rpart(class ~ sepal_length + sepal_width + petal_length + petal_width, 
                       data = data_train,
                       method = "class",
                       control = rpart.control(minsplit = 1),
                       parms = list(split = "Information"))
# faz o plot da arvore
plot_dt <- rpart.plot(decision_tree, type=3)

# testa o modelo com a base de testes
y_predicted <- predict(decision_tree, data_test[,-5], "class")

# constroi a matriz de confusao (para facilitar nos calculos das metricas)
confusion_matrix <- table(data_test$class, y_predicted)
tp = c()
fp = c()
fn = c()
precision = c()
recall = c()
f1_score = c()
nclass <- nrow(confusion_matrix)
for (i in 1:nclass){   # para cada classe
  true_positive = confusion_matrix[i,i]
  tp = c(tp, true_positive)
  false_positive <- 0
  false_negative <- 0
  for (j in 1:nclass) {
    if (j != i) {
      false_positive <- false_positive + confusion_matrix[j, i]
      false_negative <- false_negative + confusion_matrix[i, j]
    }
    prec <- true_positive / (true_positive + false_positive)
    rec <- true_positive/(true_positive + false_negative)
    precision <- c(precision, prec)
    recall <- c(recall, rec)
    f1_score <- c(f1_score, 2 * ((prec * rec) / (prec + rec)))
  }
  fp = c(fp, false_positive)
  fn = c(fn, false_negative)
}
total <- length(y_predicted)
sum(confusion_matrix[3,])
acc <- sum(tp) / total
precision <- mean(precision)
recall <- mean(recall)
f1_score <- mean(f1_score)
print(confusion_matrix)
print(paste0("Acuracia: ", acc))
print(paste0("Precisao: ", precision))
print(paste0("Revocacao: ", recall))
print(paste0("Medida F: ", f1_score))

