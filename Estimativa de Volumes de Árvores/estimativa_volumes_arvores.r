install.packages("mlbench")
install.packages("randomForest")
install.packages("caret")
library("caret")
library("mlbench")
library("randomForest")

volumes<-read.csv('Volumes.csv', sep="," ,dec=",")
volumes_index <- createDataPartition(volumes$VOL, p=0.80, list=FALSE)
training <- volumes[volumes_index,]
test <- volumes[-volumes_index,]
set.seed(7) 

#Usando métodos de Regressão

##Predict RandomForest
rf <- train(VOL~., data=training, method="rf")
predicoes.rf <- predict(rf, test)

#Predict SVM
svm <- train(VOL~., data=training, method="svmRadial")
predicoes.svm <- predict(svm, test)

#Predict RNA
rna <- train(VOL~., data=training, method="nnet")
predicoes.rna <- predict(rna, test)

#Predict SPURR
alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, data=training, start=list(b0=0.5, b1=0.5))
predicoes.alom <- predict(alom, test)

#metricas
rmse.rf <- RMSE(predicoes.rf, test$VOL)
rmse.svm <- RMSE(predicoes.svm, test$VOL)
rmse.rna <- RMSE(predicoes.rna, test$VOL)
rmse.alom <- RMSE(predicoes.alom, test$VOL)

cat("RMSE RF: ", rmse.rf, "\n")
cat("RMSE SVM: ", rmse.svm, "\n")
cat("RMSE RNA: ", rmse.rna, "\n")
cat("RMSE ALOM: ", rmse.alom, "\n")

#Coeficiente de determinação: R2 https://stackoverflow.com/questions/44136949/calculate-r-squared-var-explained-from-combined-randomforest-regression-objec

coeficiente_determinacao <- function(predict, actual){ 
  return(1- sum((predict - actual) ^ 2)/sum((actual - mean(actual))^2))
}

coeficiente_determinacao_rf = coeficiente_determinacao(predicoes.rf, volumes$VOL)
coeficiente_determinacao_rf
coeficiente_determinacao_svm = coeficiente_determinacao(predicoes.svm, volumes$VOL)
coeficiente_determinacao_svm
coeficiente_determinacao_rna = coeficiente_determinacao(predicoes.rna, volumes$VOL)
coeficiente_determinacao_rna
coeficiente_determinacao_alom = coeficiente_determinacao(predicoes.alom, volumes$VOL)
coeficiente_determinacao_alom
##Melhor modelo é o SVM

#Erro padrão da estimativa: Syx https://stackoverflow.com/questions/26237688/rmse-root-mean-square-deviation-calculation-in-r

padrao_estimativa <- function(predict, actual){ 
  return(sqrt(mean(((predict - actual)^2)/(length(actual)-2))))
}
padrao_estimativa_rf = padrao_estimativa(predicoes.rf, volumes$VOL)
padrao_estimativa_rf
padrao_estimativa_svm = padrao_estimativa(predicoes.svm, volumes$VOL)
padrao_estimativa_svm
padrao_estimativa_rna = padrao_estimativa(predicoes.rna, volumes$VOL)
padrao_estimativa_rna
padrao_estimativa_alom = padrao_estimativa(predicoes.alom, volumes$VOL)
padrao_estimativa_alom
##Melhor modelo é o SVM

#Syx% : “y com barra”, é a média (mean).A fórmula do Syx, tem que fazer “na mão“ 

syx <- function(padrao_estimativa, actual) {
  return (padrao_estimativa/mean(actual) * 100)
}

syx_rf = syx(padrao_estimativa_rf, volumes$VOL)
syx_rf
syx_svm = syx(padrao_estimativa_svm, volumes$VOL)
syx_svm
syx_rna = syx(padrao_estimativa_rna, volumes$VOL)
syx_rna
syx_alom = syx(padrao_estimativa_alom, volumes$VOL)
syx_alom
##Melhor modelo é o SVM


##Portanto, após passar por todos os as funções de validação de modelo o melhor método é o SVM!