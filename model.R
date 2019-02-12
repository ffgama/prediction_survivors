rm(list=ls())

load("data/features_selection.RData")

data_selected_train <- data_selected[!is.na(data_selected$Survived),]
data_selected_train$Survived <- as.factor(data_selected_train$Survived)

# 70% treinamento 
size_sample <-  floor(0.7 * nrow(data_selected_train))

set.seed(123)
# particionamento randômico
train_ind <- sample(seq_len(nrow(data_selected_train)), size_sample)

# dados de treinamento e teste
data_train <- data_selected_train[train_ind, ]
data_test <- data_selected_train[-train_ind, ]

library(mlr)
library(dplyr)

train_task <- makeClassifTask(data = data_train, target = "Survived")
test_task <- makeClassifTask(data = data_test, target = "Survived")

all_learners = list(
  # Random Forest
  makeLearner("classif.randomForest", id = "Random Forest", predict.type = "prob"),
  # Árvore de decisão
  makeLearner("classif.rpart", id = "RPART", predict.type = "prob"),
  # KNN
  makeLearner("classif.kknn", id = "KNN", predict.type = "prob"),
  # Linear Discriminant Analysis	
  makeLearner("classif.lda", id = "LDA", predict.type = "prob"),
  # SVM
  makeLearner("classif.ksvm", id = "SVM",predict.type = "prob"),
  # Naive Bayes
  makeLearner("classif.naiveBayes", id = "Naive Bayes", predict.type = "prob"),
  # Rede Neural
  makeLearner("classif.nnet", id = "Neural Net", predict.type = "prob")
)

# reamostragem
rdesc <- makeResampleDesc("CV", iters = 10)

# medidas que serão utilizadas
meas <- list(acc, auc)

# Definindo o benchmark 
bmr <- benchmark(learners = all_learners, tasks = train_task, resamplings = rdesc, measures = meas)
bmr

library(gridExtra)
library(ggplot2)

# Dentre os modelos testados o Random Forest foi um dos mais estáveis tanto em termos de acurácia quanto em roc. 
# A rede neural apresentou a maior variação dos resultados de acurácia
# enquanto a que nos dados de roc variou tanto quanto o lda
plot_acc<-plotBMRBoxplots(bmr, measure = acc, order.lrns = getBMRLearnerIds(bmr)) + 
  ggtitle("Perfomance dos modelos (Acurácia)") +
  aes(fill = learner.id) +
  guides(fill = FALSE) + 
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

plot_auc<-plotBMRBoxplots(bmr, measure = auc, order.lrns = getBMRLearnerIds(bmr)) + 
  ggtitle("Perfomance dos modelos (Análise ROC)") +
  aes(fill = learner.id) + 
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

# Acc
table_acc <- getBMRAggrPerformances(bmr, as.df = TRUE) %>%
  select(Modelo = learner.id, Acurácia = acc.test.mean) %>%
  mutate(Acurácia = round(Acurácia, 4))  %>%
  arrange(desc(Acurácia))

# Roc
table_roc <- getBMRAggrPerformances(bmr, as.df = TRUE) %>%
  select(Modelo = learner.id, ROC = auc.test.mean) %>%
  mutate(ROC = round(ROC, 4))  %>%
  arrange(desc(ROC))

grid.arrange(plot_acc, plot_auc,
  tableGrob(table_acc, theme=ttheme_default()),
  tableGrob(table_roc, theme=ttheme_default()),
  nrow = 2)

############################# RANDOM FOREST ############################# 

# normalização
# train_task <- normalizeFeatures(train_task, method = "standardize" )
# test_task <- normalizeFeatures(test_task, method = "standardize")

rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(mtry = 3, ntree = 100, 
                                                                                     importance = TRUE,
                                                                                     cutoff = c(0.55,0.45)))
# parametros de tuning
parameters <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 6),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

tune <- makeTuneControlRandom(maxit = 100L)

# cv
cv <- makeResampleDesc("CV",iters = 10L)

rf_tune <- tuneParams(learner = rf, resampling = cv, task = train_task, par.set = parameters, control = tune, 
                      show.info = TRUE)

# configurando os hiperparâmetros 
rf_tuning <- setHyperPars(rf, par.vals = rf_tune$x)

# configurando os parâmetros de tuning 
rf <- mlr::train(rf_tuning, train_task)

# passando o conjunto de teste para o modelo 
predict_rf<-predict(rf, test_task)

df <- data.frame(predict_rf$data)

# avaliar os resultados
library(caret)
confusionMatrix(df$response, df$truth)

library(pROC)
auc(response = df$truth, predictor = factor(df$response, ordered = TRUE))

#################### Submissão #################### 
# passando o conjunto de teste para o modelo
# data_selected_test <- data_selected[is.na(data_selected$Survived),]
# 
# # predição
# predict_rf<-predict(rf$learner.model, data_selected_test)
# 
# df <- data.frame(response = predict_rf)
# 
# # Acurácia kaggle -> 0.77990
# df_predictions <- data.frame(PassengerId = as.numeric(rownames(df)), Survived = df$response)
# head(df_predictions)
# tail(df_predictions)
# 
# write.csv(df_predictions,file="data/submits/df_predictions_1.csv", row.names=FALSE)
