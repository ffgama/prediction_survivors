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

library(caret)
library(rpart)
library(rpart.plot)
library(e1071)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(123)

# buscando o melhor valor para cp
tree <- train(Survived ~., data = data_train, method = "rpart", parms = list(split = "information"), trControl=control,
          tuneGrid = expand.grid(.cp=(1:50)*0.01))
tree

# treinando o modelo a partir do melhor valor para o parâmetro cp
tree_model <- rpart(Survived ~ ., data = data_train, control = rpart.control(cp=tree$results[c(which(tree$results$Accuracy  == max(tree$results$Accuracy))),]$cp))

prp(tree_model, box.palette = "Reds")

# passando o conjunto de teste para o modelo 
predict_tree<-predict(tree_model, data_test, type = "class")

df <- data.frame(response = predict_tree)

# avaliar os resultados
library(caret)
confusionMatrix(df$response, data_test$Survived)

#########################################################################################################

rm(list=ls())

load("data/features_selection.RData")

data_selected_train <- data_selected[!is.na(data_selected$Survived),]
data_selected_train$Survived <- as.factor(data_selected_train$Survived)

# equilibrando as classes
library(ROSE)

# método de over e undersampling
balanced_target <- ovun.sample(Survived ~ ., data = data_selected_train, method = "both",N = 950, seed = 1)$data
data_selected_train <- balanced_target

# proporção das classes
prop.table(table(data_selected_train$Survived))

# 70% treinamento 
size_sample <-  floor(0.7 * nrow(data_selected_train))

set.seed(123)
# particionamento randômico
train_ind <- sample(seq_len(nrow(data_selected_train)), size_sample)

# dados de treinamento e teste
data_train <- data_selected_train[train_ind, ]
data_test <- data_selected_train[-train_ind, ]

library(caret)
library(rpart)
library(rpart.plot)
library(e1071)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(123)

# buscando o melhor valor para cp
tree <- train(Survived ~., data = data_train, method = "rpart", parms = list(split = "information"), trControl=control,
              tuneGrid = expand.grid(.cp=(1:50)*0.01))
tree

# treinando o modelo a partir do melhor valor para o parâmetro cp
tree_model <- rpart(Survived ~ ., data = data_train, control = rpart.control(cp=tree$results[c(which(tree$results$Accuracy  == max(tree$results$Accuracy))),]$cp))

prp(tree_model, box.palette = "Reds")

# passando o conjunto de teste para o modelo 
predict_tree<-predict(tree_model, data_test, type = "class")

df <- data.frame(response = predict_tree)

# avaliar os resultados
library(caret)
confusionMatrix(df$response, data_test$Survived)
# percebe um significativa melhora nos resultados após o balanceamento das classes