rm(list=ls())

# dicionário
# survived: passageiro sobreviveu ou não? (0 = Não, 1 = Sim)
# pclass: classe da passagem no navio. (1 = primeira classe, 2 = classe executiva, 3 = classe econômica)
# name: nome do passageiro
# sex: gênero do passageiro 
# sibsp: número de irmãos/companheiros dentro do navio
# parch: número de parentes e filhos no navio ticket
# ticket: código da passagem
# fare: valor da passagem
# cabin: número da cabine
# embarked: local onde o titanic embarcou. (C = Cherbourg, Q = Queenstown, S = Southampton)

library(caret)
library(dplyr)

load("data/raw_data.RData")
load("data/full_dataset_transformed.RData")

head(full_dataset_transformed)
dim(full_dataset_transformed)

select_data_train <- full_dataset_transformed %>% na.omit()

dim(select_data_train)

ctr_rfe <- rfeControl(functions=rfFuncs, method="cv", number=10)

set.seed(123)

# algoritmo para seleção das melhores features para o modelo
rfe_algorithm <- rfe(select_data_train[,1:(ncol(select_data_train)-1)], select_data_train[,c(ncol(select_data_train))], sizes=c(1:(ncol(select_data_train)-1)), 
                     rfeControl=ctr_rfe)

# o algoritmo selecionou 6 subconjuntos das 7 variáveis. 
print(rfe_algorithm)

# quando listamos as features selecionadas percebemos que o modelo excluiu a variável Parch. 
predictors(rfe_algorithm)

# apesar da exclusão dessa variável (Parch) o decréscimo do erro não foi tão significativo. 
# a priori podemos construir o modelo a partir das variáveis sugeridas pelo algoritmo. 
plot(rfe_algorithm, type=c("g", "o"))


# passando as features para o novo objeto.
data_selected <- full_dataset_transformed %>% select(predictors(rfe_algorithm))
data_selected$Survived <- full_dataset_transformed$Survived

save(data_selected, file = "data/features_selection.RData")
