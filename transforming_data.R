rm(list=ls())

load("data/raw_data.RData")

# reordenar colunas
train <-train[,c(1,3:ncol(train),2)] 

# verificando e eliminando registros duplicados 
any(duplicated(train$passengerid))
# extrai apenas os registros únicos
train <- train[!duplicated(train$passengerid),]

# observando a existência de duplicatas em todas as colunas
sapply(train, function(x) any(duplicated(x)))

# estrutura dos datasets
str(train)
str(test)

# adicionar NA na nova coluna do dataset test
test$Survived <- NA

# unificando nome das colunas de treino x teste
colnames(train) <- colnames(test)

# unindo os datasets para as etapas de pré-processamento e seleção de features
full_dataset <- rbind(train, test)

# remover features que não interessam para a análise exploratória nem para construção do modelo (id)
full_dataset <- full_dataset[,-c(1)]

# modificando tipo e padronizando as instâncias
str(full_dataset)
# classe da passagem
full_dataset$Pclass <- as.factor(full_dataset$Pclass)
# nome 
full_dataset$Name <- as.character(full_dataset$Name)
# sexo
levels(full_dataset$Sex)[c(2,4,5,6,12,13)] <- c("female")
levels(full_dataset$Sex)[c(3:8)] <- c("male")
levels(full_dataset$Sex)[1] <- NA
# idade
full_dataset$Age <- as.integer(full_dataset$Age)
# cabine 
full_dataset$Cabin <- as.character(full_dataset$Cabin)

# embarque
levels(full_dataset$Embarked)[1] <- NA

# verificando a estrutura final 
str(full_dataset)

# verificar o resumo estatístico de cada variável 
summary(full_dataset)

# parece que há uma um outlier em Age, vamos analisar a distribuição
library(ggplot2)
# plotando em um histograma
ggplot(data=full_dataset, aes(Age)) + geom_histogram() +  theme_minimal()

# checando as instâncias em Age acima de 100 e substituindo por NA
full_dataset[(full_dataset$Age > 110 & !(is.na(full_dataset$Age))), ]$Age <- c(NA)

# é possível também um encontrar um possível outlier no Fare.
ggplot(data=full_dataset, aes(Fare)) + geom_histogram() +  theme_minimal()

# pelo menos 4 passageiros pagaram um valor bem elevado no ticket.
# pesquisas adicionais apontam que de fato esse preço poderia ter sido pago por alguns passageiros
# como foi o caso de Cardeza, Mrs. James Warburton Martinez (Charlotte Wardle Drake)
# https://www.encyclopedia-titanica.org/titanic-survivor/charlotte-cardeza.html
full_dataset[(full_dataset$Fare > 450 & !(is.na(full_dataset$Fare))), ]

# checando os valores ausentes das colunas numéricas  
sapply(full_dataset, function(x) sum(is.na(x)))

# cabin possui aproximadamente 77,4% de dados ausentes, apesar de ser potencialmente relevante pode ser que tenhamos que 
# removê-las.
nrow(full_dataset[full_dataset$Cabin == "",])/nrow(full_dataset)
# a coluna nome está totalmente preenchida
nrow(full_dataset[full_dataset$Name == "",])/nrow(full_dataset)
# a coluna ticket está totalmente preenchida
nrow(full_dataset[full_dataset$Ticket == "",])/nrow(full_dataset)

# com base na análise de instânicas ausentes poderemos destacar algumas variáveis:
# Pclass = 9 instâncias
# Sex = 45 instâncias
# Age = 265 instâncias
# Fare = 1 instância
# Cabin = 1014 instâncias
# Embarked = 2 instâncias

source("filling_data.R")

# Restaram apenas duas variáveis: Age e Cabin.
# sabemos que a idade pode ser um fator importante de sobrevivência. A cabine também poderia ser uma informação importante
# no entanto, temos uma quantidade imensa de valores não preenchidas.  Assim decidimos remover a variável Cabin.
# Além disso, entendemos que a Pclass poderia amenizar a ausência da variável Cabin uma vez que a Pclass
# pode fornecer uma noção da posição espacial dos passageiros no navio.

source("knnimputation_data.R")

# observamos que a imputação manteve uma distribuição dos dados bem aproximada do conjunto de dados original

# agora incluiremos a variável no conjunto de dados original
full_dataset$Age <- full_dataset_transf$Age

# agora podemos selecionar somente as variáveis que interessam para o restante das etapas.
str(full_dataset)

full_dataset_transformed <- subset(full_dataset, select = c(Pclass,Sex,Age,SibSp,Parch,Fare,Embarked,Survived))

save(full_dataset_transformed, file = 'data/full_dataset_transformed.RData')

