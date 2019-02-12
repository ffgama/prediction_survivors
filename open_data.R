########## Extração dos dados ##########

rm(list=ls())
train <- read.csv('data/train.csv', header = TRUE, sep = ",")
# estrutura do dataset
str(train)

test <- read.csv('data/test.csv', header = TRUE, sep = ",")

# salvando os objetos
save(list=ls(), file ="data/raw_data.RData")