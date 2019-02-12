full_dataset <- subset(full_dataset, select = c(Pclass:Fare, Embarked:Survived))

# realizando a imputação com o KNN
library(DMwR)
sub_full_dataset <- full_dataset[,c("Pclass","Sex","Age","Fare","Embarked")]

sub_full_dataset$Sex <- ifelse(sub_full_dataset$Sex == "male", 1, 2)
levels(sub_full_dataset$Embarked) <- c(1,2,3)
sub_full_dataset$Age <- as.integer(sub_full_dataset$Age)

full_dataset_transf <- knnImputation(sub_full_dataset, k = 10, scale = T)
full_dataset_transf$Age <- as.integer(full_dataset_transf$Age)

# comparar a distribuição estatística da variável Age antes e após a imputação
# antes
summary(full_dataset$Age)
summary(full_dataset_transf$Age)