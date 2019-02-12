# A priori não queremos perder nenhuma informação, primeiro vamos trabalhar na variável sexo:
full_dataset[!complete.cases(full_dataset$Sex),]

# É possível obter o sexo através da coluna Name, vamos quebrar essa variável
att_dataset <- NULL
for(ind in 1:length(full_dataset$Pclass)){
  full_dataset$LastName[ind] <- c(att_dataset, strsplit(as.character(full_dataset$Name), ",", fixed = TRUE))[[ind]][1]
  full_dataset$Name[ind] <- c(att_dataset, strsplit(as.character(full_dataset$Name), ",", fixed = TRUE))[[ind]][2]
}

# reordenar colunas
full_dataset <- full_dataset[,c(1,2,12,3:11)]
head(full_dataset)

# vamos filtrar novamente apenas aquelas instâncias onde o sexo não está preenchido
library(stringr)

for(row in 1:nrow(full_dataset)){
  
  if (is.na(full_dataset$Sex)[row]){
    if(str_detect(full_dataset$Name[row], fixed("Mrs."))  | str_detect(full_dataset$Name[row], fixed("Miss.")) | str_detect(full_dataset$Name[row], fixed("Ms.")) | str_detect(full_dataset$Name[row], fixed("the Countess."))) 
    {
      full_dataset$Sex[row] <- c("female")
    }else{
      full_dataset$Sex[row] <- c("male")
    }
  }
}

# analisando a variável cabin
head(full_dataset)

# grande parte (aprox. 87%) das cabines cadastradas são de primeira classe
table(subset(full_dataset, !Cabin == "", c(Pclass, Cabin))$Pclass)

# verificar as instâncias não preenchidas de Pclass
full_dataset[is.na(full_dataset$Pclass),]

# identificar se a coluna Ticket pode ajudar a preencher a coluna Pclass
full_dataset[full_dataset$Ticket == "113789",]
# é bem provável que seja da própria familia (conjuge) pagaram o mesmo valor na passagem e embarcaram do mesmo lugar.
# podemos atribuir o mesmo Pclass
full_dataset[full_dataset$Ticket == "113789",][[1]][1] <- 1

# vamos analisar os próximos Pclass não preenchidos
full_dataset[full_dataset$Ticket == "C.A. 33595",]

# ambos provavelmente são da mesma familia. SibSp e Parch tem o valor zero. Também pagaram pelo mesmo o 
# mesmo valor no ticket e embarcaram no mesmo local.

# podemos atribuir o mesmo Pclass
full_dataset[full_dataset$Ticket == "C.A. 33595",][[1]][1] <- 2

# observando as instâncias deste mesmo ticket
full_dataset[full_dataset$Ticket == "PC 17582",]

# é possível perceber que este passageiro compõe uma cabine preenchida, o que aumenta a probabilidade de 
# de pertencer a primeira a primeira classe. Além de possuir outros atributos semelhantes a outros passageiros
# com mesmo ticket.
full_dataset[full_dataset$Ticket == "PC 17582",][[1]]

# atribuindo
full_dataset[full_dataset$Ticket == "PC 17582",][[1]][2] <- 1

# outro passageiro
full_dataset[full_dataset$Ticket == "315089",]

# apenas um único registro podemos olhar para duas variáveis principais a primeira o preço do ticket é baixo e poderia
# indicar que este passageiro está na classe 3. Também poderemos buscar pelo LastName algum possível parantesco familiar.
full_dataset[full_dataset$LastName == "Cacic",]

# detectamos que todos que carregam o mesmo sobrenome estão na classe 3.
full_dataset[full_dataset$Ticket == "315089",][[1]][1] <- 3

# outro passageiro
full_dataset[full_dataset$Ticket == "31027",]

# parece que este passageiro está mesmo na classe 2, quando observamos as outras variáveis
full_dataset[full_dataset$Ticket == "31027",][[1]][1] <- 2

# outro passageiro
full_dataset[full_dataset$Ticket == "31028",]

# pelo preço talvez este passageiro esteja alocado na classe 3. Vamos observar o LastName. 
full_dataset[full_dataset$LastName == "Gavey",]

# é o único com este LastName. Pesquisas externas apontam que ele embarcou na classe 2.
full_dataset[full_dataset$Ticket == "31028",][[1]][1] <- 2

# vamos aos próximos pclass não preenchidos
full_dataset[is.na(full_dataset$Pclass),]

# outro passageiro
full_dataset[full_dataset$Ticket == "36864",]

# o preço pode indicar que este passageiro esteve na classe 3. Vamos ao Last Lame.
full_dataset[full_dataset$LastName == "Gallagher",]

# da mesma maneira que no caso anterior o passageiro é o único com este sobrenome. Pesquisas externas confirmam classe 3.
full_dataset[full_dataset$Ticket == "36864",][[1]][1] <- 3

# outro passageiro
full_dataset[full_dataset$Ticket == "347082",]

# os resultados indicam uma alta probabilidade de ser da classe 3, especialmente pelo parantesco.
full_dataset[full_dataset$Ticket == "347082",][[1]][6] <- 3

# outro passageiro
full_dataset[full_dataset$Ticket == "11751",]

# não existem informações suficientes para concluir sobre a classe desse passageiro. Vamos ao LastName.
full_dataset[full_dataset$LastName == "Beckwith",]

# parece que existe um vínculo entre os passageiros em questão assim podemos atribuir a classe 1.
full_dataset[full_dataset$Ticket == "11751",][[1]][2] <- 1

# atualizando nossa tabela de instâncias não preenchidas:
# Pclass = 9 instâncias (OK)
# Sex = 45 instâncias (OK)
# Age = 265 instâncias
# Fare = 1 instância
# Cabin = 1014 instâncias
# Embarked = 2 instâncias

# Manualmente poderemos também preencher as instâncias das variáveis Fare e Embarked.
full_dataset[!complete.cases(full_dataset$Fare),]

# temos algumas pistas que podem ser úteis para setar um valor para esta instância
# observe a Pclass igual a 3. Vamos verificar os valores extremos pagos por passageiros desta classe.
extract_subset <- subset(full_dataset, full_dataset$Pclass == 3, select = c(Pclass,LastName,Embarked,Fare))

# plotando em um histograma
# grande parte dos valores para Fare situam-se entre 10 e 20.
ggplot(data=extract_subset, aes(Fare)) + geom_histogram(bins = 10) +  theme_minimal()

# vamos observar outras colunas. LastName não oferece nenhuma informação adicional
extract_subset[extract_subset$LastName == "Storey",]

# 495 embarcaram de Southampton
nrow(extract_subset[extract_subset$Embarked == "S",])

extract_embarked_s <- extract_subset[extract_subset$Embarked == "S",]

# qual o preço médio do ticket cobrado por lá? 
summary(extract_embarked_s)

# seria interessante olhar para a mediana do que a média por conta dos valores discrepantes
# nesse caso temos que a mediana do valor 8.05. Considerando que o preço da tarifa para esse passageiro
# não seja tão diferente dos demais vamos verificar quais são os preços mais comumentes praticados.

df_freq_fare<-data.frame(table(extract_embarked_s$Fare))
colnames(df_freq_fare) <- c("fare",'freq')
df_freq_fare[order(df_freq_fare$freq, decreasing = TRUE),]

# observe que além da mediana os valores entre 7 e 8 são mais comuns. Decidimos setar a mediana para esta instância.
full_dataset[!complete.cases(full_dataset$Fare),][,c("Fare")] <- median(extract_embarked_s$Fare, na.rm = TRUE)

# Agora falta preenchermos as instâncias de Embarked
full_dataset[!complete.cases(full_dataset$Embarked),]

# checando a coluna LastName. Para tentar identificar pistas de vinculos familiares que podem indicar que
# embarcaram de um mesmo local. Nenhuma informação adicional.
full_dataset[full_dataset$LastName == "Icard",]
full_dataset[full_dataset$LastName == "Stone",]

# será que o número do ticket poderia ajudar? Ambos tem o mesmo número de ticket.
# aparentemente não.
full_dataset[full_dataset$Ticket == "113572",]

# o preço do ticket pode ser útil para indicar o local de embarque?
# também não.
full_dataset[full_dataset$Fare == 80,]

# qual o local mais comum de embarque?
# Hum, Southampton
table(full_dataset$Embarked)
# Em termos de proporção aproximadamente 70% dos embarques são feitos por lá.
prop.table(table(full_dataset$Embarked))

# Buscamos essa informação externamente e realmente constatamos que ambos desembarcaram em Southampton. 
# Assim, podemos preencher essa instância de forma mais segura.
full_dataset[!complete.cases(full_dataset$Embarked),][,c("Embarked")] <- c("S","S")