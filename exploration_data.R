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

load("data/raw_data.RData")
load("data/full_dataset_transformed.RData")

library(dplyr)
library(knitr)

library(ggplot2)
library(gridExtra)

# o percentual de pessoas que sobreviveram ao desastre foi de apenas 38%. Enquanto que pessoas que não sobreviveram
# representam mais de 60% do dataset.

full_dataset_transformed %>%
  na.omit() %>%  mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>% 
  ggplot(aes(x = Survived, fill = factor(Survived))) +
  geom_bar(stat="count") +
  ggtitle("Qual o número de sobreviventes do titanic? ") +
  geom_label(stat = "count", aes(label=..count..)) +
  scale_x_discrete(limits=c("No","Yes")) + 
  xlab("Sobrevivência (0 = Não / 1 = Sim)") + ylab("Total de Ocorrências") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

prop_surv <- full_dataset_transformed %>%
  na.omit() %>%
  count(Survived) %>%
  mutate(prop = n / sum(n))

kable(prop_surv)

######## Análise de frequência para Pclass
# A classe 3 possui o maior número de passageiros.
# é possível perceber que houve um maior número de fatalidades nas classes menos privilegiadas. 
# a primeira classe foi a única que houve maior número de sobreviventes.
plot_pclass <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(Pclass, fill = Survived)) + 
  geom_bar() + 
  ggtitle("Pclass") +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.9)) + 
  labs(fill='Survived') +
  xlab("Class") + ylab("Total") +
  facet_wrap(~Pclass, scale = "free") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))
  
# em termos proporcionais constata-se que grande parte dos passageiros da primeira classe sobreviveram.
# enquanto que a pior taxa de sobrevivência foram para os passageiros da terceira classe apenas (24% aprox.) sobreviveram.  
prop_surv <- full_dataset_transformed %>%
  na.omit() %>%
  count(Survived, Pclass) %>%
  group_by(Pclass) %>%
  mutate(prop = n / sum(n)) %>% arrange(desc(Survived))

kable(prop_surv)

plot_pclass

######## SibSp para cada classe de sobreviventes (número de irmãos/companheiros)

# Grande parte dos passageiros não possuem irmãos/companheiros. 
# Curioso notar que o número de sobreviventes foi maior em situações onde os passageiros estiveram acompanhados de  
# 1 irmão/companheiro.
plot_sibsp <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(SibSp, fill = Survived)) + 
  geom_bar() + 
  ggtitle("SibSp") +
  xlab("SibSp") + ylab("Total") +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))
  
plot_sibsp

######## Parch para cada classe de sobreviventes (quantidade de parentes/filhos)

# uma quantidade mais equilibrada entre sobreviventes foi alcançada quando os passageiros 
# estiveram acompanhados de parentes/filhos. O que faz sentido quando pensamos que em situações de emergência
# crianças tiveram prioridade.
plot_parch <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(Parch, fill = Survived)) + 
  geom_bar() + 
  ggtitle("Parch") +
  xlab("Parch") + ylab("Total") +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

plot_parch

######## Fare para cada classe de sobreviventes (ingresso/passagem/tarifa)

# boa parte dos passageiros pagaram até 100 pelo ingresso. 
# No geral, a proporção de sobreviventes aumentou junto com o preço do ingresso. Esse insight está relacionado
# com o que foi obtido na variável pclass. Parece que houve prioridade de emergência 
# a passageiros que pagaram mais pela passagem (ou estiveram em classes superiores).

plot_fare <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(Fare, fill = Survived)) + 
  geom_histogram() +
  scale_fill_manual(values=c("#000000", "#95b2f0")) + 
  ggtitle("Fare") +
  xlab("Fare") + ylab("Total") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

plot_fare

grid.arrange(plot_pclass, plot_sibsp, plot_parch, plot_fare, nrow = 2)

######## embarque para cada classe de sobreviventes (ingresso/passagem/tarifa)

# a quantidade de sobreviventes foi que o número de fatalidades apenas para passageiros que embarcaramem C (Cherbourg).
# podemos observar a classe mais frequentes dos passageiros que embarcaram em C.
plot_embarked <-  full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(Embarked, fill = Survived)) + 
  geom_bar() + 
  ggtitle("Embarked") +
  xlab("Embarked") + ylab("Total") +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

plot_embarked

prop_surv <- full_dataset_transformed %>%
  na.omit() %>%
  count(Survived, Embarked) %>%
  group_by(Embarked) %>%
  mutate(prop = n / sum(n)) %>% arrange(desc(Survived))

kable(prop_surv)

# analisando a classe dos embarques, concluimos que o embarque de passageiros em C (Cherbourg) obteve 
# a menor quantidade de passageiros da classe 3 o que pode ter contribuído para minimizar o percentual 
# de mortalidade de passageiros que embarcaram em C.

plot_embarked_pclass <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(Embarked, fill = factor(Pclass))) + 
  geom_bar() + 
  scale_fill_manual(values=c("#fd9b89", "#fa8072", "#dd6c6c")) + 
  ggtitle("Pclass x Embarked") +
  xlab("Embarked") + ylab("Total") +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

plot_embarked_pclass

grid.arrange(plot_embarked, plot_embarked_pclass, nrow = 1)

######## Análise da variável Sexo para cada classe

# claramente percebemos que passageiros do sexo feminino tiveram uma taxa de sobrevivência maior que os 
# passageiros do sexo masculino.
plot_sex <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(Sex, fill = Survived)) + 
  geom_bar() + 
  ggtitle("Sex") +
  scale_fill_manual(values=c("#e4888b", "#4773aa")) + 
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.9)) + 
  labs(fill='Survived') +
  xlab("Sex") + ylab("Total") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

plot_sex

# 74% de mulheres aproximadamente sobreviveram enquanto que no lado masculino 19% aprox. sobreviveram.
prop_surv <- full_dataset_transformed %>%
  na.omit() %>%
  count(Survived, Sex) %>%
  group_by(Sex) %>%
  mutate(prop = n / sum(n)) %>% arrange(desc(Survived))

kable(prop_surv)

# grande parte das mulheres das classes 1 e 2 sobreviveram. 
plot_sex_class <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(Pclass, fill = factor(Survived))) + 
  geom_bar() + 
  ggtitle("Pclass x Sex") +
  scale_fill_manual(values=c("#e4888b", "#4773aa")) + 
  labs(fill='Survived') +
  xlab("Class") + ylab("Total") +
  facet_wrap(~Sex, scale = "free") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

plot_sex_class

######## Distribuição da idade para cada classe de sobreviventes

# a curva de densidade demonstra que crianças possuem a maior taxa de sobrevivência. 
# A curva de densidade  para adultos com idade acima de 30 anos variou em torno da média.
# Os maiores picos de mortalidade ficaram entre jovens adultos entre 25-30 anos.

plot_age <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(x = Age, fill = Survived)) +
  geom_density(alpha=0.5, aes(fill=factor(Survived))) + 
  ggtitle("Sobrevivência em função da idade") +
  scale_fill_manual(values=c("#e4888b", "#4773aa")) + 
  scale_x_continuous() + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

plot_age

# idade x sexo em funçao da sobrevivência
# entre os sobreviventes do sexo feminino a média da idade é mais alta. O que poderia indicar a existências de mães.
# Assim, é provável que as mamães e seus filhos tiveram prioridade em situação de emergência.
plot_age_class <- full_dataset_transformed %>%
  na.omit() %>% mutate(Survived = ifelse(Survived == 1, "Yes", "No")) %>%
  ggplot(aes(Sex, Age, fill = factor(Survived))) + 
  geom_boxplot() + 
  ggtitle("Age x Sex") +
  labs(fill='Survived') +
  facet_wrap(~Sex, scale = "free") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))

plot_age_class

grid.arrange(plot_sex, plot_sex_class, plot_age, plot_age_class, nrow = 2)

########## Correlação
# as variáveis mais correlacionadas a survived são sex, fare e pclass.
library(ggcorrplot)

full_dataset_cor <- full_dataset_transformed %>%
  na.omit() 

full_dataset_cor <- as.data.frame(sapply(full_dataset_cor, function(x) as.numeric(x)))

full_dataset_cor %>%
  cor(full_dataset_cor) %>% round(digits = 1) %>%
  ggcorrplot(hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             title = "Correlação entre as variáveis",
             lab_size = 3, 
             method="square", 
             colors =c("#00344c", "#66cfff", "#00344c"), 
             ggtheme=theme_minimal() + theme(plot.title = element_text(hjust = 0.5)))
