# Script trabalho final do curso Manipulação de dados no R
# parte do curso Projetos de análise de dados em R

library("tidyr")

# Lendo a planilha de dados #
read.csv2("Dados/Planilha_R_editado.csv")

# Nomeando a planilha #
Dados <- read.csv2("Dados/Planilha_R_editado.csv")

# Número de linhas da planilha #
nrow(Dados)

# Entendo os dados #
head(Dados)
dim(Dados)

#### Análise exploratória dos Dados ####
summary(Dados)
class(Dados)
str(Dados)

# Média escamas ventrais #
mean(Dados$Esc_ventrais)

# Média das escamas subcaudais #
mean(Dados$Esc_subcaudais)

# Boxplot das escamas ventrais, números na horizontal #
boxplot(Dados$Esc_ventrais, las = 1)

# Boxplot das escamas subcaudais, números na horizontal #
boxplot(Dados$Esc_subcaudais, las = 1)

boxplot(Esc_ventrais ~ Especie, data = Dados, las = 1)
boxplot(Esc_subcaudais ~ Especie, data = Dados, las = 1)

boxplot(Esc_ventrais ~ Sexo, data = Dados, las = 1)
boxplot(Esc_subcaudais ~ Sexo, data = Dados, las = 1)


