# Script trabalho final do curso Manipulação de dados no R
# parte do curso Projetos de análise de dados em R

library("tidyr")

# Lendo a planilha de dados #
read.csv2("Dados/Planilha_R_editado_3.csv")

# Nomeando a planilha #
Dados <- read.csv2("Dados/Planilha_R_editado_3.csv")

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

# Média CRC
mean(Dados$CRC)

# Média CC
mean(Dados$CC)

# Boxplot das escamas ventrais, números na horizontal #
boxplot(Dados$Esc_ventrais, las = 1)

# Boxplot das escamas subcaudais, números na horizontal #
boxplot(Dados$Esc_subcaudais, las = 1)

# Boxplot das escamas ventrais e subcaudais por espécie
boxplot(Esc_ventrais ~ Especie, data = Dados, las = 1)
boxplot(Esc_subcaudais ~ Especie, data = Dados, las = 1)

# Boxplot das escamas ventrais e subcaudais por sexo
boxplot(Esc_ventrais ~ Sexo, data = Dados, las = 1)
boxplot(Esc_subcaudais ~ Sexo, data = Dados, las = 1)

# Boxplot do comprimento CRC e CC por espécie
boxplot(CRC ~ Especie, data = Dados, las = 1)
boxplot(CC ~ Especie, data = Dados, las = 1)

# Boxplot do comprimento CRC e CC por sexo
boxplot(CRC ~ Sexo, data = Dados, las = 1)
boxplot(CC ~ Sexo, data = Dados, las = 1)


par(mfrow = c(2,2))
hist(Dados$Esc_ventrais, las = 1)
hist(Dados$Esc_subcaudais, las = 1)
hist(Dados$CRC, las = 1)
hist(Dados$CC, las = 1)


par(mfrow = c(1, 1))
hist(Dados$Esc_ventrais, las = 1, breaks = 4)
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))


plot(density(Dados$Esc_ventrais))
hist(Dados$Esc_ventrais, freq = FALSE)
lines(density(Dados$Esc_ventrais), col = "blue")
