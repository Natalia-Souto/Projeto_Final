# Script trabalho final do curso Manipulação de dados no R
# parte do curso Projetos de análise de dados em R

library("tidyr")
library(FactoMineR)
library(ggplot2)
library(factoextra)

# Lendo a planilha de dados #
read.csv2("Dados/Planilha_R_editado_4.csv")

# Nomeando a planilha #
Dados <- read.csv2("Dados/Planilha_R_editado_4.csv")
Liophis_dorsalis <- Dados[1:63, 1:6]
Liophis_jaegeri <- Dados[64: 116, 1:6]

# Número de linhas da planilha #
nrow(Dados)

# Entendo os dados #
Liophis_dorsalis
Liophis_jaegeri
head(Dados)
head(Liophis_dorsalis)
dim(Dados)

#### Análise exploratória dos Dados ####
summary(Dados)
summary(Liophis_dorsalis)
summary(Liophis_jaegeri)
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
par(mfrow = c(1,2))
boxplot(Esc_ventrais ~ Especie, data = Dados, las = 1)
boxplot(Esc_subcaudais ~ Especie, data = Dados, las = 1)

# Boxplot das escamas ventrais e subcaudais por sexo e espécie
par(mfrow = c(2,2))
boxplot(Esc_ventrais ~ Sexo, data = Liophis_dorsalis, las = 1)
boxplot(Esc_subcaudais ~ Sexo, data = Liophis_dorsalis, las = 1)
boxplot(Esc_ventrais ~ Sexo, data = Liophis_jaegeri, las = 1)
boxplot(Esc_subcaudais ~ Sexo, data = Liophis_jaegeri, las = 1)

# Boxplot do comprimento CRC e CC por espécie
par(mfrow = c(1,2))
boxplot(CRC ~ Especie, data = Dados, las = 1)
boxplot(CC ~ Especie, data = Dados, las = 1)

# Boxplot do comprimento CRC e CC por sexo e espécie
par(mfrow = c(2,2))
boxplot(CRC ~ Sexo, data = Liophis_dorsalis, las = 1)
boxplot(CC ~ Sexo, data = Liophis_dorsalis, las = 1)
boxplot(CRC ~ Sexo, data = Liophis_jaegeri, las = 1)
boxplot(CC ~ Sexo, data = Liophis_jaegeri, las = 1)


# Frequência do número de Escamas ventrais por espécie
par(mfrow = c(1,2))
hist(Liophis_dorsalis$Esc_ventrais, las = 1)
hist(Liophis_jaegeri$Esc_ventrais, las = 1)

# Frequência do número de Escamas subcaudais por espécie
par(mfrow = c(1,2))
hist(Liophis_dorsalis$Esc_subcaudais, las = 1)
hist(Liophis_jaegeri$Esc_subcaudais, las = 1)

# Frequência de CRC por espécie
par(mfrow = c(1,2))
hist(Liophis_dorsalis$CRC, las = 1)
hist(Liophis_jaegeri$CRC, las = 1)

# Frequência de CC por espécie
par(mfrow = c(1, 2))
hist(Liophis_dorsalis$CC, las = 1)
hist(Liophis_jaegeri$CC, las = 1)


# não sei o que significa!
plot(density(Dados$Esc_ventrais))
hist(Dados$Esc_ventrais, freq = FALSE)
lines(density(Dados$Esc_ventrais), col = "blue")

plot(density(Dados$Esc_subcaudais))
hist(Dados$Esc_subcaudais, freq = FALSE)
lines(density(Dados$Esc_subcaudais), col = "blue")

plot(density(Dados$CRC))
hist(Dados$CRC, freq = FALSE)
lines(density(Dados$CRC), col = "blue")

plot(density(Dados$CC))
hist(Dados$CC, freq = FALSE)
lines(density(Dados$CC), col = "blue")

# Alálise de PCA
## Criando subconjuntos de linhas e colunas para a análise
Dados_PCA <- Dados[1:116, 3:6]

# Visualizar os dados
View(Dados_PCA)

# Gerando PCA
resu.pca <- prcomp(Dados_PCA)

summary(resu.pca)

# Extrair a proporção de variância dos valores de componentes principais
eig.val <- get_eigenvalue(resu.pca)
eig.val

# Plotar no gráfico mostrando a proporção de variância de cada variavel
fviz_eig(resu.pca, addlabels = T, ylim = c(0,90))

# Extrair os resultados das variaveis do PCA para plotar no gráfico
var <- get_pca_var(resu.pca)
ind <- get_pca_ind(resu.pca)

# Plotar gráfico de PCA
fviz_pca_var(resu.pca, col.var = "blue")

# Criando grupo para cluster
especie <- as.factor(Dados[ ,1])

# Plotando com grupos
fviz_pca(resu.pca, habillage = especie, title = )

