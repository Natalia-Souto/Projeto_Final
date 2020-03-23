# Script trabalho final do curso Manipulação de dados no R
# parte do curso Projetos de análise de dados em R

library("tidyr")
library(FactoMineR)
library(factoextra)

# Lendo a planilha de dados #
read.csv2("Dados/Planilha_R_editado_4.csv")

# Nomeando a planilha #
Dados <- read.csv2("Dados/Planilha_R_editado_4.csv")

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


# Frequência do número de Escamas ventrais, subcaudais e comprimento CRC e CC
par(mfrow = c(2,2))
hist(Dados$Esc_ventrais, las = 1)
hist(Dados$Esc_subcaudais, las = 1)
hist(Dados$CRC, las = 1)
hist(Dados$CC, las = 1)


# não sei o que significa!
plot(density(Dados$Esc_ventrais))
hist(Dados$Esc_ventrais, freq = FALSE)
lines(density(Dados$Esc_ventrais), col = "blue")

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
