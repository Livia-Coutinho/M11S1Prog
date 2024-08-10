# Carregar pacotes
library(tidyverse)
library(GGally)
library(corrplot)
library(gridExtra)
library(readr)

#CARREGAR O CONJUNTO DE DADOS

arquivo <- "C:/Users/Inteli/Desktop/GitHub (módulo 11)/progS1/sku_price - Sheet1.csv"
dados <- read.csv(arquivo)

#VISUALIZAÇÃO DAS PRIMEIRAS LINHAS

head(dados)
print(dados)
nrow(dados)

#VERIFICAÇÃO DA ESTRUTURA DE DADOS

str(dados)
glimpse(dados) #tidyverse

#RESUMO ESTATÍSTICO

summary(dados)

#resumo estatístico variáveis numéricas
if (!require(skimr)) install.packages("skimr")
library(skimr)
skim(dados)

#resumo estatístico com psych
if (!require(psych)) install.packages("psych")
library(psych)
describe(dados)

#descrição das variáveis
#SKU_ID: Identificador único do produto.
#START_DT: Data de início do período de preços.
#END_DT: Data de término do período de preços.
#PRICE_AMT: Valor do preço para o período especificado.

#ANÁLISE UNIVARIADA - VISUALIZAÇÃO DAS DISTRIBUIÇÕES

# pacote ggplot2
library(ggplot2)

# gráfico de densidade - variável PRICE_AMT
ggplot(dados, aes(x = PRICE_AMT)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Densidade dos Preços", x = "Preço", y = "Densidade") +
  theme_minimal()

# boxplot - variável PRICE_AMT
ggplot(dados, aes(y = PRICE_AMT)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot dos Preços", y = "Preço") +
  theme_minimal

#formato data pro histograma dar certo (estava em formato caracter)
dados$START_DT <- as.Date(dados$START_DT, format = "%Y-%m-%d")
dados$END_DT <- as.Date(dados$END_DT, format = "%Y-%m-%d")

ggplot(dados, aes(x = PRICE_AMT)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribuiçção dos preços", x = "Preço", y = "Contagem") +
  theme_minimal()

ggplot(dados, aes(x = START_DT)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribuição das datas de início", x = "Data de Início", y = "Contagem") +
  theme_minimal()

ggplot(dados, aes(x = END_DT)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribuição das datas de término", x = "Data de Término", y = "Contagem") +
  theme_minimal()

#OUTLIERS

# Calcular estatísticas descritivas para PRICE_AMT
summary_stats <- summary(dados$PRICE_AMT)
IQR_value <- IQR(dados$PRICE_AMT)

# Identificar outliers
lower_bound <- summary_stats["1st Qu."] - 1.5 * IQR_value
upper_bound <- summary_stats["3rd Qu."] + 1.5 * IQR_value

outliers <- dados$PRICE_AMT[dados$PRICE_AMT < lower_bound | dados$PRICE_AMT > upper_bound]
outliers
#retornou --> [1] 118.99 129.99
#os dois preços retornados estão além dos limites superior do intervalo interquartil

#ANÁLISE BIVARIADA 

#Visualização de relações entre variáveis

# Gráfico de dispersão entre PRICE_AMT e SKU_ID
ggplot(dados, aes(x = SKU_ID, y = PRICE_AMT)) +
  geom_point(color = "blue") +
  labs(title = "Relação entre SKU_ID e Preço", x = "SKU_ID", y = "Preço") +
  theme_minimal()

# Gráfico de linha para visualizar a tendência de preço ao longo do tempo
ggplot(dados, aes(x = START_DT, y = PRICE_AMT)) +
  geom_line(color = "blue") +
  labs(title = "Tendência de Preço ao Longo do Tempo", x = "Data de Início", y = "Preço") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")


# Análise de correlação
matriz_correlacao <- cor(dados %>% select_if(is.numeric), use = "complete.obs")

corrplot(matriz_correlacao, method = "circle", type = "full", 
         tl.col = "black", tl.srt = 45, 
         title = "Matriz de Correlação", 
         mar = c(0,0,1,0))



dados_numericos <- dados %>% select_if(is.numeric) # seleção variáveis numéricas e padronizar os dados
dados_pca <- scale(dados_numericos)

pca_result <- prcomp(dados_pca, center = TRUE, scale. = TRUE) # Realizar PCA

biplot(pca_result, scale = 0, cex = 0.7) # Biplot para visualizar os resultados
screeplot(pca_result, main = "Scree Plot") # Plotar a variância explicada por cada componente
summary(pca_result) # Resumo dos componentes principais


# SUMÁRIO E DISCUSSÃO

# Dados: O conjunto contém preços de produtos com IDs, datas de início e término dos preços.

# Distribuição dos preços:
#    - A maioria dos preços está em uma faixa específica.
#    - Identificamos alguns valores extremos como outliers (118.99 e 129.99).

# Distribuição das datas:
#    - As datas de início e término estão bem distribuídas ao longo do período.

# Relação entre variáveis:
#    - Não há uma correlação clara entre o ID do produto e o preço.
#    - Os preços mostram variações ao longo do tempo.

#Componentes Principais (PCA):
#    - Os primeiros componentes principais explicam a maior parte da variância nos dados.

# Limitações e melhorias

# Limitações:
#    - Dados: Presença de outliers e possíveis erros.
#    - Variáveis Categóricas: Não foram incluídas na análise.

# Melhorias:
#    - Limpeza de dados: Tratar outliers e erros.
#    - Incluir mais variáveis: Adicionar variáveis categóricas na análise.
#    - Explorar tendências: Analisar padrões de preços com mais detalhes.



