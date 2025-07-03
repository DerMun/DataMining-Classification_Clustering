library(tidyverse)
library(cluster)
library(factoextra)#fviz_nbclust(), fviz_pca_ind()
library(dbscan)
library(Rtsne)
#library(ClusterR)
library(plotly)
library(gridExtra)
library(GGally)

#leituras e inspeção simplificada dos dados
rm(list=ls())
data_raw <- read_csv("C:/dev/DM/BitcoinHeistData.csv", show_col_types = FALSE)
glimpse(data_raw)


#seleção de variáveis para clustering (diminui income. remove address, label e então linhas duplicadas)
data_raw$income=log(data_raw$income)
features <- data_raw %>% select(-address, -label) %>% distinct()
glimpse(features)


#amostragem e normalização (scaling) dos dados
set.seed(1903)
features_sample <- sample_n(as.data.frame(features), 10000)
features_scaled <- scale(features_sample)

# --- Gráfico 1: Conjunto Original vs. Normalizado ---
pdf("grafico_comparacao_normalizacao.pdf", width = 10, height = 5)#inicia a gravação no arquivo PDF

c1 = ggplot (features_sample, aes(x = income, y = length)) + geom_point() +
  labs (title="Conjunto Original (sample)", x = "Income", y = "Length") + theme_bw()

c2 = ggplot (features_scaled, aes(x = income, y = length)) + geom_point() +
  labs (title="Conjunto Normalizado (sample)", x = "Income", y = "Length") + theme_bw()

grid.arrange(c1, c2, ncol=2)#mostra que a normalização para agrupamento é importante e não interfere na distribuição dos pontos

dev.off()#fecha o arquivo PDF e salva o gráfico


# --- Gráfico 2: Método do Cotovelo (Elbow Method) ---
pdf("grafico_elbow_method.pdf", width = 8, height = 6)

#determina número ideal de clusters para K-Means via método do cotovelo ( O(k_max × n × k × i × d) )
set.seed(1981)
fviz_nbclust(features_scaled, kmeans, method = "wss", k.max = 25) + 
  labs(title = "Elbow Method")#y=soma total de quadrados dentro do cluster

dev.off()


#análise de distância (Heatmap)
#pdf("grafico_heatmap_distancia.pdf", width = 8, height = 7)
#dist_matrix <- get_dist(features_scaled)
#fviz_dist(dist_matrix, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + labs(title = "Distância entre observações (amostra)")
#dev.off()


#executando K-Means
set.seed(1983)
k <- 8#substituir pelo valor indicado pelo método do cotovelo
km_res <- kmeans(features_scaled, centers = k, nstart = 25)#km_res <- KMeans_rcpp(features_scaled, clusters = k, num_init = 5, max_iters = 100)
predict <- km_res$cluster#cluster ao qual cada ponto é alocado (vetor)
predict
km_res$centers#centros de cluster (matriz)
km_res$size#pontos por cluster
sampled_data <- features_sample %>% mutate(cluster_kmeans = factor(km_res$cluster))#nova coluna chamada cluster_kmeans em sampled_data. coluna para linha = cluster ao qual o ponto correspondente foi atribuído pelo K-Means


km_res$betweenss#soma entre os quadrados dos aglomerados (deve ser alta)
km_res$withinss#vetor da soma de quadrados dentro do cluster (deve ser baixa)
km_res$tot.withinss#soma total de quadrados dentro do cluster
km_res$totss#total soma dos quadrados

aggregate(features_sample, by=list(predict), mean)#média de valores para cada cluster


#agrupamento
pdf("grafico_agrupamento_ggpairs.pdf", width = 12, height = 12)
ggpairs(cbind(features_sample, cluster=as.factor(predict)),
        columns=1:8, aes(colour=cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") + theme_bw()
dev.off()

plot(features_scaled[,1:8], col=predict)
plot(predict)
clusplot(features_scaled,predict, color = T, lines = F, labels = 4)


#comparar clusters K-Means com label de ransomware (incluir label na amostra)
sampled_labeled <- data_raw %>% slice_sample(n = nrow(sampled_data), replace = FALSE) %>% mutate(cluster_kmeans = sampled_data$cluster_kmeans)
conf_matrix <- table(sampled_labeled$label, sampled_labeled$cluster_kmeans)
print(conf_matrix)


#view clusters K-Means via PCA
# --- Gráfico 3: Clusters K-Means via PCA ---
pdf("grafico_kmeans_pca.pdf", width = 8, height = 6)

pca_res <- prcomp(features_scaled, center = TRUE, scale. = TRUE)

fviz_pca_ind(pca_res, geom.ind = "point", label = FALSE,
             habillage = sampled_data$cluster_kmeans,
             palette = "jco", addEllipses = FALSE) +
  labs(title = "Clusters K-Means em PCA")

dev.off()


#view para diferentes k
#for (k_test in 2:5) {
#  pdf(paste0("grafico_kmeans_cluster_", k_test, ".pdf"), width = 8, height = 6)
#  cl <- kmeans(features_scaled, centers = k_test)
#  fviz_cluster(cl, data = features_scaled) + labs(title = paste(k_test, "clusters em sample")) %>% print()
#  dev.off()
#}


#executando DBSCAN (eps e minPts com kNNdistplot)
# --- Gráfico 4: kNNdistplot para DBSCAN ---
pdf("grafico_knndist_dbscan.pdf", width = 8, height = 6)
kNNdistplot(features_scaled, k = 5)
abline(h = 1.2, lty = 2)#ajustar eps baseado no gráfico
dev.off()

db_res <- dbscan(features_scaled, eps = 1.2, minPts = 5)
sampled_data$cluster_dbscan <- factor(db_res$cluster)#clusters -> DBSCAN


#view DBSCAN em PCA (ideal para identificar pontos que não pertencem a nenhum cluster denso. outliers)
# --- Gráfico 5: Clusters DBSCAN em PCA ---
pdf("grafico_dbscan_pca.pdf", width = 8, height = 6)

fviz_pca_ind(pca_res, geom.ind = "point", label = FALSE,
             habillage = sampled_data$cluster_dbscan,
             palette = "Dark2", addEllipses = FALSE) +
  labs(title = "Clusters DBSCAN em PCA")

dev.off()


#view t-SNE
# --- Gráfico 6: t-SNE dos clusters K-Means ---
pdf("grafico_tsne_kmeans.pdf", width = 9, height = 7)

set.seed(2017)
tsne_res <- Rtsne(features_scaled, perplexity = 30)
tsne_df <- data.frame(tsne_res$Y, cluster_kmeans = sampled_data$cluster_kmeans) 

ggplot(tsne_df, aes(x = X1, y = X2, color = cluster_kmeans)) +
  geom_point(alpha = 0.7) +
  labs(title = "t-SNE dos clusters K-Means", x = "Dim 1", y = "Dim 2") +
  theme_minimal()

dev.off()
#ggplotly(tsne_plot)
