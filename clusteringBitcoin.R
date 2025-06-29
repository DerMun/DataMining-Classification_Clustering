library(tidyverse)#o que cada um de nós estamos usando?
library(cluster)
library(factoextra)#fviz_nbclust(), fviz_pca_ind()
library(dbscan)
library(Rtsne)        # t-SNE para visualização não-linear
library(ClusterR)
library(plotly)

data_raw <- read_csv("C:/dev/DM/BitcoinHeistData.csv", show_col_types = FALSE)

glimpse(data_raw)#Inspeção simplificada em comparação à -> str(), summary()...

#sem valores ausentes, sem tratamento de NA

#seleção de variáveis para clustering
features <- data_raw %>% select(-address, -label)#remove address e label

#normalização (scaling) dos dados
##features_scaled <- scale(features)#dados completos
features_sample <- sample_n(as.data.frame(features), 8001)
features_scaled <- scale(features_sample)

#determina número ideal de clusters para K-Means via método do cotovelo ( O(k_max × n × k × i × d) )
set.seed(3)
fviz_nbclust(features_scaled, kmeans, method = "wss", k.max = 10) + labs(title = "Elbow Method (amostra de 1000)")

#executando K-Means
set.seed(1)
k <- 3#substituir pelo valor indicado pelo método do cotovelo
##km_res <- kmeans(features_scaled, centers = k, nstart = 10)
km_res <- KMeans_rcpp(features_scaled, clusters = k, num_init = 5, max_iters = 100)#mais otimizado (Cpp)

#clusters -> amostra/dataset
##data_raw$cluster_kmeans <- factor(km_res$cluster)
sampled_data <- features_sample %>% mutate(cluster_kmeans = factor(km_res$cluster))

#view clusters em PCA
pca_res <- prcomp(features_scaled, center = TRUE, scale. = TRUE)
fviz_pca_ind(pca_res, geom.ind = "point", label = FALSE,
             habillage = sampled_data$cluster_kmeans,#data_raw = sampled_data
             palette = "jco", addEllipses = FALSE) +
  labs(title = "Clusters K-Means em PCA")

#executando DBSCAN (eps e minPts com kNNdistplot)
kNNdistplot(features_scaled, k = 5)
abline(h = 1.2, lty = 2)#ajustar eps baseado no gráfico

db_res <- dbscan(features_scaled, eps = 1.2, minPts = 5)

#clusters -> DBSCAN
sampled_data$cluster_dbscan <- factor(db_res$cluster)#data_raw = sampled_data

#view DBSCAN em PCA
fviz_pca_ind(pca_res, geom.ind = "point", label = FALSE,
             habillage = sampled_data$cluster_dbscan,#data_raw = sampled_data
             palette = "Dark2", addEllipses = TRUE) +
  labs(title = "Clusters DBSCAN em PCA")

#view com t-SNE
set.seed(2)
tsne_res <- Rtsne(features_scaled, perplexity = 30)
tsne_df <- data.frame(tsne_res$Y, cluster_kmeans = sampled_data$cluster_kmeans)

tsne_plot <- ggplot(tsne_df, aes(x = X1, y = X2, color = cluster_kmeans)) +
  geom_point(alpha = 0.7) +
  labs(title = "t-SNE dos clusters K-Means", x = "Dim 1", y = "Dim 2") +
  theme_minimal()

ggplotly(tsne_plot)
#comparar clusters K-Means com label de ransomware
#conf_matrix <- table(data_raw$label, sampled_data)
#print(conf_matrix)