library(sp)
library(adehabitatHR)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("leitura_dados_acao.R")
source("resumo_macro.R")
setwd("~")

pairs(atq1.A[,c(4:9,11)], pch=16, col="navyblue", cex=0.6)
pairs(atq1.B[,2:11], pch=16, col="red4", cex=0.6)
sc.atq1A <- scale(atq1.A[,c(4:11)])
sc.atq1A <- cbind(atq1.A$ataque,sc.atq1A)
colnames(sc.atq1A)[1] <- "ataque"
sc.atq1A <- as.data.frame(sc.atq1A)

d = dist(sc.atq1A[,2:8])

agrup1 <- hclust(d, method = "complete")
plot(agrup1, xlab = "",ylab = "")

sc.def1B <- scale(def1.B[,c(4:11)])
sc.def1B <- cbind(def1.B$ataque,sc.def1B)
colnames(sc.atq1A)[1] <- "ataque"
sc.def1B <- as.data.frame(sc.def1B)

d2 = dist(sc.def1B[,2:9])

agrup2 <- hclust(d2, method = "ward.D2")
plot(agrup2, xlab = "",ylab = "", hang = -1, cex = 0.6)

clust <- cutree(agrup2,5)
clust


library(factoextra)
library(NbClust)
fviz_nbclust(sc.atq1A[,2:9], kmeans, method = "wss")+
  labs(x = "Número de grupos", y = "Soma de Quadrados Dentro total", title = "")

fviz_nbclust(sc.atq1A[,2:9], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


agrup4 <- kmeans(sc.atq1A[,2:9], centers = 6)
agrup4
agrup4$cluster

sc.atq1B <- scale(atq1.B[,2:9])
sc.atq1B <- cbind(atq1.B$ataque,sc.atq1B)
colnames(sc.atq1B)[1] <- "ataque"
sc.atq1B <- as.data.frame(sc.atq1B)

d = dist(sc.atq1B[,2:9])

agrup1 <- hclust(d, method = "complete")
plot(agrup1, xlab = "",ylab = "")


library(mclust)
fit <- Mclust(sc.atq1A[,2:10])
plot(fit) # plot results
summary(fit)