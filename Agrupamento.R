library(sp)
library(adehabitatHR)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("leitura_dados_acao.R")
source("resumo_macro.R")
setwd("~")

pairs(atq1.A[,c(4:11)], pch=16, col="navyblue", cex=0.6)
pairs(atq1.B[,2:11], pch=16, col="red4", cex=0.6)

c <- cor(atq1.A)
View(c)

k <- c(2,3,4,7,8,9)
l <- 2:9

sc.atq1A <- scale(atq1.A[,c(4:11)])
sc.atq1A <- cbind(atq1.A$ataque,sc.atq1A)
colnames(sc.atq1A)[1] <- "ataque"
sc.atq1A <- as.data.frame(sc.atq1A)

d = dist(sc.atq1A[,k])

agrup1 <- hclust(d, method = "ward.D2")
plot(agrup1, xlab = "",ylab = "", hang = -1, cex = 0.6,main = "Dendograma Atq Time A - Jogo 1 
     (sem pos bola)")

clust <- cutree(agrup1,3)
clust



sc.def1B <- scale(def1.B[,c(4:11)])
sc.def1B <- cbind(def1.B$ataque,sc.def1B)
colnames(sc.def1B)[1] <- "ataque"
sc.def1B <- as.data.frame(sc.def1B)


d2 = dist(sc.def1B[,c(3,4,7:9)])

agrup2 <- hclust(d2, method = "ward.D2")
plot(agrup2, xlab = "",ylab = "", hang = -1, cex = 0.6,main = "Dendograma Def Time B - Jogo 1 
     (sem ac/temp e pos bola)")

clust2 <- cutree(agrup2,4)
clust2


compar1.A <- data.frame(atq1.B$ataque,clust,clust2)
colnames(compar1.A) <- c("ataque","Atq","Def")
compar1.A$Atq <- as.factor(compar1.A$Atq)
compar1.A$Def <- as.factor(compar1.A$Def)

teste1 <- xtabs(~Atq+Def, compar1.A)

teste1

sc.atq1B <- scale(atq1.B[,2:9])
sc.atq1B <- cbind(atq1.B$ataque,sc.atq1B)
colnames(sc.atq1B)[1] <- "ataque"
sc.atq1B <- as.data.frame(sc.atq1B)

d = dist(sc.atq1B[,2:9])

agrup1 <- hclust(d, method = "ward.D2")
plot(agrup1, xlab = "",ylab = "", hang = -1, cex = 0.6)

sc.def1A <- scale(def1.A[,c(4:11)])
sc.def1A <- cbind(def1.A$ataque,sc.def1A)
colnames(sc.def1A)[1] <- "ataque"
sc.def1A <- as.data.frame(sc.def1A)

d2 = dist(sc.def1A[,3:9])

agrup2 <- hclust(d2, method = "ward.D2")
plot(agrup2, xlab = "",ylab = "", hang = -1, cex = 0.6)


library(mclust)
fit <- Mclust(sc.atq1A[,2:10])
plot(fit) # plot results
summary(fit)

library(factoextra)
library(NbClust)
fviz_nbclust(sc.atq1A[,2:9], kmeans, method = "wss")+
  labs(x = "Número de grupos", y = "Soma de Quadrados Dentro total", title = "")

fviz_nbclust(sc.atq1A[,2:9], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


agrup4 <- kmeans(sc.atq1A[,2:9], centers = 6)
agrup4
agrup4$cluster
