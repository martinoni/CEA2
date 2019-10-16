library(sp)
library(adehabitatHR)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("leitura_dados_acao.R")
source("resumo_macro.R")
source("cod_agrup.R")
setwd("~")

pairs(atq1.A[,c(4:11)], pch=16, col="navyblue", cex=0.6)
pairs(atq1.B[,2:11], pch=16, col="red4", cex=0.6)

c <- cor(atq1.A)
View(c)

plot(agrup.ata1A, xlab = "",ylab = "", hang = -1, cex = 0.6,main = "Dendograma Atq Time A - Jogo 1 
     (sem pos bola)")

clust <- cutree(agrup1,7)

atq1.A$clust <- clust
atq1.A <- cbind(atq1.A,clust)

plot(clust, type = "l")

m.atq1A <- aggregate(atq1.A[, 2:11], list(atq1.A$clust), mean)
v.atq1A <- aggregate(atq1.A[, 2:11], list(atq1.A$clust), sd)

library(cluster)
s.atq1A <- silhouette(clust,d)
s.atq1A2 <- silhouette(clust,d)
summary(s.atq1A2)



plot(agrup.def1B, xlab = "",ylab = "", hang = -1, cex = 0.6,main = "Dendograma Def Time B - Jogo 1 
     (sem ac/temp e pos bola)")

clust2 <- cutree(agrup2,4)


def1.B <- cbind(def1.B,clust2)
m.def1B <- aggregate(def1.B[, 2:11], list(def1.B$clust2), mean)
v.def1B <- aggregate(def1.B[, 2:11], list(def1.B$clust2), sd)

s.def1B <- silhouette(clust2,d2)
summary(s.def1B)

compar1.A <- data.frame(atq1.A$ataque,clust,clust2)
colnames(compar1.A) <- c("ataque","Atq","Def")
compar1.A$Atq <- as.factor(compar1.A$Atq)
compar1.A$Def <- as.factor(compar1.A$Def)

teste1 <- xtabs(~Atq+Def, compar1.A)

teste1


plot(agrup.atq1B, xlab = "",ylab = "", hang = -1, cex = 0.6)



plot(agrup.def1A, xlab = "",ylab = "", hang = -1, cex = 0.6)


