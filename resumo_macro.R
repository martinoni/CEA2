library(sp)
library(adehabitatHR)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("leitura_dados_acao.R")
setwd("~")

atq1.A <- c()
def1.B <- c()
for (i in 1:jogo1.A$ataque[nrow(jogo1.A)]){
  atq <- subset(jogo1.A, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centAx <- mean(atq$ncentEAx)
  centAy <- mean(atq$ncentEAy)
  centBx <- mean(atq$ncentEBx)
  centBy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.A <- mean(atq$area.EA)
  area.B <- mean(atq$area.EB)
  des.centA <- 0
  des.centB <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centA <- des.centA + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centB <- des.centB + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAx,centAy,bolax,bolay,des.centA,des.bola,area.A)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centBx,centBy,bolax,bolay,des.centB,des.bola,area.B)
  atq1.A <- rbind(atq1.A,atq.atual)
  def1.B <- rbind(def1.B,def.atual)
}

atq1.B <- c()
def1.A <- c()
for (i in 1:jogo1.B$ataque[nrow(jogo1.B)]){
  atq <- subset(jogo1.B, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centAx <- mean(atq$ncentEAx)
  centAy <- mean(atq$ncentEAy)
  centBx <- mean(atq$ncentEBx)
  centBy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.A <- mean(atq$area.EA)
  area.B <- mean(atq$area.EB)
  des.centA <- 0
  des.centB <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centA <- des.centA + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centB <- des.centB + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAx,centAy,bolax,bolay,des.centA,des.bola,area.A)
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centBx,centBy,bolax,bolay,des.centB,des.bola,area.B)
  atq1.B <- rbind(atq1.B,atq.atual)
  def1.A <- rbind(def1.A,def.atual)
}


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