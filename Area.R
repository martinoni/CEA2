library(sp)
library(adehabitatHR)
library(scales)
library(png)
library(foreach)

jogo1ime <- read.delim("C:/Users/lucas/Downloads/R descr/2019-2/Cea2/jogo2acao.txt", header=TRUE)
jogo1ime <- na.omit(jogo1ime)

setwd('C:/Users/lucas/Downloads/R descr/2019-2/Cea2/imagens')
ima <- readPNG('background_campo.png')

area.EA <- c()
area.EB <- c()
for(i in 1:nrow(jogo1ime)){
  jogosub <- jogo1ime[i,]
  jogoimp <- jogosub[,8:51]
  jogoimp <- data.frame(jogosub$noacao,jogoimp)
  colnames(jogoimp)[1] <- "noacao"
  
  noacao <- c()
  equipe <- c()
  pos.x <- c()
  pos.y <- c()
  
  for(j in 2:11){
    noacao <- c(noacao,paste0(jogoimp$noacao,"A"))
    pos.x <- c(pos.x,jogoimp[1,2*j])
    pos.y <- c(pos.y,jogoimp[1,(2*j)+1])
  }
  for(j in 13:22){
    noacao <- c(noacao,paste0(jogoimp$noacao,"B"))
    pos.x <- c(pos.x,jogoimp[1,2*j])
    pos.y <- c(pos.y,jogoimp[1,(2*j)+1])
  }
  
  jogoimp.coor <- data.frame(noacao,pos.x,pos.y)
  
  coordinates(jogoimp.coor) <- c("pos.x", "pos.y")
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100)
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}
