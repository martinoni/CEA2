library(sp)
library(adehabitatHR)
library(scales)
library(png)
library(foreach)
jogo1ime <- read.delim("C:/Users/lbortolucci/Downloads/Nova pasta/CEA2/jogo1ime.txt", header=TRUE)
jogo1ime <- na.omit(jogo1ime)

setwd('C:/Users/lbortolucci/Downloads/Nova pasta/CEA2/imagens')
ima <- readPNG('background_campo.png')


for(i in 461:462){
  jogosub <- jogo1ime[i,]
  jogoimp <- jogosub[,9:52]
  jogoimp <- data.frame(jogosub$frames,jogoimp)
  colnames(jogoimp)[1] <- "frame"
  
  noacao <- c()
  equipe <- c()
  pos.x <- c()
  pos.y <- c()
  pos.xA <- c()
  pos.yA <- c()
  pos.xB <- c()
  pos.yB <- c()
  
  for(j in 2:11){
    pos.xA <- c(pos.xA,jogoimp[1,2*j])
    pos.yA <- c(pos.yA,jogoimp[1,(2*j)+1])
  }
  for(j in 13:22){
    pos.xB <- c(pos.xB,jogoimp[1,2*j])
    pos.yB <- c(pos.yB,jogoimp[1,(2*j)+1])
  }
  pos.x <- c(min(pos.xA),min(pos.xA),max(pos.xA),max(pos.xA),mean(pos.xA),
             min(pos.xB),min(pos.xB),max(pos.xB),max(pos.xB),mean(pos.xB))
  pos.y <- c(min(pos.yA),max(pos.yA),max(pos.yA),min(pos.yA),mean(pos.yA),
             min(pos.yB),max(pos.yB),max(pos.yB),min(pos.yB),mean(pos.yB))
  noacao <- c(rep("A",5),rep("B",5))
  jogoimp.coor <- data.frame(noacao,pos.x,pos.y)
  
  coordinates(jogoimp.coor) <- c("pos.x", "pos.y")
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100)
  
  if(jogosub$posse == 'EA'){
    col = c(rep(c('red', 'black'), each=11))
    col2 = c('red', 'black')
  } else{
    col = c(rep(c('black', 'red'), each=11))
    col2 = c('black', 'red')
  }
  xs <- as.numeric(jogosub[,seq(9, 51, 2)])
  ys <- as.numeric(jogosub[,seq(10, 52, 2)])
  
  png(sprintf('campo_%06d.png', jogosub$frames), width = 630, height = 408)
  plot.new()
  lim <- par()
  rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  par(new=TRUE)
  plot(ys~xs,
       xlim = c(0, 105), ylim = c(0, 68),
       pch = c(rep(c(15, 17), each=11), 16),
       col = col)
  plot(jogo.mcp, col = alpha(col2, 0.5), add = TRUE)
  dev.off()
}
setwd("~")
