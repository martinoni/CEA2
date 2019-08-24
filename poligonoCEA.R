library(sp)
library(adehabitatHR)
library(scales)
library(png)
library(foreach)
jogo1ime <- read.delim("C:/Users/lucas/Downloads/jogo1ime.txt", header=TRUE)
jogo1ime <- na.omit(jogo1ime)

setwd('C:/Users/lucas/Downloads/R descr/2019-2/Cea2/imagens')
ima <- readPNG('background_campo.png')


for(i in 1:nrow(jogo1ime)){
  jogosub <- jogo1ime[i,]
  jogoimp <- jogosub[,7:50]
  jogoimp <- data.frame(jogosub$noacao,jogoimp)
  colnames(jogoimp)[1] <- "noacao"
  
  noacao <- c()
  equipe <- c()
  pos.x <- c()
  pos.y <- c()
  
  for(j in 1:11){
    noacao <- c(noacao,paste0(jogoimp$noacao,"A"))
    pos.x <- c(pos.x,jogoimp[1,2*j])
    pos.y <- c(pos.y,jogoimp[1,(2*j)+1])
  }
  for(j in 12:22){
    noacao <- c(noacao,paste0(jogoimp$noacao,"B"))
    pos.x <- c(pos.x,jogoimp[1,2*j])
    pos.y <- c(pos.y,jogoimp[1,(2*j)+1])
  }
  
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
  xs <- as.numeric(jogosub[,seq(7, 50, 2)])
  ys <- as.numeric(jogosub[,seq(8, 50, 2)])
  
  png(sprintf('campo_%06d.png', jogosub$noacao), width = 630, height = 408)
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
?alpha
