library(sp)
library(adehabitatHR)
library(scales)
library(png)
library(foreach)
library(doParallel)

jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE)
jogo1ime <- na.omit(jogo1ime)

setwd('~/Development/IME/cea2/campo/')
ima <- readPNG('campo2.png')

cl <- makeCluster(6)
registerDoParallel(cl)


foreach(i = 1:nrow(jogo1ime), .packages = c('magrittr', 'sp', 
                                            'adehabitatHR',
                                            'scales')) %dopar% {
  jogosub <- jogo1ime[i,]
  jogoimp <- jogosub[,8:53]
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
  
  if(jogosub$posse == 'EA'){
    col = c(rep(c('red', 'midnightblue'), each=11), 'black')
    col2 = c('red', 'midnightblue')
  } else{
    col = c(rep(c('red4', 'blue'), each=11), 'black')
    col2 = c('red4', 'blue')
  }
  xs <- as.numeric(jogosub[,seq(8, 53, 2)])
  ys <- as.numeric(jogosub[,seq(9, 53, 2)])
  
  png(sprintf('./imagens/campo_%06d.png', i), width = 630, height = 408)
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

stopCluster(cl)

system('ffmpeg -framerate 5 -i ~/Development/IME/cea2/campo/imagens/campo_%06d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p ~/Development/IME/cea2/campo/campo_poligonos.mp4 -y')
system('rm /home/thiago/Development/IME/cea2/campo/imagens/*')


