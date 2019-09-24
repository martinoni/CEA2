library(sp)
library(adehabitatHR)
library(scales)
library(png)
library(foreach)

jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE)
jogo1ime <- na.omit(jogo1ime)

setwd('~/Development/IME/cea2/')
ima <- readPNG('./campo/campo2.png')

area.EA <- c()
area.EB <- c()
for(i in 1:1093){
  jogosub <- jogo1ime[i,]
  jogoimp <- jogosub[,8:53]
  
  noacao <- c()
  equipe <- c()
  pos.xA <- c()
  pos.yA <- c()
  pos.xB <- c()
  pos.yB <- c()
  
  for(j in 2:11){
    noacao <- c(noacao,paste0(i,"A"))
    pos.xA <- c(pos.xA,jogoimp[1,(2*j)-1])
    pos.yA <- c(pos.yA,jogoimp[1,(2*j)])
  }
  for(j in 13:22){
    noacao <- c(noacao,paste0(i,"B"))
    pos.xB <- c(pos.xB,jogoimp[1,(2*j)-1])
    pos.yB <- c(pos.yB,jogoimp[1,(2*j)])
  }
  pos.x <- c(min(pos.xA),min(pos.xA),max(pos.xA),max(pos.xA),mean(pos.xA),
             min(pos.xB),min(pos.xB),max(pos.xB),max(pos.xB),mean(pos.xB))
  pos.y <- c(min(pos.yA),max(pos.yA),max(pos.yA),min(pos.yA),mean(pos.yA),
             min(pos.yB),max(pos.yB),max(pos.yB),min(pos.yB),mean(pos.yB))
  noacao <- c(rep("A",5),rep("B",5))
  jogoimp.coor <- data.frame(noacao,pos.x,pos.y)
  
  coordinates(jogoimp.coor) <- c("pos.x", "pos.y")
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100, 'm', 'm2')
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
  
  if(jogosub$posse == 'EA'){
    col = c(rep(c('red', 'midnightblue'), each=11), 'black')
    col2 = c('red', 'midnightblue')
  } else{
    col = c(rep(c('red4', 'blue'), each=11), 'black')
    col2 = c('red4', 'blue')
  }
  xs <- as.numeric(jogosub[,seq(8, 53, 2)])
  ys <- as.numeric(jogosub[,seq(9, 53, 2)])
  
  png(sprintf('./campo/imagens/campo_%06d.png', i), width = 630, height = 408)
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




jogo1ime_escalado <- jogo1ime[1:50,]
jogo1ime_escalado$ataque <- as.numeric(jogo1ime_escalado$ataque)
attach(jogo1ime_escalado)


ataque_posseEA <- c(1)
ataque_posseEB <- c(1)
for(i in 2:nrow(jogo1ime_escalado)){
  if(posse[i] == 'EA'){
    ataque_posseEA <- c(ataque_posseEA, ataque[i])
    ataque_posseEB <- c(ataque_posseEB, ataque_posseEB[i-1])
  } else if(posse[i] == 'EB'){
    ataque_posseEA <- c(ataque_posseEA, ataque_posseEA[i-1])
    ataque_posseEB <- c(ataque_posseEB, ataque[i])
  } 
}

ataque_posseEB[1:6] <- 0
jogo1ime_escalado <- cbind(jogo1ime_escalado, ataque_posseEA[1:50], ataque_posseEB[1:50])
jogo1ime_escalado <- cbind(jogo1ime_escalado, area.EA[1:50], area.EB[1:50])
colnames(jogo1ime_escalado) <- c(colnames(jogo1ime_escalado)[1:81], 'AA', 'AB')
tempo <- 1:50
jogo1ime_escalado <- cbind(jogo1ime_escalado, tempo)
jogo1ime <- jogo1ime_escalado
attach(jogo1ime)

plot(AA~tempo, xlim = c(0, nrow(jogo1ime_escalado)), 
     ylim = c(500, 2800), data = jogo1ime[1,], 
     col = 'red', lty=0, type = 'l', ylab = 'Área do Retângulo',
     xlab = 'Tempo')
tempo <- 1
abline(v=tempo, lty = 3, col = 'red')
text(x=tempo, y=2775, labels = sprintf('A%s',
                                        ataque[tempo]), col='red', cex=.8)
linha_EA <- 1
linha_EB <- 1

for(tempo in jogo1ime_escalado$tempo[2:nrow(jogo1ime_escalado)]){
  
  if(ataque_posseEA[tempo] != ataque_posseEA[tempo-1]){
    abline(v=tempo, lty = 3, col = 'red')
    text(x=tempo, y=525, labels = sprintf('d%s',
                                                 desf_ataque[tempo]), col='blue',cex=.8)
    text(x=tempo, y=2775, labels = sprintf('A%s',
                                            ataque[tempo]), col='red', cex=.8)
  }
  
  if(ataque_posseEB[tempo] != ataque_posseEB[tempo-1]){
    abline(v=tempo, lty = 3, col = 'blue')
    text(x=tempo, y=525, labels = sprintf('d%s',
                                                 desf_ataque[tempo]), col='red',cex=.8)
    text(x=tempo, y=2775, labels = sprintf('B%s',
                                            ataque[tempo]), col='blue', cex=.8)
  }
  
  lines(AA~tempo, data=jogo1ime_escalado[c(tempo-1, tempo-2),], col='red',
        lty = linha_EA)
  lines(AB~tempo, data=jogo1ime_escalado[c(tempo-1, tempo-2),], col='blue',
        lty = linha_EB)
}

legend(0, 2700, col = 'red', legend = 'TIME A', box.col = 'red', border = 'red', fill = 'red', cex = 0.7)
legend(0, 2550, col = 'blue', legend = 'TIME B', box.col = 'blue', border = 'blue', fill = 'blue', cex = 0.7)

