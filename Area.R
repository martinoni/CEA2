library(magrittr)
library(sp)
library(adehabitatHR)
library(scales)
library(png)
library(foreach)
library(plotly)

# ver correlação entre centro de gravidade e area
# fazer o grafico do centroide ao longo do jogo
# estudar a distancia do centro de gravidade e o gol
# substituir o valor do centroide pelo centroide SEM CONTAR O GOLEIRO
# distancia da bola pro gol
# 

jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE)
jogo1ime <- na.omit(jogo1ime)

setwd('~/Development/IME/cea2/')
ima <- readPNG('./campo/background_campo.png')

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
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100, 'm', 'm2')
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}

areas <- cbind(area.EA, area.EB) %>% as.data.frame()
colnames(areas) <- c('AA', 'AB')

jogo1ime <- cbind(jogo1ime, areas)
jogo1ime <- cbind(1:nrow(jogo1ime), jogo1ime)
colnames(jogo1ime) <- c('tempo', colnames(jogo1ime)[2:ncol(jogo1ime)])

attach(jogo1ime)





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
jogo1ime_escalado <- cbind(jogo1ime_escalado, ataque_posseEA, ataque_posseEB)









plot(AA~tempo, xlim = c(0, nrow(jogo1ime_escalado)), data = jogo1ime[1,], 
     col = 'red', lty=0, type = 'l', ylab = 'Área do Polígono',
     xlab = 'Tempo', ylim = c(400, 2000))
tempo <- 1
abline(v=tempo, lty = 3, col = 'red')
text(x=tempo, y=2000, labels = sprintf('A%s',
                                            ataque[tempo]), col='red', cex=.8)
linha_EA <- 1
linha_EB <- 1

for(tempo in jogo1ime_escalado$tempo[2:nrow(jogo1ime_escalado)]){
  
  if(ataque_posseEA[tempo] != ataque_posseEA[tempo-1]){
    abline(v=tempo, lty = 3, col = 'red')
    text(x=tempo, y=450, labels = sprintf('d%s',
                                                 desf_ataque[tempo]), col='blue',cex=.8)
    text(x=tempo, y=2000, labels = sprintf('A%s',
                                                ataque[tempo]), col='red', cex=.8)
  }
  
  if(ataque_posseEB[tempo] != ataque_posseEB[tempo-1]){
    abline(v=tempo, lty = 3, col = 'blue')
    text(x=tempo, y=450, labels = sprintf('d%s',
                                                 desf_ataque[tempo]), col='red',cex=.8)
    text(x=tempo, y=2000, labels = sprintf('B%s',
                                                ataque[tempo]), col='blue', cex=.8)
  }
  
  lines(AA~tempo, data=jogo1ime_escalado[c(tempo-1, tempo-2),], col='red',
        lty = linha_EA)
  lines(AB~tempo, data=jogo1ime_escalado[c(tempo-1, tempo-2),], col='blue',
        lty = linha_EB)
}

legend(0, 1975, col = 'red', legend = 'TIME A', box.col = 'red', border = 'red', fill = 'red', cex = 0.7)
legend(0, 1875, col = 'blue', legend = 'TIME B', box.col = 'blue', border = 'blue', fill = 'blue', cex = 0.7)


