library(magrittr)
library(sp)
library(adehabitatHR)
library(scales)
library(png)
library(foreach)
library(plotly)

jogo1ime <- read.delim("~/Development/IME/cea2/jogo2acao.txt", header=TRUE)
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
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100)
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}


areas <- cbind(area.EA, area.EB) %>% as.data.frame()
colnames(areas) <- c('AA', 'AB')

jogo1ime <- cbind(jogo1ime, areas)
jogo1ime <- cbind(1:nrow(jogo1ime), jogo1ime)
colnames(jogo1ime) <- c('tempo', colnames(jogo1ime)[2:ncol(jogo1ime)])

attach(jogo1ime)





jogo1ime_escalado <- jogo1ime[1:100,]
jogo1ime_escalado$ataque <- as.numeric(jogo1ime_escalado$ataque)
attach(jogo1ime_escalado)

# for(i in 1:nrow(jogo1ime_escalado)){
#   jogo1ime_escalado$ataque[i] <- sprintf('%s_%s', 
#                                          jogo1ime_escalado$ataque[i],
#                                          jogo1ime_escalado$posse[i])
# }
ataque_posseEA <- c(1)
ataque_posseEB <- c(1)
for(i in 2:nrow(jogo1ime_escalado)){
  if(posse[i] == 'EA'){
    ataque_posseEA <- c(ataque_posseEA, ataque[i])
    ataque_posseEB <- c(ataque_posseEB, ataque_posseEB[i-1])
  } else if(posse[i] == 'EB'){
    ataque_posseEA <- c(ataque_posseEA, ataque_posseEA[i-1])
    ataque_posseEB <- c(ataque_posseEB, ataque[i])}
}
jogo1ime_escalado <- cbind(jogo1ime_escalado, ataque_posseEA, ataque_posseEB)


plot(AA~tempo, xlim = c(0, nrow(jogo1ime_escalado)), 
            ylim = c(0.02464802, 0.2522278), data = jogo1ime[1,], 
     col = 'red', lty=0, type = 'l')

for(tempo in jogo1ime_escalado$tempo[2:nrow(jogo1ime_escalado)]){

  if(jogo1ime_escalado[tempo,]$posse == 'EA'){
    linha_EA <- 1
    linha_EB <- 2
  } else{
    linha_EA <- 2
    linha_EB <- 1
  }
  
  lines(AA~tempo, data=jogo1ime_escalado[c(tempo, tempo-1),], col='red',
        lty = linha_EA)
  lines(AB~tempo, data=jogo1ime_escalado[c(tempo, tempo-1),], col='blue',
        lty = linha_EB)
  
  if(ataque_posseEA[tempo] != ataque_posseEA[tempo-1]){
    abline(v=tempo, lty = 3, col = 'red')
    text(x=tempo, y=0.02464803, labels = desf_ataque[tempo], col='red')
  }
  if(ataque_posseEB[tempo] != ataque_posseEB[tempo-1]){
    abline(v=tempo, lty = 3, col = 'blue')
    text(x=tempo, y=0.02464803, labels = desf_ataque[tempo], col='blue')
  }
}

legend(0, 0.25, col = 'red', legend = 'TIME A', box.col = 'red', border = 'red', fill = 'red', cex = 0.7)
legend(0, 0.225, col = 'blue', legend = 'TIME B', box.col = 'blue', border = 'blue', fill = 'blue', cex = 0.7)

