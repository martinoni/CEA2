setwd('~/Development/IME/cea2')
library(magrittr)
library(stringr)

jogo1ime_completo <- read.delim("~/Development/IME/cea2/jogo1ime_completo.txt")
attach(jogo1ime_completo)

base_nova <- data.frame(as.factor(posse))
base_nova <- cbind(base_nova, jogo1ime_completo[,9:80])

for(i in seq(9, 52, 2)){
  jogador_x <- jogo1ime_completo[,i]
  jogador_y <- jogo1ime_completo[,i+1]
  dist_jogador_bola <- (bolay-jogador_y)^2 + (bolax-jogador_x)^2 %>% sqrt() 

  base_nova <- cbind(base_nova, dist_jogador_bola) 
  colnames(base_nova) <- c(colnames(base_nova)[1:length(base_nova)-1], 
                           sprintf('%s_bola',colnames(jogo1ime_completo)[i] %>% 
                                     str_replace('x', '')))
}

colnames(base_nova) <- c('posse', colnames(base_nova[2:length(base_nova)]))

#base_nova <- base_nova[, 2:length(base_nova)]


write.csv(base_nova, 'base_nova.txt')
