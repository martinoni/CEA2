setwd('~/Development/IME/cea2')
library(magrittr)
library(stringr)

jogo1ime <- read.delim("~/Development/IME/cea2/jogo1ime_completo.txt")
nomes <- colnames(jogo1ime)
jogo2ime <- read.delim("~/Development/IME/cea2/jogo2ime.txt")
jogo3ime <- read.delim("~/Development/IME/cea2/jogo3ime.txt")
jogo4ime <- read.delim("~/Development/IME/cea2/jogo4ime.txt")
jogo5ime <- read.delim("~/Development/IME/cea2/jogo5ime.txt")
colnames(jogo1ime) <- 1:80
colnames(jogo2ime) <- 1:80
colnames(jogo3ime) <- 1:80
colnames(jogo4ime) <- 1:80
colnames(jogo5ime) <- 1:80

jogoime_completo <- rbind(jogo1ime, jogo2ime, jogo3ime, jogo4ime, jogo5ime)
colnames(jogoime_completo) <- nomes
attach(jogoime_completo)

base_nova <- data.frame(as.factor(posse))
base_nova <- cbind(base_nova, jogoime_completo[,9:80])

for(i in seq(9, 52, 2)){
  jogador_x <- jogoime_completo[,i]
  jogador_y <- jogoime_completo[,i+1]
  dist_jogador_bola <- sqr(t(bolay-jogador_y)^2 + (bolax-jogador_x)^2)

  base_nova <- cbind(base_nova, dist_jogador_bola) 
  colnames(base_nova) <- c(colnames(base_nova)[1:length(base_nova)-1], 
                           sprintf('%s_bola',colnames(jogoime_completo)[i] %>% 
                                     str_replace('x', '')))
}

colnames(base_nova) <- c('posse', colnames(base_nova[2:length(base_nova)]))

#base_nova <- base_nova[, 2:length(base_nova)]


write.csv(base_nova, 'base_nova.txt')

