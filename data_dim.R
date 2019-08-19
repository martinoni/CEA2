setwd('~/Development/IME/cea2/')
library(magrittr)

#jogo1ime <- read.delim("~/Documentos/jogo1ime_completo.txt", header=TRUE)
jogo1ime <- read.csv('base_nova.txt', header = TRUE)
attach(jogo1ime)

data_diminuida <- jogo1ime[seq(1, nrow(jogo1ime)-1, 2), 2:length(jogo1ime)]
data_diminuida[is.na(data_diminuida)] <- 0
write.csv(data_diminuida, 'data_dim_nova.csv')