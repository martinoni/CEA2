#Componentes principais?
library(magrittr)
library(shiny)

jogo1ime <- read.delim("~/Documentos/jogo1ime.txt", header=TRUE)
attach(jogo1ime)

jogadores = jogo1ime[, 7:50]

#completamento das caselas NA (rodar varias vezes kkk):
for(i in 1:length(jogadores[is.na(jogadores)])){
  m = which(is.na(jogadores))[i]
  n = m%%nrow(jogadores)
  m = 1 + m%/%nrow(jogadores)
  
  print(sprintf('[%s, %s]:', n, m))
  print(i)
  if(!is.na(m) && !is.na(n)) jogadores[n, m] <- jogadores[n-1, m]
}

pca <- princomp(jogadores)

loadings <- pca$loadings
