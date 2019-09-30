library("factoextra")
setwd('~/Development/IME/cea2/')
jogo1ime <- read.delim("jogo1ime.txt", header=TRUE)
jogo1ime.active <- jogo1ime[, 7:50]
res.pca <- prcomp(jogo1ime.active, scale = TRUE)

sum(is.na(jogo1ime.active))

jogo1ime.active[is.na(jogo1ime.active)] <- 0

fviz_eig(res.pca)

library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)
library(ggplot2)
attach(jogo1ime)
library(magrittr)
library(png)
for(i in seq(7, ncol(jogo1ime)-1, by = 2)){
  print(sprintf('Jogadores %s e %s', 
          toString(colnames(jogo1ime)[i]), 
          toString(colnames(jogo1ime)[i+1])))
  par(ask=TRUE)
  p <- qplot(jogo1ime[, i], jogo1ime[, i+1],data=jogo1ime, geom='bin2d',
        xlim=c(0, 105), ylim=c(0, 68), 
        xlab = 'x', ylab = 'y', main = sprintf('Jogador %s', 
                                               toString(colnames(jogo1ime)[i]) %>% 
                                                 substr(1, nchar(.)-1))) +
    scale_fill_gradientn(colours=r, trans="log")
  
  plot(p)
}
