library("factoextra")
jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE)
jogo1ime.active <- jogo1ime[, 7:50]
res.pca <- prcomp(jogo1ime.active, scale = TRUE)

sum(is.na(jogo1ime.active))

jogo1ime.active[is.na(jogo1ime.active)] <- 0

fviz_eig(res.pca)

library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(22,'Spectral')))
r <- rf(32)
library(ggplot2)
attach(jogo1ime)
library(png)
devAskNewPage(FALSE)
for(i in seq(8, ncol(jogo1ime)-1, by = 2)){
  print(sprintf('Jogadores %s e %s', 
                toString(colnames(jogo1ime)[i]), 
                toString(colnames(jogo1ime)[i+1])))
  devAskNewPage(options("device.ask.default")[[1]])
  qplot(jogo1ime[, i], jogo1ime[, i+1],data=jogo1ime, geom='bin2d',
        xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y')  
}

h <- hexbin(df)
plot(h, colramp=rf)

library(magrittr)
jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE) %>% 
  rbind(read.delim("~/Development/IME/cea2/jogo2acao.txt", header=TRUE)) %>% 
  rbind(read.delim("~/Development/IME/cea2/jogo3acao.txt", header=TRUE)) %>% 
  rbind(read.delim("~/Development/IME/cea2/jogo4acao.txt", header=TRUE))
  
x <- jogo1ime[, seq(8,51,2)] %>% 
  unlist()
y <- jogo1ime[, seq(9,51,2)] %>% 
  unlist()

qplot(x, y, geom='bin2d',
      xlim=c(0, 105), ylim=c(0, 68)) +
  scale_fill_gradientn(colours=r, trans="probability", name = 'Frequência') +
  xlab('X') +
  ylab('Y')  


for(i in seq(7, ncol(jogo1ime)-1, by = 2)){
  print(sprintf('Jogadores %s e %s', 
                toString(colnames(jogo1ime)[i]), 
                toString(colnames(jogo1ime)[i+1])))
  x_c <- cut(jogo1ime[, i], seq(0, 105, 3))
  y_c <- cut(jogo1ime[, i+1], seq(0, 68, 3))
  z <- table(x_c, y_c)
  hist3D(z=z, border="black", xlim=c(0, 105),
         main = jogador, ylim = c(0,68))
  readline(prompt="Press [enter] to continue")
}



library(plot3D)
x_c <- cut(jogo1ime$EA1x, seq(0, 105, 2))
y_c <- cut(jogo1ime$EA1y, seq(0, 68, 2))
z <- table(x_c, y_c)
hist3D(z=z)





# 
# for(i in seq(7, ncol(jogo1ime)-1, by = 2)){
#   print(sprintf('Jogadores %s e %s',
#                 toString(colnames(jogo1ime)[i]),
#                 toString(colnames(jogo1ime)[i+1])))
#   jogador <- jogo1ime[i] %>%
#     colnames() %>%
#     toString() %>%
#     str_replace('E','') %>%
#     str_replace('x', '') %>%
#     sprintf('Jogador %s', .)
#   x_c <- cut(jogo1ime[, i], seq(0, 105, 3))
#   y_c <- cut(jogo1ime[, i+1], seq(0, 68, 3))
#   z <- table(x_c, y_c)
#   hist3Drgl(z=z, border="black", main = jogador)
#   rglwidget()
#   rglwidgetOutput('grafico', height = "512px", width = "1024px")
#   readline(prompt="Press [enter] to continue")
#   rgl.close()
# }

