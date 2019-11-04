library("factoextra")
jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE)
jogo1ime.active <- jogo1ime[, 7:50]
res.pca <- prcomp(jogo1ime.active, scale = TRUE)

sum(is.na(jogo1ime.active))

jogo1ime.active[is.na(jogo1ime.active)] <- 0

fviz_eig(res.pca)

library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(22,'Spectral')))
r <- rf(11)
library(ggplot2)
attach(jogo1ime)
library(magrittr)
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
jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE) #%>% 
#rbind(read.delim("~/Development/IME/cea2/jogo2acao.txt", header=TRUE)) %>% 
#rbind(read.delim("~/Development/IME/cea2/jogo3acao.txt", header=TRUE)) %>% 
#rbind(read.delim("~/Development/IME/cea2/jogo4acao.txt", header=TRUE))

x <- jogo1ime[, seq(8,51,2)] %>% 
  unlist()
y <- jogo1ime[, seq(9,51,2)] %>% 
  unlist()

qplot(x, y, geom='bin2d',
      xlim=c(0, 105), ylim=c(0, 68)) +
  scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
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



#############CLUSTERS####################################################################

# SOURCE EM PCA_por_grupos.R na parte abaixo de #Aqui começa o trampo que o fossa o borto e o LG pediram

posicoes_clusters <- list()

n_clusts <- clusters %>% 
  levels %>% 
  length()


# PARA O TIME A = 9:30
# PARA O TIME B = 31:52
for(k in 1:n_clusts){
  posicoes_em_questao <-  ataquesA_defesasB[clusters==k, 9:30]
  posicoes_clusters <- append(posicoes_clusters, list(posicoes_em_questao))
}



library(RColorBrewer)
library("factoextra")
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(11)
library(ggplot2)
library(magrittr)
library(png)



for(k in 1:n_clusts){
  for(l in 1:11){
    xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
    ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
    
    #Jogador l
    x <- xs[, l]
    y <- ys[, l]
    
    qplot(x, y, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
          xlim=c(0, 105), ylim=c(0, 68)) +
      scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
      xlab('X') +
      ylab('Y')
    ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor_clusters/j%s_g%s.png', l, k))
  }
}
