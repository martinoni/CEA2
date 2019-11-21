library("factoextra")
jogo1ime <- read.delim("~/Development/IME/cea2/CEA2/jogo1acao.txt", header=TRUE)
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


library(RColorBrewer)
library("factoextra")
rf <- colorRampPalette(rev(brewer.pal(8,'Spectral')))
r <- rf(8)
library(ggplot2)
library(magrittr)
library(png)


# #### ANALISE INDIVIDUAL ##################
# for(k in 1:n_clusts){
#   for(l in 1:11){
#     xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
#     ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
#     
#     #Jogador l
#     x <- xs[, l]
#     y <- ys[, l]
#     
#     qplot(x, y, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
#           xlim=c(0, 105), ylim=c(0, 68)) +
#       scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
#       xlab('X') +
#       ylab('Y')
#     ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor_clusters/jogo%s/j%s_ataque_A/j%s_g%s.png',
#                    njogo, njogo, l, k),
#            width = 6.17, height = 4)
#   }
# }



####################COW PLOOOTTT###############################

# criaçao das pastas
for(njogo in 1:5){
  system(sprintf('mkdir ~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor_clusters/jogo%s/', njogo))
  for(tipo in c('ataque', 'defesa')){
    for(equipekk in c('A', 'B')){
      system(sprintf('mkdir ~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor_clusters/jogo%s/j%s_%s_%s', 
                     njogo, njogo, tipo, equipekk))
    }
  }
}



# Tentar fazer o cowplot

#MUDAR AQUI O NUMERO DO JOGO PRA TODAS ESSAS VARIAVEIS
n2_jogo <- 3
cl_atqA <- cl_atq3A
cl_atqB <- cl_atq3B
cl_defA <- cl_def3A
cl_defB <- cl_def3B

#Tratamento do jogo k (escolher qual dos 5 jogos):
# ATENCAO: ALGUMAS COISAS MUDAM PRO JOGO 1 (PRINCIPALMENTE NA PARTE FORA DO COWPLOT)
jogo <- read.delim(sprintf("~/Development/IME/cea2/CEA2/jogo%sime_completo.txt", n2_jogo))
ataque_posse <- paste0(jogo$ataque, '_', jogo$posse)
clust_ataqueA <- c()
clust_ataqueB <- c()
clust_defA <- c()
clust_defB <- c()
for (i in 1:nrow(jogo)){
  if(grepl('_EA', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- cl_atqA[1:length(cl_atqA) == ataque_em_questao]
    clust_ataqueA <- c(clust_ataqueA, cluster_em_questao)
    
    cluster_em_questao <- cl_defB[1:length(cl_defB) == ataque_em_questao]
    clust_defB <- c(clust_defB, cluster_em_questao)
  } else if(grepl('_EB', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- cl_atqB[1:length(cl_atqB) == ataque_em_questao]
    clust_ataqueB <- c(clust_ataqueB, cluster_em_questao)
    
    cluster_em_questao <- cl_defA[1:length(cl_defA) == ataque_em_questao]
    clust_defA <- c(clust_defA, cluster_em_questao)
  }
}

clust_ataqueA <- as.factor(clust_ataqueA)
clust_ataqueB <-  as.factor(clust_ataqueB)
clust_defA <- as.factor(clust_defA)
clust_defB <- as.factor(clust_defB)

library(stringi)
library(magrittr)
ataquesA_defesasB <- jogo[stri_detect_fixed(ataque_posse, '_EA'),] %>% 
  cbind(clust_ataqueA) %>% 
  cbind(clust_defB)


#aqui eu vou filtrar a prte da base de dados que só tem o time A pro ataque do time A, por exemplo (e assim vai)
ataquesA_defesasB <- ataquesA_defesasB[, c(1:8, which(stri_detect_fixed(colnames(ataquesA_defesasB), 'EA'))[1:22], 81)]


for(coluna in colnames(ataquesA_defesasB)){
  ataquesA_defesasB[[coluna]] <- as.numeric(as.character(ataquesA_defesasB[[coluna]]))
}

# Mudar aqui se eh ataque ou defesa pro time A ou pro time B
library(psych)
library(BBmisc)
clusters <- clust_ataqueA %>% as.character() %>% as.numeric()
ataquesA_defesasB$clust_ataqueA <- clusters
n_clusts <- clust_ataqueA %>% levels() %>% length()


posicoes_clusters <- list()


# PARA O TIME A = 9:30
# PARA O TIME B = 31:52
for(k in 1:n_clusts){
  posicoes_em_questao <-  ataquesA_defesasB[clusters==k, 9:30]
  posicoes_clusters <- append(posicoes_clusters, list(posicoes_em_questao))
}


equipe2d <- 'A'
library(cowplot)
for(k in 1:n_clusts){
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 1
  x1 <- xs[, l]
  y1 <- ys[, l]
  
  q1 <- qplot(x1, y1, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 2
  x2 <- xs[, l]
  y2 <- ys[, l]
  
  q2 <- qplot(x2, y2, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 3
  x3 <- xs[, l]
  y3 <- ys[, l]
  
  q3 <- qplot(x3, y3, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 4
  x4 <- xs[, l]
  y4 <- ys[, l]
  
  q4 <- qplot(x4, y4, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 5
  x5 <- xs[, l]
  y5 <- ys[, l]
  
  q5 <- qplot(x5, y5, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 6
  x6 <- xs[, l]
  y6 <- ys[, l]
  
  q6 <- qplot(x6, y6, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 7
  x7 <- xs[, l]
  y7 <- ys[, l]
  
  q7 <- qplot(x7, y7, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 8
  x8 <- xs[, l]
  y8 <- ys[, l]
  
  q8 <- qplot(x8, y8, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 9
  x9 <- xs[, l]
  y9 <- ys[, l]
  
  q9 <- qplot(x9, y9, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 10
  x10 <- xs[, l]
  y10 <- ys[, l]
  
  q10 <- qplot(x10, y10, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
               xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  #Jogador l
  l <- 11
  x11 <- xs[, l]
  y11 <- ys[, l]
  
  escala_labels <- function(x, to, from){
    x[1] <- 'Mínimo'
    x[2] <- 'Máximo'
    x
  }
  
  escala_breaks <- function(x, to, from){
    from <- min(x)
    to <- max(x)
    c(from, to)
  }
  
  q11 <- qplot(x11, y11, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
               xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours = r, trans="log", name = 'Frequência',
                         labels = escala_labels, breaks=escala_breaks) +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = c(1.4,0.5),
          legend.text = element_text(size = 14),
          legend.title = element_text(vjust = 3))
  
  plot_grid(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11)
  
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor_clusters/jogo%s/j%s_ataque_%s/j%s_ataque_%s_g%s.png',
                 n2_jogo, n2_jogo, equipe2d, n2_jogo, equipe2d, k),
         width = 12.34, height = 8)
}
#
#################Ataque time B:##############


ataquesA_defesasB <- jogo[stri_detect_fixed(ataque_posse, '_EA'),] %>% 
  cbind(clust_ataqueA) %>% 
  cbind(clust_defB)
ataquesB_defesasA <- jogo[stri_detect_fixed(ataque_posse, '_EB'),] %>% 
  cbind(clust_ataqueB) %>% 
  cbind(clust_defA)


#aqui eu vou filtrar a prte da base de dados que só tem o time A pro ataque do time A, por exemplo (e assim vai)
ataquesB_defesasA <- ataquesB_defesasA[, c(1:8, which(stri_detect_fixed(colnames(ataquesB_defesasA), 'EB'))[1:22], 81)]


for(coluna in colnames(ataquesB_defesasA)){
  ataquesB_defesasA[[coluna]] <- as.numeric(ataquesB_defesasA[[coluna]])
}

library(psych)
library(BBmisc)
clusters <- clust_ataqueB %>% as.character() %>% as.numeric()
ataquesB_defesasA$clust_ataqueB <- clusters
n_clusts <- clust_ataqueB %>% levels() %>% length()

#cowplot

posicoes_clusters <- list()

# PARA O TIME A = 9:30
# PARA O TIME B = 31:52
for(k in 1:n_clusts){
  posicoes_em_questao <-  ataquesB_defesasA[clusters==k, 9:30]
  posicoes_clusters <- append(posicoes_clusters, list(posicoes_em_questao))
}



library(RColorBrewer)
library("factoextra")
rf <- colorRampPalette(rev(brewer.pal(8,'Spectral')))
r <- rf(8)
library(ggplot2)
library(magrittr)
library(png)


# mudar numero do jogo aqui
equipe2d <- 'B'
library(cowplot)
for(k in 1:n_clusts){
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 1
  x1 <- xs[, l]
  y1 <- ys[, l]
  
  q1 <- qplot(x1, y1, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 2
  x2 <- xs[, l]
  y2 <- ys[, l]
  
  q2 <- qplot(x2, y2, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 3
  x3 <- xs[, l]
  y3 <- ys[, l]
  
  q3 <- qplot(x3, y3, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 4
  x4 <- xs[, l]
  y4 <- ys[, l]
  
  q4 <- qplot(x4, y4, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 5
  x5 <- xs[, l]
  y5 <- ys[, l]
  
  q5 <- qplot(x5, y5, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 6
  x6 <- xs[, l]
  y6 <- ys[, l]
  
  q6 <- qplot(x6, y6, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 7
  x7 <- xs[, l]
  y7 <- ys[, l]
  
  q7 <- qplot(x7, y7, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 8
  x8 <- xs[, l]
  y8 <- ys[, l]
  
  q8 <- qplot(x8, y8, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 9
  x9 <- xs[, l]
  y9 <- ys[, l]
  
  q9 <- qplot(x9, y9, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 10
  x10 <- xs[, l]
  y10 <- ys[, l]
  
  q10 <- qplot(x10, y10, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
               xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  #Jogador l
  l <- 11
  x11 <- xs[, l]
  y11 <- ys[, l]
  
  escala_labels <- function(x, to, from){
    x[1] <- 'Mínimo'
    x[2] <- 'Máximo'
    x
  }
  
  escala_breaks <- function(x, to, from){
    from <- min(x)
    to <- max(x)
    c(from, to)
  }
  
  q11 <- qplot(x11, y11, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
               xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours = r, trans="log", name = 'Frequência',
                         labels = escala_labels, breaks=escala_breaks) +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = c(1.4,0.5),
          legend.text = element_text(size = 14),
          legend.title = element_text(vjust = 3))
  
  plot_grid(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11)
  
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor_clusters/jogo%s/j%s_ataque_%s/j%s_ataque_%s_g%s.png',
                 n2_jogo, n2_jogo, equipe2d, n2_jogo, equipe2d, k),
         width = 12.34, height = 8)
}




############## DEFESA EQUIPE B #######

#Defesa time B:


#Tratamento do jogo k (escolher qual dos 5 jogos):
ataque_posse <- paste0(jogo$ataque, '_', jogo$posse)
clust_ataqueA <- c()
clust_ataqueB <- c()
clust_defA <- c()
clust_defB <- c()
for (i in 1:nrow(jogo)){
  if(grepl('_EA', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- cl_atqA[1:length(cl_atqA) == ataque_em_questao]
    clust_ataqueA <- c(clust_ataqueA, cluster_em_questao)
    
    cluster_em_questao <- cl_defB[1:length(cl_defB) == ataque_em_questao]
    clust_defB <- c(clust_defB, cluster_em_questao)
  } else if(grepl('_EB', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- cl_atqB[1:length(cl_atqB) == ataque_em_questao]
    clust_ataqueB <- c(clust_ataqueB, cluster_em_questao)
    
    cluster_em_questao <- cl_defA[1:length(cl_defA) == ataque_em_questao]
    clust_defA <- c(clust_defA, cluster_em_questao)
  }
}

clust_ataqueA <- as.factor(clust_ataqueA)
clust_ataqueB <-  as.factor(clust_ataqueB)
clust_defA <- as.factor(clust_defA)
clust_defB <- as.factor(clust_defB)

library(stringi)
ataquesA_defesasB <- jogo[stri_detect_fixed(ataque_posse, '_EA'),] %>% 
  cbind(clust_ataqueA) %>% 
  cbind(clust_defB)


#aqui eu vou filtrar a prte da base de dados que só tem o time A pro ataque do time A, por exemplo (e assim vai)
ataquesA_defesasB <- ataquesA_defesasB[, c(1:8, which(stri_detect_fixed(colnames(ataquesA_defesasB), 'EB'))[1:22], 82)]


for(coluna in colnames(ataquesA_defesasB)){
  ataquesA_defesasB[[coluna]] <- as.numeric(as.character(ataquesA_defesasB[[coluna]]))
}

# Mudar aqui se eh ataque ou defesa pro time A ou pro time B
library(psych)
library(BBmisc)
clusters <- clust_defB %>% as.character() %>% as.numeric()
ataquesA_defesasB$clust_defB <- clusters
n_clusts <- clust_defB %>% levels() %>% length()


#cowplot 

posicoes_clusters <- list()


# PARA O TIME A = 9:30
# PARA O TIME B = 31:52
for(k in 1:n_clusts){
  posicoes_em_questao <-  ataquesA_defesasB[clusters==k, 9:30]
  posicoes_clusters <- append(posicoes_clusters, list(posicoes_em_questao))
}



library(RColorBrewer)
library("factoextra")
rf <- colorRampPalette(rev(brewer.pal(8,'Spectral')))
r <- rf(8)
library(ggplot2)
library(magrittr)
library(png)


# mudar numero do jogo aqui
equipe2d <- 'B'
library(cowplot)
for(k in 1:n_clusts){
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 1
  x1 <- xs[, l]
  y1 <- ys[, l]
  
  q1 <- qplot(x1, y1, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 2
  x2 <- xs[, l]
  y2 <- ys[, l]
  
  q2 <- qplot(x2, y2, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 3
  x3 <- xs[, l]
  y3 <- ys[, l]
  
  q3 <- qplot(x3, y3, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 4
  x4 <- xs[, l]
  y4 <- ys[, l]
  
  q4 <- qplot(x4, y4, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 5
  x5 <- xs[, l]
  y5 <- ys[, l]
  
  q5 <- qplot(x5, y5, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 6
  x6 <- xs[, l]
  y6 <- ys[, l]
  
  q6 <- qplot(x6, y6, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 7
  x7 <- xs[, l]
  y7 <- ys[, l]
  
  q7 <- qplot(x7, y7, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 8
  x8 <- xs[, l]
  y8 <- ys[, l]
  
  q8 <- qplot(x8, y8, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 9
  x9 <- xs[, l]
  y9 <- ys[, l]
  
  q9 <- qplot(x9, y9, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 10
  x10 <- xs[, l]
  y10 <- ys[, l]
  
  q10 <- qplot(x10, y10, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
               xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  #Jogador l
  l <- 11
  x11 <- xs[, l]
  y11 <- ys[, l]
  
  escala_labels <- function(x, to, from){
    x[1] <- 'Mínimo'
    x[2] <- 'Máximo'
    x
  }
  
  escala_breaks <- function(x, to, from){
    from <- min(x)
    to <- max(x)
    c(from, to)
  }
  
  q11 <- qplot(x11, y11, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
               xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours = r, trans="log", name = 'Frequência',
                         labels = escala_labels, breaks=escala_breaks) +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = c(1.4,0.5),
          legend.text = element_text(size = 14),
          legend.title = element_text(vjust = 3))
  
  plot_grid(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11)
  
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor_clusters/jogo%s/j%s_defesa_%s/j%s_defesa_%s_g%s.png',
                 n2_jogo, n2_jogo, equipe2d, n2_jogo, equipe2d, k),
         width = 12.34, height = 8)
}



############# DEFESA TIME A ##########

# Mudar aqui se eh ataque ou defesa pro time A ou pro time B

library(stringi)
ataquesB_defesasA <- jogo[stri_detect_fixed(ataque_posse, '_EB'),] %>% 
  cbind(clust_ataqueB) %>% 
  cbind(clust_defA)


#aqui eu vou filtrar a prte da base de dados que só tem o time A pro ataque do time A, por exemplo (e assim vai)
ataquesB_defesasA <- ataquesB_defesasA[, c(which(stri_detect_fixed(colnames(ataquesB_defesasA), 'EA'))[1:22], 82)]

for(coluna in colnames(ataquesB_defesasA)){
  ataquesB_defesasA[[coluna]] <- as.numeric(ataquesB_defesasA[[coluna]])
}

library(psych)
library(BBmisc)
clusters <- clust_defA %>% as.character() %>% as.numeric()
ataquesB_defesasA$clust_defA <- clusters
n_clusts <- clust_defA %>% levels() %>% length()

posicoes_clusters <- list()


# PARA O TIME A = 9:30
# PARA O TIME B = 31:52
for(k in 1:n_clusts){
  posicoes_em_questao <-  ataquesB_defesasA[clusters==k, 1:22]
  posicoes_clusters <- append(posicoes_clusters, list(posicoes_em_questao))
}


# mudar numero do jogo aqui
equipe2d <- 'A'
library(cowplot)
for(k in 1:n_clusts){
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 1
  x1 <- xs[, l]
  y1 <- ys[, l]
  
  q1 <- qplot(x1, y1, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 2
  x2 <- xs[, l]
  y2 <- ys[, l]
  
  q2 <- qplot(x2, y2, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 3
  x3 <- xs[, l]
  y3 <- ys[, l]
  
  q3 <- qplot(x3, y3, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 4
  x4 <- xs[, l]
  y4 <- ys[, l]
  
  q4 <- qplot(x4, y4, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 5
  x5 <- xs[, l]
  y5 <- ys[, l]
  
  q5 <- qplot(x5, y5, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 6
  x6 <- xs[, l]
  y6 <- ys[, l]
  
  q6 <- qplot(x6, y6, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 7
  x7 <- xs[, l]
  y7 <- ys[, l]
  
  q7 <- qplot(x7, y7, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 8
  x8 <- xs[, l]
  y8 <- ys[, l]
  
  q8 <- qplot(x8, y8, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 9
  x9 <- xs[, l]
  y9 <- ys[, l]
  
  q9 <- qplot(x9, y9, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
              xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  xs <- posicoes_clusters[[k]][, seq(1,22, 2)]
  ys <- posicoes_clusters[[k]][, seq(2,22, 2)]
  
  #Jogador l
  l <- 10
  x10 <- xs[, l]
  y10 <- ys[, l]
  
  q10 <- qplot(x10, y10, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
               xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = 'none')
  
  #Jogador l
  l <- 11
  x11 <- xs[, l]
  y11 <- ys[, l]
  
  escala_labels <- function(x, to, from){
    x[1] <- 'Mínimo'
    x[2] <- 'Máximo'
    x
  }
  
  escala_breaks <- function(x, to, from){
    from <- min(x)
    to <- max(x)
    c(from, to)
  }
  
  q11 <- qplot(x11, y11, geom='bin2d', main = sprintf('Grupo %s, Jogador %s', k, l),
               xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours = r, trans="log", name = 'Frequência',
                         labels = escala_labels, breaks=escala_breaks) +
    xlab('X') +
    ylab('Y') +
    theme(legend.position = c(1.4,0.5),
          legend.text = element_text(size = 14),
          legend.title = element_text(vjust = 3))
  
  plot_grid(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11)
  
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor_clusters/jogo%s/j%s_defesa_%s/j%s_defesa_%s_g%s.png',
                 n2_jogo, n2_jogo, equipe2d, n2_jogo, equipe2d, k),
         width = 12.34, height = 8)
}





################## histograma de todos os jogadores ppor time ########

for(n_jogo in 1:5){
  jogo1ime <- read.delim(sprintf("~/Development/IME/cea2/CEA2/jogo%sacao.txt", n_jogo), header=TRUE)
  
  #Jogo A
  jogo1A <- jogo1ime[, 8:29]
  xs_A <- jogo1A[, seq(1, ncol(jogo1A), 2)] %>% 
    as.matrix() %>% 
    matrix(ncol = 1)
  ys_A <- jogo1A[, seq(2, ncol(jogo1A), 2)] %>% 
    as.matrix() %>% 
    matrix(ncol = 1)
  
  qplot(xs_A, ys_A, geom='bin2d', main = sprintf(''),
        xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y')
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor/separado_por_time/j%s_A.png', 
                 n_jogo, njogo),
         width = 6.37, height = 4)
  
  
  #Jogo B
  jogo1B <- jogo1ime[, 30:51]
  xs_B <- jogo1B[, seq(1, ncol(jogo1A), 2)] %>% 
    as.matrix() %>% 
    matrix(ncol = 1)
  ys_B <- jogo1B[, seq(2, ncol(jogo1A), 2)] %>% 
    as.matrix() %>% 
    matrix(ncol = 1)
  
  qplot(xs_B, ys_B, geom='bin2d', main = sprintf(''),
        xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y')
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor/separado_por_time/j%s_B.png', n_jogo),
         width 
         njogo, njogo,= 6.37, height = 4)
  
  
  xs <- c(xs_A %>% as.vector(), xs_B %>% as.vector())
  ys <- c(ys_A %>% as.vector(), ys_B %>% as.vector())
  
  qplot(xs, ys, geom='bin2d', main = sprintf(''),
        xlim=c(0, 105), ylim=c(0, 68)) +
    scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
    xlab('X') +
    ylab('Y')
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor/separado_por_time/j%s_todos.png', n_jogo),
         width 
         njogo, njogo,= 6.37, height = 4)
  
}



# mapa de calor de cada um dos jogadores
library(stringr)
for(n_jogo in 1:5){
  for(i in seq(8, 51, 2)){
    jogo1ime <- read.delim(sprintf("~/Development/IME/cea2/CEA2/jogo%sacao.txt", n_jogo), header=TRUE)
    
    jogador <- colnames(jogo1ime)[i] %>% 
      str_remove('x')
    
    jogador_x <- jogo1ime[,i]
    jogador_y <- jogo1ime[,i+1]
    
    qplot(jogador_x, jogador_y, geom='bin2d', main = sprintf(''),
          xlim=c(0, 105), ylim=c(0, 68)) +
      scale_fill_gradientn(colours=r, trans="log", name = 'Frequência') +
      xlab('X') +
      ylab('Y')
    
    ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/mapas_de_calor/separado_por_jogador/j%s_jogador%s.png', n_jogo, 
                   jogadornjogo, njogo,),
           width = 6.37, height = 4)
  }
}
