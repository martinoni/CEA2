source('Agrupamento.R')




#Componentes principais?
library(magrittr)
library(shiny)

jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE)

jogo1ime[is.na(jogo1ime)] <- 0
base_pca <- jogo1ime[, c(5, 6, 8:79)]
pca <- princomp(base_pca)

loadings <- pca$loadings

#tratamento ataques time a e time b
#posse_ataque <- sprintf('%s_%s', jogo1ime$posse, jogo1ime$ataque)

escores_por_ataque <- cbind(posse_ataque, pca$scores) %>% as.data.frame()
#escores_por_ataque$posse_ataque <- as.factor(escores_por_ataque$posse_ataque)


library(reshape2)
#teste: aqui eu tenho que ter o dataframe que indica a qual grupo cada ataque pertence
#escores_por_ataque$posse_ataque 
clust <- cbind(clust, 1:length(clust)) %>% as.data.frame()
colnames(clust) <- c('grupo', 'ataque')

escores_por_ataque$grupo <- 0
for(i in 1:nrow(clust)){
  escores_por_ataque$grupo[jogo1.A$ataque == clust$ataque[i]] <- clust$grupo[i]
}

for(j in 1:ncol(escores_por_ataque)){
  escores_por_ataque[,j] <- as.numeric(escores_por_ataque[,j])
}
  
escores_por_ataque <- escores_por_ataque[, c(2:10, ncol(escores_por_ataque))]  
escores_por_ataque$grupo <- as.factor(escores_por_ataque$grupo)

df.m <- melt(escores_por_ataque, id.var = "grupo")
require(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=grupo)) +
  xlab('Componente') +
  ylab('Escore Componente') +
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) 


######################################################################

# tentativa falha de fazer com as variaveis do borto abaixp:
ataque1A <- cbind(atq1.A, clust)
colnames(ataque1A) <- c(colnames(ataque1A)[1:ncol(ataque1A)-1], 'grupo')
library(BBmisc)
ataque1A[, 1:11] <- ataque1A[, 1:11] %>% normalize()
df.m <- melt(ataque1A[1:12], id.var = "grupo")
df.m$grupo <- as.factor(df.m$grupo)
require(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=grupo)) +
  xlab('Variável do borto') +
  ylab('Valor da variável') +
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) 

