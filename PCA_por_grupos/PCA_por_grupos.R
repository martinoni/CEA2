source('~/Development/IME/cea2/CEA2/agrupamento.R')




#Componentes principais?
library(magrittr)
library(shiny)

jogo1ime <- read.delim("~/Development/IME/cea2/CEA2/jogo1acao.txt", header=TRUE)

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


#######################################################################

#Aqui começa o trampo que o fossa o borto e o LG pediram

# criação de uma base auxiliar ligando os ataques a seus respectivos clusters
clusters_ataques <- 1:length(cl_atq1A) %>% 
  cbind(cl_atq1A) %>% 
  cbind(cl_atq1B) %>% 
  cbind(cl_def1A) %>% 
  cbind(cl_def1B) %>% 
  as.data.frame()
colnames(clusters_ataques) <- c('ataque', 'atqA', 'atqB', 'defA', 'defB')


#Tratamento do jogo:
jogo <- read.delim("~/Development/IME/cea2/CEA2/jogo1ime_completo.txt")
ataque_posse <- paste0(jogo$ataque, '_', jogo$posse)
clust_ataqueA <- c()
clust_ataqueB <- c()
clust_defA <- c()
clust_defB <- c()
for (i in 1:nrow(jogo)){
  if(grepl('_EA', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- clusters_ataques$atqA[clusters_ataques$ataque == ataque_em_questao]
    clust_ataqueA <- c(clust_ataqueA, cluster_em_questao)
    
    cluster_em_questao <- clusters_ataques$defB[clusters_ataques$ataque == ataque_em_questao]
    clust_defB <- c(clust_defB, cluster_em_questao)
  } else if(grepl('_EB', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- clusters_ataques$atqB[clusters_ataques$ataque == ataque_em_questao]
    clust_ataqueB <- c(clust_ataqueB, cluster_em_questao)
    
    cluster_em_questao <- clusters_ataques$defA[clusters_ataques$ataque == ataque_em_questao]
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
ataquesB_defesasA <- jogo[stri_detect_fixed(ataque_posse, '_EB'),] %>% 
  cbind(clust_ataqueB) %>% 
  cbind(clust_defA)



for(coluna in colnames(ataquesA_defesasB)){
  ataquesA_defesasB[[coluna]] <- as.numeric(ataquesA_defesasB[[coluna]])
}

library(psych)
library(BBmisc)
clusters <- clust_ataqueA
n_clusts <- clust_ataqueA %>% levels() %>% length()
pcas <- list()
loadings <- list()
for(k in 1:n_clusts){
  dados_em_questao_nao_normalizados <- ataquesA_defesasB[ataquesA_defesasB$clust_ataqueA == k, c(6,7,9:54)]
  dados_em_questao <- ataquesA_defesasB[ataquesA_defesasB$clust_ataqueA == k, c(6,7,9:54)] %>% 
    normalize()
  pca_em_questao <- principal(dados_em_questao,
                              scores=T, missing = T, 
                              nfactors = 4)
  pcas <- append(pcas, list(pca_em_questao))
  loadings <- loadings %>% 
    append(list(pca_em_questao$loadings[]))
}

save(loadings,
     file = '~/Development/IME/cea2/CEA2/PCA_por_grupos/pcas_ataquesA_defesasB.RData')
save(dados_em_questao_nao_normalizados,
     file = '~/Development/IME/cea2/CEA2/PCA_por_grupos/dados_em_questao.RData')


