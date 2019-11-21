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
lista_ataquesA <- list(atq1.A, atq2.A, atq3.A, atq4.A, atq5.A)
#Ataque A
i <- 1
for(nome in lista_ataquesA){
  ataque1A <- nome[, c(2:4, 5:ncol(nome))]
  colnames(ataque1A) <- c('Duração', 'Quantidade de Ações', 'Quantidade de Ações por tempo',
                          'Centroide Time x', 'Centroide Time y', 'Bola x', 'Bola y',
                          'Deslocamento Centroide Time', 'Deslocamento Bola', 'Área de ataque',
                          'Grupo')
  library(BBmisc)
  library(reshape2)
  ataque1A[, 1:10] <- ataque1A[, 1:10] %>% normalize()
  df.m <- melt(ataque1A, id.var = "Grupo")
  df.m$Grupo <- as.factor(df.m$Grupo)
  require(ggplot2)
  ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Grupo)) +
    xlab('Variável') +
    ylab('Valor normalizado da variável') +
    theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8),
          axis.title = element_text(size=10), text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(sprintf('Jogo %s: Ataque equipe A', i))
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/boxplots_borto/todos/ataque/jogo%s_atq_A.png', i))
  i <- i + 1
}


# Ataque B
lista_ataquesB <- list(atq1.B, atq2.B, atq3.B, atq4.B, atq5.B)
i <- 1
for(nome in lista_ataquesB){
  ataque1A <- nome[, c(2:4, 5:ncol(nome))]
  colnames(ataque1A) <- c('Duração', 'Quantidade de Ações', 'Quantidade de Ações por tempo',
                          'Centroide Time x', 'Centroide Time y', 'Bola x', 'Bola y',
                          'Deslocamento Centroide Time', 'Deslocamento Bola', 'Área de ataque',
                          'Grupo')
  library(BBmisc)
  library(reshape2)
  ataque1A[, 1:10] <- ataque1A[, 1:10] %>% normalize()
  df.m <- melt(ataque1A, id.var = "Grupo")
  df.m$Grupo <- as.factor(df.m$Grupo)
  require(ggplot2)
  ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Grupo)) +
    xlab('Variável') +
    ylab('Valor normalizado da variável') +
    theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8),
          axis.title = element_text(size=10), text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(sprintf('Jogo %s: Ataque equipe B', i))
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/boxplots_borto/todos/ataque/jogo%s_atq_B.png', i))
  i <- i + 1
}

# Defesas A
lista_defesas <- list(def1.A, def2.A, def3.A, def4.A, def5.A)
i <- 1
for(nome in lista_defesas){
  ataque1A <- nome[, c(2:3, 5:ncol(nome))]
  colnames(ataque1A) <- c('Duração', 'Quantidade de Ações',
                          'Centroide Time x', 'Centroide Time y', 'Bola x', 'Bola y',
                          'Deslocamento Centroide Time', 'Deslocamento Bola', 'Área de defesa',
                          'Grupo')
  library(BBmisc)
  library(reshape2)
  ataque1A[, 1:9] <- ataque1A[, 1:9] %>% normalize()
  df.m <- melt(ataque1A, id.var = "Grupo")
  df.m$Grupo <- as.factor(df.m$Grupo)
  require(ggplot2)
  ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Grupo)) +
    xlab('Variável') +
    ylab('Valor normalizado da variável') +
    theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8),
          axis.title = element_text(size=10), text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(sprintf('Jogo %s: Defesa equipe A', i))
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/boxplots_borto/todos/defesa/jogo%s_def_A.png', i))
  i <- i + 1
}

# Defesas B
lista_defesas <- list(def1.B, def2.B, def3.B, def4.B, def5.B)
i <- 1
for(nome in lista_defesas){
  ataque1A <- nome[, c(2:3, 5:ncol(nome))]
  colnames(ataque1A) <- c('Duração', 'Quantidade de Ações',
                          'Centroide Time x', 'Centroide Time y', 'Bola x', 'Bola y',
                          'Deslocamento Centroide Time', 'Deslocamento Bola', 'Área de defesa',
                          'Grupo')
  library(BBmisc)
  library(reshape2)
  ataque1A[, 1:9] <- ataque1A[, 1:9] %>% normalize()
  df.m <- melt(ataque1A, id.var = "Grupo")
  df.m$Grupo <- as.factor(df.m$Grupo)
  require(ggplot2)
  ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Grupo)) +
    xlab('Variável') +
    ylab('Valor normalizado da variável') +
    theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8),
          axis.title = element_text(size=10), text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(sprintf('Jogo %s: Defesa equipe B', i))
  
  ggsave(sprintf('~/Development/IME/cea2/CEA2/descritiva/boxplots_borto/todos/defesa/jogo%s_def_B.png', i))
  i <- i + 1
}


#Boxplot separado
nome = def3.B
ataque1A <- nome[, c(2:3, 5:ncol(nome))]
colnames(ataque1A) <- c('Duração', 'Quantidade de Ações',
                        'Centroide Time x', 'Centroide Time y', 'Bola x', 'Bola y',
                        'Deslocamento Centroide Time', 'Deslocamento Bola', 'Área de defesa',
                        'Grupo')

ataque1A <- ataque1A[, c('Bola y', 'Grupo')]
library(BBmisc)
df.m <- melt(ataque1A, id.var = "Grupo")
df.m$Grupo <- as.factor(df.m$Grupo)
require(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Grupo)) +
  xlab('Posição vertical do centroide do time (por grupo)') +
  ylab('Valor da variável (m)') +
  theme(axis.text.x=element_text(color = "black", size=0, angle=30, vjust=.8, hjust=0.8),
        axis.title = element_text(size=14), text = element_text(size = 16))

#######################################################################

#Aqui começa o trampo que o fossa o borto e o LG pediram

# MUDAR O JOGO AQUI

#Tratamento do jogo k (escolher qual dos 5 jogos):
jogo <- read.delim("~/Development/IME/cea2/CEA2/jogo3ime_completo.txt")
ataque_posse <- paste0(jogo$ataque, '_', jogo$posse)
clust_ataqueA <- c()
clust_ataqueB <- c()
clust_defA <- c()
clust_defB <- c()
for (i in 1:nrow(jogo)){
  if(grepl('_EA', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- cl_atq2A[1:length(cl_atq2A) == ataque_em_questao]
    clust_ataqueA <- c(clust_ataqueA, cluster_em_questao)
    
    cluster_em_questao <- cl_def2B[1:length(cl_def2B) == ataque_em_questao]
    clust_defB <- c(clust_defB, cluster_em_questao)
  } else if(grepl('_EB', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- cl_atq2B[1:length(cl_atq2B) == ataque_em_questao]
    clust_ataqueB <- c(clust_ataqueB, cluster_em_questao)
    
    cluster_em_questao <- cl_def2A[1:length(cl_def2A) == ataque_em_questao]
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
ataquesA_defesasB <- ataquesA_defesasB[, c(1:8, 31:56, 59:70, 81)]


for(coluna in colnames(ataquesA_defesasB)){
  ataquesA_defesasB[[coluna]] <- as.numeric(as.character(ataquesA_defesasB[[coluna]]))
}

# Mudar aqui se eh ataque ou defesa pro time A ou pro time B
library(psych)
library(BBmisc)
clusters <- clust_ataqueA %>% as.character() %>% as.numeric()
ataquesA_defesasB$clust_ataqueA <- clusters
n_clusts <- clust_ataqueA %>% levels() %>% length()
pcas <- list()
loadings <- list()
for(k in 1:n_clusts){
  dados_em_questao_nao_normalizados <- ataquesA_defesasB[ataquesA_defesasB$clust_ataqueA == k, c(6,7,9:47)]
  #mudar pra 31 pra times separadaos e 54 pros dois times:s
  dados_em_questao <- ataquesA_defesasB[clust_ataqueA == k, c(6,7,9:46)] %>% 
    normalize()
  pca_em_questao <- principal(dados_em_questao,
                              scores=T, missing = T, 
                              nfactors = 4)
  pcas <- append(pcas, list(pca_em_questao))
  loadings <- loadings %>% 
    append(list(pca_em_questao$loadings[]))
}

save(loadings,
     file = '~/Development/IME/cea2/CEA2/PCA_por_grupos/pcas_ataquesA_defesasB2.RData')
save(dados_em_questao_nao_normalizados,
     file = '~/Development/IME/cea2/CEA2/PCA_por_grupos/dados_em_questao2.RData')



##########################################
#Ataque Time B

ataquesA_defesasB <- jogo[stri_detect_fixed(ataque_posse, '_EA'),] %>% 
  cbind(clust_ataqueA) %>% 
  cbind(clust_defB)
ataquesB_defesasA <- jogo[stri_detect_fixed(ataque_posse, '_EB'),] %>% 
  cbind(clust_ataqueB) %>% 
  cbind(clust_defA)


#aqui eu vou filtrar a prte da base de dados que só tem o time A pro ataque do time A, por exemplo (e assim vai)
ataquesB_defesasA <- ataquesB_defesasA[, c(1:30, 53:56, 59:70, 81)]


for(coluna in colnames(ataquesA_defesasB)){
  ataquesA_defesasB[[coluna]] <- as.numeric(as.character(ataquesA_defesasB[[coluna]]))
}

for(coluna in colnames(ataquesB_defesasA)){
  ataquesB_defesasA[[coluna]] <- as.numeric(ataquesB_defesasA[[coluna]])
}
 
library(psych)
library(BBmisc)
clusters <- clust_ataqueB %>% as.character() %>% as.numeric()
ataquesB_defesasA$clust_ataqueB <- clusters
n_clusts <- clust_ataqueB %>% levels() %>% length()

####################################################
# DEFESA TIME B

#Aqui começa o trampo que o fossa o borto e o LG pediram

# MUDAR O JOGO AQUI

#Tratamento do jogo k (escolher qual dos 5 jogos):
ataque_posse <- paste0(jogo$ataque, '_', jogo$posse)
clust_ataqueA <- c()
clust_ataqueB <- c()
clust_defA <- c()
clust_defB <- c()
for (i in 1:nrow(jogo)){
  if(grepl('_EA', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- cl_atq2A[1:length(cl_atq2A) == ataque_em_questao]
    clust_ataqueA <- c(clust_ataqueA, cluster_em_questao)
    
    cluster_em_questao <- cl_def2B[1:length(cl_def2B) == ataque_em_questao]
    clust_defB <- c(clust_defB, cluster_em_questao)
  } else if(grepl('_EB', ataque_posse[i])){
    ataque_em_questao <- jogo$ataque[i]
    cluster_em_questao <- cl_atq2B[1:length(cl_atq2B) == ataque_em_questao]
    clust_ataqueB <- c(clust_ataqueB, cluster_em_questao)
    
    cluster_em_questao <- cl_def2A[1:length(cl_def2A) == ataque_em_questao]
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
ataquesA_defesasB <- ataquesA_defesasB[, c(1:30, 82)]


for(coluna in colnames(ataquesA_defesasB)){
  ataquesA_defesasB[[coluna]] <- as.numeric(as.character(ataquesA_defesasB[[coluna]]))
}

for(coluna in colnames(ataquesB_defesasA)){
  ataquesB_defesasA[[coluna]] <- as.numeric(ataquesB_defesasA[[coluna]])
}

# Mudar aqui se eh ataque ou defesa pro time A ou pro time B
library(psych)
library(BBmisc)
clusters <- clust_defB %>% as.character() %>% as.numeric()
ataquesA_defesasB$clust_defB <- clusters
n_clusts <- clust_defB %>% levels() %>% length()
pcas <- list()
loadings <- list()



####################################################
# DEFESA TIME A

# Mudar aqui se eh ataque ou defesa pro time A ou pro time B

library(stringi)
ataquesA_defesasB <- jogo[stri_detect_fixed(ataque_posse, '_EA'),] %>% 
  cbind(clust_ataqueA) %>% 
  cbind(clust_defB)
ataquesB_defesasA <- jogo[stri_detect_fixed(ataque_posse, '_EB'),] %>% 
  cbind(clust_ataqueB) %>% 
  cbind(clust_defA)


#aqui eu vou filtrar a prte da base de dados que só tem o time A pro ataque do time A, por exemplo (e assim vai)
ataquesB_defesasA <- ataquesB_defesasA[, c(31:56, 82)]

for(coluna in colnames(ataquesB_defesasA)){
  ataquesB_defesasA[[coluna]] <- as.numeric(ataquesB_defesasA[[coluna]])
}

library(psych)
library(BBmisc)
clusters <- clust_defA %>% as.character() %>% as.numeric()
ataquesB_defesasA$clust_defA <- clusters
n_clusts <- clust_defB %>% levels() %>% length()
