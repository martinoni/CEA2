jogo1 <- jogo1ime

lista <- c()
for(i in 1:nrow(jogo1)){
  if(jogo1$posse == 'EA'){
    if(i != 1 && jogo1$ataque[i] != jogo1$ataque[i-1]){
      tupla <- c(jogo1$ataque[i], jogo1$desf_ataque[i])
      lista <- c(lista, tupla)
      print(tupla)
    }
  }
}

lista <- c(c(1, jogo1$desf_ataque[1]), lista)

lista <- matrix(lista, nrow=2)
lista <- lista %>% t() %>% as.data.frame()
lista <- lista %>% cbind(sc.atq1A)
lista <- subset(lista, select = -c(V1, ataque))
lista$V2 <- as.factor(lista$V2)
par(mfrow=c(2,4))
for(i in 2:9){
  boxplot(lista[,i]~V2, data=lista,
          main = colnames(lista)[i],
          xlab = 'desfecho')
}

colnames(lista) <- c('Desfecho', colnames(lista)[2:length(colnames(lista))])
library(reshape2)
df.m <- melt(lista, id.var = "Desfecho")
require(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Desfecho)) +
  xlab('Variável') +
  ylab('Valor padronizado')
  


variaveis <- 
plot(1:71, lista$centAx, data = lista,
     col = lista$V2)
for(i in 1:71){
  lines(i:(i+1), lista$centAx[i:(i+1)], data=lista,
        col = lista$V2[i])
}


library(gamlss)

model <- gamlss(V2~., family = BI('logit'), data = lista,
                sigma.formula = ~ac_d)
