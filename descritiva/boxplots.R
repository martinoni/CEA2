library(magrittr)
jogo1 <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE) %>% 
  rbind(read.delim("~/Development/IME/cea2/jogo2acao.txt", header=TRUE)) %>% 
  rbind(read.delim("~/Development/IME/cea2/jogo3acao.txt", header=TRUE)) %>% 
  rbind(read.delim("~/Development/IME/cea2/jogo4acao.txt", header=TRUE)) %>% 
  rbind(read.delim("~/Development/IME/cea2/jogo5acao.txt", header=TRUE))
  

ataques <- rbind(atq1.A, atq1.B, 
                 atq2.A, atq2.B,
                 atq3.A, atq3.B,
                 atq4.A, atq4.B,
                 atq5.A, atq5.B)

defesas <- rbind(def1.A, def1.B, 
                 def2.A, def2.B,
                 def3.A, def3.B,
                 def4.A, def4.B,
                 def5.A, def5.B)


ataques <- cbind(ataques, desfechos)
defesas <- cbind(defesas, desfechos)


colnames(ataques) <- c(colnames(ataques)[1:length(colnames(ataques))-1], 'Desfecho')
colnames(ataques) <- c('Ações/minuto,
                       ')
ataques <- ataques[, 4:13]
ataques <- ataques[, c(1:8, 10)]
library(BBmisc)
ataques$Desfecho <- as.factor(ataques$Desfecho)
ataques <- normalize(ataques)
library(reshape2)
df.m <- melt(ataques, id.var = "Desfecho")
require(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Desfecho)) +
  xlab('Variável') +
  ylab('Valor padronizado') +
  ylim(c(-3, 3)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) +
  



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
