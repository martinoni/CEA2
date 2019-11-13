library(magrittr)
jogo1 <- read.delim("~/Development/IME/cea2/CEA2/jogo1acao.txt", header=TRUE) %>% 
  rbind(read.delim("~/Development/IME/cea2/CEA2/jogo2acao.txt", header=TRUE)) %>% 
  rbind(read.delim("~/Development/IME/cea2/CEA2/jogo3acao.txt", header=TRUE)) %>% 
  rbind(read.delim("~/Development/IME/cea2/CEA2/jogo4acao.txt", header=TRUE)) %>% 
  rbind(read.delim("~/Development/IME/cea2/CEA2/jogo5acao.txt", header=TRUE))
  

ataques <- rbind(atq1.A[1:11])
, atq1.B[1:11], 
                 atq2.A[1:11], atq2.B[1:11],
                 atq3.A[1:11], atq3.B[1:11],
                 atq4.A[1:11], atq4.B[1:11],
                 atq5.A[1:11], atq5.B[1:11])

defesas <- rbind(def1.A[1:11], def1.B[1:11], 
                 def2.A[1:11], def2.B[1:11],
                 def3.A[1:11], def3.B[1:11],
                 def4.A[1:11], def4.B[1:11],
                 def5.A[1:11], def5.B[1:11])


ataques <- cbind(ataques, desfechos[1:71,2])
defesas <- cbind(defesas, desfechos)


colnames(ataques) <- c('Ataque', 'Duração', 'Quantidade de Ações', 'Quantidade de ações por tempo',
                       'Centroide Time x', 'Centroide Time y', 'Bola x', 'Bola y',
                       'Deslocamento Centroide Time', 'Deslocamento Bola', 'Área de ataque', 'Desfecho')
#colnames(ataques) <- c('Ações/minuto,
#                       ')
library(BBmisc)
ataques$Desfecho <- as.factor(ataques$Desfecho)
ataques <- normalize(ataques)[, 2:ncol(ataques)]
library(reshape2)
df.m <- melt(ataques, id.var = "Desfecho")
require(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Desfecho)) +
  xlab('Variável') +
  ylab('Valor padronizado') +
  ylim(c(-3, 3)) +
  theme(axis.text.x=element_text(color = "black", size=14, angle=30, vjust=.8, hjust=0.8), 
        text = element_text(size=15), plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Jogo 1 - Ataque time A')
  
