library(dplyr)
jogo1ime <- read.delim("C:/Users/lucas/Downloads/R descr/2019-2/Cea2/jogo4acao.txt", header=TRUE)
new_DF <- def1.A[rowSums(is.na(def1.A)) > 0,]
wtf <- subset(jogo1ime, ataque ==66)
golm1 <- subset(new_DF, desf_ataque == 1)

ataque.A <- subset(jogo1ime, posse == "EA",select = c("ataque","desf_ataque"))
ataque.A <- ataque.A %>% unique()
ataque.B <- subset(jogo1ime, posse == "EB",select = c("ataque","desf_ataque"))
ataque.B <- ataque.B %>% unique()
ataque.A$desf_ataque <- ataque.A$desf_ataque %>% as.factor()
ataque.A %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))
ataque.B %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))

