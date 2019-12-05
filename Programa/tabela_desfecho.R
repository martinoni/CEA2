library(dplyr)
jogo1ime <- read.delim("C:/Users/lucas/Downloads/R descr/2019-2/Cea2/jogo4acao.txt", header=TRUE)

new_DF <- def1.A[rowSums(is.na(def1.A)) > 0,]
wtf <- subset(jogo1ime, ataque ==66)

golm1 <- subset(new_DF, desf_ataque == 1)

ataque.A5 <- subset(jogo5, posse == "EA",select = c("ataque","desf_ataque"))
ataque.A5 <- ataque.A5 %>% unique()
ataque.B5 <- subset(jogo5, posse == "EB",select = c("ataque","desf_ataque"))
ataque.B5 <- ataque.B5 %>% unique()
ataque.A$desf_ataque <- ataque.A$desf_ataque %>% as.factor()
ataque.A %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))
ataque.B %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))

lul <- rbind(ataque.A1,ataque.B1,ataque.A2,ataque.B2,ataque.A3,ataque.B3,ataque.A4,ataque.B4,ataque.A5,ataque.B5)
