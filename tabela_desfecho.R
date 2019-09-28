library(dplyr)
jogo1ime <- read.delim("C:/Users/lucas/Downloads/R descr/2019-2/Cea2/jogo1acao.txt", header=TRUE)


ataque.A <- subset(jogo1ime, posse == "EA",select = c("ataque","desf_ataque"))
ataque.A <- ataque.A %>% unique()
ataque.B <- subset(jogo1ime, posse == "EB",select = c("ataque","desf_ataque"))
ataque.B <- ataque.B %>% unique()
ataque.A$desf_ataque <- ataque.A$desf_ataque %>% as.factor()
ataque.A %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))
ataque.B %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))

