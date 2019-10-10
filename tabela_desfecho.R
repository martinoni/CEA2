library(dplyr)
jogo1ime <- read.delim("~/Development/IME/cea2/jogo1acao.txt", header=TRUE)


ataque.A1 <- subset(jogo1ime, posse == "EA",select = c("ataque","desf_ataque"))
ataque.A1 <- ataque.A1 %>% unique()
ataque.B1 <- subset(jogo1ime, posse == "EB",select = c("ataque","desf_ataque"))
ataque.B1 <- ataque.B1 %>% unique()
#ataque.A1$desf_ataque <- ataque.A$desf_ataque %>% as.factor()
ataque.A1 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))
ataque.B1 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))

library(dplyr)
jogo2ime <- read.delim("~/Development/IME/cea2/jogo2acao.txt", header=TRUE)


ataque.A2 <- subset(jogo2ime, posse == "EA",select = c("ataque","desf_ataque"))
ataque.A2 <- ataque.A2 %>% unique()
ataque.B2 <- subset(jogo2ime, posse == "EB",select = c("ataque","desf_ataque"))
ataque.B2 <- ataque.B2 %>% unique()
#ataque.A2$desf_ataque <- ataque.A$desf_ataque %>% as.factor()
ataque.A2 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))
ataque.B2 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))

library(dplyr)
jogo3ime <- read.delim("~/Development/IME/cea2/jogo3acao.txt", header=TRUE)


ataque.A3 <- subset(jogo3ime, posse == "EA",select = c("ataque","desf_ataque"))
ataque.A3 <- ataque.A3 %>% unique()
ataque.B3 <- subset(jogo3ime, posse == "EB",select = c("ataque","desf_ataque"))
ataque.B3 <- ataque.B3 %>% unique()
#taque.A3$desf_ataque <- ataque.A$desf_ataque %>% as.factor()
ataque.A3 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))
ataque.B3 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))

library(dplyr)
jogo4ime <- read.delim("~/Development/IME/cea2/jogo4acao.txt", header=TRUE)


ataque.A4 <- subset(jogo4ime, posse == "EA",select = c("ataque","desf_ataque"))
ataque.A4 <- ataque.A4 %>% unique()
ataque.B4 <- subset(jogo4ime, posse == "EB",select = c("ataque","desf_ataque"))
ataque.B4 <- ataque.B4 %>% unique()
#ataque.A4$desf_ataque <- ataque.A$desf_ataque %>% as.factor()
ataque.A4 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))
ataque.B4 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))

library(dplyr)
jogo5ime <- read.delim("~/Development/IME/cea2/jogo5acao.txt", header=TRUE)


ataque.A5 <- subset(jogo5ime, posse == "EA",select = c("ataque","desf_ataque"))
ataque.A5 <- ataque.A5 %>% unique()
ataque.B5 <- subset(jogo5ime, posse == "EB",select = c("ataque","desf_ataque"))
ataque.B5 <- ataque.B5 %>% unique()
#ataque.A5$desf_ataque <- ataque.A$desf_ataque %>% as.factor()
ataque.A5 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))
ataque.B5 %>% group_by(desf_ataque) %>% 
  summarise(no_rows = length(ataque))


desfechos <- ataque.A1 %>% 
  rbind(ataque.A2) %>% 
  rbind(ataque.A3) %>% 
  rbind(ataque.A4) %>% 
  rbind(ataque.A5) %>% 
  rbind(ataque.B1) %>% 
  rbind(ataque.B2) %>% 
  rbind(ataque.B3) %>% 
  rbind(ataque.B4) %>% 
  rbind(ataque.B5)
  


