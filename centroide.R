library(magrittr)
library(sp)
library(adehabitatHR)
library(scales)
library(png)
library(foreach)
library(ggplot2)

# ver correlação entre centro de gravidade e area
# fazer o grafico do centroide ao longo do jogo
# estudar a distancia do centro de gravidade e o gol
# substituir o valor do centroide pelo centroide SEM CONTAR O GOLEIRO
# distancia da bola pro gol
# 

jogo1ime <- read.delim("C:/Users/lucasb/Downloads/jogo5acao.txt", header=TRUE)
jogo1ime <- na.omit(jogo1ime)

setwd('~/Development/IME/cea2/')
ima <- readPNG('./campo/background_campo.png')

area.EA <- c()
area.EB <- c()
for(i in 1:nrow(jogo1ime)){
  jogosub <- jogo1ime[i,]
  jogoimp <- jogosub[,8:51]
  jogoimp <- data.frame(jogosub$noacao,jogoimp)
  colnames(jogoimp)[1] <- "noacao"
  
  noacao <- c()
  equipe <- c()
  pos.x <- c()
  pos.y <- c()
  
  for(j in 2:11){
    noacao <- c(noacao,paste0(jogoimp$noacao,"A"))
    pos.x <- c(pos.x,jogoimp[1,2*j])
    pos.y <- c(pos.y,jogoimp[1,(2*j)+1])
  }
  for(j in 13:22){
    noacao <- c(noacao,paste0(jogoimp$noacao,"B"))
    pos.x <- c(pos.x,jogoimp[1,2*j])
    pos.y <- c(pos.y,jogoimp[1,(2*j)+1])
  }
  
  jogoimp.coor <- data.frame(noacao,pos.x,pos.y)
  
  coordinates(jogoimp.coor) <- c("pos.x", "pos.y")
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100, 'm', 'm2')
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}

jogo.area <- cbind(jogo1ime,area.EA,area.EB)

cor(jogo.area$centEAx,jogo.area$area.EA)

ggplot(jogo.area,aes(x = centEAx, y = area.EA))+
  geom_point(color = "red") + 
  labs(title = "Jogo 1 - TIME A: corr = 0.2848011",x = "posição horizontal do centroide (m)", y = "Área(m²)") +
  theme(plot.title = element_text(hjust = 0.5))

cor(jogo.area$centEBx,jogo.area$area.EB)

ggplot(jogo.area,aes(x = centEBx, y = area.EB))+
  geom_point(color = "blue") +
  labs(title = "Jogo 1 - TIME B: corr = -0.1921221",x = "posição horizontal do centroide (m)", y = "Área(m²)") +
  theme(plot.title = element_text(hjust = 0.5))

xs <- data.frame(jogo1ime[,seq(10, 51, 2)])
centEAx.novo <- c()
centEBx.novo <- c()
for (i in 1:nrow(xs)){
  centEAx.novo <- c(centEAx.novo,mean(as.numeric(xs[i,1:10])))
  centEBx.novo <- c(centEBx.novo,mean(as.numeric(xs[i,11:20])))
  }

jogo.area <- cbind(jogo.area,centEAx.novo,centEBx.novo)

cor(jogo.area$centEAx.novo,jogo.area$area.EA)

ggplot(jogo.area,aes(x = centEAx.novo, y = area.EA))+
  geom_point(color = "red") +
  labs(title = "Jogo 5 - TIME A: corr = 0.3064157",x = "posição horizontal do centroide (m)", y = "Área(m²)") +
  theme(plot.title = element_text(hjust = 0.5))


cor(jogo.area$centEBx.novo,jogo.area$area.EB)

ggplot(jogo.area,aes(x = centEBx.novo, y = area.EB))+
  geom_point(color = "blue") +
  labs(title = "Jogo 5 - TIME B: corr = -0.1980347",x = "posição horizontal do centroide (m)", y = "Área(m²)") +
  theme(plot.title = element_text(hjust = 0.5))

desf <- subset(jogo1ime, desf_ataque == 1)

ver <- jogo1ime[850:869,]

gol <- c(rep(0,260),rep(1,5),rep(2,nrow(jogo1ime)-265))

jogo.area <- cbind(jogo.area,gol)

ggplot(jogo.area,aes(x = centEAx.novo, y = area.EA, color = factor(gol)))+
  geom_point() +
  labs(title = "Jogo 5 - TIME A",x = "posição horizontal do centroide (m)", y = "Área(m²)", color = "Período") +
  scale_color_manual(labels = c("antes","gol","depois"), values = c("red","green","blue")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(jogo.area,aes(x = centEBx.novo, y = area.EB, colour = factor(gol)))+
  geom_point() +
  labs(title = "Jogo 5 - TIME B",x = "posição horizontal do centroide (m)",  y = "Área(m²)", color = "Período") +
  scale_color_manual(labels = c("antes","gol","depois"), values = c("red","green","blue")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(jogo.area,aes(x = centEAx.novo, y = area.EA, color = factor(periodo)))+
  geom_point() +
  labs(title = "Jogo 5 - TIME A",x = "posição horizontal do centroide (m)", y = "Área(m²)", color = "Período") +
  scale_color_manual(labels = c("1º Tempo","2ºTempo"), values = c("red","blue")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(jogo.area,aes(x = centEBx.novo, y = area.EB, colour = factor(periodo)))+
  geom_point() +
  labs(title = "Jogo 5 - TIME B",x = "posição horizontal do centroide (m)",  y = "Área(m²)", color = "Período") +
  scale_color_manual(labels = c("1º Tempo","2ºTempo"), values = c("red","blue")) +
  theme(plot.title = element_text(hjust = 0.5))
