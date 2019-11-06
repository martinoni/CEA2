library(ggplot2)
ppi = 500
corg <- c()
for (i in 1:length(cl_atq1B)){
  if (cl_atq1B[i] == 1){
    corg[i] = "red"
  }
  if (cl_atq1B[i] == 2){
    corg[i] = "blue"
  }
  if (cl_atq1B[i] == 3){
    corg[i] = "darkgreen"
  }
  if (cl_atq1B[i] == 4){
    corg[i] = "darkorchid3"
  }
  if (cl_atq1B[i] == 5){
    corg[i] = "chocolate4"
  }
}

png('Desen.Grup_A1B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq1.B[,2:11], pch=16, col=corg, cex=0.6, main = "Jogo 1 - Ataque Time B")
dev.off()

cor3 <- c("red","blue3","darkgreen")
cor4 <- c("red","blue3","darkgreen","darkorchid3")
cor5 <- c("red","blue3","darkgreen","darkorchid3","chocolate4")
pairs(atq3.B[,2:11], pch=16, col=corg, cex=0.6)

pairs(atq1.A[,2:11], pch=16, col=corg, cex=0.6)


ggplot(atq1.A, aes(x = area.Atq,y = des.bola, color = as.factor(cl_atq1A))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  ggtitle("Jogo 1 - Ataque Time A") +
  labs(x = "Área Média do Ataque (m²)", y = "Deslocamento da Bola no Ataque (m)", color = "Grupo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('GA1A_area_desbola.png', plot = last_plot())

ggplot(atq1.B, aes(x = area.Atq,y = des.bola, color = as.factor(cl_atq1B))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  ggtitle("Jogo 1 - Ataque Time B") +
  labs(x = "Área Média do Ataque (m²)", y = "Deslocamento da Bola no Ataque (m)", color = "Grupo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('GA1B_area_desbola.png', plot = last_plot())

corg <- c()
for (i in 1:length(cl_def1B)){
  if (cl_def1B[i] == 1){
    corg[i] = "red"
  }
  if (cl_def1B[i] == 2){
    corg[i] = "blue"
  }
  if (cl_def1B[i] == 3){
    corg[i] = "darkgreen"
  }
  if (cl_def1B[i] == 4){
    corg[i] = "darkorchid3"
  }
  if (cl_def1B[i] == 5){
    corg[i] = "chocolate4"
  }
}

png('Desen.Grup_D1B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def1.B[,2:11], pch=16, col=corg, cex=0.6, main = "Jogo 1 - Defesa Time B")
dev.off()

ggplot(def1.A, aes(x = bolax,y = bolay, color = as.factor(cl_def1A))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  ggtitle("Jogo 1 - Defesa Time A") +
  labs(x = "Posição Média Bola(X) (m)", y = "Posição Média Bola(Y) (m)", color = "Grupo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('GD1A_bolaxy.png', plot = last_plot())

ggplot(def1.B, aes(x = bolax,y = bolay, color = as.factor(cl_def1B))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  ggtitle("Jogo 1 - Defesa Time B") +
  labs(x = "Posição Média Bola(X) (m)", y = "Posição Média Bola(Y) (m)", color = "Grupo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('GD1B_bolaxy.png', plot = last_plot())

#####################################SÉRIE TEMPORAL DOS GRUPOS##################################
trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

tm.atq1A <- trans.matrix(t(as.matrix(cl_atq1A)))
tm.atq1B <- trans.matrix(t(as.matrix(cl_atq1B)))

tm.def1A <- trans.matrix(t(as.matrix(cl_def1A)))
tm.def1B <- trans.matrix(t(as.matrix(cl_def1B)))

ggplot(compar1.A) +
  geom_line(aes(x = ataque,y = as.numeric(Atq), color = "A(Ataque)")) +
  geom_point(aes(x = ataque,y = as.numeric(Atq), color = "A(Ataque)")) +
  geom_line(aes(x = ataque,y = as.numeric(Def), color = "B(Defesa)")) +
  geom_point(aes(x = ataque,y = as.numeric(Def), color = "B(Defesa)")) +
  ggtitle("Jogo 1 - Ataque Time A vs Defesa Time B") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A1AvD1B.png', plot = last_plot())

ggplot(compar1.B) +
  geom_line(aes(x = ataque,y = as.numeric(Atq), color = "B(Ataque)")) +
  geom_point(aes(x = ataque,y = as.numeric(Atq), color = "B(Ataque)")) +
  geom_line(aes(x = ataque,y = as.numeric(Def), color = "A(Defesa)")) +
  geom_point(aes(x = ataque,y = as.numeric(Def), color = "A(Defesa)")) +
  ggtitle("Jogo 1 - Ataque Time B vs Defesa Time A") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A1BvD1A.png', plot = last_plot())

dat<-data.frame(replicate(20,sample(c("A", "B", "C","D"), size = 100, replace=TRUE)))
