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

tm.atq2A <- trans.matrix(t(as.matrix(cl_atq2A)))
tm.atq2B <- trans.matrix(t(as.matrix(cl_atq2B)))

tm.def2A <- trans.matrix(t(as.matrix(cl_def2A)))
tm.def2B <- trans.matrix(t(as.matrix(cl_def2B)))

tm.atq3A <- trans.matrix(t(as.matrix(cl_atq3A)))
tm.atq3B <- trans.matrix(t(as.matrix(cl_atq3B)))

tm.def3A <- trans.matrix(t(as.matrix(cl_def3A)))
tm.def3B <- trans.matrix(t(as.matrix(cl_def3B)))

tm.atq4A <- trans.matrix(t(as.matrix(cl_atq4A)))
tm.atq4B <- trans.matrix(t(as.matrix(cl_atq4B)))

tm.def4A <- trans.matrix(t(as.matrix(cl_def4A)))
tm.def4B <- trans.matrix(t(as.matrix(cl_def4B)))

tm.atq5A <- trans.matrix(t(as.matrix(cl_atq5A)))
tm.atq5B <- trans.matrix(t(as.matrix(cl_atq5B)))

tm.def5A <- trans.matrix(t(as.matrix(cl_def5A)))
tm.def5B <- trans.matrix(t(as.matrix(cl_def5B)))

l.1A <- data.frame(ataque = c(compar1.A$ataque,compar1.A$ataque), 
                   time = c(rep("Atq", nrow(compar1.A)),rep("Def", nrow(compar1.A))), 
                   grupo = c(compar1.A$Atq, compar1.B$Def))

l.1B <- data.frame(ataque = c(compar1.B$ataque,compar1.B$ataque), 
                   time = c(rep("Atq", nrow(compar1.B)),rep("Def", nrow(compar1.B))), 
                   grupo = c(compar1.B$Atq, compar1.B$Def))

ggplot(l.1A,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 1 - Ataque Time A vs Defesa Time B") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A1AvD1B.png', plot = last_plot())

l.1B <- data.frame(ataque = c(compar1.B$ataque,compar1.B$ataque), 
                   time = c(rep("Atq", nrow(compar1.B)),rep("Def", nrow(compar1.B))), 
                   grupo = c(compar1.B$Atq, compar1.B$Def))
ggplot(l.1B,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 1 - Ataque Time B vs Defesa Time A") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A1BvD1A.png', plot = last_plot())

#################################BOXPLOT pRIMEIRA APRESENTAÇÃO####################################
area <- c(c(jogo1.A$area.EA),c(jogo1.B$area.EB),c(jogo1.B$area.EA),c(jogo1.A$area.EB))
time <- c(rep("ATQ A",nrow(jogo1.A)),rep("ATQ B",nrow(jogo1.B)),rep("DEF A",nrow(jogo1.B)),
          rep("DEF B",nrow(jogo1.A)))
b1A <- data.frame(area,time)

ggplot(b1A, aes(x=time, y=area, fill = time)) +
  geom_boxplot() +
  scale_fill_manual(values=c("red", "blue", "red4", "midnightblue")) +
  ggtitle("Jogo 1 - Boxplot Área") +
  theme_bw() +
  labs(y = "Área")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15,face="bold"),legend.position="none",
        plot.title = element_text(hjust = 0.5))
ggsave('G1_boxArea.png', plot = last_plot())