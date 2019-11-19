library(ggplot2)
ppi = 500

###############################Gráficos Dispersão################################################

cor.des <- function(agrup){
  corg <- c()
  for (i in 1:length(agrup)){
    if (agrup[i] == 1){
      corg[i] = "red"
    }
    if (agrup[i] == 2){
      corg[i] = "blue"
    }
    if (agrup[i] == 3){
      corg[i] = "darkgreen"
    }
    if (agrup[i] == 4){
      corg[i] = "darkorchid3"
    }
    if (agrup[i] == 5){
      corg[i] = "chocolate4"
    }
  }
  return(corg)
}
cor3 <- c("red","blue3","darkgreen")
cor4 <- c("red","blue3","darkgreen","darkorchid3")
cor5 <- c("red","blue3","darkgreen","darkorchid3","chocolate4")

#ATQ
corg <- cor.des(cl_atq1A)
png('Desen.Grup_A1A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq1.A[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 1 - Ataque Equipe A", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq1.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()


corg <- cor.des(cl_atq1B)
png('Desen.Grup_A1B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq1.B[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 1 - Ataque Equipe B", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq1.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_atq2A)
png('Desen.Grup_A2A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq2.A[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 2 - Ataque Equipe A", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq2.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_atq2B)
png('Desen.Grup_A2B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq2.B[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 2 - Ataque Equipe B", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq2.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_atq3A)
png('Desen.Grup_A3A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq3.A[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 3 - Ataque Equipe A", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq3.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_atq3B)
png('Desen.Grup_A3B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq3.B[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 3 - Ataque Equipe B", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq3.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_atq4A)
png('Desen.Grup_A4A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq4.A[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 4 - Ataque Equipe A", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq4.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_atq4B)
png('Desen.Grup_A4B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq4.B[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 4 - Ataque Equipe B", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq4.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_atq5A)
png('Desen.Grup_A5A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq5.A[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 5 - Ataque Equipe A", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq5.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_atq5B)
png('Desen.Grup_A5B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(atq5.B[,4:11], pch=16, col=corg, cex=0.6, main = "Jogo 5 - Ataque Equipe B", oma=c(3,3,5,11),
      labels = c("Ação/Min","Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(atq5.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()


#DEF
corg <- cor.des(cl_def1A)
png('Desen.Grup_D1A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def1.A[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 1 - Defesa Equipe A", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def1.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def1B)
png('Desen.Grup_D1B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def1.B[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 1 - Defesa Equipe B", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def1.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def2A)
png('Desen.Grup_D2A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def2.A[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 2 - Defesa Equipe A", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def2.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def2B)
png('Desen.Grup_D2B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def2.B[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 2 - Defesa Equipe B", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def2.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def3A)
png('Desen.Grup_D3A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def3.A[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 3 - Defesa Equipe A", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def3.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def3B)
png('Desen.Grup_D3B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def3.B[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 3 - Defesa Equipe B", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def3.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def4A)
png('Desen.Grup_D4A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def4.A[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 4 - Defesa Equipe A", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def4.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def4B)
png('Desen.Grup_D4B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def4.B[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 4 - Defesa Equipe B", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def4.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def5A)
png('Desen.Grup_D5A.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def5.A[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 5 - Defesa Equipe A", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def5.A$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()

corg <- cor.des(cl_def5B)
png('Desen.Grup_D5B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def5.B[,5:11], pch=16, col=corg, cex=0.6, main = "Jogo 5 - Defesa Equipe B", oma=c(3,3,5,11),
      labels = c("Centroide X","Centroide Y","Bola X","Bola Y","Desloc. \nCentroide",
                 "Desloc. \nBola","Área"))
par(xpd = TRUE)
legend("right",legend = c(levels(as.factor(def5.B$classif))),  
       fill=cor5,cex = 0.5, pt.cex = 0.2, title = "Grupos")
dev.off()


ggplot(atq1.A, aes(x = area.Atq,y = des.centAtq, color = as.factor(classif))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  ggtitle("Jogo 1 - Ataque Equipe A") +
  labs(x = "Área Média do Ataque (m²)", y = "Deslocamento do Centroide no Ataque (m)", color = "Grupo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('GA1A_area_descen.png', plot = last_plot())

ggplot(atq1.B, aes(x = area.Atq,y = des.bola, color = as.factor(classif))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  ggtitle("Jogo 1 - Ataque Equipe B") +
  labs(x = "Área Média do Ataque (m²)", y = "Deslocamento da Bola no Ataque (m)", color = "Grupo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('GA1B_area_desbola.png', plot = last_plot())


png('Desen.Grup_D1B.png', width=7*ppi, height=6*ppi, res=ppi)
pairs(def1.B[,2:11], pch=16, col=corg, cex=0.6, main = "Jogo 1 - Defesa Equipe B")
dev.off()

ggplot(def1.A, aes(x = area.Def,y = des.centDef, color = as.factor(classif))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  ggtitle("Jogo 1 - Defesa Equipe A") +
  labs(x = "Área Média do Ataque (m²)", y = "Deslocamento do Centroide no Ataque (m)", color = "Grupo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('GD1A_area_descent.png', plot = last_plot())

ggplot(def1.B, aes(x = bolax,y = bolay, color = as.factor(classif))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  ggtitle("Jogo 1 - Defesa Equipe B") +
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
                   grupo = c(compar1.A$Atq, compar1.A$Def))

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

ggplot(l.1B,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 1 - Ataque Time B vs Defesa Time A") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A1BvD1A.png', plot = last_plot())

l.2A <- data.frame(ataque = c(compar2.A$ataque,compar2.A$ataque), 
                   time = c(rep("Atq", nrow(compar2.A)),rep("Def", nrow(compar2.A))), 
                   grupo = c(compar2.A$Atq, compar2.A$Def))

l.2B <- data.frame(ataque = c(compar2.B$ataque,compar2.B$ataque), 
                   time = c(rep("Atq", nrow(compar2.B)),rep("Def", nrow(compar2.B))), 
                   grupo = c(compar2.B$Atq, compar2.B$Def))

ggplot(l.2A,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 2 - Ataque Time A vs Defesa Time B") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A2AvD2B.png', plot = last_plot())

ggplot(l.2B,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 2 - Ataque Time B vs Defesa Time A") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A2BvD2A.png', plot = last_plot())

l.3A <- data.frame(ataque = c(compar3.A$ataque,compar3.A$ataque), 
                   time = c(rep("Atq", nrow(compar3.A)),rep("Def", nrow(compar3.A))), 
                   grupo = c(compar3.A$Atq, compar3.A$Def))

l.3B <- data.frame(ataque = c(compar3.B$ataque,compar3.B$ataque), 
                   time = c(rep("Atq", nrow(compar3.B)),rep("Def", nrow(compar3.B))), 
                   grupo = c(compar3.B$Atq, compar3.B$Def))

ggplot(l.3A,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 3 - Ataque Time A vs Defesa Time B") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A3AvD3B.png', plot = last_plot())

ggplot(l.3B,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 3 - Ataque Time B vs Defesa Time A") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A3BvD3A.png', plot = last_plot())

l.4A <- data.frame(ataque = c(compar4.A$ataque,compar4.A$ataque), 
                   time = c(rep("Atq", nrow(compar4.A)),rep("Def", nrow(compar4.A))), 
                   grupo = c(compar4.A$Atq, compar4.A$Def))

l.4B <- data.frame(ataque = c(compar4.B$ataque,compar4.B$ataque), 
                   time = c(rep("Atq", nrow(compar4.B)),rep("Def", nrow(compar4.B))), 
                   grupo = c(compar4.B$Atq, compar4.B$Def))

ggplot(l.4A,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 4 - Ataque Time A vs Defesa Time B") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A4AvD4B.png', plot = last_plot())

ggplot(l.4B,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 4 - Ataque Time B vs Defesa Time A") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A4BvD4A.png', plot = last_plot())

l.5A <- data.frame(ataque = c(compar5.A$ataque,compar5.A$ataque), 
                   time = c(rep("Atq", nrow(compar5.A)),rep("Def", nrow(compar5.A))), 
                   grupo = c(compar5.A$Atq, compar5.A$Def))

l.5B <- data.frame(ataque = c(compar5.B$ataque,compar5.B$ataque), 
                   time = c(rep("Atq", nrow(compar5.B)),rep("Def", nrow(compar5.B))), 
                   grupo = c(compar5.B$Atq, compar5.B$Def))

ggplot(l.5A,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 5 - Ataque Time A vs Defesa Time B") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A5AvD5B.png', plot = last_plot())

ggplot(l.5B,aes(x = ataque,y = as.numeric(grupo), color = time)) +
  geom_line(linetype = "longdash") +
  geom_point() +
  ggtitle("Jogo 5 - Ataque Time B vs Defesa Time A") +
  labs(x = "Nº Ataque", y = "Grupo", color = "Time") +
  scale_color_manual(values= cor5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('STG_A5BvD5A.png', plot = last_plot())
#################################BOXPLOT PRIMEIRA APRESENTAÇÃO####################################
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
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))
ggsave('G1_boxArea.png', plot = last_plot())

area <- c(c(jogo2.A$area.EA),c(jogo2.B$area.EB),c(jogo2.B$area.EA),c(jogo2.A$area.EB))
time <- c(rep("ATQ A",nrow(jogo2.A)),rep("ATQ B",nrow(jogo2.B)),rep("DEF A",nrow(jogo2.B)),
          rep("DEF B",nrow(jogo2.A)))
b2A <- data.frame(area,time)

ggplot(b2A, aes(x=time, y=area, fill = time)) +
  geom_boxplot() +
  scale_fill_manual(values=c("red", "blue", "red4", "midnightblue")) +
  ggtitle("Jogo 2 - Boxplot Área") +
  theme_bw() +
  labs(y = "Área")+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))
ggsave('G2_boxArea.png', plot = last_plot())

area <- c(c(jogo3.A$area.EA),c(jogo3.B$area.EB),c(jogo3.B$area.EA),c(jogo3.A$area.EB))
time <- c(rep("ATQ A",nrow(jogo3.A)),rep("ATQ B",nrow(jogo3.B)),rep("DEF A",nrow(jogo3.B)),
          rep("DEF B",nrow(jogo3.A)))
b3A <- data.frame(area,time)

ggplot(b3A, aes(x=time, y=area, fill = time)) +
  geom_boxplot() +
  scale_fill_manual(values=c("red", "blue", "red4", "midnightblue")) +
  ggtitle("Jogo 3 - Boxplot Área") +
  theme_bw() +
  labs(y = "Área")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))
ggsave('G3_boxArea.png', plot = last_plot())

area <- c(c(jogo4.A$area.EA),c(jogo4.B$area.EB),c(jogo4.B$area.EA),c(jogo4.A$area.EB))
time <- c(rep("ATQ A",nrow(jogo4.A)),rep("ATQ B",nrow(jogo4.B)),rep("DEF A",nrow(jogo4.B)),
          rep("DEF B",nrow(jogo4.A)))
b4A <- data.frame(area,time)

ggplot(b4A, aes(x=time, y=area, fill = time)) +
  geom_boxplot() +
  scale_fill_manual(values=c("red", "blue", "red4", "midnightblue")) +
  ggtitle("Jogo 4 - Boxplot Área") +
  theme_bw() +
  labs(y = "Área")+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))
ggsave('G4_boxArea.png', plot = last_plot())

area <- c(c(jogo5.A$area.EA),c(jogo5.B$area.EB),c(jogo5.B$area.EA),c(jogo5.A$area.EB))
time <- c(rep("ATQ A",nrow(jogo5.A)),rep("ATQ B",nrow(jogo5.B)),rep("DEF A",nrow(jogo5.B)),
          rep("DEF B",nrow(jogo5.A)))
b5A <- data.frame(area,time)

ggplot(b5A, aes(x=time, y=area, fill = time)) +
  geom_boxplot() +
  scale_fill_manual(values=c("red", "blue", "red4", "midnightblue")) +
  ggtitle("Jogo 5 - Boxplot Área") +
  theme_bw() +
  labs(y = "Área")+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))
ggsave('G5_boxArea.png', plot = last_plot())
