#NÃO RODAR ESSE CÓDIGO SEPARADAMENTE(VER AGRUPAMENTO.R)

##################################################JOGO 1##########################################
l <- 2:9
k <- 3:9
ppi = 300

#Ata A / Def B

sc.atqA <- scale(atq1.A[,c(4:11)])
sc.atqA <- cbind(atq1.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atq1A = dist(sc.atqA[,l])
agrup.atq1A <- hclust(d.atq1A, method = "ward.D2")

sc.defB <- scale(def1.B[,c(4:11)])
sc.defB <- cbind(def1.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.def1B = dist(sc.defB[,k])
agrup.def1B <- hclust(d.def1B, method = "ward.D2")


#Ata B / Def A

sc.atqB <- scale(atq1.B[,4:11])
sc.atqB <- cbind(atq1.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq1B = dist(sc.atqB[,l])
agrup.atq1B <- hclust(d.atq1B, method = "ward.D2")

sc.defA <- scale(def1.A[,c(4:11)])
sc.defA <- cbind(def1.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def1A = dist(sc.defA[,k])
agrup.def1A <- hclust(d.def1A, method = "ward.D2")

png('Dend_Atq1A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq1A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 1", sub="")
abline(h = 11.5)
abline(v= 22.5, col = "red")
abline(v= 44.5, col = "red")
dev.off()

png('Dend_Def1B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def1B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 1", sub="")
abline(h = 9)
abline(v= 17.5, col = "red")
abline(v= 41.5, col = "red")
abline(v= 56.5, col = "red")
dev.off()

png('Dend_Atq1B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq1B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 1", sub="")
abline(h = 9.5)
abline(v= 17.5, col = "red")
abline(v= 22.5, col = "red")
abline(v= 34.5, col = "red")
dev.off()

png('Dend_Def1A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def1A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 1", sub="")
abline(h = 9.5)
abline(v= 22.5, col = "red")
abline(v= 35.5, col = "red")
abline(v= 52.5, col = "red")
dev.off()
##################################################JOGO 2##########################################

#Ata A / Def B

sc.atqA <- scale(atq2.A[,c(4:11)])
sc.atqA <- cbind(atq2.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atq2A = dist(sc.atqA[,l])
agrup.atq2A <- hclust(d.atq2A, method = "ward.D2")

sc.defB <- scale(def2.B[,c(4:11)])
sc.defB <- cbind(def2.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.def2B = dist(sc.defB[,k])
agrup.def2B <- hclust(d.def2B, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq2.B[,4:11])
sc.atqB <- cbind(atq2.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq2B = dist(sc.atqB[,l])
agrup.atq2B <- hclust(d.atq2B, method = "ward.D2")

sc.defA <- scale(def2.A[,c(4:11)])
sc.defA <- cbind(def2.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def2A = dist(sc.defA[,k])
agrup.def2A <- hclust(d.def2A, method = "ward.D2")

png('Dend_Atq2A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq2A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 2", sub="")
abline(h = 9.5)
abline(v= 8.5, col = "red")
abline(v= 29.5, col = "red")
abline(v= 48.5, col = "red")
abline(v= 62.5, col = "red")
dev.off()

png('Dend_Def2B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def2B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 2", sub="")
abline(h = 8)
abline(v= 15.5, col = "red")
abline(v= 32.5, col = "red")
abline(v= 48.5, col = "red")
abline(v= 65.5, col = "red")
dev.off()

png('Dend_Atq2B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq2B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 2", sub="")
abline(h = 9.5)
abline(v= 16.5, col = "red")
abline(v= 30.5, col = "red")
abline(v= 51.5, col = "red")
abline(v= 68.5, col = "red")
dev.off()

png('Dend_Def2A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def2A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 2", sub="")
abline(h = 10)
abline(v= 13.5, col = "red")
abline(v= 48.5, col = "red")
abline(v= 66.5, col = "red")
dev.off()
##################################################JOGO 3##########################################

#Ata A / Def B

sc.atqA <- scale(atq3.A[,c(4:11)])
sc.atqA <- cbind(atq3.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atq3A = dist(sc.atqA[,l])
agrup.atq3A <- hclust(d.atq3A, method = "ward.D2")

sc.defB <- scale(def3.B[,c(4:11)])
sc.defB <- cbind(def3.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.def3B = dist(sc.defB[,k])
agrup.def3B <- hclust(d.def3B, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq3.B[,4:11])
sc.atqB <- cbind(atq3.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq3B = dist(sc.atqB[,l])
agrup.atq3B <- hclust(d.atq3B, method = "ward.D2")

sc.defA <- scale(def3.A[,c(4:11)])
sc.defA <- cbind(def3.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def3A = dist(sc.defA[,k])
agrup.def3A <- hclust(d.def3A, method = "ward.D2")

png('Dend_Atq3A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq3A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 3", sub="")
abline(h = 11)
abline(v= 17.5, col = "red")
abline(v= 33.5, col = "red")
dev.off()

png('Dend_Def3B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def3B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 3", sub="")
abline(h = 8)
abline(v= 11.5, col = "red")
abline(v= 29.5, col = "red")
abline(v= 51.5, col = "red")
dev.off()

png('Dend_Atq3B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq3B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 3", sub="")
abline(h = 7)
abline(v= 9.5, col = "red")
abline(v= 16.5, col = "red")
abline(v= 30.5, col = "red")
abline(v= 46.5, col = "red")
dev.off()

png('Dend_Def3A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def3A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 3", sub="")
abline(h = 8.5)
abline(v= 17.5, col = "red")
abline(v= 41.5, col = "red")
abline(v= 50.5, col = "red")
dev.off()
##################################################JOGO 4##########################################

#Ata A / Def B

sc.atqA <- scale(atq4.A[,c(4:11)])
sc.atqA <- cbind(atq4.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atq4A = dist(sc.atqA[,l])
agrup.atq4A <- hclust(d.atq4A, method = "ward.D2")

sc.defB <- scale(def4.B[,c(4:11)])
sc.defB <- cbind(def4.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.def4B = dist(sc.defB[,k])
agrup.def4B <- hclust(d.def4B, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq4.B[,4:11])
sc.atqB <- cbind(atq4.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq4B = dist(sc.atqB[,l])
agrup.atq4B <- hclust(d.atq4B, method = "ward.D2")

sc.defA <- scale(def4.A[,c(4:11)])
sc.defA <- cbind(def4.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def4A = dist(sc.defA[,k])
agrup.def4A <- hclust(d.def4A, method = "ward.D2")

png('Dend_Atq4A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq4A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 4", sub="")
abline(h = 8)
abline(v= 8.5, col = "red")
abline(v= 27.5, col = "red")
abline(v= 46.5, col = "red")
abline(v= 59.5, col = "red")
dev.off()

png('Dend_Def4B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def4B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 4", sub="")
abline(h = 9.25)
abline(v= 7.5, col = "red")
abline(v= 19.5, col = "red")
abline(v= 41.5, col = "red")
dev.off()

png('Dend_Atq4B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq4B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 4", sub="")
abline(h = 10.5)
abline(v= 32.5, col = "red")
abline(v= 57.5, col = "red")
dev.off()

png('Dend_Def4A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def4A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 4", sub="")
abline(h = 10)
abline(v= 31.5, col = "red")
abline(v= 53.5, col = "red")
dev.off()
##################################################JOGO 5##########################################

#Ata A / Def B

sc.atqA <- scale(atq5.A[,c(4:11)])
sc.atqA <- cbind(atq5.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atq5A = dist(sc.atqA[,l])
agrup.atq5A <- hclust(d.atq5A, method = "ward.D2")

sc.defB <- scale(def5.B[,c(4:11)])
sc.defB <- cbind(def5.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.def5B = dist(sc.defB[,k])
agrup.def5B <- hclust(d.def5B, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq5.B[,4:11])
sc.atqB <- cbind(atq5.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq5B = dist(sc.atqB[,l])
agrup.atq5B <- hclust(d.atq5B, method = "ward.D2")

sc.defA <- scale(def5.A[,c(4:11)])
sc.defA <- cbind(def5.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def5A = dist(sc.defA[,k])
agrup.def5A <- hclust(d.def5A, method = "ward.D2")

png('Dend_Atq5A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq5A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 5", sub="")
abline(h = 8)
abline(v= 8.5, col = "red")
abline(v= 33.5, col = "red")
abline(v= 47.5, col = "red")
abline(v= 60.5, col = "red")
dev.off()

png('Dend_Def5B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def5B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 5", sub="")
abline(h = 7.5)
abline(v= 6.5, col = "red")
abline(v= 20.5, col = "red")
abline(v= 39.5, col = "red")
abline(v= 46.5, col = "red")
dev.off()

png('Dend_Atq5B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq5B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 5", sub="")
abline(h = 8)
abline(v= 6.5, col = "red")
abline(v= 22.5, col = "red")
abline(v= 37.5, col = "red")
abline(v= 46.5, col = "red")
dev.off()

png('Dend_Def5A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def5A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 5", sub="")
abline(h = 8)
abline(v= 18.5, col = "red")
abline(v= 33.5, col = "red")
abline(v= 44.5, col = "red")
dev.off()

rm("sc.atqA","sc.atqB","sc.defA","sc.defB")


png('Dend_Jogo1.png', width=7*ppi, height=6*ppi, res=ppi)
par(mfrow=c(2,2))
plot(agrup.atq1A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 1", sub="")
abline(h = 11.5)
abline(v= 22.5, col = "red")
abline(v= 44.5, col = "red")

plot(agrup.def1B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 1", sub="")
abline(h = 9)
abline(v= 17.5, col = "red")
abline(v= 41.5, col = "red")
abline(v= 56.5, col = "red")

plot(agrup.atq1B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 1", sub="")
abline(h = 9.5)
abline(v= 17.5, col = "red")
abline(v= 22.5, col = "red")
abline(v= 34.5, col = "red")

plot(agrup.def1A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 1", sub="")
abline(h = 9.5)
abline(v= 22.5, col = "red")
abline(v= 35.5, col = "red")
abline(v= 52.5, col = "red")
dev.off()

png('Dend_Jogo2.png', width=7*ppi, height=6*ppi, res=ppi)
par(mfrow=c(2,2))
plot(agrup.atq2A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 2", sub="")
abline(h = 9.5)
abline(v= 8.5, col = "red")
abline(v= 29.5, col = "red")
abline(v= 48.5, col = "red")
abline(v= 62.5, col = "red")

plot(agrup.def2B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 2", sub="")
abline(h = 8)
abline(v= 15.5, col = "red")
abline(v= 32.5, col = "red")
abline(v= 48.5, col = "red")
abline(v= 65.5, col = "red")

plot(agrup.atq2B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 2", sub="")
abline(h = 9.5)
abline(v= 16.5, col = "red")
abline(v= 30.5, col = "red")
abline(v= 51.5, col = "red")
abline(v= 68.5, col = "red")

plot(agrup.def2A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 2", sub="")
abline(h = 10)
abline(v= 13.5, col = "red")
abline(v= 48.5, col = "red")
abline(v= 66.5, col = "red")
dev.off()

png('Dend_Jogo3.png', width=7*ppi, height=6*ppi, res=ppi)
par(mfrow=c(2,2))
plot(agrup.atq3A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 3", sub="")
abline(h = 11)
abline(v= 17.5, col = "red")
abline(v= 33.5, col = "red")

plot(agrup.def3B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 3", sub="")
abline(h = 8)
abline(v= 11.5, col = "red")
abline(v= 29.5, col = "red")
abline(v= 51.5, col = "red")

plot(agrup.atq3B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 3", sub="")
abline(h = 8)
abline(v= 9.5, col = "red")
abline(v= 16.5, col = "red")
abline(v= 30.5, col = "red")
abline(v= 46.5, col = "red")

plot(agrup.def3A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 3", sub="")
abline(h = 8.5)
abline(v= 17.5, col = "red")
abline(v= 41.5, col = "red")
abline(v= 50.5, col = "red")
dev.off()

png('Dend_Jogo4.png', width=7*ppi, height=6*ppi, res=ppi)
par(mfrow=c(2,2))
plot(agrup.atq4A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 4", sub="")
abline(h = 8)
abline(v= 8.5, col = "red")
abline(v= 27.5, col = "red")
abline(v= 46.5, col = "red")
abline(v= 59.5, col = "red")

plot(agrup.def4B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 4", sub="")
abline(h = 9.25)
abline(v= 7.5, col = "red")
abline(v= 19.5, col = "red")
abline(v= 41.5, col = "red")

plot(agrup.atq4B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 4", sub="")
abline(h = 10.5)
abline(v= 32.5, col = "red")
abline(v= 57.5, col = "red")

plot(agrup.def4A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 4", sub="")
abline(h = 10)
abline(v= 31.5, col = "red")
abline(v= 53.5, col = "red")
dev.off()

png('Dend_Jogo5.png', width=7*ppi, height=6*ppi, res=ppi)
par(mfrow=c(2,2))
plot(agrup.atq5A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe A - Jogo 5", sub="")
abline(h = 8)
abline(v= 8.5, col = "red")
abline(v= 33.5, col = "red")
abline(v= 47.5, col = "red")
abline(v= 60.5, col = "red")

plot(agrup.def5B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe B - Jogo 5", sub="")
abline(h = 7.5)
abline(v= 6.5, col = "red")
abline(v= 20.5, col = "red")
abline(v= 39.5, col = "red")
abline(v= 46.5, col = "red")

plot(agrup.atq5B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Ataque Equipe B - Jogo 5", sub="")
abline(h = 8)
abline(v= 6.5, col = "red")
abline(v= 22.5, col = "red")
abline(v= 37.5, col = "red")
abline(v= 46.5, col = "red")

plot(agrup.def5A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendrograma Defesa Equipe A - Jogo 5", sub="")
abline(h = 8)
abline(v= 18.5, col = "red")
abline(v= 33.5, col = "red")
abline(v= 44.5, col = "red")
dev.off()