##################################################JOGO 1##########################################
l <- 2:9
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
d.def1B = dist(sc.defB[,c(3:9)])
agrup.def1B <- hclust(d.def1B, method = "ward.D2")


#Ata B / Def A

sc.atqB <- scale(atq1.B[,4:11])
sc.atqB <- cbind(atq1.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq1B = dist(sc.atqB[,2:9])
agrup.atq1B <- hclust(d.atq1B, method = "ward.D2")

sc.defA <- scale(def1.A[,c(4:11)])
sc.defA <- cbind(def1.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def1A = dist(sc.defA[,3:9])
agrup.def1A <- hclust(d.def1A, method = "ward.D2")

png('Dend_Atq1A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq1A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time A - Jogo 1")
dev.off()

png('Dend_Def1B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def1B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time B - Jogo 1")
dev.off()

png('Dend_Atq1B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq1B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time B - Jogo 1")
dev.off()

png('Dend_Def1A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def1A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time A - Jogo 1")
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
d.def2B = dist(sc.defB[,c(3:9)])
agrup.def2B <- hclust(d.def2B, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq2.B[,4:11])
sc.atqB <- cbind(atq2.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq2B = dist(sc.atqB[,2:9])
agrup.atq2B <- hclust(d.atq2B, method = "ward.D2")

sc.defA <- scale(def2.A[,c(4:11)])
sc.defA <- cbind(def2.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def2A = dist(sc.defA[,3:9])
agrup.def2A <- hclust(d.def2A, method = "ward.D2")

png('Dend_Atq2A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq2A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time A - Jogo 2")
dev.off()

png('Dend_Def2B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def2B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time B - Jogo 2")
dev.off()

png('Dend_Atq2B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq2B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time B - Jogo 2")
dev.off()

png('Dend_Def2A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def2A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time A - Jogo 2")
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
d.def3B = dist(sc.defB[,c(3:9)])
agrup.def3B <- hclust(d.def3B, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq1.B[,4:11])
sc.atqB <- cbind(atq1.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq3B = dist(sc.atqB[,2:9])
agrup.atq3B <- hclust(d.atq3B, method = "ward.D2")

sc.defA <- scale(def3.A[,c(4:11)])
sc.defA <- cbind(def3.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def3A = dist(sc.defA[,3:9])
agrup.def3A <- hclust(d.def3A, method = "ward.D2")

png('Dend_Atq3A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq3A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time A - Jogo 3")
dev.off()

png('Dend_Def3B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def3B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time B - Jogo 3")
dev.off()

png('Dend_Atq3B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq3B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time B - Jogo 3")
dev.off()

png('Dend_Def3A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def3A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time A - Jogo 3")
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
d.def4B = dist(sc.defB[,c(3:9)])
agrup.def4B <- hclust(d.def4B, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq4.B[,4:11])
sc.atqB <- cbind(atq4.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq4B = dist(sc.atqB[,2:9])
agrup.atq4B <- hclust(d.atq4B, method = "ward.D2")

sc.defA <- scale(def4.A[,c(4:11)])
sc.defA <- cbind(def4.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def4A = dist(sc.defA[,3:9])
agrup.def4A <- hclust(d.def4A, method = "ward.D2")

png('Dend_Atq4A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq4A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time A - Jogo 4")
dev.off()

png('Dend_Def4B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def4B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time B - Jogo 4")
dev.off()

png('Dend_Atq4B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq4B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time B - Jogo 4")
dev.off()

png('Dend_Def4A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def4A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time A - Jogo 4")
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
d.def5B = dist(sc.defB[,c(3:9)])
agrup.def5B <- hclust(d.def5B, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq5.B[,4:11])
sc.atqB <- cbind(atq5.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atq5B = dist(sc.atqB[,2:9])
agrup.atq5B <- hclust(d.atq5B, method = "ward.D2")

sc.defA <- scale(def5.A[,c(4:11)])
sc.defA <- cbind(def5.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.def5A = dist(sc.defA[,3:9])
agrup.def5A <- hclust(d.def5A, method = "ward.D2")

png('Dend_Atq5A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq5A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time A - Jogo 5")
dev.off()

png('Dend_Def5B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def5B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time B - Jogo 5")
dev.off()

png('Dend_Atq5B.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.atq5B, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Atq Time B - Jogo 5")
dev.off()

png('Dend_Def5A.png', width=7*ppi, height=6*ppi, res=ppi)
plot(agrup.def5A, xlab = "",ylab = "", hang = -1, cex = 0.6, main = "Dendograma Def Time A - Jogo 5")
dev.off()

rm("sc.atqA","sc.atqB","sc.defA","sc.defB")




