##################################################JOGO 1##########################################
l <- 2:9

#Ata A / Def B

sc.atqA <- scale(atq1.A[,c(4:11)])
sc.atqA <- cbind(atq1.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atqA = dist(sc.atqA[,l])
agrup.ata1A <- hclust(d.atqA, method = "ward.D2")

sc.defB <- scale(def1.B[,c(4:11)])
sc.defB <- cbind(def1.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.defB = dist(sc.defB[,c(3:9)])
agrup.def1B <- hclust(d.defB, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq1.B[,4:11])
sc.atqB <- cbind(atq1.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atqB = dist(sc.atqB[,2:9])
agrup.atq1B <- hclust(d.atqB, method = "ward.D2")

sc.defA <- scale(def1.A[,c(4:11)])
sc.defA <- cbind(def1.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.defA = dist(sc.defA[,3:9])
agrup.def1A <- hclust(d.defA, method = "ward.D2")

##################################################JOGO 2##########################################

#Ata A / Def B

sc.atqA <- scale(atq2.A[,c(4:11)])
sc.atqA <- cbind(atq2.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atqA = dist(sc.atqA[,l])
agrup.ata2A <- hclust(d.atqA, method = "ward.D2")

sc.defB <- scale(def2.B[,c(4:11)])
sc.defB <- cbind(def2.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.defB = dist(sc.defB[,c(3:9)])
agrup.def2B <- hclust(d.defB, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq2.B[,4:11])
sc.atqB <- cbind(atq2.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atqB = dist(sc.atqB[,2:9])
agrup.atq2B <- hclust(d.atqB, method = "ward.D2")

sc.defA <- scale(def2.A[,c(4:11)])
sc.defA <- cbind(def2.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.defA = dist(sc.defA[,3:9])
agrup.def2A <- hclust(d.defA, method = "ward.D2")

##################################################JOGO 3##########################################

#Ata A / Def B

sc.atqA <- scale(atq3.A[,c(4:11)])
sc.atqA <- cbind(atq3.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atqA = dist(sc.atqA[,l])
agrup.ata3A <- hclust(d.atqA, method = "ward.D2")

sc.defB <- scale(def3.B[,c(4:11)])
sc.defB <- cbind(def3.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.defB = dist(sc.defB[,c(3:9)])
agrup.def3B <- hclust(d.defB, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq1.B[,4:11])
sc.atqB <- cbind(atq1.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atqB = dist(sc.atqB[,2:9])
agrup.atq1B <- hclust(d.atqB, method = "ward.D2")

sc.defA <- scale(def3.A[,c(4:11)])
sc.defA <- cbind(def3.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.defA = dist(sc.defA[,3:9])
agrup.def3A <- hclust(d.defA, method = "ward.D2")

##################################################JOGO 4##########################################

#Ata A / Def B

sc.atqA <- scale(atq4.A[,c(4:11)])
sc.atqA <- cbind(atq4.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atqA = dist(sc.atqA[,l])
agrup.ata4A <- hclust(d.atqA, method = "ward.D2")

sc.defB <- scale(def4.B[,c(4:11)])
sc.defB <- cbind(def4.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.defB = dist(sc.defB[,c(3:9)])
agrup.def4B <- hclust(d.defB, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq4.B[,4:11])
sc.atqB <- cbind(atq4.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atqB = dist(sc.atqB[,2:9])
agrup.atq4B <- hclust(d.atqB, method = "ward.D2")

sc.defA <- scale(def4.A[,c(4:11)])
sc.defA <- cbind(def4.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.defA = dist(sc.defA[,3:9])
agrup.def4A <- hclust(d.defA, method = "ward.D2")

##################################################JOGO 5##########################################

#Ata A / Def B

sc.atqA <- scale(atq5.A[,c(4:11)])
sc.atqA <- cbind(atq5.A$ataque,sc.atqA)
colnames(sc.atqA)[1] <- "ataque"
sc.atqA <- as.data.frame(sc.atqA)
d.atqA = dist(sc.atqA[,l])
agrup.ata5A <- hclust(d.atqA, method = "ward.D2")

sc.defB <- scale(def5.B[,c(4:11)])
sc.defB <- cbind(def5.B$ataque,sc.defB)
colnames(sc.defB)[1] <- "ataque"
sc.defB <- as.data.frame(sc.defB)
d.defB = dist(sc.defB[,c(3:9)])
agrup.def5B <- hclust(d.defB, method = "ward.D2")

#Ata B / Def A

sc.atqB <- scale(atq5.B[,4:11])
sc.atqB <- cbind(atq5.B$ataque,sc.atqB)
colnames(sc.atqB)[1] <- "ataque"
sc.atqB <- as.data.frame(sc.atqB)
d.atqB = dist(sc.atqB[,2:9])
agrup.atq5B <- hclust(d.atqB, method = "ward.D2")

sc.defA <- scale(def5.A[,c(4:11)])
sc.defA <- cbind(def5.A$ataque,sc.defA)
colnames(sc.defA)[1] <- "ataque"
sc.defA <- as.data.frame(sc.defA)
d.defA = dist(sc.defA[,3:9])
agrup.def5A <- hclust(d.defA, method = "ward.D2")