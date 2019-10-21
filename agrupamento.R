library(sp)
library(adehabitatHR)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("leitura_dados_acao.R")
source("resumo_macro.R")
source("cod_agrup.R")
setwd("~")

pairs(atq1.A[,c(4:11)], pch=16, col="navyblue", cex=0.6)
pairs(atq1.B[,2:11], pch=16, col="red4", cex=0.6)

c <- cor(atq1.A)
View(c)

###CLassificação dos ATQs e DEFs
cl_atq1A <- cutree(agrup.atq1A,3)
cl_atq1B <- cutree(agrup.atq1B,4)
cl_atq2A <- cutree(agrup.atq2A,5)
cl_atq2B <- cutree(agrup.atq2B,5)
cl_atq3A <- cutree(agrup.atq3A,3)
cl_atq3B <- cutree(agrup.atq3B,5)
cl_atq4A <- cutree(agrup.atq4A,5)
cl_atq4B <- cutree(agrup.atq4B,3)
cl_atq5A <- cutree(agrup.atq5A,5)
cl_atq5B <- cutree(agrup.atq5B,5)

cl_def1A <- cutree(agrup.def1A,4)
cl_def1B <- cutree(agrup.def1B,4)
cl_def2A <- cutree(agrup.def2A,4)
cl_def2B <- cutree(agrup.def2B,5)
cl_def3A <- cutree(agrup.def3A,4)
cl_def3B <- cutree(agrup.def3B,4)
cl_def4A <- cutree(agrup.def4A,3)
cl_def4B <- cutree(agrup.def4B,4)
cl_def5A <- cutree(agrup.def5A,4)
cl_def5B <- cutree(agrup.def5B,6)

####SILHUETA
library(cluster)
s.atq1A <- silhouette(cl_atq1A,d.atq1A)
s.atq1B <- silhouette(cl_atq1B,d.atq1B)
s.atq2A <- silhouette(cl_atq2A,d.atq2A)
s.atq2B <- silhouette(cl_atq2B,d.atq2B)
s.atq3A <- silhouette(cl_atq3A,d.atq3A)
s.atq3B <- silhouette(cl_atq3B,d.atq3B)
s.atq4A <- silhouette(cl_atq4A,d.atq4A)
s.atq4B <- silhouette(cl_atq4B,d.atq4B)
s.atq5A <- silhouette(cl_atq5A,d.atq5A)
s.atq5B <- silhouette(cl_atq5B,d.atq5B)

s.def1A <- silhouette(cl_def1A,d.def1A)
s.def1B <- silhouette(cl_def1B,d.def1B)
s.def2A <- silhouette(cl_def2A,d.def2A)
s.def2B <- silhouette(cl_def2B,d.def2B)
s.def3A <- silhouette(cl_def3A,d.def3A)
s.def3B <- silhouette(cl_def3B,d.def3B)
s.def4A <- silhouette(cl_def4A,d.def4A)
s.def4B <- silhouette(cl_def4B,d.def4B)
s.def5A <- silhouette(cl_def5A,d.def5A)
s.def5B <- silhouette(cl_def5B,d.def5B)

summary(s.atq1A)
summary(s.atq1B)
summary(s.atq2A)
summary(s.atq2B)
summary(s.atq3A)
summary(s.atq3B)
summary(s.atq4A)
summary(s.atq4B)
summary(s.atq5A)
summary(s.atq5B)
summary(s.def1A)
summary(s.def1B)
summary(s.def2A)
summary(s.def2B)
summary(s.def3A)
summary(s.def3B)
summary(s.def4A)
summary(s.def4B)
summary(s.def5A)
summary(s.def5B)

#Medidas descritivas grupos
atq1.A <- cbind(atq1.A,cl_atq1A)
m.atq1A <- aggregate(atq1.A[, 2:11], list(atq1.A$cl_atq1A), mean)
v.atq1A <- aggregate(atq1.A[, 2:11], list(atq1.A$cl_atq1A), sd)

atq1.B <- cbind(atq1.B,cl_atq1B)
m.atq1B <- aggregate(atq1.B[, 2:11], list(atq1.B$cl_atq1B), mean)
v.atq1B <- aggregate(atq1.B[, 2:11], list(atq1.B$cl_atq1B), sd)

atq2.A <- cbind(atq2.A,cl_atq2A)
m.atq2A <- aggregate(atq2.A[, 2:11], list(atq2.A$cl_atq2A), mean)
v.atq2A <- aggregate(atq2.A[, 2:11], list(atq2.A$cl_atq2A), sd)

atq2.B <- cbind(atq2.B,cl_atq2B)
m.atq2B <- aggregate(atq2.B[, 2:11], list(atq2.B$cl_atq2B), mean)
v.atq2B <- aggregate(atq2.B[, 2:11], list(atq2.B$cl_atq2B), sd)

atq3.A <- cbind(atq3.A,cl_atq3A)
m.atq3A <- aggregate(atq3.A[, 2:11], list(atq3.A$cl_atq3A), mean)
v.atq3A <- aggregate(atq3.A[, 2:11], list(atq3.A$cl_atq3A), sd)

atq3.B <- cbind(atq3.B,cl_atq3B)
m.atq3B <- aggregate(atq3.B[, 2:11], list(atq3.B$cl_atq3B), mean)
v.atq3B <- aggregate(atq3.B[, 2:11], list(atq3.B$cl_atq3B), sd)

atq4.A <- cbind(atq4.A,cl_atq4A)
m.atq4A <- aggregate(atq4.A[, 2:11], list(atq4.A$cl_atq4A), mean)
v.atq4A <- aggregate(atq4.A[, 2:11], list(atq4.A$cl_atq4A), sd)

atq4.B <- cbind(atq4.B,cl_atq4B)
m.atq4B <- aggregate(atq4.B[, 2:11], list(atq4.B$cl_atq4B), mean)
v.atq4B <- aggregate(atq4.B[, 2:11], list(atq4.B$cl_atq4B), sd)

atq5.A <- cbind(atq5.A,cl_atq5A)
m.atq5A <- aggregate(atq5.A[, 2:11], list(atq5.A$cl_atq5A), mean)
v.atq5A <- aggregate(atq5.A[, 2:11], list(atq5.A$cl_atq5A), sd)

atq5.B <- cbind(atq5.B,cl_atq5B)
m.atq5B <- aggregate(atq5.B[, 2:11], list(atq5.B$cl_atq5B), mean)
v.atq5B <- aggregate(atq5.B[, 2:11], list(atq5.B$cl_atq5B), sd)

def1.A <- cbind(def1.A,cl_def1A)
m.def1A <- aggregate(def1.A[, 2:11], list(def1.A$cl_def1A), mean)
v.def1A <- aggregate(def1.A[, 2:11], list(def1.A$cl_def1A), sd)

def1.B <- cbind(def1.B,cl_def1B)
m.def1B <- aggregate(def1.B[, 2:11], list(def1.B$cl_def1B), mean)
v.def1B <- aggregate(def1.B[, 2:11], list(def1.B$cl_def1B), sd)

def2.A <- cbind(def2.A,cl_def2A)
m.def2A <- aggregate(def2.A[, 2:11], list(def2.A$cl_def2A), mean)
v.def2A <- aggregate(def2.A[, 2:11], list(def2.A$cl_def2A), sd)

def2.B <- cbind(def2.B,cl_def2B)
m.def2B <- aggregate(def2.B[, 2:11], list(def2.B$cl_def2B), mean)
v.def2B <- aggregate(def2.B[, 2:11], list(def2.B$cl_def2B), sd)

def3.A <- cbind(def3.A,cl_def3A)
m.def3A <- aggregate(def3.A[, 2:11], list(def3.A$cl_def3A), mean)
v.def3A <- aggregate(def3.A[, 2:11], list(def3.A$cl_def3A), sd)

def3.B <- cbind(def3.B,cl_def3B)
m.def3B <- aggregate(def3.B[, 2:11], list(def3.B$cl_def3B), mean)
v.def3B <- aggregate(def3.B[, 2:11], list(def3.B$cl_def3B), sd)

def4.A <- cbind(def4.A,cl_def4A)
m.def4A <- aggregate(def4.A[, 2:11], list(def4.A$cl_def4A), mean)
v.def4A <- aggregate(def4.A[, 2:11], list(def4.A$cl_def4A), sd)

def4.B <- cbind(def4.B,cl_def4B)
m.def4B <- aggregate(def4.B[, 2:11], list(def4.B$cl_def4B), mean)
v.def4B <- aggregate(def4.B[, 2:11], list(def4.B$cl_def4B), sd)

def5.A <- cbind(def5.A,cl_def5A)
m.def5A <- aggregate(def5.A[, 2:11], list(def5.A$cl_def5A), mean)
v.def5A <- aggregate(def5.A[, 2:11], list(def5.A$cl_def5A), sd)

def5.B <- cbind(def5.B,cl_def5B)
m.def5B <- aggregate(def5.B[, 2:11], list(def5.B$cl_def5B), mean)
v.def5B <- aggregate(def5.B[, 2:11], list(def5.B$cl_def5B), sd)

#Matriz de frequecias ATQ vs DEF
compar1.A <- data.frame(atq1.A$ataque,as.factor(cl_atq1A),as.factor(cl_def1B))
colnames(compar1.A) <- c("ataque","Atq","Def")
teste1A <- xtabs(~Atq+Def, compar1.A)

compar1.B <- data.frame(atq1.B$ataque,as.factor(cl_atq1B),as.factor(cl_def1A))
colnames(compar1.B) <- c("ataque","Atq","Def")
teste1B <- xtabs(~Atq+Def, compar1.B)

compar2.A <- data.frame(atq2.A$ataque,as.factor(cl_atq2A),as.factor(cl_def2B))
colnames(compar2.A) <- c("ataque","Atq","Def")
teste2A <- xtabs(~Atq+Def, compar2.A)

compar2.B <- data.frame(atq2.B$ataque,as.factor(cl_atq2B),as.factor(cl_def2A))
colnames(compar2.B) <- c("ataque","Atq","Def")
teste2B <- xtabs(~Atq+Def, compar2.B)

compar3.A <- data.frame(atq3.A$ataque,as.factor(cl_atq3A),as.factor(cl_def3B))
colnames(compar3.A) <- c("ataque","Atq","Def")
teste3A <- xtabs(~Atq+Def, compar3.A)

compar3.B <- data.frame(atq3.B$ataque,as.factor(cl_atq3B),as.factor(cl_def3A))
colnames(compar3.B) <- c("ataque","Atq","Def")
teste3B <- xtabs(~Atq+Def, compar3.B)

compar4.A <- data.frame(atq4.A$ataque,as.factor(cl_atq4A),as.factor(cl_def4B))
colnames(compar4.A) <- c("ataque","Atq","Def")
teste4A <- xtabs(~Atq+Def, compar4.A)

compar4.B <- data.frame(atq4.B$ataque,as.factor(cl_atq4B),as.factor(cl_def4A))
colnames(compar4.B) <- c("ataque","Atq","Def")
teste4B <- xtabs(~Atq+Def, compar4.B)

compar5.A <- data.frame(atq5.A$ataque,as.factor(cl_atq5A),as.factor(cl_def5B))
colnames(compar5.A) <- c("ataque","Atq","Def")
teste5A <- xtabs(~Atq+Def, compar5.A)

compar5.B <- data.frame(atq5.B$ataque,as.factor(cl_atq5B),as.factor(cl_def5A))
colnames(compar5.B) <- c("ataque","Atq","Def")
teste5B <- xtabs(~Atq+Def, compar5.B)
