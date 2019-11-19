man.atq1A <- manova(cbind(ac_d, area.Atq) ~ as.factor(cl_atq1A), data = atq1.A)

man.atq1A <- manova(cbind(bolax, bolay) ~ as.factor(cl_atq1A), data = atq1.A)
summary(man.atq1A)
man.atq1A$coefficients
man.atq1A$residuals
sqd <- sum((man.atq1A$residuals[,1]^2)+(man.atq1A$residuals[,2]^2))
summary.aov(man.atq1A)

View(m.atq1A)

sqdx <- 0
sqdy <- 0
sqtx <- 0
sqty <- 0
for (i in 1:nrow(atq1.A)){
  grupo <- atq1.A$cl_atq1A[i]
  sqdx <- sqdx + (atq1.A$bolax[i]-m.atq1A$bolax[grupo])^2
  sqdy <- sqdy + (atq1.A$bolay[i]-m.atq1A$bolay[grupo])^2
  sqtx <- sqtx + (atq1.A$bolax[i]-mean(atq1.A$bolax))^2
  sqty <- sqty + (atq1.A$bolay[i]-mean(atq1.A$bolay))^2
}

i <- 1
5920.3+9764.4


##ROLÊ OFICIAL

razao_sq_atq <- function(atq,m.atq){
  sqd1 <- 0
  sqd2 <- 0
  sqd3 <- 0
  sqd4 <- 0
  sqd5 <- 0
  sqd6 <- 0
  sqd7 <- 0
  sqd8 <- 0
  sqt1 <- 0
  sqt2 <- 0
  sqt3 <- 0
  sqt4 <- 0
  sqt5 <- 0
  sqt6 <- 0
  sqt7 <- 0
  sqt8 <- 0
  for (i in 1:nrow(atq)){
    grupo <- atq$classif[i]
    sqd1 <- sqd1 + (atq$ac_d[i]-m.atq$ac_d[grupo])^2
    sqt1 <- sqt1 + (atq$ac_d[i]-mean(atq$ac_d))^2
    
    sqd2 <- sqd2 + (atq$centAtqx[i]-m.atq$centAtqx[grupo])^2
    sqt2 <- sqt2 + (atq$centAtqx[i]-mean(atq$centAtqx))^2
    
    sqd3 <- sqd3 + (atq$centAtqy[i]-m.atq$centAtqy[grupo])^2
    sqt3 <- sqt3 + (atq$centAtqy[i]-mean(atq$centAtqy))^2
    
    sqd4 <- sqd4 + (atq$bolax[i]-m.atq$bolax[grupo])^2
    sqt4 <- sqt4 + (atq$bolax[i]-mean(atq$bolax))^2
    
    sqd5 <- sqd5 + (atq$bolay[i]-m.atq$bolay[grupo])^2
    sqt5 <- sqt5 + (atq$bolay[i]-mean(atq$bolay))^2
    
    sqd6 <- sqd6 + (atq$des.centAtq[i]-m.atq$des.centAtq[grupo])^2
    sqt6 <- sqt6 + (atq$des.centAtq[i]-mean(atq$des.centAtq))^2
    
    sqd7 <- sqd7 + (atq$des.bola[i]-m.atq$des.bola[grupo])^2
    sqt7 <- sqt7 + (atq$des.bola[i]-mean(atq$des.bola))^2
    
    sqd8 <- sqd8 + (atq$area.Atq[i]-m.atq$area.Atq[grupo])^2
    sqt8 <- sqt8 + (atq$area.Atq[i]-mean(atq$area.Atq))^2
  }
  raz <- c(sqd1/sqt1,sqd2/sqt2,sqd3/sqt3,sqd4/sqt4,sqd5/sqt5,sqd6/sqt6,sqd7/sqt7,sqd8/sqt8)
  nomes <- colnames(atq1.A)[4:11]
  razf <- data.frame(nomes,raz)
  return(razf)
}

razao_sq_atq2 <- function(atq,m.atq){
  sqd1 <- 0
  sqd2 <- 0
  sqd3 <- 0
  sqd4 <- 0
  sqd5 <- 0
  sqd6 <- 0
  sqd7 <- 0
  sqd8 <- 0
  sqt1 <- 0
  sqt2 <- 0
  sqt3 <- 0
  sqt4 <- 0
  sqt5 <- 0
  sqt6 <- 0
  sqt7 <- 0
  sqt8 <- 0
  for (i in 1:nrow(atq)){
    grupo <- atq$classif[i]
    sqd1 <- sqd1 + (atq$ac_d[i]-m.atq$ac_d[grupo])^2
    sqt1 <- sqt1 + (atq$ac_d[i]-mean(atq$ac_d))^2
    
    sqd2 <- sqd2 + (atq$centAtqx[i]-m.atq$centAtqx[grupo])^2
    sqt2 <- sqt2 + (atq$centAtqx[i]-mean(atq$centAtqx))^2
    
    sqd3 <- sqd3 + (atq$centAtqy[i]-m.atq$centAtqy[grupo])^2
    sqt3 <- sqt3 + (atq$centAtqy[i]-mean(atq$centAtqy))^2
    
    sqd4 <- sqd4 + (atq$bolax[i]-m.atq$bolax[grupo])^2
    sqt4 <- sqt4 + (atq$bolax[i]-mean(atq$bolax))^2
    
    sqd5 <- sqd5 + (atq$bolay[i]-m.atq$bolay[grupo])^2
    sqt5 <- sqt5 + (atq$bolay[i]-mean(atq$bolay))^2
    
    sqd6 <- sqd6 + (atq$des.centAtq[i]-m.atq$des.centAtq[grupo])^2
    sqt6 <- sqt6 + (atq$des.centAtq[i]-mean(atq$des.centAtq))^2
    
    sqd7 <- sqd7 + (atq$des.bola[i]-m.atq$des.bola[grupo])^2
    sqt7 <- sqt7 + (atq$des.bola[i]-mean(atq$des.bola))^2
    
    sqd8 <- sqd8 + (atq$area.Atq[i]-m.atq$area.Atq[grupo])^2
    sqt8 <- sqt8 + (atq$area.Atq[i]-mean(atq$area.Atq))^2
  }
  raz2 <- c((sqd1/sqt1),(sqd1+sqd2)/(sqt1+sqt2),(sqd1+sqd3)/(sqt1+sqt3),(sqd1+sqd4)/(sqt1+sqt4),
            (sqd1+sqd5)/(sqt1+sqt5),(sqd1+sqd6)/(sqt1+sqt6),(sqd1+sqd7)/(sqt1+sqt7),
            (sqd1+sqd8)/(sqt1+sqt8),1,(sqd2/sqt2),(sqd2+sqd3)/(sqt2+sqt3),(sqd2+sqd4)/(sqt2+sqt4),
            (sqd2+sqd5)/(sqt2+sqt5),(sqd2+sqd6)/(sqt2+sqt6),(sqd2+sqd7)/(sqt2+sqt7),
            (sqd2+sqd8)/(sqt2+sqt8),1,1,(sqd3/sqt3),(sqd3+sqd4)/(sqt3+sqt4), (sqd3+sqd5)/(sqt3+sqt5),
            (sqd3+sqd6)/(sqt3+sqt6),(sqd3+sqd7)/(sqt3+sqt7),(sqd3+sqd8)/(sqt3+sqt8),1,1,1,
            (sqd4/sqt4), (sqd4+sqd5)/(sqt4+sqt5),(sqd4+sqd6)/(sqt4+sqt6),(sqd4+sqd7)/(sqt4+sqt7),
            (sqd4+sqd8)/(sqt4+sqt8),1,1,1,1,(sqd5/sqt5),(sqd5+sqd6)/(sqt5+sqt6),(sqd5+sqd7)/(sqt5+sqt7),
            (sqd5+sqd8)/(sqt5+sqt8),1,1,1,1,1,(sqd6/sqt6),(sqd6+sqd7)/(sqt6+sqt7),(sqd6+sqd8)/(sqt6+sqt8),
            1,1,1,1,1,1,(sqd7/sqt7),(sqd7+sqd8)/(sqt7+sqt8),1,1,1,1,1,1,1,(sqd8/sqt8))
  raz2 <- 1 - raz2
  mraz <- matrix(raz2, ncol = 8)
  colnames(mraz) <- colnames(atq)[4:11]
  rownames(mraz) <- colnames(atq)[4:11]
  return(mraz)
}


razao_sq_atq_pad2 <- function(atq,m.atq){
  atq[,2:11] <- scale(atq[2:11])
  m.atq <- aggregate(atq[,2:11], list(atq$classif), mean)
  sqd1 <- 0
  sqd2 <- 0
  sqd3 <- 0
  sqd4 <- 0
  sqd5 <- 0
  sqd6 <- 0
  sqd7 <- 0
  sqd8 <- 0
  sqt1 <- 0
  sqt2 <- 0
  sqt3 <- 0
  sqt4 <- 0
  sqt5 <- 0
  sqt6 <- 0
  sqt7 <- 0
  sqt8 <- 0
  for (i in 1:nrow(atq)){
    grupo <- atq$classif[i]
    sqd1 <- sqd1 + (atq$ac_d[i]-m.atq$ac_d[grupo])^2
    sqt1 <- sqt1 + (atq$ac_d[i]-mean(atq$ac_d))^2
    
    sqd2 <- sqd2 + (atq$centAtqx[i]-m.atq$centAtqx[grupo])^2
    sqt2 <- sqt2 + (atq$centAtqx[i]-mean(atq$centAtqx))^2
    
    sqd3 <- sqd3 + (atq$centAtqy[i]-m.atq$centAtqy[grupo])^2
    sqt3 <- sqt3 + (atq$centAtqy[i]-mean(atq$centAtqy))^2
    
    sqd4 <- sqd4 + (atq$bolax[i]-m.atq$bolax[grupo])^2
    sqt4 <- sqt4 + (atq$bolax[i]-mean(atq$bolax))^2
    
    sqd5 <- sqd5 + (atq$bolay[i]-m.atq$bolay[grupo])^2
    sqt5 <- sqt5 + (atq$bolay[i]-mean(atq$bolay))^2
    
    sqd6 <- sqd6 + (atq$des.centAtq[i]-m.atq$des.centAtq[grupo])^2
    sqt6 <- sqt6 + (atq$des.centAtq[i]-mean(atq$des.centAtq))^2
    
    sqd7 <- sqd7 + (atq$des.bola[i]-m.atq$des.bola[grupo])^2
    sqt7 <- sqt7 + (atq$des.bola[i]-mean(atq$des.bola))^2
    
    sqd8 <- sqd8 + (atq$area.Atq[i]-m.atq$area.Atq[grupo])^2
    sqt8 <- sqt8 + (atq$area.Atq[i]-mean(atq$area.Atq))^2
  }
  raz2 <- c((sqd1/sqt1),(sqd1+sqd2)/(sqt1+sqt2),(sqd1+sqd3)/(sqt1+sqt3),(sqd1+sqd4)/(sqt1+sqt4),
            (sqd1+sqd5)/(sqt1+sqt5),(sqd1+sqd6)/(sqt1+sqt6),(sqd1+sqd7)/(sqt1+sqt7),
            (sqd1+sqd8)/(sqt1+sqt8),1,(sqd2/sqt2),(sqd2+sqd3)/(sqt2+sqt3),(sqd2+sqd4)/(sqt2+sqt4),
            (sqd2+sqd5)/(sqt2+sqt5),(sqd2+sqd6)/(sqt2+sqt6),(sqd2+sqd7)/(sqt2+sqt7),
            (sqd2+sqd8)/(sqt2+sqt8),1,1,(sqd3/sqt3),(sqd3+sqd4)/(sqt3+sqt4), (sqd3+sqd5)/(sqt3+sqt5),
            (sqd3+sqd6)/(sqt3+sqt6),(sqd3+sqd7)/(sqt3+sqt7),(sqd3+sqd8)/(sqt3+sqt8),1,1,1,
            (sqd4/sqt4), (sqd4+sqd5)/(sqt4+sqt5),(sqd4+sqd6)/(sqt4+sqt6),(sqd4+sqd7)/(sqt4+sqt7),
            (sqd4+sqd8)/(sqt4+sqt8),1,1,1,1,(sqd5/sqt5),(sqd5+sqd6)/(sqt5+sqt6),(sqd5+sqd7)/(sqt5+sqt7),
            (sqd5+sqd8)/(sqt5+sqt8),1,1,1,1,1,(sqd6/sqt6),(sqd6+sqd7)/(sqt6+sqt7),(sqd6+sqd8)/(sqt6+sqt8),
            1,1,1,1,1,1,(sqd7/sqt7),(sqd7+sqd8)/(sqt7+sqt8),1,1,1,1,1,1,1,(sqd8/sqt8))
  raz2 <- 1 - raz2
  mraz <- matrix(raz2, ncol = 8)
  colnames(mraz) <- colnames(atq)[4:11]
  rownames(mraz) <- colnames(atq)[4:11]
  return(mraz)
}

razao_sq_def_pad2 <- function(def,m.def){
  def[,2:11] <- scale(def[2:11])
  m.def <- aggregate(def[,2:11], list(def$classif), mean)
  sqd1 <- 0
  sqd2 <- 0
  sqd3 <- 0
  sqd4 <- 0
  sqd5 <- 0
  sqd6 <- 0
  sqd7 <- 0
  sqd8 <- 0
  sqt1 <- 0
  sqt2 <- 0
  sqt3 <- 0
  sqt4 <- 0
  sqt5 <- 0
  sqt6 <- 0
  sqt7 <- 0
  sqt8 <- 0
  for (i in 1:nrow(def)){
    grupo <- def$classif[i]
    
    sqd2 <- sqd2 + (def$centDefx[i]-m.def$centDefx[grupo])^2
    sqt2 <- sqt2 + (def$centDefx[i]-mean(def$centDefx))^2
    
    sqd3 <- sqd3 + (def$centDefy[i]-m.def$centDefy[grupo])^2
    sqt3 <- sqt3 + (def$centDefy[i]-mean(def$centDefy))^2
    
    sqd4 <- sqd4 + (def$bolax[i]-m.def$bolax[grupo])^2
    sqt4 <- sqt4 + (def$bolax[i]-mean(def$bolax))^2
    
    sqd5 <- sqd5 + (def$bolay[i]-m.def$bolay[grupo])^2
    sqt5 <- sqt5 + (def$bolay[i]-mean(def$bolay))^2
    
    sqd6 <- sqd6 + (def$des.centDef[i]-m.def$des.centDef[grupo])^2
    sqt6 <- sqt6 + (def$des.centDef[i]-mean(def$des.centDef))^2
    
    sqd7 <- sqd7 + (def$des.bola[i]-m.def$des.bola[grupo])^2
    sqt7 <- sqt7 + (def$des.bola[i]-mean(def$des.bola))^2
    
    sqd8 <- sqd8 + (def$area.Def[i]-m.def$area.Def[grupo])^2
    sqt8 <- sqt8 + (def$area.Def[i]-mean(def$area.Def))^2
  }
  raz2 <- c((sqd2/sqt2),(sqd2+sqd3)/(sqt2+sqt3),(sqd2+sqd4)/(sqt2+sqt4),
            (sqd2+sqd5)/(sqt2+sqt5),(sqd2+sqd6)/(sqt2+sqt6),(sqd2+sqd7)/(sqt2+sqt7),
            (sqd2+sqd8)/(sqt2+sqt8),1,(sqd3/sqt3),(sqd3+sqd4)/(sqt3+sqt4), (sqd3+sqd5)/(sqt3+sqt5),
            (sqd3+sqd6)/(sqt3+sqt6),(sqd3+sqd7)/(sqt3+sqt7),(sqd3+sqd8)/(sqt3+sqt8),1,1,
            (sqd4/sqt4), (sqd4+sqd5)/(sqt4+sqt5),(sqd4+sqd6)/(sqt4+sqt6),(sqd4+sqd7)/(sqt4+sqt7),
            (sqd4+sqd8)/(sqt4+sqt8),1,1,1,(sqd5/sqt5),(sqd5+sqd6)/(sqt5+sqt6),(sqd5+sqd7)/(sqt5+sqt7),
            (sqd5+sqd8)/(sqt5+sqt8),1,1,1,1,(sqd6/sqt6),(sqd6+sqd7)/(sqt6+sqt7),(sqd6+sqd8)/(sqt6+sqt8),
            1,1,1,1,1,(sqd7/sqt7),(sqd7+sqd8)/(sqt7+sqt8),1,1,1,1,1,1,(sqd8/sqt8))
  raz2 <- 1 - raz2
  mraz <- matrix(raz2, ncol = 7)
  colnames(mraz) <- colnames(def)[5:11]
  rownames(mraz) <- colnames(def)[5:11]
  return(mraz)
}


med.atq1A <- razao_sq_atq_pad2(atq1.A,m.atq1A)
med.atq1B <- razao_sq_atq_pad2(atq1.B,m.atq1B)
med.atq2A <- razao_sq_atq_pad2(atq2.A,m.atq2A)
med.atq2B <- razao_sq_atq_pad2(atq2.B,m.atq2B)
med.atq3A <- razao_sq_atq_pad2(atq3.A,m.atq3A)
med.atq3B <- razao_sq_atq_pad2(atq3.B,m.atq3B)
med.atq4A <- razao_sq_atq_pad2(atq4.A,m.atq4A)
med.atq4B <- razao_sq_atq_pad2(atq4.B,m.atq4B)
med.atq5A <- razao_sq_atq_pad2(atq5.A,m.atq5A)
med.atq5B <- razao_sq_atq_pad2(atq5.B,m.atq5B)

med.def1A <- razao_sq_def_pad2(def1.A,m.def1A)
med.def1B <- razao_sq_def_pad2(def1.B,m.def1B)
med.def2A <- razao_sq_def_pad2(def2.A,m.def2A)
med.def2B <- razao_sq_def_pad2(def2.B,m.def2B)
med.def3A <- razao_sq_def_pad2(def3.A,m.def3A)
med.def3B <- razao_sq_def_pad2(def3.B,m.def3B)
med.def4A <- razao_sq_def_pad2(def4.A,m.def4A)
med.def4B <- razao_sq_def_pad2(def4.B,m.def4B)
med.def5A <- razao_sq_def_pad2(def5.A,m.def5A)
med.def5B <- razao_sq_def_pad2(def5.B,m.def5B)


medB <- razao_sq_atq(atq1.B,m.atq1B)
med2B <- razao_sq_atq2(atq1.B,m.atq1B)
med3B <- razao_sq_atq_pad2(atq1.B,m.atq1B)
View(scale(atq1.A))

x <- matrix( 1:25, nrow=5, ncol=5)
x
library(gdata)
lowerTriangle(x)
