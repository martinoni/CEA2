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


med <- razao_sq_atq(atq1.A,m.atq1A)
med2 <- razao_sq_atq2(atq1.A,m.atq1A)
med3 <- razao_sq_atq_pad2(atq1.A,m.atq1A)

medB <- razao_sq_atq(atq1.B,m.atq1B)
med2B <- razao_sq_atq2(atq1.B,m.atq1B)
med3B <- razao_sq_atq_pad2(atq1.B,m.atq1B)
View(scale(atq1.A))

x <- matrix( 1:25, nrow=5, ncol=5)
x
library(gdata)
lowerTriangle(x)
