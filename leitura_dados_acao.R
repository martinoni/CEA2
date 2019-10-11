##################################################JOGO 1#########################################
library(sp)
library(adehabitatHR)
jogo1ime <- read.delim("jogo1acao.txt", header=TRUE)

area.EA <- c()
area.EB <- c()
for(i in 1:nrow(jogo1ime)){
  jogosub <- jogo1ime[i,]
  if(rowSums(is.na(jogosub)) > 0){
    for(j in seq(10,28,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(10,28,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(11,29,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(11,29,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(32,50,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(32,50,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(33,51,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(33,51,2)]), na.rm = TRUE)
      }
    }
    
  }
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
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100,"m","m2")
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}

jogo1ime <- cbind(jogo1ime,area.EA,area.EB)

ncentEAx <- c()
ncentEAy <- c()
ncentEBx <- c()
ncentEBy <- c()
for (i in 1:nrow(jogo1ime)){
  posAx <- as.numeric(jogo1ime[i,seq(10,28,2)])
  posAy <- as.numeric(jogo1ime[i,seq(11,29,2)])
  posBx <- as.numeric(jogo1ime[i,seq(32,50,2)])
  posBy <- as.numeric(jogo1ime[i,seq(33,51,2)])
  ncentEAx <- c(ncentEAx,mean(posAx, na.rm = TRUE))
  ncentEAy <- c(ncentEAy,mean(posAy, na.rm = TRUE))
  ncentEBx <- c(ncentEBx,mean(posBx, na.rm = TRUE))
  ncentEBy <- c(ncentEBy,mean(posBy, na.rm = TRUE))
}
jogo1ime <- cbind(jogo1ime,ncentEAx,ncentEAy,ncentEBx,ncentEBy)

jogo1 <- jogo1ime

jogo1.A <- subset(jogo1, posse == "EA")
jogo1.B <- subset(jogo1, posse == "EB")

##################################################JOGO 2##########################################

jogo1ime <- read.delim("jogo2acao.txt", header=TRUE)

area.EA <- c()
area.EB <- c()
for(i in 1:nrow(jogo1ime)){
  jogosub <- jogo1ime[i,]
  if(rowSums(is.na(jogosub)) > 0){
    for(j in seq(10,28,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(10,28,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(11,29,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(11,29,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(32,50,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(32,50,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(33,51,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(33,51,2)]), na.rm = TRUE)
      }
    }
    
  }
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
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100,"m","m2")
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}

jogo1ime <- cbind(jogo1ime,area.EA,area.EB)

ncentEAx <- c()
ncentEAy <- c()
ncentEBx <- c()
ncentEBy <- c()
for (i in 1:nrow(jogo1ime)){
  posAx <- as.numeric(jogo1ime[i,seq(10,28,2)])
  posAy <- as.numeric(jogo1ime[i,seq(11,29,2)])
  posBx <- as.numeric(jogo1ime[i,seq(32,50,2)])
  posBy <- as.numeric(jogo1ime[i,seq(33,51,2)])
  ncentEAx <- c(ncentEAx,mean(posAx, na.rm = TRUE))
  ncentEAy <- c(ncentEAy,mean(posAy, na.rm = TRUE))
  ncentEBx <- c(ncentEBx,mean(posBx, na.rm = TRUE))
  ncentEBy <- c(ncentEBy,mean(posBy, na.rm = TRUE))
}
jogo1ime <- cbind(jogo1ime,ncentEAx,ncentEAy,ncentEBx,ncentEBy)

jogo2 <- jogo1ime

jogo2.A <- subset(jogo2, posse == "EA")
jogo2.B <- subset(jogo2, posse == "EB")

##################################################JOGO 3##########################################

jogo1ime <- read.delim("jogo3acao.txt", header=TRUE)

area.EA <- c()
area.EB <- c()
for(i in 1:nrow(jogo1ime)){
  jogosub <- jogo1ime[i,]
  if(rowSums(is.na(jogosub)) > 0){
    for(j in seq(10,28,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(10,28,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(11,29,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(11,29,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(32,50,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(32,50,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(33,51,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(33,51,2)]), na.rm = TRUE)
      }
    }
    
  }
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
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100,"m","m2")
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}

jogo1ime <- cbind(jogo1ime,area.EA,area.EB)

ncentEAx <- c()
ncentEAy <- c()
ncentEBx <- c()
ncentEBy <- c()
for (i in 1:nrow(jogo1ime)){
  posAx <- as.numeric(jogo1ime[i,seq(10,28,2)])
  posAy <- as.numeric(jogo1ime[i,seq(11,29,2)])
  posBx <- as.numeric(jogo1ime[i,seq(32,50,2)])
  posBy <- as.numeric(jogo1ime[i,seq(33,51,2)])
  ncentEAx <- c(ncentEAx,mean(posAx, na.rm = TRUE))
  ncentEAy <- c(ncentEAy,mean(posAy, na.rm = TRUE))
  ncentEBx <- c(ncentEBx,mean(posBx, na.rm = TRUE))
  ncentEBy <- c(ncentEBy,mean(posBy, na.rm = TRUE))
}
jogo1ime <- cbind(jogo1ime,ncentEAx,ncentEAy,ncentEBx,ncentEBy)

jogo3 <- jogo1ime

jogo3.A <- subset(jogo3, posse == "EA")
jogo3.B <- subset(jogo3, posse == "EB")

##################################################JOGO 4##########################################

jogo1ime <- read.delim("jogo4acao.txt", header=TRUE)

area.EA <- c()
area.EB <- c()
for(i in 1:nrow(jogo1ime)){
  jogosub <- jogo1ime[i,]
  if(rowSums(is.na(jogosub)) > 0){
    for(j in seq(10,28,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(10,28,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(11,29,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(11,29,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(32,50,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(32,50,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(33,51,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(33,51,2)]), na.rm = TRUE)
      }
    }
    
  }
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
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100,"m","m2")
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}

jogo1ime <- cbind(jogo1ime,area.EA,area.EB)

ncentEAx <- c()
ncentEAy <- c()
ncentEBx <- c()
ncentEBy <- c()
for (i in 1:nrow(jogo1ime)){
  posAx <- as.numeric(jogo1ime[i,seq(10,28,2)])
  posAy <- as.numeric(jogo1ime[i,seq(11,29,2)])
  posBx <- as.numeric(jogo1ime[i,seq(32,50,2)])
  posBy <- as.numeric(jogo1ime[i,seq(33,51,2)])
  ncentEAx <- c(ncentEAx,mean(posAx, na.rm = TRUE))
  ncentEAy <- c(ncentEAy,mean(posAy, na.rm = TRUE))
  ncentEBx <- c(ncentEBx,mean(posBx, na.rm = TRUE))
  ncentEBy <- c(ncentEBy,mean(posBy, na.rm = TRUE))
}
jogo1ime <- cbind(jogo1ime,ncentEAx,ncentEAy,ncentEBx,ncentEBy)

jogo4 <- jogo1ime

jogo4.A <- subset(jogo4, posse == "EA")
jogo4.B <- subset(jogo4, posse == "EB")

##################################################JOGO 5##########################################

jogo1ime <- read.delim("jogo5acao.txt", header=TRUE)

area.EA <- c()
area.EB <- c()
for(i in 1:nrow(jogo1ime)){
  jogosub <- jogo1ime[i,]
  if(rowSums(is.na(jogosub)) > 0){
    for(j in seq(10,28,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(10,28,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(11,29,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(11,29,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(32,50,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(32,50,2)]), na.rm = TRUE)
      }
    }
    for(j in seq(33,51,2)){
      if(is.na(jogosub[,j])){
        jogosub[,j] <- mean(as.numeric(jogosub[,seq(33,51,2)]), na.rm = TRUE)
      }
    }
    
  }
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
  
  jogo.mcp <- mcp(jogoimp.coor, percent = 100,"m","m2")
  area.EA <- c(area.EA,jogo.mcp$area[1])
  area.EB <- c(area.EB,jogo.mcp$area[2])
}

jogo1ime <- cbind(jogo1ime,area.EA,area.EB)

ncentEAx <- c()
ncentEAy <- c()
ncentEBx <- c()
ncentEBy <- c()
for (i in 1:nrow(jogo1ime)){
  posAx <- as.numeric(jogo1ime[i,seq(10,28,2)])
  posAy <- as.numeric(jogo1ime[i,seq(11,29,2)])
  posBx <- as.numeric(jogo1ime[i,seq(32,50,2)])
  posBy <- as.numeric(jogo1ime[i,seq(33,51,2)])
  ncentEAx <- c(ncentEAx,mean(posAx, na.rm = TRUE))
  ncentEAy <- c(ncentEAy,mean(posAy, na.rm = TRUE))
  ncentEBx <- c(ncentEBx,mean(posBx, na.rm = TRUE))
  ncentEBy <- c(ncentEBy,mean(posBy, na.rm = TRUE))
}
jogo1ime <- cbind(jogo1ime,ncentEAx,ncentEAy,ncentEBx,ncentEBy)

jogo5 <- jogo1ime

jogo5.A <- subset(jogo5, posse == "EA")
jogo5.B <- subset(jogo5, posse == "EB")

for (i in 29:nrow(jogo5.A)){
  jogo5.A$ataque[i] <- jogo5.A$ataque[i] - 1
}

rm("jogo.mcp","jogo1ime","jogoimp","jogoimp.coor","jogosub","area.EA","area.EB","ncentEAx",
   "ncentEAy","ncentEBx","ncentEBy","noacao","pos.x","pos.y","posAx","posAy","posBx","posBy")