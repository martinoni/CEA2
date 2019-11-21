##################################################JOGO 1##########################################
atq1.A <- c()
def1.B <- c()
for (i in 1:jogo1.A$ataque[nrow(jogo1.A)]){
  atq <- subset(jogo1.A, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centAtqx <- mean(atq$ncentEAx)
  centAtqy <- mean(atq$ncentEAy)
  centDefx <- mean(atq$ncentEBx)
  centDefy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Atq <- mean(atq$area.EA)
  area.Def <- mean(atq$area.EB)
  des.centAtq <- 0
  des.centDef <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq1.A <- rbind(atq1.A,atq.atual)
  def1.B <- rbind(def1.B,def.atual)
}

atq1.B <- c()
def1.A <- c()
for (i in 1:jogo1.B$ataque[nrow(jogo1.B)]){
  atq <- subset(jogo1.B, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centDefx <- mean(atq$ncentEAx)
  centDefy <- mean(atq$ncentEAy)
  centAtqx <- mean(atq$ncentEBx)
  centAtqy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Def <- mean(atq$area.EA)
  area.Atq <- mean(atq$area.EB)
  des.centDef <- 0
  des.centAtq <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq1.B <- rbind(atq1.B,atq.atual)
  def1.A <- rbind(def1.A,def.atual)
}

##################################################JOGO 2#############################################

atq2.A <- c()
def2.B <- c()
for (i in 1:jogo2.A$ataque[nrow(jogo2.A)]){
  atq <- subset(jogo2.A, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centAtqx <- mean(atq$ncentEAx)
  centAtqy <- mean(atq$ncentEAy)
  centDefx <- mean(atq$ncentEBx)
  centDefy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Atq <- mean(atq$area.EA)
  area.Def <- mean(atq$area.EB)
  des.centAtq <- 0
  des.centDef <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq2.A <- rbind(atq2.A,atq.atual)
  def2.B <- rbind(def2.B,def.atual)
}

atq2.B <- c()
def2.A <- c()
for (i in 1:jogo2.B$ataque[nrow(jogo2.B)]){
  atq <- subset(jogo2.B, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centDefx <- mean(atq$ncentEAx)
  centDefy <- mean(atq$ncentEAy)
  centAtqx <- mean(atq$ncentEBx)
  centAtqy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Def <- mean(atq$area.EA)
  area.Atq <- mean(atq$area.EB)
  des.centDef <- 0
  des.centAtq <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq2.B <- rbind(atq2.B,atq.atual)
  def2.A <- rbind(def2.A,def.atual)
}

##################################################JOGO 3#############################################

atq3.A <- c()
def3.B <- c()
for (i in 1:jogo3.A$ataque[nrow(jogo3.A)]){
  atq <- subset(jogo3.A, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centAtqx <- mean(atq$ncentEAx)
  centAtqy <- mean(atq$ncentEAy)
  centDefx <- mean(atq$ncentEBx)
  centDefy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Atq <- mean(atq$area.EA)
  area.Def <- mean(atq$area.EB)
  des.centAtq <- 0
  des.centDef <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq3.A <- rbind(atq3.A,atq.atual)
  def3.B <- rbind(def3.B,def.atual)
}

atq3.B <- c()
def3.A <- c()
for (i in 1:jogo3.B$ataque[nrow(jogo3.B)]){
  atq <- subset(jogo3.B, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centDefx <- mean(atq$ncentEAx)
  centDefy <- mean(atq$ncentEAy)
  centAtqx <- mean(atq$ncentEBx)
  centAtqy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Def <- mean(atq$area.EA)
  area.Atq <- mean(atq$area.EB)
  des.centDef <- 0
  des.centAtq <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq3.B <- rbind(atq3.B,atq.atual)
  def3.A <- rbind(def3.A,def.atual)
}

##################################################JOGO 4#############################################

atq4.A <- c()
def4.B <- c()
for (i in 1:jogo4.A$ataque[nrow(jogo4.A)]){
  atq <- subset(jogo4.A, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centAtqx <- mean(atq$ncentEAx)
  centAtqy <- mean(atq$ncentEAy)
  centDefx <- mean(atq$ncentEBx)
  centDefy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Atq <- mean(atq$area.EA)
  area.Def <- mean(atq$area.EB)
  des.centAtq <- 0
  des.centDef <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq4.A <- rbind(atq4.A,atq.atual)
  def4.B <- rbind(def4.B,def.atual)
}

atq4.B <- c()
def4.A <- c()
for (i in 1:jogo4.B$ataque[nrow(jogo4.B)]){
  atq <- subset(jogo4.B, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centDefx <- mean(atq$ncentEAx)
  centDefy <- mean(atq$ncentEAy)
  centAtqx <- mean(atq$ncentEBx)
  centAtqy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Def <- mean(atq$area.EA)
  area.Atq <- mean(atq$area.EB)
  des.centDef <- 0
  des.centAtq <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq4.B <- rbind(atq4.B,atq.atual)
  def4.A <- rbind(def4.A,def.atual)
}

##################################################JOGO 5#############################################

atq5.A <- c()
def5.B <- c()
for (i in 1:jogo5.A$ataque[nrow(jogo5.A)]){
  atq <- subset(jogo5.A, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centAtqx <- mean(atq$ncentEAx)
  centAtqy <- mean(atq$ncentEAy)
  centDefx <- mean(atq$ncentEBx)
  centDefy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Atq <- mean(atq$area.EA)
  area.Def <- mean(atq$area.EB)
  des.centAtq <- 0
  des.centDef <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq5.A <- rbind(atq5.A,atq.atual)
  def5.B <- rbind(def5.B,def.atual)
}

atq5.B <- c()
def5.A <- c()
for (i in 1:jogo5.B$ataque[nrow(jogo5.B)]){
  atq <- subset(jogo5.B, ataque == i)
  ataque <- i
  duracao <- atq$duracao[1]
  qtde_acoes <- atq$qtde_acoes[1]
  ac_d <- atq$duracao[1]/atq$qtde_acoes[1]
  centDefx <- mean(atq$ncentEAx)
  centDefy <- mean(atq$ncentEAy)
  centAtqx <- mean(atq$ncentEBx)
  centAtqy <- mean(atq$ncentEBy)
  bolax <- mean(atq$bolax)
  bolay <- mean(atq$bolay)
  area.Def <- mean(atq$area.EA)
  area.Atq <- mean(atq$area.EB)
  des.centDef <- 0
  des.centAtq <- 0
  des.bola <- sqrt(((atq$bolax[nrow(atq)]-atq$bolax[1])^2)+((atq$bolay[nrow(atq)]-atq$bolay[1])^2))
  for (j in 1:(nrow(atq)-1)){
    des.centDef <- des.centDef + 
      sqrt(((atq$ncentEAx[j+1]-atq$ncentEAx[j])^2)+((atq$ncentEAy[j+1]-atq$ncentEAy[j])^2))
    des.centAtq <- des.centAtq + 
      sqrt(((atq$ncentEBx[j+1]-atq$ncentEBx[j])^2)+((atq$ncentEBy[j+1]-atq$ncentEBy[j])^2))
  }
  desfecho <- atq$desf_ataque[1]
  atq.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centAtqx,centAtqy,bolax,bolay,des.centAtq,des.bola,area.Atq,desfecho)
  def.atual <- data.frame(ataque,duracao,qtde_acoes,ac_d,centDefx,centDefy,bolax,bolay,des.centDef,des.bola,area.Def,desfecho)
  atq5.B <- rbind(atq5.B,atq.atual)
  def5.A <- rbind(def5.A,def.atual)
}

rm("atq","ataque","qtde_acoes","duracao","ac_d","centAtqx","centAtqy","centDefx","centDefy","bolax","bolay",
   "area.Atq","area.Def","des.bola","des.centAtq","des.centDef","def.atual","atq.atual","desfecho")