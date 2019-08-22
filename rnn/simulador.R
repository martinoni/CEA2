setwd('~/Development/IME/cea2/rnn')
library(magrittr)

jogo1ime <- read.csv("~/Development/IME/cea2/rnn/jogo_simulado.txt", header=FALSE)
colnames(jogo1ime) <- c("EA1x","EA1y","EA2x","EA2y",
                        "EA3x","EA3y","EA4x","EA4y",
                        "EA5x","EA5y","EA6x","EA6y",
                        "EA7x","EA7y","EA8x","EA8y",
                        "EA9x","EA9y","EA10x","EA10y",
                        "EA11x","EA11y","EB18x","EB18y",
                        "EB19x","EB19y","EB20x","EB20y",
                        "EB21x","EB21y","EB22x","EB22y",
                        "EB23x","EB23y","EB24x","EB24y",
                        "EB25x","EB25y","EB26x","EB26y",
                        "EB27x","EB27y","EB28x","EB28y",
                        "bolax","bolay")


# jogo1ime[,seq(1,95,2)] <- jogo1ime[,seq(1,95,2)]*105
# jogo1ime[,seq(2,95,2)] <- jogo1ime[,seq(2,95,2)]*68

attach(jogo1ime)


indices_times <- c(rep('1',11), rep('2', 11))

legenda_jogador <- function(xs, ys){
  for(jogador in 1:22){
    legend(xs[jogador]-3.6, ys[jogador]+3.9, jogador,
           bty = 'n')
  }
}

library(png)
ima <- readPNG('../campo/background_campo.png')

library(foreach)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)

ptime <- system.time({
  foreach(tempo = 1:1001, .packages = c('magrittr')) %dopar% {
    
    if(tempo%%1 == 0){
      xs <- jogo1ime[tempo, seq(1, 44, 2)] %>% as.numeric() %>% 
        c(jogo1ime$bolax[tempo])
      ys <- jogo1ime[tempo, seq(2, 44, 2)] %>% as.numeric() %>% 
        c(jogo1ime$bolay[tempo])
      
      
     # if(jogo1ime[tempo,]$posse == 'EA'){
        col = c(rep(c('red', 'black'), each=11), 'blue')
     # } else{
     #   col = c(rep(c('black', 'red'), each=11), 'blue')
     # }
      
      png(sprintf('./imagens/campo_%06d.png', tempo), width = 630, height = 408)
      plot.new()
      lim <- par()
      rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
      par(new=TRUE)
      plot(ys~xs,
           xlim = c(0, 105), ylim = c(0, 68),
           pch = c(rep(c(15, 17), each=11), 16),
           col = col, main = sprintf('Tempo: %s segundos', round(tempo/30, 1)))
      legenda_jogador(xs, ys)
      
      
      dev.off()
    }
  }})
ptime

system('ffmpeg -framerate 6 -i ~/Development/IME/cea2/rnn/imagens/campo_%06d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p ~/Development/IME/cea2/rnn/simulação_todos_jogos2.mp4 -y')

