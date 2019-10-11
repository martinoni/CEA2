setwd('~/Development/IME/cea2/campo/')
library(magrittr)

jogo1ime <- read.delim("~/Documentos/jogo1ime_completo.txt", header=TRUE)
attach(jogo1ime)

indices_times <- c(rep('1',11), rep('2', 11))

legenda_jogador <- function(xs, ys){
  for(jogador in 1:22){
    legend(xs[jogador]-3.6, ys[jogador]+3.9, jogador,
           bty = 'n')
  }
}
legenda_jogador(xs, ys)

library(png)
ima <- readPNG('background_campo.png')

library(foreach)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)

ptime <- system.time({
  foreach(tempo = 1:2000, .packages = c('magrittr')) %dopar% {
    
    if(tempo%%1 == 0){
      xs <- jogo1ime[tempo, seq(9, 52, 2)] %>% as.numeric() %>% 
        c(jogo1ime$bolax[tempo])
      ys <- jogo1ime[tempo, seq(10, 52, 2)] %>% as.numeric() %>% 
        c(jogo1ime$bolay[tempo])
      
      if(jogo1ime[tempo,]$posse == 'EA'){
        col = c(rep(c('red', 'black'), each=11), 'blue')
      } else{
        col = c(rep(c('black', 'red'), each=11), 'blue')
      }
      
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

system('ffmpeg -framerate 15 -i ~/Development/IME/cea2/campo/imagens/campo_%06d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p ~/Development/IME/cea2/campo/campo.mp4 -y')
