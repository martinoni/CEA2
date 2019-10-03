setwd('~/Development/IME/cea2/campo/')
library(magrittr)

jogo1ime <- read.delim("~/Development/IME/cea2/jogo2ime.txt", header=TRUE)
attach(jogo1ime)
posse_ataque <- sprintf('%s_%s', posse, ataque)
jogo1ime = cbind(jogo1ime, posse_ataque)


constroi_video_de_ataque <- function(time_ataque, jogo1ime){
  attach(jogo1ime)
  jogo1ime <- jogo1ime[jogo1ime$posse_ataque == time_ataque, ]
  attach(jogo1ime)
  indices_times <- c(rep('1',11), rep('2', 11))
  
  legenda_jogador <- function(xs, ys){
    for(jogador in 1:22){
      legend(xs[jogador]-3.6, ys[jogador]+3.9, jogador,
             bty = 'n')
    }
  }

  library(png)
  ima <- readPNG('campo2.png')
  
  library(foreach)
  library(doParallel)
  cl <- makeCluster(6)
  registerDoParallel(cl)
  
  ptime <- system.time({
    foreach(tempo = 1:nrow(jogo1ime), .packages = c('magrittr')) %dopar% {
      
      if(tempo%%1 == 0){
        xs <- jogo1ime[tempo, seq(9, 52, 2)] %>% as.numeric() %>% 
          c(jogo1ime$bolax[tempo])
        ys <- jogo1ime[tempo, seq(10, 52, 2)] %>% as.numeric() %>% 
          c(jogo1ime$bolay[tempo])
        
        if(jogo1ime[tempo,]$posse == 'EA'){
          col = c(rep(c('red', 'royalblue3'), each=11), 'black')
        } else{
          col = c(rep(c('red3', 'royalblue'), each=11), 'black')
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
  print(ptime)
  
  doParallel::stopImplicitCluster(cl)
  
}

time_ataque <- 'EA_44'
constroi_video_de_ataque(time_ataque, jogo1ime)
system('ffmpeg -framerate 30 -i ~/Development/IME/cea2/campo/imagens/campo_%06d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p ~/Development/IME/cea2/campo/EA_1.mp4 -y')
system('rm /home/thiago/Development/IME/cea2/campo/imagens/*')
