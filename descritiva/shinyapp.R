# Link útil: http://gallery.htmlwidgets.org/
# Biblioteca útil num futuro pra aplicação de shiny em larga escala: Golem

library(shiny)
library(plot3D)
library(plot3Drgl)
library(rgl)
library(Cairo)
library(stringr)
library(magrittr)
jogo1ime <- read.delim("./Development/IME/cea2/jogo1ime.txt", header=TRUE)
# jogo1ime <- read.delim("~/Documentos/jogo1ime_completo.txt", header=TRUE)

ui <- fluidPage(
  selectInput('select', "Escolhe aí o jogador", 
              list('A1','A2','A3','A4','A5','A6',
                   'A7','A8','A9','A10','A11','B18',
                   'B19','B20','B21','B22','B23','B24',
                   'B25','B26','B27','B28')),
  rglwidgetOutput('grafico')
)

server <- function(input, output, session) {
  output$grafico <- renderRglwidget({
    player <- input$select
    try({rgl.close()})
    x_nome <- jogo1ime[[paste0('E', player, 'x')]]
    y_nome <- jogo1ime[[paste0('E', player, 'y')]]
    jogador <- paste0('E', player, 'x') %>% 
      toString() %>%
      str_replace('E','') %>%
      str_replace('x', '') %>%
      sprintf('Jogador %s', .)
    x_c <- cut(x_nome, seq(0, 105, 3))
    y_c <- cut(y_nome, seq(0, 68, 3))
    z <- table(x_c, y_c)
    hist3Drgl(z=z, border="black", main = jogador)
    rglwidget(width = 1280, height = 720)
  }, outputArgs =  list(width = "1280px", height = "720px"))
}


shinyApp(ui, server)

