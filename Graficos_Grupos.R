library(ggplot2)
corg <- c()
for (i in 1:length(cl_atq5B)){
  if (cl_atq5B[i] == 1){
    corg[i] = "red"
  }
  if (cl_atq5B[i] == 2){
    corg[i] = "blue"
  }
  if (cl_atq5B[i] == 3){
    corg[i] = "darkgreen"
  }
  if (cl_atq5B[i] == 4){
    corg[i] = "darkorchid3"
  }
  if (cl_atq5B[i] == 5){
    corg[i] = "chocolate4"
  }
}
pairs(atq5.B[,2:11], pch=16, col=corg, cex=0.6)

cor3 <- c("red","blue3","darkgreen")
cor4 <- c("red","blue3","darkgreen","darkorchid3")
cor5 <- c("red","blue3","darkgreen","darkorchid3","chocolate4")
pairs(atq3.B[,2:11], pch=16, col=corg, cex=0.6)

pairs(atq1.A[,2:11], pch=16, col=corg, cex=0.6)

ggplot(atq1.A, aes(x = centAtqy,y = des.bola, color = as.factor(cl_atq1A))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor3) +
  labs(x = "Centroide Y", y = "Deslocamento da Bola", color = "Grupo")+
  theme_bw()

corg <- c()
for (i in 1:length(cl_def3A)){
  if (cl_def3A[i] == 1){
    corg[i] = "red"
  }
  if (cl_def3A[i] == 2){
    corg[i] = "blue"
  }
  if (cl_def3A[i] == 3){
    corg[i] = "darkgreen"
  }
  if (cl_def3A[i] == 4){
    corg[i] = "darkorchid3"
  }
  if (cl_def3A[i] == 5){
    corg[i] = "chocolate4"
  }
}

pairs(def3.A[,2:11], pch=16, col=corg, cex=0.6)

ggplot(def3.A, aes(x = centDefy,y = des.bola, color = as.factor(cl_def3A))) +
  geom_point(pch = 16) +
  scale_color_manual(values= cor5) +
  labs(x = "Centroide Y", y = "Deslocamento da Bola", color = "Grupo")+
  theme_bw()

