# pacotes necessarios
library(tidyverse)
#lendo dos dados do dir "dados"
dados_1 <- read_csv2("dados/dados_1.txt",col_types = "ddddddddddddddd")
#observando as estruturas, tipos e classes
glimpse(dados_1)
#renomeando as colunas
names(dados_1) <- c("t","x","y","v_y","a_y","v","r","L","p_x","p_y","p","a")

#################################################################################
#                      Tópico 8                                                         #
# VELOCIDADE A CADA 4 Variações de medida: Velocidade a cada 32 frames => 1 seg #
#                                                                               #  
#################################################################################
espaco <- as.vector(dados_1$y)
tempo <- as.vector(dados_1$t)
length(tempo)
length(espaco)
#alocando memoria
vel <- vector()
for (i in seq(1, 57, by=4)) {
    vel[i] <- (espaco[i+4] - espaco[i])/ (tempo[i+4] - tempo[i])
    print(i)
}

# VELOCIDADE A CADA 5  Iterações
vel2 <- vector()
for (i in seq(1, length(espaco), by=5)) {
    vel2[i] <- (espaco[i+5] - espaco[i])/ (tempo[i+5] - tempo[i])
    print(i)
}
#VELOCIDADE A CADA 8 iterações
vel3 <- vector()
for (i in seq(1, length(espaco), by=8)) {
    vel3[i] <- (espaco[i+8] - espaco[i])/ (tempo[i+8] - tempo[i])
    print(i)
}

#gerando um data frame com os dados de velocidade

ok <- complete.cases(vel)#vetor logico contem os indices apenas dos valores numericos
vel_4 <- as.numeric(vel[ok]) # obtendo os itens validos
tempo4 <- as.numeric(tempo[seq(0, 57, by=4)]) #vetor com o mesmo tamanho de tempo para concatenar em um data.frame
velocidade4 <- cbind(vel_4,tempo4) # concatenando os vetores como colunas
velocidade4 <- as.data.frame(velocidade4) # transformando em data.frame

#gerando um data frame com os dados de velocidade

ok <- complete.cases(vel2)
vel_5 <- as.numeric(vel2[ok])
tempo5 <- as.numeric(tempo[seq(0, 57, by=5)])
velocidade5 <- cbind(vel_5,tempo5)
velocidade5 <- as.data.frame(velocidade5)

#gerando um data frame com os dados de velocidade

ok <- complete.cases(vel3)
vel_8 <- as.numeric(vel3[ok])
tempo8 <- as.numeric(tempo[seq(0, 57, by=8)])
velocidade8 <- cbind(vel_8,tempo8)
velocidade8 <- as.data.frame(velocidade8)

#########################################################################
#                      Plotando os dados
#########################################################################
library(cowplot)
iter1 <- iter4 <- ggplot(data = dados_1, aes(x = t, 
                                             y = dados_1$`v_{y}`))+
    geom_line()+
    geom_point(col="blue")+
    labs(x = "Tempo [s]",
         y = "Velocidade [cm/s]")+
    theme_light()
iter4 <- ggplot(data = velocidade4, aes(x = tempo4, 
                                        y = vel_4))+
    geom_line()+
    geom_point(col="blue")+
    labs(x = "Tempo [s]",
         y = "Velocidade [cm/s]")+
    theme_light()
iter5 <- ggplot(data = velocidade5, aes(x = tempo5, 
                                        y = vel_5))+
    geom_line()+
    geom_point(col="blue")+
    labs(x = "Tempo [s]",
         y = "Velocidade [cm/s]")+
    theme_light()
iter8 <- ggplot(data = velocidade8, aes(x = tempo8, 
                                        y = vel_8))+
    geom_line()+
    geom_point(col="blue")+
    labs(x = "Tempo [s]",
         y = "Velocidade [cm/s]")+
    theme_light()
plot_grid(iter1,iter4,iter5,iter8, 
          labels = c("1 iteração","4 iterações", "5 iterações", " 8 iterações"))

################################################################################
#      TOPICO 5
# 
################################################################################
ggplot(data = dados_1,mapping = aes(x = t,
                                     y = y))+
    geom_line(col="blue")+
    geom_point(col="blue")+
    labs(x = "Tempo [s]",
         y = "Variação de altura  Y [cm]",
         title = "Variação do nivel da água ao longo do tempo",
         subtitle = "Tópico 5",
         caption = "Os autores, 2021")+
    theme_light()

################################################################################
#  TOPICO 7
#
################################################################################
neg <- ggplot(data = dados_1,mapping = aes(x = t,
                                    y = dados_1$`v_{y}`))+
    geom_line(col="blue")+
    geom_point(col="blue")+
    labs(title = "Velocidade x Tempo",
         caption = "Os autores, 2021",
         x = "Tempo [s]",
         y = "Velocidade em y[cm/s]")+
    theme_light()

dados_1$v_y_plus_minus_one <- dados_1$`v_{y}` * (-1)

pos <- ggplot(data = dados_1,
              mapping = aes(x = y,
                            y = dados_1$`v_{y}`))+
    geom_line(col="blue")+
    geom_point(col="blue")+
    labs(title = "Velocidade x Espaço",
         caption = "Os autores, 2021",
         x = "espaço [cm]",
         y = "Velocidade em y[cm/s]")+
    theme_light()

cowplot::plot_grid(neg, pos, 
          labels = "auto",
          nrow=2)

#################################################################################
#                           TOPICO 6
#################################################################################

D <- 1.956
d <- 0.23

result <- vector()
f_y <- vector()
u_y <- vector()
y_novo <- vector()
for (i in seq(1,57,by=1)) {
    result[i] <- (sqrt(espaco[i])-((d/D)^2)*sqrt(9.81/2)*tempo[i])^2 #exata
    f_y[i] <- (-(d/D)^2)*sqrt(2*9.81*espaco[i]) #Solução na mão
    u_y[i] <- espaco[i]*(-2*9.81*(d/D)^4)
    y_novo[i] <- (espaco[1] - 1.286*tempo[i] + 0.047*tempo[i]^2)
}
dy_dx <- vector()
for (i in seq(1,57,by=2)) {
    dy_dx[i] <- (espaco[i+2] - espaco[i])/(tempo[i+2] - tempo[i])
}


plot(x=dados_1$t,
     y=y_novo)
positiva <- f_y*-1

dados_1$u_y <- u_y
dados_1$y_gino <- y_novo
reg <- lm(u_y~t,data = dados_1)

dados_1$y_exata <- result

dados_1$f_y_pos <- positiva

dados_1$u_quadrado <- (dados_1$`v_{y}`)^2

reg <- lm(u_quadrado~u_y,data = dados_1) 
plot(x=dados_1$u_y,
     y= dados_1$u_quadrado)

abline(reg)

ggplot(dados_1)+
    geom_line(aes(x=t,y=y), 
              col="red")+
    geom_point(aes(x=t,y=y), 
               col="red")+
    geom_point(aes(x=t,y=y_exata), 
               col="blue")+
    geom_line(aes(x=t,y=y_exata), 
               col="blue")+
    geom_point(aes(x=t, y=y_gino),
               col="BLack")+
    geom_line(aes(x=t, y=y_gino),
               col="Black")+
    labs(x="Tempo [s]",
         y="Espaço [cm]",
         title = "Variação do espaço ao longo do tempo",
         subtitle = "Comparação entre o resultado prático e teórico",
         caption = "Os autores, 2021")+
    theme_minimal()
    
################################################################################
#                   Topico 9
################################################################################

C <- -2*9.81*(d/D)^4
f_y <- vector()
for (i in seq(1,57,by=1)) {
    f_y[i] <- espaco[i]*(-0.003750895)
}

C <- (dados_1$u_quadrado/dados_1$y)
mean(C,
     na.rm = T)

dados_1$f_y  <- f_y

plot(x=dados_1$f_y ,
     dados_1$u_quadrado)

abline(reg)

reg <- lm(u_quadrado~y,dados_1)

gravidade <- -(0.0790/2)*(D/d)^4

dados <- data.frame(dados_1$t[1:53],dados_1$y[1:53])
dados$vel_q <- dados_1$`v_{y}`dados_1$`v_{y}`^2
dados$vy <- dados_1$`v_{y}`[1:53]

names(dados) <- c("t","y")

plot(x=dados$vy, y=dados$vel_q)
abline(lm(vel_q~vy, dados))
