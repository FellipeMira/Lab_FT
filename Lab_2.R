# pacotes necessarios
library(tidyverse)
#lendo dos dados do dir "dados"
dados_1 <- read_csv2("dados/dados_1.txt",col_types = "ddddddddddddddd")
#observando as estruturas, tipos e classes
glimpse(dados_1)
#renomeando as colunas
names(dados_1) <- c("t","x","y","v_y","a_y","v","r","L","p_x","p_y","p","a")

#################################################################################
#                                                                               #
# VELOCIDADE A CADA 4 Variações de medida: Velocidade a cada 32 frames => 1 seg #
#                                                                               #  
#################################################################################
espaco <- as.vector(dados_1$y)
tempo <- as.vector(dados_1$t)

#alocando memoria
vel <- vector()
for (i in seq(1, 112, by=4)) {
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
vel_4 <- na.omit(vel)
tempo4 <- tempo[seq(1, 108, by=4)]
velocidade4 <- cbind(vel_4,tempo4)
velocidade4 <- data.frame(velocidade4)

vel_5 <- na.omit(vel2)
tempo5 <- tempo[seq(1, 108, by=5)]
velocidade5 <- cbind(vel_5,tempo5)
velocidade5 <- data.frame(velocidade5)

vel_8 <- na.omit(vel3)
tempo8 <- tempo[seq(1, 100, by=8)]
velocidade8 <- cbind(vel_8,tempo8)
velocidade8 <- data.frame(velocidade8)

#########################################################################
#                      Plotando os dados
#########################################################################
library(cowplot)

iter4 <- ggplot()+
    geom_line(data = velocidade4, aes(x = tempo4, 
                                      y = vel_4))+
    labs(x = "Tempo [s]",
         y = "Velocidade [cm/s]")+
    theme_light()
iter5 <- ggplot()+
    geom_line(data = velocidade5, aes(x = tempo5, 
                                      y = vel_5))+
    labs(x = "Tempo [s]",
         y = "Velocidade [cm/s]")+
    theme_light()
iter8 <- ggplot()+
    geom_line(data = velocidade8, aes(x = tempo8, 
                                      y = vel_8))+
    labs(x = "Tempo [s]",
         y = "Velocidade [cm/s]")+
    theme_light()
plot_grid(iter4,iter5,iter8, labels = c("4 iterações", "5 iterações", " 8 iterações"),
          label_size = 12,
          nrow = 3)
######################################################################

function   
    
