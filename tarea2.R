library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
df <- read_excel("tarea2TEFI/data_tarea_2.xlsx")
#elegir acciones
a1<-"06738310"#BARD C R INC
a2<-"17989510"#CLARCOR INC
a3<-"92035510"#VALSPAR CORP
a4<-"87262510"#TRC
a5<-"86860710"#SUPREME
a6<-"30064510"#EXAR CORP
#dt
df_a1<- df[df$CUSIP == a1, ]
df_a1<-df_a1 %>%
  mutate(r_bruto = 1+RET)
df_a2<- df[df$CUSIP == a2, ]
df_a2<-df_a2 %>%
  mutate(r_bruto = 1+RET)
df_a3<- df[df$CUSIP == a3, ]
df_a3<-df_a3 %>%
  mutate(r_bruto = 1+RET)
df_a4<- df[df$CUSIP == a4, ]
df_a4<-df_a4 %>%
  mutate(r_bruto = 1+RET)
df_a5<- df[df$CUSIP == a5, ]
df_a5<-df_a5 %>%
  mutate(r_bruto = 1+RET)
df_a6<- df[df$CUSIP == a6, ]
df_a6<-df_a6 %>%
  mutate(r_bruto = 1+RET)

#voy a usar promedio geometrico ya que tengo el retorno neto
mean_a1<-round(geometric.mean(df_a1$r_bruto),4)-1
mean_a2<-round(geometric.mean(df_a2$r_bruto),4)-1
mean_a3<-round(geometric.mean(df_a3$r_bruto),4)-1
mean_a4<-round(geometric.mean(df_a4$r_bruto),4)-1
mean_a5<-round(geometric.mean(df_a5$r_bruto),4)-1
mean_a6<-round(geometric.mean(df_a6$r_bruto),4)-1
#ahora la volatilidad
sd_a1 <- round(sqrt(var(df_a1$RET)), 4)
sd_a2 <- round(sqrt(var(df_a2$RET)), 4)
sd_a3 <- round(sqrt(var(df_a3$RET)), 4)
sd_a4 <- round(sqrt(var(df_a4$RET)), 4)
sd_a5 <- round(sqrt(var(df_a5$RET)), 4)
sd_a6 <- round(sqrt(var(df_a6$RET)), 4)
#matriz de covarianza, creo un df con las columnas de los retornos
columnas <- data.frame(
  a1 = df_a1$RET,
  a2 = df_a2$RET,
  a3 = df_a3$RET,
  a4 = df_a4$RET,
  a5 = df_a5$RET,
  a6 = df_a6$RET
)
#y luego la matriz:
matriz_covarianza <- cov(columnas)
#creo un df con la media y la desviacion de las acciones a1 a4 a5
df_msd <- data.frame(
  nombre     = c(df_a1$COMNAM[1],df_a4$COMNAM[1],df_a5$COMNAM[1]),
  media      = c(mean_a1,mean_a4,mean_a5),
  desviacion = c(sd_a1,sd_a4,sd_a5)
)
#grafico
ggplot(df_msd, aes(x = desviacion, y = media, label = nombre)) +
  geom_point() +  # Puntos
  geom_text(vjust = -0.5, hjust = 1.5) +
  geom_segment(aes(x = desviacion, y = 0, xend = desviacion, yend = media), 
               linetype = "dashed", color = "gray") + 
  geom_segment(aes(x = 0, y = media, xend = desviacion, yend = media), 
               linetype = "dashed", color = "gray") + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")) +
  labs(x = "Desviación Estándar", y = "Media", title = "Gráfico de Media vs Desviación Estándar") +
  coord_cartesian(xlim = c(0, max(df_msd$desviacion)), ylim = c(0, max(df_msd$media)))

