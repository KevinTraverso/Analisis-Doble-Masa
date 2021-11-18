
# Instalar las librerias
# install.packages("readxl")
# install.packages("tibble")
# install.packages("ggplot2")
# install.packages("xts")

# Cargar las Librerias 

library(readxl)
library(tibble)
library(ggplot2)
library(xts)
library(reshape)

# Importar los datos del archivo Excel ------------------------------------

RainfallData <- read_excel("./Data/RainfallData.xlsx", 
                           skip = 4)

# View(RainfallData)
# RainfallData2 <- summary(RainfallData)

RainfallData <- RainfallData %>%
  add_column(PM = rowMeans(RainfallData[-1]))

ggplot2::ggplot(data = RainfallData) + 
  geom_point(mapping = aes(PM, C)) +
  labs(x = "Promedio Estaciones",
       y = "Estación D")

RainfallData2 <- cumsum(RainfallData[,-1])
RainfallData2 <- cbind(RainfallData[,1], RainfallData2) 

a <- data.frame(a=c(1:max(RainfallData2)), 
                b=c(1:max(RainfallData2)))

ggplot(data = RainfallData2, mapping = aes(PM, C)) +
  geom_point() + geom_line() +
  geom_line(data = a, mapping = aes(a,b),linetype = "dashed") + 
  coord_fixed()

plot1 <- melt(RainfallData2[-1], id.vars = "PM", variable_name = "Estaciones")

ggplot(plot1, aes(PM,value)) + geom_line(aes(colour = Estaciones)) + 
  geom_point(aes(colour = Estaciones)) +
  geom_line(data = a, mapping = aes(a,b),linetype = "dashed") +
  coord_fixed()

# plot1 <- melt(RainfallData2[-1], id.vars = "PM", variable_name = "Estaciones")
# 
# ggplot(plot1, aes(PM,value)) + geom_point(aes(colour = Estaciones))

# Corrección de serie -----------------------------------------------------
# Mediante analisis de regresion lineal
# Seleccion

RainfallData2[,1] <- as.Date(paste(RainfallData2[,1], "01", "01",sep="-"),
                             format="%Y-%m-%d")

RainfallData2 <- xts(RainfallData2[,-1], RainfallData2[,1])
# write.csv(RainfallData2, file = "./Salidas/Data2.csv")

# Obteniendo la pendiente m1
a1 = 1970
a2 = 1978

d1 <- window(RainfallData2, 
             start = as.Date(paste(a1, "01", "01",sep="-"), 
                             format="%Y-%m-%d"), 
             end = as.Date(paste(a2, "01", "01",sep="-"),
                           format="%Y-%m-%d"))

x = d1$PM
y = d1$C

lm_rainfall = lm(y ~ +1 + x)
lm <- summary(lm_rainfall)
m1 <- lm$coefficients[2]
m1

# Obteniendo la pendiente m2
b1=1979
b2=1986

d2 <- window(RainfallData2, 
             start = as.Date(paste(b1, "01", "01",sep="-"), 
                             format="%Y-%m-%d"), 
             end = as.Date(paste(b2, "01", "01",sep="-"),
                           format="%Y-%m-%d"))

x2 = d2$PM
y2 = d2$C

lm_rainfall2 = lm(y2 ~ +1 + x2)
lm2 <- summary(lm_rainfall2)
m2 <- lm2$coefficients[2]
m2
# Obteniendo el factor de correción
factor = m1/m2
factor

RainfallData3 <- data.frame(RainfallData)
RainfallData3[,1] <- as.Date(paste(RainfallData3[,1], "01", "01",sep="-"),
                             format="%Y-%m-%d")

RainfallData3 <- as.xts(RainfallData3[,-1],
                        order.by = RainfallData3[,1])

RainfallData3a <- window(RainfallData3, 
                         start = as.Date(paste(a1, "01", "01",sep="-"), 
                                         format="%Y-%m-%d"), 
                         end = as.Date(paste(a2, "01", "01",sep="-"),
                                       format="%Y-%m-%d"))

RainfallData3b <- window(RainfallData3, 
                         start = as.Date(paste(b1, "01", "01",sep="-"), 
                                         format="%Y-%m-%d"), 
                         end = as.Date(paste(b2, "01", "01",sep="-"),
                                       format="%Y-%m-%d"))

RainfallData3b$C <-  RainfallData3b$C * factor

RainfallData4 <- rbind(RainfallData3a,
                       RainfallData3b[-1,])
RainfallData4$PM <- NULL
RainfallData4$PM <- rowMeans(RainfallData4)
RainfallData4

RainfallData5 <- cumsum(RainfallData4)
RainfallData5

ab<- data.frame(a=c(1:max(RainfallData5)), 
                b=c(1:max(RainfallData5)))

ggplot2::ggplot(data = RainfallData4) + 
  geom_point(mapping = aes(PM, C)) +
  labs(x = "Promedio Estaciones",
       y = "Estación C")

ggplot(data = RainfallData5, mapping = aes(PM, C)) +
  geom_point() + geom_line()+
  geom_line(data = ab, mapping = aes(a,b),
            linetype = "dashed") + coord_fixed()

