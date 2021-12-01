
#* FUNCIONES DE EJECUCION PARA DOBLE MASA
#* @autor: Kevin Traverso
#* Desarrollo de funcion para ingreso de datos Json anuales
#* lectura y acjuste de datos

# PRIMERA FUNCION: Ajuste de datos y obtencion da acumulados para grafico

Data_db1 <- function(Datos,Inicio,Fin){
  
  # Datos = Datos anuales en formato json
  # Inicio = Fecha de inicio de serie
  # Fin = Fecha final de serie
  
  library(xts)
  library(tibble)
  library(zoo)
  library(ggplot2)
  library(reshape)
  library(hydroTSM)
  
  datos1 <- jsonify::from_json(Datos)
  cnames <- data.frame()
  
  for (i in 1:length(datos1)) {
    cnames[1,i] <- paste0("Est_", i)
  }
  
  a <- list()
  
  for (i in 1:length(datos1)) {
    a[[i]] <- xts::xts(x = datos1[[i]]$D,
                       order.by = as.Date(datos1[[i]]$F))
  }
  
  b <- do.call(merge,lapply(a,as.xts))
  colnames(b) <- cnames
  c <- window(b, start = Inicio, end = Fin)
  
  # llevando los datos diarios a anuales
  # para la version final cambiar T a F
  c1 <- hydroTSM::daily2annual(c,
                               FUN = sum, 
                               na.rm = T)
  m1 <- data.frame(c1)
  
  m1 <- m1 %>%
    add_column(PM = rowMeans(m1))
  
  m2 <- cumsum(m1)
  m2 <- data.frame(Year = seq(as.Date(Inicio),
                              as.Date(Fin),
                              by = "year"),
                   m2)
  
  m1 <- data.frame(Year = seq(as.Date(Inicio), 
                              as.Date(Fin),
                              by = "year"), 
                   m1)
  
  # REALIZANDO EL GRAFICO DOBLE MASA
  k <- data.frame(a=c(1:max(m2$PM)),
                  b=c(1:max(m2$PM)))
  
  plot1 <- melt(m2[,-1], 
                id.vars = "PM", 
                variable_name = "Estaciones")
  
  grf1 <- ggplot(plot1, aes(PM,value)) +
    geom_line(aes(colour = Estaciones)) + 
    geom_point(aes(colour = Estaciones)) +
    geom_line(data = k,
              mapping = aes(a,b),
              linetype = "dashed") +
    coord_fixed()
  
  return(list(m1, m2, grf1)) # matrices para mostar en el portal
  
}

analisis <- Data_db1(Datos = "./Data/SerieDiariaRamis4Esta.json",
                     Inicio = "1970-01-01", 
                      Fin = "2010-12-31")

# Funcion para encontrar los valores de m1, deacuerdo ala seleccion del 
# año de quiebre y estacion a corregir

Data_db1 <- function(Inicio, Fin, a1, Estacion){
  
  # Inicio = Fecha inicial de serie
  # Fin = Fecha final de serie
  # a1 = quiebre
  # Estacion = numero de columna de la estacion a corregir
  
  d1 <- window(xts(analisis[[1]], 
                   order.by = as.Date(row.names(analisis[[1]]))), 
               start = as.Date(Inicio,
                               format="%Y-%m-%d"), 
               end = as.Date(paste(a1, "01", "01",
                                   sep="-"),
                             format="%Y-%m-%d"))
  
  d2 <- window(xts(analisis[[1]], 
                   order.by = as.Date(row.names(analisis[[1]]))),
               start = as.Date(paste(a1+1, "01", "01", 
                                     sep="-"),
                               format="%Y-%m-%d"), 
               end = as.Date(Fin,
                             format="%Y-%m-%d"))
  
  # obteniendo el valor de m1
  Estacion = 1
  x.1 = d1$PM
  y.1 = d1[,Estacion]
  
  lm_rainfall1 = lm(y.1 ~ + 1 + x.1)
  lm <- summary(lm_rainfall1)
  m1 <- lm$coefficients[2]
  m1
  
  # Obteniendo el valor de m2
  x.2 = d2$PM
  y.2 = d2[,Estacion]
  
  lm_rainfall2 = lm(y.2 ~ + 1 + x.2)
  lm2 <- summary(lm_rainfall2)
  m2 <- lm2$coefficients[2]
  m2
  
  return(list(d1, d2, m1, m2))
  
}

# la funcion anterior retorna el valor de m1 y m2 para la estacion a corregirse
# para posteriormente encontrar el factor m1/m2 o m2/m1

# funcion para encontrar el valor de factor m1/m2
# corregir el segundo periodo en base al primero

factorm1 <- function(m1,m2){
  
  factor.1 <- m1/m2
  
  c1 <- factor.1*d2[,Estacion]
  d2[,Estacion] <- c1
  
  c2 <- rbind(d1, d2) # Agregando los nuevos datos
  
  c2$PM <- NULL
  c2$PM <- rowMeans(c2)
  
  c3 <- cumsum(c2) # Datos acumulados nuevos
  c3
  
  return(list(c2,c3))
  
}


# funcion para encontrar el valor de factor m2/m1
# corregir el segundo periodo en base al primero

factorm1 <- function(m1,m2){
  
  factor.2 <- m2/m1
  
  c1 <- factor.2*d1[,Estacion]
  d1[,Estacion] <- c1
  
  c2 <- rbind(d1, d2) # Agregando los nuevos datos
  
  c2$PM <- NULL
  c2$PM <- rowMeans(c2)
  
  c3 <- cumsum(c2) # Datos acumulados nuevos
  c3
  
  return(list(c2,c3))
  
}

