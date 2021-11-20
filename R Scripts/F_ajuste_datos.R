
#* 
#* Desarrollo de funcion para ingreso de datos Json anuales
#* lectura y acjuste de datos
#* 

#*
#* PRIMERA FUNCION
#* 

# Datos = Datos anuales en formato json
# Inicio = Fecha de inicio de serie
# Fin = Fecha final de serie

Data_db1 <- function(Datos,Inicio,Fin){
  
  library(xts)
  library(tibble)
  
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
  
  c <- window(b,start = Inicio, end = Fin)
  m1 <- data.frame(c)
  m1 <- m1 %>%
    add_column(PM = rowMeans(m1))
  
  m2 <- cumsum(m1)
  
  return(list(m1, m2)) # matrices para mostar en el portal
}

anlaisis1 <- Data_db1(Datos = "./Data/SerieAnual4EstRamis_anual.json",
                      Inicio = "2000-01-01", 
                      Fin = "2010-12-31")


Data_db1 <- 
