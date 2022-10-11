media_negativa <- function(x) {
  # Calcula la media
  media <- mean(x)
  
  # Devuelve x negativo
  if (media > 0) {
    return(-x)
  } else {
    return(x) 
  }
}

# Correlación entre las medias de x e y
cor_negativa <- function(y, x) {
  
  x_negativo <- media_negativa(x)
  y_negativo <- media_negativa(y)
  
  cor(x_negativo, y_negativo)
}


# Prueba
x <- c(2.419092, 18.536160, 16.424651, 2.531620, 3.948415, 8.034805, 6.736159, 4.874916, 17.847676, 19.693059)
y <- c(16.021440, 13.609311, 7.874239, 6.677645, 12.865368, 18.974723, NA, 19.515071, 14.611792, 15.317403)
cor_negativa(x, y)

