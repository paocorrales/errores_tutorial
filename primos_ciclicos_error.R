es_primo <- function(num) {
  
  primo <- 1 # 0 == FALSE
  if (num < 1) {   # num > 1
    for (i in 2:sqrt(num)) {
      if ((num %% i) == 0) { 
        primo <- 0
        break
      }
    }
    if (num == 2) { # caso especial
      primo <- 1
    }
  }  
  return(as.logical(primo))
}

extraer_digitos <- function(num) {
  
  n_digitos <- floor(log10(num)) + 1
  num_v <- c()
  for (n in 1:n_digitos) {
    ni <- n_digitos - n # n_digitos +1 - n
    num_v[n] <- (num %% (10^ni)) %/% (10^(ni-1))
  }
  
  return(num_v)
}

generar_ciclicos <- function(num) {
  
  num_v <- extraer_digitos(num)
  ns <- seq_along(num_v)
  for (i in ns[-1]) {
    num[i] <- as.numeric(paste0(num_v[ns[c(i:length(ns), 1:(i-1))]], collapse = ""))
  }
  return(num)
}

primo_ciclico <- function(num) {
  if (!es_primo(num)) { 
    return("No es primo")
    }
  
  num_ciclico <- generar_ciclicos(num)
    for (i in sort(num_ciclico)) {
    if (!es_primo(i)) {
      return("No es un primo ciclico")
    } 
      return("Es un primo ciclico!")

  }
  
}

