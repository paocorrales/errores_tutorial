# Extrae el e-nesimo digito de un numero (arrancando desde la izquierda)
get_digito <- function(num, n) {
  # remueve los digitos de la izquierda, luego los de la derecha
  (num %% (10 ^ n)) %/% (10 ^ n)
}

# Revisa si un numero positivo es un palindromo
palindromo <- function(num) {
  digitos <- floor(log(num, 10)) + 1
  for (x in 1:((digits %/% 2))) {
    digito1 <- get_digito(num, x)
    digito2 <- get_digito(num, (digits + 1) - x)
    if (digito1 != digito2)
      return(FALSE)
  }
  return(TRUE)
}

# Encuentra el palindromo mas grande contruido multiplicando 2 numeros de 3 digitos
palindromo_grande <- function() {
  mejor <- 0
  for (x in 100:999) {
    for (y in x:999) {
      candidato <- x * y
      if (candidato > mejor && palindromo(candidato)) {
        mejor <- candidato
      }
    }
  }
  mejor
}