stringreverso <- function(str) {
  vec <- strsplit(str, "")
  vec <- rev(unlist(vec))
  paste(vec, collapse = "")
}

crazify <- function(str) {
  vec <- strsplit(str, " ")
  vec <- lapply(unlist(vec), stringreverso)
  paste(vec, collapse = " ")
}

prueba <- function() {
  oraciones <- data.frame(
    titulo = c("abecedario", 
               "lorem"),
    texto = c("a caballo regalado no se le miran los dientes",
             "lorem ipsum dolor sit amet, consectetur adipiscing elit."),
    stringsAsFactors = TRUE)
  oraciones$texto <- vapply(oraciones$texto, crazify, "character")
  oraciones
}
