editar_nombres <- function(nombres) {
  nombres2 <- tolower(nombres)  
  nombres2 <- str_replace_all(nombres2, pattern = " ", replacement = "_")
  nombres2 <- str_replace_all(nombres2, pattern = "sexo\r\n1=hombre\r\n2=mujer", replacement = "sexo")
  nombres2
}