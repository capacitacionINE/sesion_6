################################
#VALIDACIÓN INICIAL DEL SISTEMA#
################################

library(tidyverse); library(validate);library(haven)

#Cargar base de datos 
ed <- read_dta(file = "klaus/descarga_bases/ed_listanf.dta")
mh <- read_dta(file = "klaus/descarga_bases/lista_mh.dta")

#Base compacta
edu <- mh %>% 
  select(interview__id:ed11__14) %>% 
  as.data.frame() %>% 
  mutate(edad = as.numeric(mh02),
         id = paste0(interview__id, lista_mh__id)) %>% 
  select(ed01:ed04, id) %>% 
  mutate_at(vars(ed01:ed04), .funs = funs(as.numeric(.))) %>% 
  mutate_at(vars(ed01:ed04), .funs = funs(if_else(is.na(.), -77, .)))  

edu %>%
  View() 

#Confirmar existencia de las variables
variables <- c("ed01_1", "ed02", "ed02_1")
existe_var <-  variables %in% names(edu)
no_existen <-  variables[!existe_var]

#Confirmar tipo de variable
check <- edu %>% 
  check_that(is.character(ed01) )
check_df <- as.data.frame(check)

#Confirmar rango variables de 1 a 2
rango <- validator( var_group(ed01, ed02) >= 1 & var_group(ed01, ed02) <= 2 )
rango <- validator(G := var_group(ed01, ed02), G >= 1 & G <= 2)


cf_rango <- confront(edu, rango, key = "id")



#Generar un reporte escrito de las variables inexistentes 

#






