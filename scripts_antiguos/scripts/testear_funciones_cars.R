

library(tidyverse);library(validate);library(haven);library(ggplot2);library(extrafont)

#Cargar funciones
source("klaus/scripts/helpers_validacion.R")


#Usar base iris 
cars <- mtcars %>% 
  mutate(id2 = paste0("id", 1:dim(mtcars)[1]))

#Generar dos reglas
v_car <- validator(rule1 = mpg > 20,  
                   rule2 = if (disp == 169) mpg > 21) 


#Dejar todos los validadores en una lista
vals <- list(v_car)

#Confrontar las reglas con cada una de las variables 
validados <- validate_data(cars, vals, "id2")

#Etiquetas de las variables 
labels <- c("ed01")

#Dejar solo los datos útiles
clean <- map2(validados, labels, clean_report, "id2" )

#Unir todos los resultados de la validación en una tabla
data_merge <- reduce(clean, full_join, "id2")
View(data_merge)

#Reporte gráfico de cada una de las variables
columns <-  names(data_merge)[names(data_merge) != "id2"]
plots <-  map(columns, ~plot_results(data_merge, .x))
names(plots) <- labels

