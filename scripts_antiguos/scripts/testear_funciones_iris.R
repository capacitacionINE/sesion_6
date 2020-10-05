
###########################
#PROBAR FUNCIONES CON IRIS#
###########################

library(tidyverse);library(validate);library(haven);library(ggplot2);library(extrafont)

#Cargar funciones
source("klaus/scripts/helpers_validacion.R")


#Usar base iris 
iris2 <- iris %>% 
  mutate(id2 = paste0("id", 1:dim(iris)[1]))

#Generar dos reglas
v_sepal_length <- validator(rule1 = Sepal.Length > 0,  
                            rule2 = if (Sepal.Length == mean(Sepal.Length)) Sepal.Width <= 3) 


#Dejar todos los validadores en una lista
vals <- list(v_sepal_length)

#Confrontar las reglas con cada una de las variables 
validados <- validate_data(iris2, vals, "id2")

#Etiquetas de las variables 
labels <- c("ed01")

#Dejar solo los datos útiles
cleaned <- map2(validados, labels, clean_report, "id2" )

#Unir todos los resultados de la validación en una tabla
data_merge <- reduce(cleaned, full_join, "id2")
View(data_merge)

#Reporte gráfico de cada una de las variables
columns <-  names(data_merge)[names(data_merge) != "id2"]
plots <-  map(columns, ~plot_results(data_merge, .x))
names(plots) <- labels



#############################
#MIRAR DE CERCA CON VALIDATE#
#############################


#Contrastar la variable con el validador 
cf <- confront(iris2, v_sepal_length, key = "id2")
summary(cf)

report <- as.data.frame(cf_ed02) 
View(report)

#Hacer resumen de las reglas y dejar un solo registro por línea
small_report <- report %>% 
  group_by(id) %>% 
  mutate(all_rules_true = mean(value, na.rm = T),
         all_rules_true = if_else(all_rules_true == 1, 1, 0)) %>% 
  filter(row_number() == 1) 

View(small_report)

#Generar una base tipo spread de las reglas
report_spread(report) %>% 
  View()



