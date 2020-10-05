#############################
#VALIDACIÓN EDUCACIÓN FORMAL#
#############################

library(tidyverse);library(validate);library(haven);library(ggplot2);library(extrafont);library(hsurvey2)

#Cargar base de datos 
ed <- read_dta(file = "klaus/descarga_bases/ed_listanf.dta")
mh <- read_dta(file = "klaus/descarga_bases/lista_mh.dta")

#Cargar funciones
#source("klaus/scripts/helpers_validacion.R")

#Base compacta
edu <- mh %>% 
  select(interview__id:ed11__14) %>% 
  as.data.frame() %>% 
  mutate(edad = as.numeric(mh02),
         id2 = paste0(interview__id, lista_mh__id)) %>% 
  select(ed01:ed04, id2) %>% 
  mutate_at(vars(ed01:ed04), .funs = funs(as.numeric(.))) %>% 
  mutate_at(vars(ed01:ed04), .funs = funs(if_else(is.na(.), -77, .)))  

#############
#VALIDADORES#
#############
#Generar validadores de ejemplo
v_ed01 <- validator(rule1 = ed01 >= 1 & ed01 <= 16) 

v_ed02 <- validator(rule1 = if (ed01 == 7)  ed02_1 >= 1 & ed02_1 <= 8,  
                    rule2 = if (ed01 == 10) ed02_1 >= 1 & ed02_1 <= 4,
                    rule3 = if (ed01 == 14) ed02_1 >= 1 & ed02_1 <= 7,
                    rule4 = if (ed01 == 16) ed02_1 == -77) 

v_ed03 <- validator(rule1 = ed03 >= 1 & ed03 <= 2) 


############
#VALIDACIÓN#
############

#Dejar todos los validadores en una lista. 
vals <- list(v_ed01, v_ed02, v_ed03)

#Etiquetas de las variables. Se usan para nombrar las columnas de la tabla final 
labels <- c("ed01", "ed02", "ed03")

#Confrontar las reglas con cada una de las variables. Esta función devuelve una lista con un data frame para cada columna evaluada  
validados <- validate_data(edu, vals, "id2")

#Dejar solo los datos útiles. Se recorre toda la lista anterior para dejar únicamente las variables que interesan. 
clean <- map2(validados, labels, clean_report, "id2" )

#Unir todos los resultados de la validación en una tabla. La idea es hacer un join de todas las tablas generadas. 
data_merge <- reduce(clean, full_join, "id2")
View(data_merge)

#Reporte gráfico de cada una de las variables
columns <-  names(data_merge)[names(data_merge) != "id2"]
plots <-  map(columns, ~plot_results(data_merge, .x))
names(plots) <- labels



###################
#TUTORIAL VALIDATE#
###################
#Contrastar la variable con el validador 
cf_ed02 <- confront(edu, v_ed02, key = "id2")
summary(cf_ed02)

report <- as.data.frame(cf_ed02) 
View(report)

#Hacer resumen de las reglas y dejar un solo registro por línea
small_report <- report %>% 
  group_by(id2) %>% 
  mutate(all_rules_true = mean(value, na.rm = T),
         all_rules_true = if_else(all_rules_true == 1, 1, 0)) %>% 
  filter(row_number() == 1) 

View(small_report)

#Generar una base tipo spread de las reglas
report_spread(report) %>% 
  View()







