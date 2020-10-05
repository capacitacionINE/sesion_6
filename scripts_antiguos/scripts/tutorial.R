#########################################
#OPERACIONES BÁSICAS PARA MANEJAR TABLAS#
#########################################

#NOTA: Este script tiene el objetivo de proveer al equipo de herramientas básicas para el manejo de tablas en R. 

library(tidyverse);library(haven)

#Abrir base de datos con haven
casen <- read_dta(file = "klaus/bases_auxiliares/casen 2017.dta")


#-----------------------#
#Funciones para explorar#
#-----------------------#

#Nombres de las columnas
names(casen)

#Dimensiones de la tabla
dim(casen)

#Estadísticas generales. Con el operador $, podemos acceder a las columnas dentro del data.frame
summary(casen$zona)

#------------------------------------#
#Consultas simples a la base de datos#
#------------------------------------#
#NOTA: En tidyverse el operador %>% (pipe) permite concatenar funciones. Es equivalente al uso de paréntesis 

#Agrupar y luego obtener la media (o cualquier otro indicador). group_by es equivalente a bys en Stata
casen %>%
  group_by(sexo) %>% 
  summarise(mean(yotp, na.rm = T)) #se excluyen los NA


#Podemos reescribir lo anterior del siguiente modo. Este estilo es más cercano al lenguaje matemático, pero es mucho menos legible. Se recomienda evitarlo. 
summarise(group_by(casen, sexo), mean(yotp, na.rm = T)) 

#Filtrar, agrupar y sumar 
casen %>%
  filter(edad >= 30) %>% 
  group_by(sexo) %>% 
  summarise(suma = sum(yotp, na.rm = T), #se excluyen los NA
            contar = n()) %>%  #la función n() se usa para contar 
  View()



#---------------------#
#Creación de variables#
#---------------------#

#Con la función mutate podemos crear variables. Estamos asignando una suma a la columna "sumar_algo" 
casen <- casen %>% 
  mutate(sumar_algo = yotp + ytoth)

#Podemos combinar mutate con la función if_else. Si sexo es 1, haz la suma. En otro caso, escribe 0. Puedo crear todas las variables que quiera dentro de mutate
casen <- casen %>% 
  mutate(sumar_algo = if_else(sexo == 1, ytotcorh + ytoth, 0 ),
         sumar_algo2 = if_else(sexo == 2, ytotcorh + ytoth, 0))

#Si queremos cambiar varias columnas utilizando una misma condición, podemos usar mutate_at. Esto es equivalente a un loop, pero implementado con una sintaxis dplyr.   
casen <- casen %>% 
  mutate_at(.vars = vars(sumar_algo, sumar_algo2), 
            .funs = funs(if_else( edad  <= 15, 99999999, .)) )

#Si queremos crear nuevas columnas y no modificar las anteriores, podemos asignar la función a nuevas variables, las cuales tendrán como sufijo el nombre elegido. En este caso, "operación"  
casen <- casen %>% 
  mutate_at(.vars = vars(sumar_algo, sumar_algo2), 
            .funs = funs(operacion = if_else( edad  <= 15, 99999999, .)) )


#-----#
#Joins#
#-----#

#Generar  bases de datos a nivel de hogar y persona, para hacer el ejercicio.
hogares <- casen %>% 
  select(ing_total = ytotcorh, folio) %>% #base compacta
  group_by(folio) %>% 
  filter(row_number() == 1) #esta línea deja un registro por hogar 

personas <- casen %>% 
  select(sexo_pero = sexo, folio, o)
  
#Ahora queremos unir nuestra base original con nuestra base a nivel de hogar 
#Para ello, se utiliza la familia de la función join. 

casen2 <- casen %>% 
  left_join( hogares, by = "folio")

#Si queremos agregar más columnas a la llave, la sintaxis es las siguiente
casen3 <- casen %>% 
  left_join(personas, by = c("folio", "o"))


#-------------------------------#
#Más sobre creación de variables#
#-------------------------------#

#Crear porcentaje de mujeres y hombres por hogar 
#El uso de ungroup no es obligatorio, pero acelera la ejecución de algunas funciones
casen <- casen %>%
  group_by(folio, sexo) %>% 
  mutate(suma_sexo = n() ) %>% 
  group_by(folio) %>% 
  mutate(n_hogar = n()) %>% 
  ungroup() %>%
  mutate(por_sexo = suma_sexo/n_hogar)


#----------------------------------#
#Modificar estructura base de datos#
#----------------------------------#

#Generar una base pequeña para practicar
base <- casen %>% 
  slice(1:20) %>% 
  select(folio, edad, o)

#Hacer spread
s <- spread(data = base,  o, edad)

#Hacer gather
g <- s %>% 
  gather(o, edad, -folio) %>% 
  na.omit() %>% #se borran algunas NA 
  mutate(o = as.numeric(o)) #Se el tipo para que coincidan ambas tablas

#Hacer join
comprobar <- base %>% 
  left_join(g, by = c("folio", "o"))

#Comprobar coincidencia
mean(comprobar$edad.x == comprobar$edad.y)


