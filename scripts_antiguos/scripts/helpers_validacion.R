#Función para crear base spread. Recibe un reporte completo y devuelve una base de datos en formato spread con todas las reglas  
report_spread <- function(report) {
  report_df <- as.data.frame(report)
  report_spread <- report_df %>% 
    select(-expression) %>% 
    spread(name, value)  
  return(report_spread)
}


#Función para validar variable. Recibe 3 argumentos:
#data: datos que se están evaluando
#validator: objeto de validate que contiene reglas de validación
#id: identificador de cada registro

validate_var <- function(data, validator, id) {
  #Función para generar reporte reducido. Esta función recibe el resultado de todas las reglas y devuelve un registro resumido que dice sí o no.  
  small_report <- function(report, id) {
    id_sym <- sym(id)
    report_df <- as.data.frame(report) 
    
    small_report <- report_df %>% 
      group_by(!!id_sym) %>% 
      mutate(all_rules_true = mean(value, na.rm = T),
             all_rules_true = if_else(all_rules_true == 1, 1, 0)) %>% 
      filter(row_number() == 1)
    #names(small_report)[dim(small_report)[2]] <- variable
    return(small_report)
  }
  
  
  #Confrontar los datos con las reglas de validación establecidas
  cf <- confront(data, validator , key = id)  
  
  #Capturar el nombre del input para poner el nombre a la variable
  #variable <- deparse(substitute(validator))
  
  return(small_report(cf, id))
  }



#Función para validar todos los datos. Recibe datos, una lista de validadores y un identificador. Devuelve una base validada por cada una de las variables en una lista.
validate_data <- function(data, vals, id) {
  validators <- vals
  reports <- list()
  for (i in 1:length(validators)) {
    reports[[i]] <- validate_var(data, validators[[i]], id)
  }
  return(reports)
}


#Función para dejar solo la variable de interés y nombrar 
clean_report <- function(report, label, id) {
  id_sym <- sym(id)
  unpacked_data <- report %>% 
    select(all_of(id_sym), !!label := all_rules_true)
  return(unpacked_data)
  
}

#Función para generar reporte gráfico
plot_report <- function(data) {
  report %>% 
    na.omit() %>%
    ggplot(data, mapping = aes(x = factor(name), fill = value)) +
    geom_bar(position = "dodge" )  
}


plot_results <- function(data, var) {
  
  x_var <- sym(var)
  plot <- data %>% 
    group_by(!!x_var) %>% 
    summarise(suma = n()) %>% 
    ggplot(aes(x = as.factor(!!x_var), y = suma)) +
    ggtitle(paste("Resultados", var )) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = suma),
              size = 2.5, 
              position = position_dodge(width = 0.6), 
              vjust = -0.3, 
              hjust = 0.5) +
    theme(text = element_text(size = 10))
  return(plot)
} 

#Función para escribir mensajes de error en un archivo de texto
write_log <- function(char_vector) {
  
  file <- "klaus/logs/reporte_inicial.txt"
  
  #Escribir título del apartado
  write_lines("***VARIABLES NO PRESENTES EN LA BASE***",
              file, 
              sep = "\n",  append = T)  
  
  write_lines("",
              file, 
              sep = "\n",  append = T)  
  
  for (i in 1:length(char_vector)) {
    write_lines(paste(char_vector[i], "no existe"),
                file, 
                sep = "\n",  append = T)  
  }
  
}







