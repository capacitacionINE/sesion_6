nacimientos_e <- nacimientos %>% 
  mutate(random = runif(n = nrow(.)),
         dia_ins = if_else(random >= 0.9, 32, dia_ins),
         peso = if_else(random <= 0.1 & peso != 9999, peso * -1, peso ),
         edad_p = if_else(random <= 0.1 & edad_p != 9999, 201, edad_p))
save(nacimientos_e, file = "datos/nacimientos_2017_e.RData")
