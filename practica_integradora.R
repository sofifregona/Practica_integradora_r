# ** PRÁCTICA INTEGRADORA **

# ** CURSO: Data Science - Fundación Start Coding IA **
# ** AUTOR: Fregona, María Sofía - @sofifregona **



# ** LIBRERÍAS **

library(tidyverse)
library(sf)
library(lubridate)
library(ggmap)
library(leaps)



# Importación de datasets de recorridos de ecobicis para el año 2018,
# y de usuarios para los años 2015 a 2018.

ecobicis_df = read_csv("datasets/recorridos-realizados-2018.csv")
usuarios_2018 = read_csv("datasets/usuarios-ecobici-2018.csv")
usuarios_2017 = read_csv("datasets/usuarios-ecobici-2017.csv")
usuarios_2016 = read_csv("datasets/usuarios-ecobici-2016.csv")
usuarios_2015 = read_csv("datasets/usuarios-ecobici-2015.csv")



# Unión de datos de usuarios en una única tabla

usuarios_df = rbind(usuarios_2015, usuarios_2016, usuarios_2017, usuarios_2018)



# ** EDA ***

glimpse(ecobicis_df)
summary(ecobicis_df)

glimpse(usuarios_df)
summary(usuarios_df)

# Se observan valores incoherentes en la edad de los usuarios, como por ejemplo,
# 0 y valores por arriba de 100



# ** LIMPIEZA Y TRANSFORMACIÓN DE DATOS **

ecobicis_df$genero_usuario %>% unique 
usuarios_df$genero_usuario %>% unique

# Se observan NA's y valores 'N' y 'O' para los géneros de los usuarios en ambas
# tablas, los cuales no tienen coherencia

usuarios_df$edad_usuario %>% hist

ecobicis_df$tiempo_recorrido <- as.numeric(ecobicis_df$fecha_destino_recorrido - ecobicis_df$fecha_origen_recorrido)

ecobicis_df <- ecobicis_df %>% 
  select(-duracion_recorrido ) %>% 
  filter(genero_usuario == c("M", "F"))

usuarios_df <- usuarios_df %>% 
  filter(genero_usuario == c("M", "F"), edad_usuario > 0 & edad_usuario < 100)  


# ** EDA corregido **

glimpse(ecobicis_df)
summary(ecobicis_df)

glimpse(usuarios_df)
summary(usuarios_df)



# ** GRÁFICOS UNIVARIADOS **

# Viajes realizados según género

ecobicis_df %>% 
  ggplot()+
  geom_bar(aes(x = as.factor(genero_usuario)))+
  xlab("Género")+
  ylab("Cantidad")+
  ggtitle("Vaijes realizados según género")

# La cantidad de viajes realizados por usuarios de sexo masculino es muy superior
# a la cantidad de viajes realizados por usuarios de sexo femenino.

# Distribución del tiempo recorrido (minutos)

tiempo_hist = ecobicis_df %>% 
  ggplot()+
  geom_histogram(aes(x = tiempo_recorrido, y = after_stat(..count../sum(..count..))), col = "white", na.rm = T)+
  xlab("Minutos recorridos")

tiempo_hist+
  ggtitle("Distribución del tiempo recorrido")

# La distribución del tiempo recorrido en minutos se observa acampanada y asimétrica
# sesgada hacia la izquierda Se sugiere realizar una transformación

tiempo_hist+
  scale_x_log10()+
  ggtitle("Distribución logarítmica del tiempo recorrido")

# La transformación logarítimica del tiempo recorrido en minutos se observa 
# mucho más acampanada y simétrica que el caso anterior

# Usuarios registrados entre 2015 y 2018 según género

usuarios_df %>% 
  ggplot()+
  geom_bar(aes(x = as.factor(genero_usuario)))+
  xlab("Género")+
  ylab("Cantidad")+
  ggtitle("Usuarios registrados entre 2015 y 2018 según su género")

# La cantidad de usuarios registrados de sexo masculino durante los años 2015
# a 2018 es ligeramente superior a la cantidad de usuarios de sexo femenino
# registrados durante el mismo período. 

# Distribución de las edades de los usuarios registrados entre 2015 y 2018

edad_hist = usuarios_df %>% 
  ggplot()+
  geom_histogram(aes(x = edad_usuario), col = "white", na.rm = T)+
  xlab("Edad")

edad_hist+
  ggtitle("Distribución de la edad del usuario")

# La distribución de las edades de los usuarios se observa acampanada y asimétrica
# sesgada hacia la izquierda. Se sugiere realizar una transformación

edad_hist+
  scale_x_log10()+
  ggtitle("Distribución logarítmica del tiempo recorrido")

# La transformación logarítimica se observa un poco más simétrica

# Recorridos diarios

ecobicis_df %>% 
  group_by(dia = as.Date(fecha_origen_recorrido)) %>% 
  summarise(recorridos = n()) %>% 
  ggplot()+
  geom_line(aes(x = dia, y = recorridos), col = "steelblue")+
  theme_bw()+
  ggtitle("Recorridos diarios")

# Recorridos mensuales

ecobicis_df %>% 
  group_by(mes = month(fecha_origen_recorrido, label = T, abbr = T)) %>% 
  summarise(recorridos = n()) %>% 
  ggplot()+
  geom_col(aes(x = mes, y = recorridos), fill = "steelblue")+
  theme_bw()+
  ggtitle("Recorridos diarios")

# Se observa un aumento en los recorridos durante los meses de abril a noviembre



# ** GRÁFICOS BIVARIADOS **


bicis_usuarios <- inner_join(ecobicis_df, usuarios_df, by = c("id_usuario" = "id_usuario"))

# Tiempo recorrido en función de la edad

bicis_usuarios %>% 
  ggplot()+
  geom_point(aes(x = edad_usuario, y = tiempo_recorrido, col = "red"), na.rm = T, alpha = .1)

# Se observa que a medida que la edad aumenta el tiempo recorrido disminuye,
# sin embargo esta relación no es lineal.

# Distribución de las edades de los usuarios según género

usuarios_df %>% 
  ggplot()+
  geom_histogram(aes(x = edad_usuario, group = genero_usuario, fill = genero_usuario), position = "dodge", col = "white", na.rm = T)+
  geom_density(aes(x = edad_usuario, group = genero_usuario, col = genero_usuario), na.rm = T)+
  xlab("Edad")+
  scale_x_log10()

# Las distribuciones de las variables según el género se observan bastante 
# similares

# Distribución del tiempo recorrido (minutos) según género

ecobicis_df %>% 
  ggplot()+
  geom_boxplot(aes(x = genero_usuario, y = tiempo_recorrido), fill = "steelblue", na.rm = T)+
  scale_y_log10()

# Las cajas de los boxplots se encuentran bastante solapadas, por lo que no 
# parecieran existir diferencias significativas entre los tiempos recorridos
# por las personas de sexo femenino y las de sexo masculino. En ambos casos
# se observan outliers

# Duraciones de los recorridos por semana

ecobicis_df$dia_semana <- ecobicis_df$fecha_origen_recorrido %>% wday(label = T, abbr = F)

ggplot(ecobicis_df %>% sample_n(2e4),
       aes(x = dia_semana, y = tiempo_recorrido))+
  geom_jitter(aes(col = dia_semana), alpha = .15)+
  coord_cartesian(ylim = c(0,75))+
  geom_boxplot(outlier.colour = NA, alpha = 0)+
  theme_bw()+
  ggtitle("Duracipon de viaje por día de la semana")+
  xlab("Día")+
  ylab("Duración del recorrido (minutos)")

# Se observa que la variabilidad en los recorridos los días de fin de semana
# es mayor a la variabilidad de los días de semana. Además la mediana pareciera
# ser superior los fines de semana a los días de semana.

# Cantidad de viajes por día de la semana

ecobicis_df %>% 
  group_by(dia_semana) %>% 
  summarise(cantidad = n()) %>% 
  ggplot()+
  geom_col(aes(x = dia_semana, y = cantidad), alpha = .8)+
  ggtitle("Número de operaciones por día de la semana")+
  theme_bw()

# Se observa que la cantidad de viajes realizados los fines de semana es menor
# a la cantidad de viajes realizados los días laborables



# ** OUTLIERS **

# Histograma de los outliers superiores (tiempos más altos)

ecobicis_df %>% 
  filter(tiempo_recorrido > 1.5*IQR(tiempo_recorrido, na.rm = T)+
           quantile(tiempo_recorrido, .75, na.rm = T)) %>%
  ggplot()+
  geom_histogram(aes(x = tiempo_recorrido), col = "white", binwidth = 3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(60,180,20))+
  ggtitle("Histograma de outliers")

# ** INTERVALOS DE CONFIANZA **

# IC del 95% para la cantidad de viajes realizado por día

cantidad_dia <- ecobicis_df %>% 
  group_by(fecha = as.Date(fecha_origen_recorrido)) %>% 
  summarise(registros = n()) %>% 
  mutate(dia_semana = wday(fecha, label = T, abbr = F))

cantidad_dia_summ <- cantidad_dia %>% 
  group_by(dia_semana) %>% 
  summarise(media = mean(registros),
            int_min = t.test(registros, conf.level = .95)$conf.int[1],
            int_max = t.test(registros, conf.level = .95)$conf.int[2])

ggplot(cantidad_dia_summ, aes(x = fct_rev(dia_semana)))+
  geom_segment(aes(xend = dia_semana, y = int_min, yend = int_max), color = "grey")+
  geom_point(aes(y = int_min), col = rgb(0.7,0.2,0.1,0.5), size = 5)+
  geom_point(aes(y = int_max), col = rgb(0.2,0.7,0.1,0.5), size = 5)+
  geom_point(aes(y = media), size = 2, shape = 1)+
  coord_flip()+
  theme_bw()
  theme(
    legend.position = "none",
  )+
  xlab("Día de la semana")+
  ylab("Cantidad de registros")+
  ggtitle("Intervalos de confianza de registros por día de semana")
  
  # A partir de los intervalos de confianza calculados podemos afirmar
  # que existen diferencias significativas entre la cantidad de viajes
  # que se realizan los días de semana y los días de fin de semana,
  # siendo los primeros significativamente mayores que los segundos.
  
  
  
  # ** GRÁFICO DE MAPA **
  
  # Observaciones de las estaciones en el mapa
  
  estaciones_df <- ecobicis_df %>% 
    select(id_estacion_origen, lat_estacion_origen, long_estacion_origen) %>% 
    unique() %>% 
    filter(complete.cases(.))
  
  bbox <- c(min(estaciones_df$long_estacion_origen) - .005,
            min(estaciones_df$lat_estacion_origen) - .005,
            max(estaciones_df$long_estacion_origen) + .005,
            max(estaciones_df$lat_estacion_origen) + .005)

  ggmap(get_stamenmap(bbox, zoom = 13))+
    geom_point(data = estaciones_df, aes(x = long_estacion_origen, y = lat_estacion_origen))
  
  
  
  # ** PIRÁMIDE POBLACIONAL DE USUARIO **
  
  usuarios_plot <- usuarios_df %>% 
    filter(genero_usuario %in% c("M", "F")) %>% 
    mutate(rango_etario = cut(edad_usuario, breaks = seq(15,100,5))) %>% 
    filter(!is.na(rango_etario)) %>% 
    group_by(genero_usuario, rango_etario) %>% 
    summarise(total = n())

  usuarios_plot$total[usuarios_plot$genero_usuario == "M"] <-
    -1 * usuarios_plot$total[usuarios_plot$genero_usuario == "M"]

  ggplot(usuarios_plot, aes(x = rango_etario, y = total, fill = genero_usuario))+
    geom_bar(stat = "identity")+
    scale_y_continuous(breaks = seq(-24e3, 20e3, 4e3),
                       labels = c(seq(24e3,0,-4e3), seq(4e3,20e3,4e3)))+
    coord_flip()+
    theme_bw()+
    ggtitle("Pirámide poblacional de usuarios de ECOBICI")+
    ylab("Número de usuarios")+
    xlab("Rango etario (años)")+
    labs(fill = "Sexo")

  
  
  # **  **    
  
  #
  
  sl_data <- viajes_vs_registro %>% 
    filter(id_estacion_origen == 177,
           id_estacion_destino == 5,
           !is.na(tiempo_recorrido)) %>% 
    mutate(dia_semana = wday(as.Date(fecha_origen_recorrido)), inicio_time = hour(fecha_origen_recorrido) * 60 + minute(fecha_origen_recorrido)) %>% 
    select(dia_semana, genero_usuario.x, edad_usuario, hora_alta, inicio_time, tiempo_recorrido) %>% 
    mutate(hora_alta = as.numeric(hora_alta))
  
  
  
  # ** MACHINE LEARNING **
  
  # Training test split
  
  set.seed(0)
  keep <- sample(1:nrow(sl_data), size = floor(.8*nrow(sl_data)), replace = F)
  train_df <- sl_data[keep,]
  test_df <- sl_data[-keep,]

  # BSS
  
  bss <- regsubsets(tiempo_recorrido~., train_df, method = "exhaustive")
  
  summary(bss)
  summary(bss)$adjr2 %>% which.max()
  summary(bss)$adjr2[summary(bss)$adjr2 %>% which.max()]
  summary(bss)$bic %>% which.min()
  summary(bss)$bic[summary(bss)$bic %>% which.min()]

  # train model
  
  lin_model <- lm(tiempo_recorrido~., data = train_df)
  summary(lin_model)

  # test performance
  
  preds <- predict(lin_model, test_df)

  RSQUARE = function(y_actual, y_predict){
    cor(y_actual, y_predict)^2
  }

  RSQUARE(test_df$tiempo_recorrido, preds)  
