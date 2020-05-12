### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Importar y procesar datos ----
source("02_codigo/01_importar_preparar_datos.R") 

### Cambio diario en el número total de muertes incluido en la base de datos ----
mx_datos_todos %>%
  filter(resultado == "Positivo SARS-CoV-2") %>% 
  group_by(fecha_actualizacion) %>% 
  summarise(num_muertes = sum(!is.na(fecha_def))) %>% 
  ungroup() %>% 
  mutate(diferencia = num_muertes - lag(num_muertes)) %>% 
  print(n = Inf)
  
### Gráfica 01: Nuevos registros de muertes incluidos en la base de datos del 24 de abril que no estaban en la base de datos del día previo, por fecha de registro de las defunciones ----
mx_datos_todos %>%
  filter(resultado == "Positivo SARS-CoV-2",
         !is.na(fecha_def)) %>% 
  group_by(fecha_actualizacion, fecha_def) %>% 
  summarise(num_muertes = n()) %>% 
  ungroup() %>%
  filter(fecha_def >= as_date("2020-03-18"), 
         fecha_actualizacion %in% c(as_date("2020-04-23"), as_date("2020-04-24"))) %>% 
  arrange(fecha_def, fecha_actualizacion) %>% 
  group_by(fecha_def) %>% 
  mutate(cambio_muertes_en_bd = num_muertes - lag(num_muertes),
         cambio_muertes_en_bd = ifelse(fecha_actualizacion == as_date("2020-04-24") & fecha_def == as_date("2020-04-24"), num_muertes, cambio_muertes_en_bd)) %>% 
  ungroup() %>% 
  filter(!is.na(cambio_muertes_en_bd), 
         fecha_def >= as_date("2020-04-03")) %>% 
  mutate(etiqueta_grandes = ifelse(cambio_muertes_en_bd >= 5, cambio_muertes_en_bd, ""),
         etiqueta_peques_positivas = ifelse(cambio_muertes_en_bd < 5 & cambio_muertes_en_bd >= 0, cambio_muertes_en_bd, ""),
         etiqueta_peques_negativas = ifelse(cambio_muertes_en_bd < 0, cambio_muertes_en_bd, "")) %>% 
  # print(n = Inf)
  ggplot(aes(x = fecha_def, y = cambio_muertes_en_bd)) +
  geom_col(color = "white", fill = "grey10") +
  geom_text(aes(label = etiqueta_grandes), vjust = 1.5, color = "white", size = 5, fontface = "bold") +
  geom_text(aes(label = etiqueta_peques_positivas), vjust = -0.5, color = "grey40", size = 5, fontface = "bold") +
  geom_text(aes(label = etiqueta_peques_negativas), vjust = 1.5, color = "grey40", size = 5, fontface = "bold") +
  scale_x_date(breaks = seq(as_date("2020-04-02"), as_date("2020-04-24"), 1),
               date_labels = "%b-%d", 
               expand = c(0, 0)) +
  scale_y_continuous(limits = c(-3, 35)) +
  labs(title = "Cambio en los registros de muertes realizados en la base de datos del 24 de abril respecto\na la base de datos del día previo, por fecha de registro de las defunciones",
       x = "\nFecha de registro de defunciones",
       y = NULL,
       caption = "\nElaborado por @segasi y @jorgeacast / Fuente: Bases de datos abiertos publicadas por la Secretaría de Salud entre el 13 de abril y el 10 de mayo.\n\nNota: La gráfica no incluye un fallecimiento incluido en la base de datos del 24 de abril, con fecha de registro del 23 de enero de 2020, porque de acuerdo con la Secretaría Salud\nlas primeras dos muertes por Covid-19 en México ocurrieron el 18 de marzo.") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave("03_graficas/01_numero_nuevas_muertes_24_abril_por_fecha_registro.png", dpi = 200, width = 16.5, height = 9)


### Gráfica 02: Número de casos confirmados de Covid-19 que fallecieron el 24 de abril, de acuerdo con cada corte de la base de datos publicado a partir de esa fecha ----
mx_datos_todos %>%
  filter(resultado == "Positivo SARS-CoV-2",
         !is.na(fecha_def)) %>% 
  group_by(fecha_actualizacion, fecha_def) %>% 
  summarise(num_muertes = n()) %>% 
  ungroup() %>% 
  filter(fecha_def == as_date("2020-04-24")) %>% 
  arrange(fecha_def, fecha_actualizacion) %>% 
  mutate(etiqueta_grandes = ifelse(num_muertes > 10, num_muertes, ""),
         etiqueta_peques = ifelse(num_muertes < 10, num_muertes, "")) %>% 
  ggplot(aes(x = fecha_actualizacion, y = num_muertes)) +
  geom_col(color = "white", fill = "grey10") +
  geom_text(aes(label = etiqueta_grandes), vjust = 1.5, color = "white", size = 5, fontface = "bold") +
  geom_text(aes(label = etiqueta_peques), vjust = -0.8, color = "grey40", size = 5, fontface = "bold") +
  scale_x_date(breaks = seq(as_date("2020-04-24"), max(mx_datos_todos$fecha_actualizacion), 1),
               date_labels = "%b-%d", 
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     # limits = c(0, 60),
                     breaks = seq(0, 200, 10)) +
  labs(title = "Número de casos confirmados de Covid-19 que fallecieron el 24 de abril, de acuerdo\ncon cada corte de la base de datos publicado a partir de esa fecha",
       x = "\nFecha de publicación de la base de datos    ",
       y = NULL,
       caption = "\nElaborado por @segasi y @jorgeacast / Fuente: Bases de datos abiertos publicadas por la Secretaría de Salud entre el 13 de abril y el 10 de mayo.") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave("03_graficas/02_numero_muertes_24_abril_por_corte_bd.png", dpi = 200, width = 16, height = 9)


### Gráfica 03: Número de casos confirmados de Covid-19 que fallecieron cada fecha desde el 18 de marzo, de acuerdo con cada corte de la base de datos publicado a partir de esa fecha ----
mx_datos_todos %>%
  filter(resultado == "Positivo SARS-CoV-2",
         !is.na(fecha_def)) %>% 
  group_by(fecha_actualizacion, fecha_def) %>% 
  summarise(num_muertes = n()) %>% 
  ungroup() %>% 
  filter(fecha_def >= as_date("2020-03-22")) %>% 
  arrange(fecha_def, fecha_actualizacion) %>% 
  mutate(mes_texto = case_when(month(fecha_def) == 3 ~ "marzo",
                               month(fecha_def) == 4 ~ "abril",
                               month(fecha_def) == 5 ~ "mayo",
                               month(fecha_def) == 6 ~ "junio",
                               month(fecha_def) == 7 ~ "julio",
                               month(fecha_def) == 8 ~ "agosto",
                               month(fecha_def) == 9 ~ "septiembre",
                               month(fecha_def) == 10 ~ "octubre",
                               month(fecha_def) == 11 ~ "noviembre",
                               month(fecha_def) == 12 ~ "diciembre"),
         fecha_texto = str_wrap(str_c("Muertes ocurridas el ", day(fecha_def), " de ", mes_texto), width = 18), 
         fecha_texto = fct_reorder(fecha_texto, fecha_def)) %>% 
  ggplot(aes(x = fecha_actualizacion, y = num_muertes)) +
  geom_col(color = "white", fill = "grey10") +
  facet_wrap(~ fecha_texto, ncol = 10) +
  labs(title = "Número de casos confirmados de Covid-19 que fallecieron cada fecha desde el 22 de marzo,\nde acuerdo con cada corte de la base de datos publicado a partir de esa fecha",
       x = "\nFecha de publicación de la base de datos",
       y = "Número de muertes reportadas en cada corte\n",
       caption = "\nElaborado por @segasi y @jorgeacast / Fuente: Bases de datos abiertos publicadas por la Secretaría de Salud entre el 13 de abril y el 10 de mayo.") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
        strip.text = element_text(size = 12))  +
  ggsave("03_graficas/03_numero_muertes_por_fecha_por_corte_bd.png", dpi = 200, width = 18, height = 12)


### Gráfica 04: Número de muertes de casos confirmados de Covid-19 ocurridos en cada fecha, de acuerdo con los diferentes cortes de la base de datos ----
foo <- 
  mx_datos_todos %>%
  filter(resultado == "Positivo SARS-CoV-2",
         !is.na(fecha_def)) %>% 
  group_by(fecha_actualizacion, fecha_def) %>% 
  summarise(num_muertes = n()) %>% 
  ungroup() %>% 
  filter(fecha_def >= as_date("2020-03-18")) %>% 
  arrange(fecha_def, fecha_actualizacion) %>%  
  mutate(mes_texto = case_when(month(fecha_def) == 3 ~ "marzo",
                               month(fecha_def) == 4 ~ "abril",
                               month(fecha_def) == 5 ~ "mayo",
                               month(fecha_def) == 6 ~ "junio",
                               month(fecha_def) == 7 ~ "julio",
                               month(fecha_def) == 8 ~ "agosto",
                               month(fecha_def) == 9 ~ "septiembre",
                               month(fecha_def) == 10 ~ "octubre",
                               month(fecha_def) == 11 ~ "noviembre",
                               month(fecha_def) == 12 ~ "diciembre")) %>% 
  group_by(fecha_def) %>% 
  mutate(dias_desde_fecha_muerte = row_number(),
         dias_desde_fecha_muerte = dias_desde_fecha_muerte - 1, 
         aumento_diario = num_muertes - lag(num_muertes),
         aumento_diario = ifelse(is.na(aumento_diario), 0, aumento_diario),
         aumento_diario_acumulado = cumsum(aumento_diario),
         puntito_final = ifelse(dias_desde_fecha_muerte == max(dias_desde_fecha_muerte), num_muertes, NA),
         texto_puntito_final_altos = ifelse(!is.na(puntito_final) & puntito_final > 50 & dias_desde_fecha_muerte > 2, str_c(day(fecha_def), " de ", mes_texto, "\n",comma(puntito_final), " muertes"), ""),
         texto_puntito_final_recientes = ifelse(!is.na(puntito_final) & dias_desde_fecha_muerte <= 2, str_c(day(fecha_def), " de ", mes_texto, "\n",comma(puntito_final), " muertes"), ""),
         texto_puntito_final_estables = ifelse(!is.na(puntito_final) & puntito_final > 32 & puntito_final < 50 & dias_desde_fecha_muerte > 20, str_c(day(fecha_def), " de ", mes_texto, "\n",comma(puntito_final), " muertes"), "")) %>% 
  ungroup() 

set.seed(1)
foo %>% 
  ggplot(aes(x = dias_desde_fecha_muerte)) +
  geom_line(aes(y = num_muertes, group = fecha_def),
            size = 1, 
            alpha = 0.6) +
  geom_point(aes(y = puntito_final, group = fecha_def), color = "salmon", size = 2) +
  geom_label_repel(aes(label = texto_puntito_final_altos, 
                       y = puntito_final),
                   size = 3.5) +
  geom_label_repel(aes(label = texto_puntito_final_recientes, 
                       y = puntito_final),
                   size = 3.5,
                   direction = "y") +
  geom_label_repel(aes(label = texto_puntito_final_estables, 
                       y = puntito_final),
                   size = 3.5,
                   direction = "x",
                   nudge_x = 0.05) +
  ggplot2::annotate(geom = "text", label = "De acuerdo con la base de datos publicada el 10 de mayo,\n138 casos confirmados murieron el 24 de abril. En la base\nde cinco días atrás, esta cifra fue 122, cinco días antes 93\ny así sucesivamente hasta la base del 24 de abril, cuando\nse reportaron tres muertes.", 
                    x = 18.8, 
                    y = 145, 
                    hjust = 0,
                    size = 4.5,
                    fontface = "bold", 
                    color = "grey30") +
  ggplot2::annotate(geom = "segment", x = 17.9, xend = 18.7, y = 142, yend = 145, size = 0.5, color = "grey70") +
  scale_x_continuous(limits = c(0, max(foo$dias_desde_fecha_muerte) + 1),
                     breaks = 0:max(foo$dias_desde_fecha_muerte)) +
  scale_y_continuous(breaks = seq(0, 400, 10),
                     limits = c(0, 150)) +
  labs(title = "Número de muertes de casos confirmados de Covid-19 ocurridos en cada fecha,\nde acuerdo con los diferentes cortes de la base de datos",
       x = "\nDías desde el primer corte de la base de datos en el que               \nse registró al menos una muerte en la fecha correspondiente               ",
       y = "Número\n",
       caption = "\nElaborado por @segasi y @jorgeacast / Fuente: Bases de datos abiertos publicadas por la Secretaría de Salud entre el 13 de abril y el 10 de mayo.") +
  tema +
  ggsave("03_graficas/04_numero_muertes_en_cada_fecha_de_fallecimiento_por_corte_bd.png", dpi = 200, width = 16, height = 12)



### Gráfica 05: Número de casos confirmados que reportaron el inicio de síntomas en una fecha particular, de acuerdo con los diferentes cortes de la base de datos ----
foo <- 
  mx_datos_todos %>%
  filter(resultado == "Positivo SARS-CoV-2") %>% 
  group_by(fecha_actualizacion, fecha_sintomas) %>% 
  summarise(num_casos = n()) %>% 
  ungroup() %>% 
  filter(fecha_sintomas >= as_date("2020-02-28")) %>% 
  arrange(fecha_sintomas, fecha_actualizacion) %>%  
  mutate(mes_texto = case_when(month(fecha_sintomas) == 2 ~ "febrero",
                               month(fecha_sintomas) == 3 ~ "marzo",
                               month(fecha_sintomas) == 4 ~ "abril",
                               month(fecha_sintomas) == 5 ~ "mayo",
                               month(fecha_sintomas) == 6 ~ "junio",
                               month(fecha_sintomas) == 7 ~ "julio",
                               month(fecha_sintomas) == 8 ~ "agosto",
                               month(fecha_sintomas) == 9 ~ "septiembre",
                               month(fecha_sintomas) == 10 ~ "octubre",
                               month(fecha_sintomas) == 11 ~ "noviembre",
                               month(fecha_sintomas) == 12 ~ "diciembre")) %>% 
  group_by(fecha_sintomas) %>% 
  mutate(dias_desde_inicio_sintomas = row_number(),
         dias_desde_inicio_sintomas = dias_desde_inicio_sintomas - 1, 
         aumento_diario = num_casos - lag(num_casos),
         aumento_diario = ifelse(is.na(aumento_diario), 0, aumento_diario),
         aumento_diario_acumulado = cumsum(aumento_diario),
         puntito_final = ifelse(dias_desde_inicio_sintomas == max(dias_desde_inicio_sintomas), aumento_diario_acumulado, NA),
         texto_puntito_final_altos = ifelse(!is.na(puntito_final) & puntito_final > 725 & dias_desde_inicio_sintomas > 2, str_c(day(fecha_sintomas), " de ", mes_texto, "\n",comma(puntito_final), " casos"), ""),
         texto_puntito_final_recientes = ifelse(!is.na(puntito_final) & dias_desde_inicio_sintomas <= 5, str_c(day(fecha_sintomas), " de ", mes_texto, "\n",comma(puntito_final), " casos"), ""),
         texto_puntito_final_estables = ifelse(!is.na(puntito_final) & puntito_final > 250 & puntito_final < 750 & dias_desde_inicio_sintomas > 20, str_c(day(fecha_sintomas), " de ", mes_texto, "\n",comma(puntito_final), " casos"), "")) %>% 
  ungroup() 

set.seed(15)
foo %>% 
  ggplot(aes(x = dias_desde_inicio_sintomas)) +
  geom_line(aes(y = aumento_diario_acumulado, group = fecha_sintomas),
            size = 1, 
            alpha = 0.6) +
  geom_point(aes(y = puntito_final, group = fecha_sintomas), color = "salmon", size = 2) + 
  geom_label_repel(aes(label = texto_puntito_final_altos, y = puntito_final),
                   size = 3.5) +
  geom_label_repel(aes(label = texto_puntito_final_recientes, y = puntito_final),
                   size = 3.5,
                   # direction = "y",
                   # nudge_y = 0.1,
                   ylim = c(0, 750)) +
  geom_label_repel(aes(label = texto_puntito_final_estables, y = puntito_final),
                   ylim = c(0, 750),
                   size = 3.5) +
  scale_x_continuous(limits = c(0, max(foo$dias_desde_inicio_sintomas) + 0.5),
                     breaks = 0:max(foo$dias_desde_inicio_sintomas)) +
  scale_y_continuous(breaks = seq(0, 4000, 250), 
                     labels = comma) +
  labs(title = "Número de casos confirmados que reportaron el inicio de síntomas en una fecha\nparticular, de acuerdo con los diferentes cortes de la base de datos",
       x = "\nDías desde la publicación de la primera base de datos que incluía el               \nprimer caso confirmado con la fecha de síntomas correspondiente               ",
       y = "Número\n",
       caption = "\nElaborado por @segasi y @jorgeacast / Fuente: Bases de datos abiertos publicadas por la Secretaría de Salud entre el 13 de abril y el 10 de mayo.") +
  tema +
  ggsave("03_graficas/05_numero_casos_confirmados_fecha_sintomas_por_corte_bd.png", dpi = 200, width = 16, height = 11)

