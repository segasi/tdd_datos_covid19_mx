### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Importar y procesar datos abiertos de Salud ----

# Necesitamos importar las bases de datos en dos pasos porque entre el 13 y 18 de abril (i) no existiía la columna id_registro y (ii) en uno o más cortes diarios las columnas habla_lengua_indig y otra_com fueron llamadas habla_lengua_indi y otra_con

# Bases de datos publicadas entre el 13 y el 18 de abril
mx_datos_13_18_abril <- 
  list.files(path = "../../../../../../10 recursos/datos/ssa/dg_epidemiologia/covid_19/01_bd_sin_id/", # Ojo: necesitas adecuar la ruta de acuerdo con a la estructura de tu disco duro para poder importar los archivos
             pattern="*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))) %>% 
  # "Limpiar" nombres de variables
  clean_names() 

# Bases de datos publicadas a partir del 19 de abril
mx_datos_19_abril_en_adelante <- 
  list.files(path = "../../../../../../10 recursos/datos/ssa/dg_epidemiologia/covid_19/02_bd_con_id/", # Ojo: necesitas adecuar la ruta de acuerdo con a la estructura de tu disco duro para poder importar los archivos
             pattern="*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., locale = locale(encoding = "latin1"), col_types = cols(.default = "c"))) %>% 
  # "Limpiar" nombres de variables
  clean_names() 

### Hacer diversos ajustes a mx_datos_sin_id para que tenga el mismo número de columnas que mx_datos_con_id, y las columnbas de ambs se llamen igual ----
mx_datos_13_18_abril <- 
  mx_datos_13_18_abril %>% 
  # Generar una sola versión de las variables habla_lengua_indig y habla_lengua_indi, así como otra_con y otra_com, conservando solo la primeras
  mutate(habla_lengua_indig = ifelse(test = is.na(habla_lengua_indig), 
                                     yes = habla_lengua_indi, 
                                     no = habla_lengua_indig),
         otra_com = ifelse(test = is.na(otra_com),
                           yes = otra_con,
                           no = otra_com)) %>% 
  # Eliminar columnas habla_lengua_indi y otra_con
  select(-c(habla_lengua_indi, otra_con)) %>% 
  # Generar columna id_registro, misma que tendrá valor NA para todas las observaciones. Esto es necesario para después poder unir los dos tibbles
  mutate(id_registro = NA)

### Unir los dos tibbles para tener en un solo objeto todos los cortes de la bd publicadas hasta la fecha ----
mx_datos_todos <- rbind(mx_datos_13_18_abril, mx_datos_19_abril_en_adelante)

### Importar el catálogo de muncipios para después tener el nombre de cada municipio en la base de datos ----
cve_mpo <- 
  read_excel("../../../../../../10 recursos/datos/ssa/dg_epidemiologia/covid_19/Catalogos_0412.xlsx",  # Ojo: necesitas adecuar la ruta de acuerdo con a la estructura de tu disco duro para poder importar el archivo
             sheet = "Catálogo MUNICIPIOS") %>% 
  clean_names() %>% 
  mutate(cve_mpo = str_c(clave_entidad, clave_municipio))


## Realizar diversas tranformaciones a bd_datos_todos ----
limpiar_datos_abiertos <- 
  function(bd) {
    
    bd <- 
      bd %>% 
      mutate(fecha_actualizacion = as_date(fecha_actualizacion),
             fecha_ingreso = as_date(fecha_ingreso),
             fecha_sintomas = as_date(fecha_sintomas),
             fecha_def = as_date(fecha_def),
             origen = case_when(origen == 1 ~ "USMER",
                                origen == 2 ~ "Fuera de USMER",
                                origen == 3 ~ "No especificado"),
             sector = case_when(sector == 1 ~ "Cruz Roja",
                                sector == 2 ~ "DIF",
                                sector == 3 ~ "Estatal",
                                sector == 4 ~ "IMSS",
                                sector == 5 ~ "IMSS-Bienestar",
                                sector == 6 ~ "ISSSTE",
                                sector == 7 ~ "Municipal",
                                sector == 8 ~ "PEMEX",
                                sector == 9 ~ "Privada",
                                sector == 10 ~ "SEDENA",
                                sector == 11 ~ "SEMAR",
                                sector == 12 ~ "SSA",
                                sector == 13 ~ "Universitario",
                                sector == 99 ~ "No especificado"),
             sexo = case_when(sexo == 1 ~ "Mujer",
                              sexo == 2 ~"Hombre",
                              sexo == 99 ~ "No especificado"),
             tipo_paciente = case_when(tipo_paciente == 1 ~ "Ambulatorio",
                                       tipo_paciente == 2 ~ "Hospitalizado",
                                       tipo_paciente == 99 ~ "No especificado"),
             nacionalidad = case_when(nacionalidad == 1 ~ "Mexicana",
                                      nacionalidad == 2 ~ "Extranjera",
                                      nacionalidad == 99 ~ "No especificada"),
             resultado = case_when(resultado == 1 ~ "Positivo SARS-CoV-2",
                                   resultado == 2 ~ "No positivo SARS-CoV-2",
                                   resultado == 3 ~ "Resultado pendiente"),
             entidad_nac = case_when(entidad_nac == "01" ~ "AGUASCALIENTES",
                                     entidad_nac == "02" ~ "BAJA CALIFORNIA",
                                     entidad_nac == "03" ~ "BAJA CALIFORNIA SUR",
                                     entidad_nac == "04" ~ "CAMPECHE",
                                     entidad_nac == "05" ~ "COAHUILA",
                                     entidad_nac == "06" ~ "COLIMA",
                                     entidad_nac == "07" ~ "CHIAPAS",
                                     entidad_nac == "08" ~ "CHIHUAHUA",
                                     entidad_nac == "09" ~ "CIUDAD DE MÉXICO",
                                     entidad_nac == "10" ~ "DURANGO",
                                     entidad_nac == "11" ~ "GUANAJUATO",
                                     entidad_nac == "12" ~ "GUERRERO",
                                     entidad_nac == "13" ~ "HIDALGO",
                                     entidad_nac == "14" ~ "JALISCO",
                                     entidad_nac == "15" ~ "MÉXICO",
                                     entidad_nac == "16" ~ "MICHOACÁN",
                                     entidad_nac == "17" ~ "MORELOS",
                                     entidad_nac == "18" ~ "NAYARIT",
                                     entidad_nac == "19" ~ "NUEVO LEÓN",
                                     entidad_nac == "20" ~ "OAXACA",
                                     entidad_nac == "21" ~ "PUEBLA",
                                     entidad_nac == "22" ~ "QUERÉTARO",
                                     entidad_nac == "23" ~ "QUINTANA ROO",
                                     entidad_nac == "24" ~ "SAN LUIS POTOSÍ",
                                     entidad_nac == "25" ~ "SINALOA",
                                     entidad_nac == "26" ~ "SONORA",
                                     entidad_nac == "27" ~ "TABASCO",
                                     entidad_nac == "28" ~ "TAMAULIPAS",
                                     entidad_nac == "29" ~ "TLAXCALA",
                                     entidad_nac == "30" ~ "VERACRUZ",
                                     entidad_nac == "31" ~ "YUCATÁN",
                                     entidad_nac == "32" ~ "ZACATECAS",
                                     entidad_nac == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                     entidad_nac == "97" ~ "NO APLICA",
                                     entidad_nac == "98" ~ "SE IGNORA",
                                     entidad_nac == "99" ~ "NO ESPECIFICADO"),
             entidad_nac = str_to_title(entidad_nac),
             entidad_residencia = case_when(entidad_res == "01" ~ "AGUASCALIENTES",
                                            entidad_res == "02" ~ "BAJA CALIFORNIA",
                                            entidad_res == "03" ~ "BAJA CALIFORNIA SUR",
                                            entidad_res == "04" ~ "CAMPECHE",
                                            entidad_res == "05" ~ "COAHUILA",
                                            entidad_res == "06" ~ "COLIMA",
                                            entidad_res == "07" ~ "CHIAPAS",
                                            entidad_res == "08" ~ "CHIHUAHUA",
                                            entidad_res == "09" ~ "CIUDAD DE MÉXICO",
                                            entidad_res == "10" ~ "DURANGO",
                                            entidad_res == "11" ~ "GUANAJUATO",
                                            entidad_res == "12" ~ "GUERRERO",
                                            entidad_res == "13" ~ "HIDALGO",
                                            entidad_res == "14" ~ "JALISCO",
                                            entidad_res == "15" ~ "MÉXICO",
                                            entidad_res == "16" ~ "MICHOACÁN",
                                            entidad_res == "17" ~ "MORELOS",
                                            entidad_res == "18" ~ "NAYARIT",
                                            entidad_res == "19" ~ "NUEVO LEÓN",
                                            entidad_res == "20" ~ "OAXACA",
                                            entidad_res == "21" ~ "PUEBLA",
                                            entidad_res == "22" ~ "QUERÉTARO",
                                            entidad_res == "23" ~ "QUINTANA ROO",
                                            entidad_res == "24" ~ "SAN LUIS POTOSÍ",
                                            entidad_res == "25" ~ "SINALOA",
                                            entidad_res == "26" ~ "SONORA",
                                            entidad_res == "27" ~ "TABASCO",
                                            entidad_res == "28" ~ "TAMAULIPAS",
                                            entidad_res == "29" ~ "TLAXCALA",
                                            entidad_res == "30" ~ "VERACRUZ",
                                            entidad_res == "31" ~ "YUCATÁN",
                                            entidad_res == "32" ~ "ZACATECAS",
                                            entidad_res == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                            entidad_res == "97" ~ "NO APLICA",
                                            entidad_res == "98" ~ "SE IGNORA",
                                            entidad_res == "99" ~ "NO ESPECIFICADO"),
             entidad_residencia = str_to_title(entidad_residencia),
             entidad_residencia = str_replace(entidad_residencia, " De ", " de "),
             entidad_uni_med = case_when(entidad_um == "01" ~ "AGUASCALIENTES",
                                         entidad_um == "02" ~ "BAJA CALIFORNIA",
                                         entidad_um == "03" ~ "BAJA CALIFORNIA SUR",
                                         entidad_um == "04" ~ "CAMPECHE",
                                         entidad_um == "05" ~ "COAHUILA",
                                         entidad_um == "06" ~ "COLIMA",
                                         entidad_um == "07" ~ "CHIAPAS",
                                         entidad_um == "08" ~ "CHIHUAHUA",
                                         entidad_um == "09" ~ "CIUDAD DE MÉXICO",
                                         entidad_um == "10" ~ "DURANGO",
                                         entidad_um == "11" ~ "GUANAJUATO",
                                         entidad_um == "12" ~ "GUERRERO",
                                         entidad_um == "13" ~ "HIDALGO",
                                         entidad_um == "14" ~ "JALISCO",
                                         entidad_um == "15" ~ "MÉXICO",
                                         entidad_um == "16" ~ "MICHOACÁN",
                                         entidad_um == "17" ~ "MORELOS",
                                         entidad_um == "18" ~ "NAYARIT",
                                         entidad_um == "19" ~ "NUEVO LEÓN",
                                         entidad_um == "20" ~ "OAXACA",
                                         entidad_um == "21" ~ "PUEBLA",
                                         entidad_um == "22" ~ "QUERÉTARO",
                                         entidad_um == "23" ~ "QUINTANA ROO",
                                         entidad_um == "24" ~ "SAN LUIS POTOSÍ",
                                         entidad_um == "25" ~ "SINALOA",
                                         entidad_um == "26" ~ "SONORA",
                                         entidad_um == "27" ~ "TABASCO",
                                         entidad_um == "28" ~ "TAMAULIPAS",
                                         entidad_um == "29" ~ "TLAXCALA",
                                         entidad_um == "30" ~ "VERACRUZ",
                                         entidad_um == "31" ~ "YUCATÁN",
                                         entidad_um == "32" ~ "ZACATECAS",
                                         entidad_um == "36" ~ "ESTADOS UNIDOS MEXICANOS",
                                         entidad_um == "97" ~ "NO APLICA",
                                         entidad_um == "98" ~ "SE IGNORA",
                                         entidad_um == "99" ~ "NO ESPECIFICADO"),
             entidad_uni_med = str_to_title(entidad_uni_med),
             entidad_uni_med = str_replace(entidad_uni_med, " De ", " de "),
             intubado = case_when(intubado == 1 ~ "Sí",
                                  intubado == 2 ~ "No",
                                  intubado == 97 ~ "No aplica",
                                  intubado == 98 ~ "Se ignora",
                                  intubado == 99 ~ "No especificado"),
             neumonia = case_when(neumonia == 1 ~ "Sí",
                                  neumonia == 2 ~ "No",
                                  neumonia == 97 ~ "No aplica",
                                  neumonia == 98 ~ "Se ignora",
                                  neumonia == 99 ~ "No especificado"),
             embarazo = case_when(embarazo == 1 ~ "Sí",
                                  embarazo == 2 ~ "No",
                                  embarazo == 97 ~ "No aplica",
                                  embarazo == 98 ~ "Se ignora",
                                  embarazo == 99 ~ "No especificado"),
             habla_lengua_indig = case_when(habla_lengua_indig == 1 ~ "Sí",
                                            habla_lengua_indig == 2 ~ "No",
                                            habla_lengua_indig== 97 ~ "No aplica", 
                                            habla_lengua_indig== 98 ~ "Se ignora",
                                            habla_lengua_indig == 99 ~ "No especificado"),
             diabetes = case_when(diabetes == 1 ~ "Sí",
                                  diabetes == 2 ~ "No",
                                  diabetes == 97 ~ "No aplica",
                                  diabetes == 98 ~ "Se ignora",
                                  diabetes == 99 ~ "No especificado"),
             epoc = case_when(epoc == 1 ~ "Sí",
                              epoc == 2 ~ "No",
                              epoc == 97 ~ "No aplica",
                              epoc == 98 ~ "Se ignora",
                              epoc == 99 ~ "No especificado"),
             asma = case_when(asma == 1 ~ "Sí",
                              asma == 2 ~ "No",
                              asma == 97 ~ "No aplica",
                              asma == 98 ~ "Se ignora",
                              asma == 99 ~ "No especificado"),
             inmusupr = case_when(inmusupr == 1 ~ "Sí",
                                  inmusupr == 2 ~ "No",
                                  inmusupr == 97 ~ "No aplica",
                                  inmusupr == 98 ~ "Se ignora",
                                  inmusupr == 99 ~ "No especificado"),
             hipertension = case_when(hipertension == 1 ~ "Sí",
                                      hipertension == 2 ~ "No",
                                      hipertension == 97 ~ "No aplica",
                                      hipertension == 98 ~ "Se ignora",
                                      hipertension == 99 ~ "No especificado"),
             otra_com = case_when(otra_com == 1 ~ "Sí",
                                  otra_com == 2 ~ "No",
                                  otra_com == 97 ~ "No aplica",
                                  otra_com == 98 ~ "Se ignora",
                                  otra_com == 99 ~ "No especificado"),
             cardiovascular = case_when(cardiovascular == 1 ~ "Sí",
                                        cardiovascular == 2 ~ "No",
                                        cardiovascular == 97 ~ "No aplica",
                                        cardiovascular == 98 ~ "Se ignora",
                                        cardiovascular == 99 ~ "No especificado"),
             obesidad = case_when(obesidad == 1 ~ "Sí",
                                  obesidad == 2 ~ "No",
                                  obesidad == 97 ~ "No aplica",
                                  obesidad == 98 ~ "Se ignora",
                                  obesidad == 99 ~ "No especificado"),
             renal_cronica = case_when(renal_cronica == 1 ~ "Sí",
                                       renal_cronica == 2 ~ "No",
                                       renal_cronica == 97 ~ "No aplica",
                                       renal_cronica == 98 ~ "Se ignora",
                                       renal_cronica == 99 ~ "No especificado"),
             tabaquismo = case_when(tabaquismo == 1 ~ "Sí",
                                    tabaquismo == 2 ~ "No",
                                    tabaquismo == 97 ~ "No aplica",
                                    tabaquismo == 98 ~ "Se ignora",
                                    tabaquismo == 99 ~ "No especificado"),
             otro_caso = case_when(otro_caso == 1 ~ "Sí",
                                   otro_caso == 2 ~ "No",
                                   otro_caso == 97 ~ "No aplica",
                                   otro_caso == 98 ~ "Se ignora",
                                   otro_caso == 99 ~ "No especificado"),
             uci = case_when(uci == 1 ~ "Sí",
                             uci == 2 ~ "No",
                             uci == 97 ~ "No aplica",
                             uci == 98 ~ "Se ignora",
                             uci == 99 ~ "No especificado"),
             cve_mpo = str_c(entidad_res, municipio_res))
  }

mx_datos_todos <- limpiar_datos_abiertos(mx_datos_todos)
# OJO: el warning que aparece después de este paso se debe a que los valores 9999-99-99 en la variable fecha_def fueron transformados en NAs

### Eliminar las observaciones posteriores al 8 de mayo. No las incluímos porque rompen la simetría en el número de facetas de la tercer gráfica ----
mx_datos_todos <-
  mx_datos_todos %>%
  filter(fecha_actualizacion < as_date("2020-05-11"))
