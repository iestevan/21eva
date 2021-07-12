
source ("script-librerias.R")

# DF a mano----
## levanto cuestionarios-----

#directorio
directorio_ejercicios  <-  "ejercicios"

#levanto csvs
listado_ejercicios <- dir(here(directorio_ejercicios), pattern = ".csv") %>%
  str_subset("[:digit:]")

ejercicios  <- tibble(filename = listado_ejercicios) %>%
  #levanto los archivos
  mutate(file_contents = map(filename, ~ read_csv(file.path(here(directorio_ejercicios), .)))) %>%
  #elijo las primeras columnas
  mutate(file_contents = map(file_contents, ~ .x %>% select(ci = 3, estado = 6, comenzado = 7, finalizado = 8, tiempo = 9, cuestionario_nota = 10))) %>%
  #convierto nota en string
  mutate(file_contents = map(file_contents, ~ .x %>% mutate(cuestionario_nota = as.character(cuestionario_nota))))

ejercicios <- unnest(ejercicios, cols = c(file_contents))

rm(directorio_ejercicios, listado_examen)

## acomodo valores ----

# arreglo variables

ejercicios  <-  ejercicios %>%
  mutate (
    ci = as.character(ci),
    ci = str_remove(ci, "uy-do-"),
    ci = str_sub(ci, 1, 7),
    cuestionario_nota = str_replace(cuestionario_nota, ",", "."),
    cuestionario_nota = as.numeric(cuestionario_nota),
    cuestionario_numero = str_extract(filename, "\\d*"),
  ) %>%
  filter(!is.na(ci)) %>% #filtro los promedios por ejercicio
  select(-filename) %>%
  #pongo NAs
  mutate(across(everything(), na_if, "")) %>%
  #convierto factores en characters
  mutate(across(c(estado, comenzado, finalizado, tiempo), as.character)) %>%
  #convierto fechas con lubridate
  mutate(across(c(comenzado, finalizado), dmy_hm)) %>%
  #convierto el tiempo en hms
  mutate(
    tiempo_d = str_match(tiempo, "[:digit:]*(?=[:blank:]día.)"),
    tiempo_h = str_match(tiempo, "[:digit:]*(?=[:blank:]hora.)"),
    tiempo_m = str_match(tiempo, "[:digit:]*(?=[:blank:]minuto.)"),
    tiempo_s = str_match(tiempo, "[:digit:]*(?=[:blank:]segundo.)")
  ) %>%
  mutate(across(c(tiempo_d:tiempo_s), as.numeric)) %>%
  mutate(across(c(tiempo_d:tiempo_s), replace_na, 0)) %>%
  mutate(tiempo = days(tiempo_d)+hours(tiempo_h)+minutes(tiempo_m)+seconds(tiempo_s)) %>%
  select (-c(tiempo_d:tiempo_s))

#agrego ci que no están
# ejercicios  <-  ejercicios %>%
#   mutate(
#     ci = replace(ci, apellido == "Pascualetti Alvez", "4640167"),
#     ci = replace(ci, apellido == "Rodríguez Haretche", "45094925")
#   )

## agrego fecha cierre ----
ejercicios <- ejercicios %>%
  mutate(cierre = case_when(
    cuestionario_numero == "01" | cuestionario_numero == "02" | cuestionario_numero == "03" ~ ymd_hm("2021-04-27 23:59"),
    cuestionario_numero == "04" | cuestionario_numero == "05" | cuestionario_numero == "06" ~ ymd_hm("2021-05-04 23:59"),
    cuestionario_numero == "07" ~ ymd_hm("2021-05-11 23:59"),
    cuestionario_numero == "08" | cuestionario_numero == "09" ~ ymd_hm("2021-05-21 23:59"),
    cuestionario_numero == "10" | cuestionario_numero == "11" ~ ymd_hm("2021-06-13 23:59"),
    cuestionario_numero == "12" ~ ymd_hm("2021-06-20 23:59"),
    cuestionario_numero == "13" ~ ymd_hm("2021-06-27 23:59"),
    cuestionario_numero == "14" ~ ymd_hm("2021-07-11 23:59")
  )) %>%
  mutate(
    antes = (as.numeric(cierre) - as.numeric(comenzado))/(60*60)
    )

## guardo ----
save(ejercicios, file="ejercicios.RData")


# DF por escrapeo -----

## levanto csvs -----

# nombro directorio
directorio  <-  "cuestionarios_neuro_notas"

#levanto csvs
listado_csv <- dir(here(directorio), pattern = ".csv") %>%
  str_subset("[:digit:]")

cuestionarios_notas  <- tibble(filename = listado_csv) %>%
  #levanto los archivos
  mutate(file_contents = map(filename, ~ read_csv(file.path(here(directorio), .)))) %>%
  #convierto nota en string
  mutate(file_contents = map(file_contents, ~ .x %>% mutate(nota = as.character(nota))))

cuestionarios_notas <- unnest(cuestionarios_notas, cols = c(file_contents))

## acomodo -----

# agreglo indice de cuestionario
cuestionarios_notas <- cuestionarios_notas %>%
  mutate(cuestionario_numero = as.numeric(str_extract(filename, "[:digit:]+")))

# arreglo las notas
cuestionarios_notas <- cuestionarios_notas %>%
  mutate(nota = str_replace(nota, ",", ".")) %>%
  mutate(nota = str_extract(nota, "[:digit:]*\\.*[:digit:]*$")) %>%
  mutate(nota = as.numeric(nota)) %>%
  mutate(nota = if_else(filename == "cuestionario_8.csv", nota/10, nota))

# arreglo las fechas
cuestionarios_notas <- cuestionarios_notas %>%
  mutate(across(matches("hora"), ~dmy_hm(.x)))

# agrego las cédulas a partir de los ids
load("estudiantes.RData")

cuestionarios_notas <- estudiantes %>%
  mutate(id = as.numeric(id)) %>%
  left_join(., cuestionarios_notas, by = "id")

rm(estudiantes)

# agrego nombre y tipo de cuestionario
load("cuestionarios.RData")

cuestionarios_notas <- cuestionarios_notas %>%
  left_join(.,
            cuestionarios %>%
              select(cuestionario_numero, cuestionario_nombre, cuestionario_tipo),
            by = "cuestionario_numero")

rm(cuestionarios)

# guardo -----
save(cuestionarios_notas, file = "cuestionarios_notas.RData")



# VISUALIZO -----

## ejercicios ----
load("examenes_notas.RData") #viene de la carpeta de Evaluaciones/Parciales

ejercicios  %>%
  left_join(., examenes_notas %>% select(ci, correcta, incorrecta, NC, total, nota), by = "ci") %>%
  ggplot(aes(cuestionario_numero, fill = as_factor(nota)))+
  geom_bar(position = "fill")

rm(examenes_notas)

ejercicios  %>%
  ggplot(aes(x = total, y = cuestionario_numero, fill = cuestionario_numero))+
  geom_density_ridges(scale = 3, rel_min_height = 0.01)

#tiempo ----
ejercicios %>%
  ggplot(aes(x = antes, y = cuestionario_numero, fill = cuestionario_numero))+
  geom_density_ridges(scale = 3, rel_min_height = 0.01)

ejercicios %>%
  ggplot(aes(antes, total))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~cuestionario_numero)

ejercicios %>%
  mutate(antes = log10(antes)) %>%
  select(cuestionario_numero, antes, total) %>%
  group_by(cuestionario_numero) %>%
  correlation(., p_adjust = "none")

ejercicios %>%
  mutate(fecha = date(comenzado)) %>%
  ggplot(aes(fecha, fill = cuestionario_numero)) +
  geom_density (alpha = .5)

## fecha ----
