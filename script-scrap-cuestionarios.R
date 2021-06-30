
source ("script-librerias.R")

##########################
# escrapeo notas cuestionario
# Es necesario ir a calificaciones, y precisa al menos un cuestionario
# Hay que definir la 1a vez desde EVA que muestre >5000 intentos
##########################

# me logueo----

## logueo EVA viejo -----

#Address of the login webpage
login_viejo <-"https://eva.psico.edu.uy/login/index.php"

#Log in using rvest
my_session_viejo <- session(login_viejo) #Create a persistant session

login_session_viejo <- my_session_viejo %>%
  html_form() %>% # find all forms in the web page
  .[[3]] %>%  # select the form you need to fill
  html_form_set(., username="***",password="***") %>% # fill the form
  session_submit(my_session_viejo, .)  #submit filled form to the session

## logueo EVA nuevo ----

# Dirección
login<-"https://login.udelar.edu.uy/idp/profile/SAML2/Redirect/SSO;jsessionid=26BB2D143865294B02B48CEF3375F020?execution=e1s1"

# Log in using rvest
my_session <- session(login) #Create a persistant session

login_session <- my_session %>%
  html_form() %>% # find all forms in the web page
  .[[1]] %>%  # select the form you need to fill
  html_form_set(., j_username="***",j_password="***") %>% # fill the form
  session_submit(my_session, .)  #submit filled form to the session


#save cookies
my_cookies_table <- cookies(login_session) # get the cookies as a table
my_cookies <- my_cookies_table$value %>% setNames(my_cookies_table$name) # save the cookies as a named vector that you can use in your requests
save(my_cookies, file="my_cookies.RData")

#creando una nueva sesión a partir de las cookies ya guardadas
load("my_cookies.RData")
new_session <- session(login, set_cookies(my_cookies)) # making requests using the same cookies/session

rm(my_session, my_cookies, my_cookies_table)

# identifico los cuestionarios ----

# creo url
url_gral_cuestionarios <- "https://eva.psico.edu.uy/mod/quiz/index.php?id="
# url_gral_cuestionarios <- "https://eva.psico.udelar.edu.uy/mod/quiz/index.php?id="

# id cursos
id_neuro = "1085"
# id_neuro = "27"

# modificar esta equivalencia segun curso
id_curso <- id_neuro

# genero el string de la web
url_cuestionarios <- paste0(url_gral_cuestionarios,id_curso)

# levanto la web de los cuestionarios
cuestionarios_scrap <- my_session_viejo %>%
  session_jump_to(url_cuestionarios)

#guardo textos
cuestionarios <- cuestionarios_scrap %>%
  html_nodes(xpath = '//*[@class="cell c1"]') %>%
  #lavanto un atributo de los input de esa clase
  html_text() %>%
  tibble() %>%
  rename(cuestionario_nombre = 1) %>%
  mutate(cuestionario_nombre = as.character(cuestionario_nombre))

cuestionarios <- cuestionarios_scrap %>%
  html_nodes(xpath = '//*[@class="cell c2"]') %>%
  #lavanto un atributo de los input de esa clase
  html_text() %>%
  tibble() %>%
  rename(cuestionario_cierre = 1) %>%
  mutate(cuestionario_cierre = as.character(cuestionario_cierre)) %>%
  mutate(cuestionario_cierre = as.character(map(str_split(cuestionario_cierre, pattern = ", ", n = 3), 2))) %>%
  mutate(cuestionario_cierre = dmy(cuestionario_cierre)) %>%
  bind_cols(cuestionarios, .)

cuestionarios <- cuestionarios_scrap %>%
  html_nodes(xpath = '//*[@class="cell c3 lastcol"]') %>%
  #lavanto un atributo de los input de esa clase
  html_text() %>%
  tibble() %>%
  rename(cuestionario_intentos = 1) %>%
  mutate(cuestionario_intentos = as.character(cuestionario_intentos)) %>%
  mutate(cuestionario_intentos = as.character(map(str_split(cuestionario_intentos, pattern = ": ", n = 3), 2))) %>%
  mutate(cuestionario_intentos = as.numeric(cuestionario_intentos)) %>%
  bind_cols(cuestionarios, .)

#selecciono y creo índice
cuestionarios <- cuestionarios %>%
  filter(!is.na(cuestionario_intentos))

#les pego la web de resultados
cuestionarios <- cuestionarios_scrap %>%
  html_nodes(xpath = '//*[@class="cell c3 lastcol"]') %>%
  xml_children %>%
  html_attr('href') %>%
  tibble() %>%
  rename(cuestionario_url = 1) %>%
  mutate(cuestionario_url = as.character(cuestionario_url)) %>%
  bind_cols(cuestionarios, .)

#selecciono y creo índice
cuestionarios <- cuestionarios %>%
  filter(!str_detect (cuestionario_nombre, "Parcial")) %>%
  filter(cuestionario_intentos > 1) %>%
  mutate(cuestionario_numero = 1:nrow(.))

# creo categoría
cuestionarios <- cuestionarios %>%
  mutate(cuestionario_tipo = if_else(is.na(cuestionario_cierre), "libre", "obligatorio"))

save(cuestionarios, file = "cuestionarios.RData")

# levanto los cuestionarios -----

#############
# probar identificando la cantidad de paginas y avanzando en las paginas si != NA

# Especifica filtros de registro
url_fin ="&mode=overview&attempts=enrolled_with&onlygraded&states=overdue-finished&group=0&onlyregraded&slotmarks=1"

#contador a cero
cuestionario_numero <- 0
for (url in cuestionarios$cuestionario_url){

  #contador
  cuestionario_numero = 1 + cuestionario_numero

  registroi = my_session_viejo %>%
    session_jump_to(paste0(url,url_fin))

  id =  registroi %>%
    html_nodes(xpath = '//*[@class="cell c2 bold"]/a[1]') %>%
    html_attr("href") %>%
    str_extract( "(?<=\\id=)\\d+")%>%
    tibble() %>%
    rename(id = 1)

  # la ci da problemas porque algunos están vacios... hay que levantarlas de otro lado usando los ids

  # ci <- registroi %>%
  #   html_nodes(xpath = '//*[@class="cell c3"]/text()')%>%
  #   html_text() %>%
  #   tibble() %>%
  #   rename(ci = 1) %>%
  #   mutate(ci = as.numeric(ci)) %>%
  #   na.omit()

  hora_inicio<- registroi %>%
    html_nodes(xpath = '//*[@class="cell c6"]/text()') %>%
    html_text() %>%
    tibble() %>%
    rename(hora_inicio = 1)

  hora_fin <- registroi %>%
    html_nodes(xpath = '//*[@class="cell c7"]/text()') %>%
    html_text() %>%
    tibble() %>%
    rename(hora_fin = 1)

  nota <- registroi %>%
    html_nodes(xpath = '//*[@class="cell c9 bold"]/a') %>%
    html_text() %>%
    tibble() %>%
    rename(nota = 1)

  cbind(id, #ci,
        hora_inicio, hora_fin, nota) %>%
    mutate_all(as.character) %>%
    mutate_all(na_if, "") %>%
    na.omit() %>%
    write_csv(., paste0(getwd(),"/cuestionarios_neuro_notas/", "cuestionario_",cuestionario_numero, ".csv"))

  print(paste0('el cuestionario ',cuestionario_numero, ' is ready' ))

}
