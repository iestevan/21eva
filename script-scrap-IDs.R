
source ("script-librerias.R")


# me logueo ----
#Address of the login webpage
login<-"https://eva.psico.edu.uy/login/index.php"

#Log in using rvest
my_session <- session(login) #Create a persistant session

login_session <- my_session %>%
  html_form() %>% # find all forms in the web page
  .[[3]] %>%  # select the form you need to fill
  html_form_set(., username="iestevan",password="IE*1206") %>% # fill the form
  session_submit(my_session, .)  #submit filled form to the session

#save cookies
my_cookies_table <- cookies(login_session) # get the cookies as a table
my_cookies <- my_cookies_table$value %>% setNames(my_cookies_table$name) # save the cookies as a named vector that you can use in your requests
save(my_cookies, file="my_cookies.RData")

#creando una nueva sesión a partir de las cookies ya guardadas
load("my_cookies.RData")
new_session <- session(login, set_cookies(my_cookies)) # making requests using the same cookies/session

rm(my_session, my_cookies, my_cookies_table)

# defino y levanto usuarios ----

#total de paginas usuarios
total_usuarios = new_session %>%
  session_jump_to("https://eva.psico.edu.uy/user/index.php?id=1085") %>%
  html_node(xpath = '//*[@class="paging"]') %>%
  html_text() %>%
  str_match_all ("[:digit:]+") %>%
  simplify() %>%
  last() %>%
  as.numeric %>%
  #si solo tienen una pagina de registros
  replace_na (., 1)

#creo url
url_usuarios_neuro <- paste0("https://eva.psico.edu.uy/user/index.php?contextid=86885&id=1085&perpage=",  total_usuarios*20, "&unified-filters%5B0%5D=4%3A5")

#levanto datos según clases

usuarios = new_session %>%
  jump_to(url_usuarios_neuro)

id =  usuarios %>%
  html_nodes(xpath = '//*[@class="usercheckbox"]') %>%
  #lavanto un atributo de los input de esa clase
  html_attr("name")  %>%
  as.data.frame() %>%
  rename(id = 1)  %>%
  mutate_all(na_if, "") %>%
  drop_na()

# nombre =  usuarios %>%
#   html_nodes(xpath = '//*[@class="cell c1"]') %>%
#   html_text() %>%
#   as.data.frame() %>%
#   rename(nombre = 1)  %>%
#   mutate_all(na_if, "") %>%
#   drop_na()

ci = usuarios %>%
  html_nodes(xpath = '//*[@class="cell c2"]') %>%
  html_text() %>%
  as.data.frame() %>%
  rename(ci = 1) %>%
  #porque me genera N perpage de rows, y hay CI vacias
  slice(1:nrow(id))

# mails = usuarios %>%
#   html_nodes(xpath = '//*[@class="cell c3"]') %>%
#   html_text() %>%
#   as.data.frame() %>%
#   rename(mails = 1) %>%
#   #porque me genera N perpage de rows
#   slice(1:nrow(id))

#pego
estudiantes = bind_cols(id, ci, mails)
rm(url_usuarios_neuro, usuarios, total_usuarios, id, ci, mails)


#arreglo
estudiantes = estudiantes %>%
  mutate_all(as.character) %>%
  mutate(id = str_remove(id, "user") ) %>%
  #remuevo pibes sin ci porque además me trancan el loop de registro
  mutate_all(na_if, "") %>%
  drop_na()

#guardo
save(estudiantes, file="estudiantes.RData")
