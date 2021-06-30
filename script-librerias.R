
#################
# PARÁMETROS
#################

#librerías----
#es importante el orden (hms antes de lubridate)
library(hms); library(tidyverse); library(lubridate);library (circlize); library (sugrrants)

library(afex); library(emmeans); library(jtools); library(lmerTest); library(car); library (correlation)
# library(mediation)

library (ggridges)

library(readxl)

library(xlsx)

library(here)

library(broom); library(broom.mixed)

#scrape
library(rvest); library(httr); library(xml2); library(RSelenium)

#tema de gráficos----

mitema =  theme(text=element_text(family="serif"),
                axis.title=element_text(size=12),
                axis.text.y = element_text(size = 11, colour="black"),
                axis.text= element_text(size = 11, colour="black"),
                legend.text=element_text(size=11),
                legend.title=element_text(size=12))

#funciones----
media_tabla <- function(x, redondeo, na.rm = FALSE) {
  media = mean(x, na.rm = na.rm) %>% round(.,redondeo) %>% sprintf(paste0('%.',redondeo,'f'),.)
  desvio = sd(x, na.rm = na.rm) %>% round(.,redondeo) %>% sprintf(paste0('%.',redondeo,'f'),.)

  return = paste0 (media, " ± ", desvio)
}

rango_tabla <- function(x, redondeo, na.rm = FALSE) {
  rango_i = range (x)[1] %>% round(.,redondeo) %>% sprintf(paste0('%.',redondeo,'f'),.)
  rango_s = range (x)[2] %>% round(.,redondeo) %>% sprintf(paste0('%.',redondeo,'f'),.)

  return = paste0 (rango_i, " - ", rango_s)
}
