
source ("script-librerias.R")

# armo DFs ----
## DF  -----

load("ejercicios.RData") #a mano
load("cuestionarios_notas.RData")

analisis_actividad <- cuestionarios_notas %>%
  filter(cuestionario_tipo == "obligatorio", cuestionario_numero<=9) %>%
  group_by(ci) %>%
  summarise(
    ejerciciosobligatorios_n = n(),
    ejerciciosobligatorios_media = mean(nota/100)
  )

analisis_actividad <- cuestionarios_notas %>%
  filter(cuestionario_tipo == "libre", cuestionario_numero<=30) %>%
  group_by(ci) %>%
  summarise(
    ejercicioslibres_n = n(),
    ejercicioslibres_media = mean(nota, na.rm=TRUE)
  ) %>%
  left_join(analisis_actividad, ., by = "ci") %>%
  mutate(across(matches("ejercicioslibres"), ~replace_na(.x, 0)))

rm(ejercicios, cuestionarios_notas)

## agrego las notas del parcial ----

load("examenes_notas.RData") #viene de la carpeta de Evaluaciones/Parciales

analisis_actividad <- analisis_actividad %>%
  inner_join(., examenes_notas %>% select(ci, grupo, correcta, incorrecta, NC, total, nota), by = "ci") %>%
  mutate(
    grupo = as_factor(grupo),
    norespuesta = 15-correcta,
    correcta_prop = correcta/(correcta+incorrecta+NC)
    )

rm(examenes_notas)

## filtro por que sean estudiantes matriculados de psico ----

analisis_actividad <- read_csv(here("listado.csv")) %>%
  select(-c(2:3)) %>%
  rename("ci" = 1) %>%
  mutate(ci = as.character(ci)) %>%
  left_join(., analisis_actividad, by = "ci")


## guardo ----
save(analisis_actividad, file = "analisis_actividad.RData")
write_csv(analisis_actividad, file = "analisis_actividad.csv")

# filtro casos que voy a usar y acomodo -----
## filtro casos ----
analisis_actividad_limpia <- analisis_actividad %>%
  filter(!is.na(nota)) %>%  # 1012 no llegaron a hacer el parcial
  filter(ejerciciosobligatorios_n>=8) #463 no llegan a 8 ejercicios obligatorios, 727 no hicieron los 9.

## creo categoría de ejercitación -----

analisis_actividad_limpia %>%
  filter(ejercicioslibres_n>0) %>%
  summarise(
    mediana = quantile (ejercicioslibres_n, p = .5)
  )

analisis_actividad_limpia <- analisis_actividad_limpia %>%
  mutate(ejercicioslibres_n_cat = case_when(
    ejercicioslibres_n == 0 ~ "0_nada",
    ejercicioslibres_n < 21 ~ "1_poco",
    TRUE ~ "2_mucho",
  ))


# descriptivos ----
## de la población ----

# inscriptos al curso
inscriptos <- analisis_actividad %>%
  summarise(N = n()) %>%
  mutate(porc_incremento = (N - 2827)/2827*100) # en 2020 fueron 2827 inscriptos

# estudiantes que llegan al parcial
comprometidos <- analisis_actividad_limpia %>%
  summarise(N = n()) %>%
  mutate(N_porc = round(N/inscriptos$N*100, 1))

## Tabla 01 (categorías de ejercitación) ----

tabla_01 <- analisis_actividad_limpia %>%
  group_by(ejercicioslibres_n_cat) %>%
  summarise(N = n()) %>%
  mutate(N_porc = round(N/comprometidos$N*100, 1)) %>%
  transmute(
    ejercicioslibres_n_cat = ejercicioslibres_n_cat,
    N = paste0(N, " (", N_porc, "%)")
    )

tabla_01 <- analisis_actividad_limpia %>%
  group_by(ejercicioslibres_n_cat) %>%
  summarise(across(c(ejerciciosobligatorios_media, correcta_prop), ~media_tabla(.x, 2))) %>%
  left_join(tabla_01, ., by = "ejercicioslibres_n_cat")

tabla_01 <- analisis_actividad_limpia %>%
  group_by(ejercicioslibres_n_cat) %>%
  summarise(across(c(ejercicioslibres_n), ~media_tabla(.x, 1))) %>%
  left_join(tabla_01, ., by = "ejercicioslibres_n_cat")


tabla_01_total <- analisis_actividad_limpia %>%
  summarise(N = n()) %>%
  mutate(N_porc = N/comprometidos$N*100) %>%
  transmute(
    N = paste0(N, " (", N_porc, "%)")
  )

tabla_01_total <- analisis_actividad_limpia %>%
  summarise(across(c(ejerciciosobligatorios_media, correcta_prop), ~media_tabla(.x*100, 2))) %>%
  bind_cols(tabla_01_total, .)


tabla_01_total <- analisis_actividad_limpia %>%
  summarise(across(c(ejercicioslibres_n), ~media_tabla(.x, 1))) %>%
  bind_cols(tabla_01_total, .)

analisis_actividad_limpia %>%
  summarise(across(c(ejercicioslibres_n), ~rango_tabla(.x, 1)))

tabla_01_total <- tabla_01_total %>%
  mutate(ejercicioslibres_n_cat = "Total")

tabla_01 <- bind_rows(tabla_01, tabla_01_total) %>%
  select(ejercicioslibres_n_cat, N, ejercicioslibres_n, ejerciciosobligatorios_media, correcta_prop)

write.csv(tabla_01, file = "tabla_01.csv")

rm(tabla_01, tabla_01_total)

# análisis estadísticos ----

## comparación entre categorías -----
glm_categorias <- analisis_actividad_limpia %>%
  lm(ejerciciosobligatorios_media ~ ejercicioslibres_n_cat-1, data = .)

glm_categorias %>% anova(.)
glm_categorias %>% summary(.)
glm_categorias %>% summ(.)
glm_categorias %>% emmeans(., pairwise ~ ejercicioslibres_n_cat, lmer.df = "satterthwaite", adjust = "tukey")


## cómo les va a los que no llegan a 8 actividades? ----
analisis_actividad %>%
  filter(!is.na(correcta_prop)) %>%
  group_by(ejerciciosobligatorios_n) %>%
  summarise(
    N = n(),
    correcta_prop_mean = mean(correcta_prop)
  )

analisis_actividad %>%
  filter(!is.na(correcta_prop)) %>%
  group_by(ejerciciosobligatorios_n) %>%
  summarise(
    N = n(),
    correcta_prop_mean = mean(correcta_prop)
  ) %>%
  ggplot(aes(as_factor(ejerciciosobligatorios_n), correcta_prop_mean))+
  geom_point(aes(size = N))

## modelo ----

glm_parcial_sc <- analisis_actividad_limpia %>%


  mutate (ejerciciosobligatorios_media = scale(ejerciciosobligatorios_media)) %>%
  glm (cbind(correcta, norespuesta)~ejerciciosobligatorios_media + ejercicioslibres_n_cat, family=binomial(link = "logit"), data = .)

glm_parcial_sc %>% glance(.)
glm_parcial_sc %>% anova(., test = "Chi")
glm_parcial_sc %>% summ(.)

glm_parcial <- analisis_actividad_limpia %>%
  glm(cbind(correcta, norespuesta)~ejerciciosobligatorios_media + ejercicioslibres_n_cat, family=binomial(link = "logit"), data = .)

glm_parcial %>% summ(.)

glm_parcial_10 <- analisis_actividad_limpia %>%
  mutate(ejerciciosobligatorios_media = ejerciciosobligatorios_media*10) %>%
  glm(cbind(correcta, norespuesta)~ejerciciosobligatorios_media + ejercicioslibres_n_cat, family=binomial(link = "logit"), data = .)

glm_parcial_10 %>%
  summ(., scale = F, exp=T)

glm_parcial_10 %>%
  summary(.)%>%
  coef %>%
  exp(.)

glm_parcial_10 %>%
  emtrends (., ~ turno | sueno.examen.SD.H, var="sueno.examen.SD.H", type = "response")

glm_parcial_10 %>%
  emmeans(., pairwise~ejercicioslibres_n_cat, type = "response")

## tabla 2 -----
tabla_02 <- glm_parcial %>%
  tidy %>%
  mutate(across(c(estimate, std.error, statistic), ~round(.x, 1))) %>%
  mutate(
    p.value =
      case_when(
        p.value < 0.001 ~ "< 0.001",
        TRUE ~ as.character(round(p.value,3))
      )
  )  %>%
  mutate(estimate = paste0(estimate, " ± ", std.error)) %>%
  select(term, estimate, statistic, p.value)

tabla_02 <- glm_parcial_sc %>%
  tidy %>%
  mutate(across(c(estimate, std.error, statistic), ~round(.x, 1))) %>%
  mutate(estimate_sc = paste0(estimate, " ± ", std.error)) %>%
  select(term, estimate_sc) %>%
  left_join(tabla_02, ., by = "term") %>%
  select(term, estimate, estimate_sc, statistic, p.value)

write.csv(tabla_02, file = "tabla_02.csv")

rm(tabla_02)

## predicciones ----

predichos <- expand_grid(ejerciciosobligatorios_media = c(0.5, 1), ejercicioslibres_n_cat = c("0_nada", "1_poco", "2_mucho"))

predichos <- predichos %>%
  mutate(
    correcta_prop_estimated = predict(glm_parcial, predichos, se.fit = TRUE, type = "response")$fit,
    correcta_prop_se = predict(glm_parcial, predichos, se.fit = TRUE, type = "response")$se.fit
    ) %>%
  mutate(across (c(correcta_prop_estimated, correcta_prop_se), round, 2)) %>%
  mutate(estimado = paste0(correcta_prop_estimated, " ± ", correcta_prop_se))

# gráficos -----
## gráfico 1 ----

a <- analisis_actividad_limpia %>%
  mutate (
    predichas = predict(glm_parcial, ., se.fit = TRUE, type = "response")$fit,
    predichas_se = predict(glm_parcial, ., se.fit = TRUE, type = "response")$se.fit,
    predichas_upper = predichas + (1.96)*predichas_se,
    predichas_lower = predichas - (1.96)*predichas_se,
  ) %>%
  ggplot()+
  geom_jitter(size = 1, alpha = 0.3, aes(x = ejerciciosobligatorios_media, y = correcta_prop, color = ejercicioslibres_n_cat) )+
  geom_line(aes (x = ejerciciosobligatorios_media, y = predichas, color = ejercicioslibres_n_cat))+
  geom_ribbon(aes(x = ejerciciosobligatorios_media, y = NULL, ymin = predichas_lower, ymax = predichas_upper, fill = ejercicioslibres_n_cat), alpha = .5) +
  labs (x = "Puntaje medio en\nlas actividades obligatorias", y = "Proporción de correctas\nen el parcial", color = "Cantidad de ejercicios\nopcionales", fill = "Cantidad de ejercicios\nopcionales")+
  theme()+
  mitema+
  theme_bw()+
  ylim(0,1.05)

b <- analisis_actividad_limpia %>%
  ggplot(aes(ejercicioslibres_n_cat, ejerciciosobligatorios_media))+
  geom_point(aes(color = ejercicioslibres_n_cat), position = position_jitter(width = .2), size = 1, alpha = 0.2)+
  geom_boxplot(aes(color = ejercicioslibres_n_cat), outlier.shape = NA, size = 1.5, alpha = 0)+
  mitema+
  theme_bw()+
  labs (x = "Cantidad de ejercicios\nopcionales", y = "Puntaje medio en\nlas actividades obligatorias", color = "Cantidad de ejercicios\nopcionales")+
  scale_y_continuous(position="right")+
  coord_flip()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )


c <- glm_parcial3 %>%
  emmeans(., ~ejercicioslibres_n_cat, type = "response") %>%
  tidy() %>%
  ggplot(aes(ejercicioslibres_n_cat, prob))+
  geom_point(data = analisis_actividad_limpia, aes(ejercicioslibres_n_cat, correcta_prop, color = ejercicioslibres_n_cat),
             position = position_jitter(width = .2), size = 1, alpha = 0.2)+
  geom_linerange (aes(ymin = prob-(1.96)*std.error, ymax = prob+(1.96)*std.error), size = 1)+
  geom_point(aes(color = ejercicioslibres_n_cat), size = 1)+
  geom_point(shape = 1,size = 1,colour = "black")+
  mitema+
  theme_bw()+
  labs (x = "Cantidad de ejercicios\nopcionales", y = "Proporción de correctas\nen el parcial", color = "Cantidad de ejercicios\nopcionales")+
  scale_y_continuous(position="right", limits = c(0,1.05))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        )

d <- ggpubr::get_legend(a) %>%
  ggpubr::as_ggplot(.)

ggpubr::ggarrange (b, d, a, c,
                   ncol = 2, nrow = 2,
                   widths = c(2, 1), heights = c(1, 2),
                   labels = c("a", "", "b", "c"),
                   align = "hv",
                   legend = "none")

ggsave("Fig_1.png", width = 20, height = 20, units = c("cm"), dpi = 500)
rm(a, b, c, d)
