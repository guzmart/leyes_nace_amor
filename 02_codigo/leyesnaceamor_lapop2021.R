# 0. Configuración inicial ----
require(devtools)
# devtools::install_github('thomasp85/gganimate')
# devtools::install_github("lchiffon/wordcloud2")
require(pacman)
p_load(haven,
       MASS, survey, srvyr, # análisis de encuestas
       wordcloud, wordcloud2, RColorBrewer, tm, # text mining
       gganimate, ggcorrplot, gridExtra, ggthemes, magick, # Animación y elementos extras para ggplot
       rgeos, transformr, # mapas y hexbins
       webshot, htmlwidgets) # guardar wordcloud2
require(tidyverse)

inp <- "~/leyes_nace_amor/01_datos/"
out <- "~/leyes_nace_amor/03_out/"

format_date <- "%Y-%m-%d"

# 1. Procesamiento de LAPOP -----
lapop_2012 <- read_dta(paste0(inp, "LAPOP_2012.dta"), encoding = 'latin1') %>% 
  janitor::clean_names() %>% 
  select(cve_ent = prov, idnum:fecha, sex = sexi, matrimonio = d6, ideologia = l1, educacion_anio = ed, religion = q5b, edad = q2, wt) %>% 
  mutate(
    idnum = as.character(idnum),
    cve_ent = str_pad(str_sub(cve_ent,2,3), 2, "l", "0"),
    fecha = str_replace_all(fecha, "1582", "2012"),
    fecha = ifelse(is.na(fecha), "2012-02-29", fecha),
    fecha = as.Date.character(fecha, "%Y-%m-%d"),
    anio = "2012",
    anio_2012 = "1",
    anio_2014 = "0",
    anio_2017 = "0",
    anio_2019 = "0"
  ) %>% 
  glimpse 

lapop_2014 <- read_dta(paste0(inp, "LAPOP_2014.dta"), encoding = 'latin1') %>% 
  janitor::clean_names() %>% 
  select(cve_ent = prov, idnum:fecha, sex = sexi, matrimonio = d6, ideologia = l1, educacion_anio = ed, religion = q5b, edad = q2, wt) %>% 
  mutate(
    idnum = as.character(idnum),
    cve_ent = str_pad(str_sub(cve_ent,2,3), 2, "l", "0"),
    fecha = ifelse(is.na(fecha), "2014-02-22", fecha),
    fecha = as.Date.character(fecha, "%Y-%m-%d"),
    anio = "2014",
    anio_2012 = "0",
    anio_2014 = "1",
    anio_2017 = "0",
    anio_2019 = "0"
  ) %>% 
  glimpse

lapop_2017 <- read_dta(paste0(inp, "LAPOP_2017.dta"), encoding = 'latin1') %>% 
  janitor::clean_names() %>% 
  select(cve_ent = prov, idnum:fecha, estratopri, estratosec, sex = sexi, matrimonio = d6, ideologia = l1, educacion_anio = ed, religion = q5b, edad = q2, wt) %>% 
  mutate(
    idnum = as.character(idnum),
    cve_ent = str_pad(str_sub(cve_ent,2,3), 2, "l", "0"),
    anio = "2017",
    anio_2012 = "0",
    anio_2014 = "0",
    anio_2017 = "1",
    anio_2019 = "0"
  ) %>% 
  glimpse

lapop_2019 <- read_dta(paste0(inp, "LAPOP_2019.dta"), encoding = 'latin1') %>% 
  janitor::clean_names() %>% 
  select(cve_ent = prov, idnum:fecha, sex = sexi, matrimonio = d6, ideologia = l1, educacion_anio = ed, religion = q5b, edad = q2, wt) %>% 
  mutate(
    idnum = as.character(idnum),
    cve_ent = str_pad(str_sub(cve_ent,2,3), 2, "l", "0"),
    anio = "2019",
    anio_2012 = "0",
    anio_2014 = "0",
    anio_2017 = "0",
    anio_2019 = "1"
  ) %>% 
  glimpse

lapop <- bind_rows(
  lapop_2012, lapop_2014
) %>% 
  bind_rows(lapop_2017) %>% 
  bind_rows(lapop_2019)

# 2. Legislaciones ----
legis <- readxl::read_excel(paste0(inp, "legislaciones.xlsx")) %>% 
  select(-c(matr_igua, ley_conv)) %>% 
  left_join(
    read.csv(paste0(inp,"entidades.csv")) %>% 
      mutate(cve_ent = str_pad(ent, 2, "l", "0")) %>% 
      select(cve_ent, abrv)
  ) %>% 
  mutate_at(
    vars(starts_with("fecha")),
    ~as.Date.character(as.character(.), "%Y-%m-%d")
  ) %>% 
  glimpse

legis_long <- legis %>% 
  pivot_longer(
    starts_with("inclusi"),
    names_prefix = "inclusion_",
    names_to = "anio",
    values_to = "inclusion"
  ) %>% 
  mutate(
    inclusion_total = ifelse(inclusion == 2, "1", "0"),
    inclusion_parcial = ifelse(inclusion == 1, "1", "0")
  )

# 3. Consolidación de base de datos ----
data <- lapop %>% 
  left_join(legis_long) %>% 
  mutate(
    dias_matr = as.Date.character(fecha, format_date)-as.Date.character(fecha_matr, format_date),
    dias_matr = ifelse(dias_matr<0,NA,dias_matr),
    dias_lsc = ifelse(
      is.na(fecha_matr), 
      as.Date.character(fecha, format_date)-as.Date.character(fecha_lsc, format_date),
      as.Date.character(fecha_matr, format_date)-as.Date.character(fecha_lsc, format_date)
    ),
    dias_lsc = ifelse(dias_lsc<0,NA,dias_lsc)
  )

# 4. Diseño muestral ----
design <- svydesign(id=~upm, # ID de la observación
                      strata=~estratopri, # Estratificación
                      weights=~lapop$wt, # Peso
                      data = data, nest = TRUE)

# 5. Descriptivos ----
## 5.1. Mapa de calor del nivel de inclusión en legislaciones estatales ----
fiuf <- "Nivel de inclusión en legislaciones locales"
fiuffi <- "Fuente: Elaboración propia con revisión de legislaciones estatales.\nActualización: octubre de 2021 | @guzmart_"

ggplot(data = legis_long) +
  geom_tile(mapping = aes(x = as.numeric(anio), 
                          y = fct_rev(abrv),
                          fill = as.factor(inclusion)),
            col = "white") +
  scale_fill_manual("Niveles de inclusión",
                    values=c("#DEEBF7", "#5E88AE", "#1F4E79"),
                    labels = c("Nula",
                               "Parcial",
                               "Total")) +
  labs(x = "", y = "",
       title = str_wrap(fiuf, width = 100),
       caption = fiuffi) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(position = "top",breaks = 2005:2021,labels = paste0("'", str_pad(5:21,2,"l","0"))) +
  theme_hc() +
  theme(plot.title = element_text(size = 35, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777",hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Ubuntu"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 10, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 25),
        legend.position = "bottom") +
  coord_fixed()
ggsave(filename = "01_inclusion_tile_2005_2021.png", path = out, width = 15, height = 10, dpi = 100)  

## 5.2. Actitudes hacia matrimonios del mismo sexo ----
a <- 
lapop %>% 
  mutate(apr_matr = case_when(
    matrimonio < 5 ~ "2. Desaprueba",
    matrimonio > 5 ~ "1. Aprueba",
    matrimonio == 5 ~ "3. No aprueba ni desaprueba",
    T ~ NA_character_
  )) %>% 
  drop_na(apr_matr) %>% 
  as_survey_design(weights = wt) %>% 
  group_by(anio, apr_matr) %>% 
  summarise(prop = survey_mean(na.rm = T)) %>% 
  ungroup() %>% 
  left_join(
    lapop %>% 
      as_survey_design(weights = wt) %>% 
      group_by(anio) %>% 
      summarise(act_prom = survey_mean(matrimonio, na.rm = T)) %>% 
      ungroup()
  ) %>% 
  mutate(anio_act = paste0(anio, "\nActitud promedio: ", round(act_prom,1)))

fiuf <- "Actitudes hacia el matrimonio igualitario"
fiuffi <- "Fuente: Elaboración propia con base en encuestas LAPOP (2012-2019). | @guzmart_"
ggplot(
  a,
  aes(
    x = anio_act,
    y = prop,
    fill = apr_matr,
    label = paste0(round(prop*100,1),"%")
  )
) +
  geom_col(position = "dodge2") +
  geom_text(position = position_dodge(width = .9), vjust = -0.6, size = 7) +
  scale_fill_manual("",
                    values=c("#1F4E79", "#DEEBF7", "#5E88AE"),
                    labels = c("Aprueba",
                               "Desaprueba",
                               "Ni aprueba ni desaprueba")) +
  labs(x = "", y = "",
       title = str_wrap(fiuf, width = 100),
       caption = fiuffi) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,.65))+
  theme_hc() +
  theme(plot.title = element_text(size = 35, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777",hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Ubuntu"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 25),
        legend.position = "top")
ggsave(filename = "02_actitudes_2012_2019.png", path = out, width = 15, height = 10, dpi = 100)  


## 5.3. Densidad ----
means <- data %>% 
  filter(inclusion==0) %>% 
  as_survey_design(weights = wt) %>% 
  summarise(mean = survey_mean(matrimonio, na.rm = T)) %>% 
  mutate(inclusion = 0) %>% 
  bind_rows(
    data %>% 
      filter(inclusion==1) %>% 
      as_survey_design(weights = wt) %>% 
      summarise(mean = survey_mean(matrimonio, na.rm = T)) %>% 
      mutate(inclusion = 1)
  ) %>% 
  bind_rows(
    data %>% 
      filter(inclusion==2) %>% 
      as_survey_design(weights = wt) %>% 
      summarise(mean = survey_mean(matrimonio, na.rm = T)) %>% 
      mutate(inclusion = 2)
  )

fiuf <- "Dispersión de actitudes hacia el matrimonio igualitario"

ggplot(data, aes(matrimonio, fill = as.factor(inclusion), colour = as.factor(inclusion))) +
  geom_density(alpha = 0.1) +
  geom_vline(xintercept = means$mean[means$inclusion==0], col = "#A5A5A5")+
  geom_vline(xintercept = means$mean[means$inclusion==1], col = "#5E88AE")+
  geom_vline(xintercept = means$mean[means$inclusion==2], col = "#1F4E79")+
  scale_fill_manual(name="Niveles de inclusión",
                    values=c("#A5A5A5","#5E88AE", "#1F4E79"),
                    labels=c("Nulo", "Parcial", "Total"))  +
  scale_color_manual(name="Niveles de inclusión",
                    values=c("#A5A5A5","#5E88AE", "#1F4E79"),
                    labels=c("Nulo", "Parcial", "Total"))  +
  labs(x = "Actitudes", y = "Densidad",
       title = str_wrap(fiuf, width = 100),
       caption = fiuffi) +
  theme_hc() +
  theme(plot.title = element_text(size = 35, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777",hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Ubuntu"),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 25),
        legend.position = "top")
ggsave(filename = "03_dispersión.png", path = out, width = 15, height = 10, dpi = 100)  
# 6. Relación entre nivel de inclusión en legislaciones estatales y actitudes hacia el matrimonio igualitario----
# Estimación del modelo
svymodniv <- svyglm(matrimonio ~ inclusion + 
                      ideologia + religion + educacion_anio + 
                      edad + sex + anio_2014 + anio_2017 +anio_2019,
                    design = design)
stargazer::stargazer(svymodniv,type = "text")
# Usaré métodos de simulación estadística para obtener valores predichos
# obtener coeficientes y matriz de covariazas (sigma)
coef <- coefficients(svymodniv)
sigma <- vcov(svymodniv)

# simulación
set.seed(12345)
sim <- mvrnorm(n = 10000, mu = coef, Sigma = sigma)

# Primero hacemos una prueba
# Variar inclusión entre 0, 1 y 2
inclusion <- seq(0,2,1)
# fijar las demás variables en promedio (redondeado para categóricas)
ideologia <- mean(data$ideologia, na.rm = TRUE)
ideologia <- round(ideologia, digits = 0)
religion <- mean(data$religion, na.rm = TRUE)
religion <- round(religion, digits = 0)
educacion_anio <- mean(data$educacion_anio, na.rm = TRUE)
educacion_anio <- round(educacion_anio, digits = 0)
edad <- mean(data$edad, na.rm = TRUE)
sex <- mean(as.numeric(data$sex), na.rm = TRUE)
sex <- round(sex, digits = 0)


theta <-  sim[,"(Intercept)"] +
  sim[,"inclusion"]*0 +
  sim[,"ideologia"]*ideologia +
  sim[,"religion"]*religion +
  sim[,"educacion_anio"]*educacion_anio +
  sim[,"edad"]*edad +
  sim[,"sex"]*sex +
  sim[,"anio_20141"]*0 +
  sim[,"anio_20171"]*0 +
  sim[,"anio_20191"]*0 

# intervalos de confianza para valores esperados de inclusion nula
# en 2012 (ésta es la prueba)
quantile(theta, c(0.025, 0.975))

# Función para estimar intervalos de confianza
mi.confianza <- function(i,j,k,l){
  theta <-   sim[,"(Intercept)"] +
    sim[,"inclusion"]*i +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"sex"]*sex +
    sim[,"anio_20141"]*j +
    sim[,"anio_20171"]*k +
    sim[,"anio_20191"]*l 
  quantile(theta, c(0.1, 0.90)) # al 90% de confianza
}

# Función para estimar promedios
mi.promedio <- function(i,j,k,l){
  theta <-  sim[,"(Intercept)"] +
    sim[,"inclusion"]*i +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"sex"]*sex +
    sim[,"anio_20141"]*j +
    sim[,"anio_20171"]*k +
    sim[,"anio_20191"]*l 
  mean(theta)
}

# La "i" se refiere al nivel de inclusión: si es total o no
# La "j", la "k" y la "l refieren al año

# Un "for" es muy útil para construir el dataframe final que contenga
# promedios e intervalos de confianza, de acuerdo con niveles de inclusión.
niveles <- c("nul", "parc", "tot")
base_final <- data.frame()
for(x in 0:2){
  tempo_prom <- data.frame("2012" = c(2012, mi.promedio(x,0,0,0)), 
                           "2014" = c(2014, mi.promedio(x,1,0,0)), 
                           "2017" = c(2017, mi.promedio(x,0,1,0)),
                           "2019" = c(2019, mi.promedio(x,0,0,1)))
  tempo <- data.frame("2012" = mi.confianza(x,0,0,0), 
                      "2014" = mi.confianza(x,1,0,0), 
                      "2017" = mi.confianza(x,0,1,0),
                      "2019" = mi.confianza(x,0,0,1))
  tempo <- rbind(tempo_prom, tempo)
  tempo <- data.frame(tempo, row.names = c("year", "promedios", "intinf", "intsup"))
  tempo <- as.data.frame(t(tempo))
  tempo$grupo <- x
  base_final <- bind_rows(base_final, tempo)
  rm(tempo_prom, tempo)
}

# 6.1 Gráfica: inclusión en legislaciones estatales y actitudes hacia el matrimonio igualitario----
fiuf <- "Valores predichos: nivel de inclusión en legislaciones estatales y actitudes hacia el matrimonio igualitario"
fiuff <- "Intervalos de confianza al 85%"
fiuffi <- "Fuente: Elaboración propia con base en encuestas LAPOP (2012-2019). | @guzmart_"

ggplot(data = base_final,
         aes(x = year,
             y = promedios,
             group = grupo)) +
  geom_line(aes(color=factor(grupo))) +
  geom_point(aes(color=factor(grupo))) +
  geom_ribbon(aes(ymin = intinf,
                  ymax = intsup,
                  x = year,
                  fill = factor(grupo)), alpha = 0.2) +
  labs(x = "Año", y = "Actitud promedio",
       title = str_wrap(fiuf, width = 70),
       subtitle = str_wrap(fiuff, width = 85),
       caption = fiuffi) +
  scale_colour_manual(name="Niveles de inclusión",
                      values=c("#A5A5A5","#5E88AE", "#1F4E79"),
                      labels=c("Nulo", "Parcial", "Total")) +
  scale_fill_manual(name="Niveles de inclusión",
                    values=c("#A5A5A5","#5E88AE", "#1F4E79"),
                    labels=c("Nulo", "Parcial", "Total"))  +
  scale_x_continuous(breaks = 2012:2019)+
  ylim(4,7.57) +
  theme_hc() +
  theme(plot.title = element_text(size = 30, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777",hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Ubuntu"),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 25),
        legend.position = "top")
ggsave(filename = "04_01_vals_pred.png", path = out, width = 15, height = 10, dpi = 100)  

# 6.2 Gráfica: inclusión en legislaciones estatales y actitudes hacia el matrimonio igualitario (sin inclusión parcial) ----
ggplot(data = base_final %>% filter(!grupo==1),
         aes(x = year,
             y = promedios,
             group = grupo)) +
    geom_line(aes(color=factor(grupo))) +
    geom_point(aes(color=factor(grupo))) +
    geom_ribbon(aes(ymin = intinf,
                    ymax = intsup,
                    x = year,
                    fill = factor(grupo)), alpha = 0.2) +
  labs(x = "Año", y = "Actitud promedio",
       title = str_wrap(fiuf, width = 70),
       subtitle = str_wrap(fiuff, width = 85),
       caption = fiuffi) +
    scale_colour_manual(name="Niveles de inclusión",
                        values=c("#A5A5A5", "#1F4E79"),
                        labels=c("Nulo", "Total")) +
    scale_fill_manual(name="Niveles de inclusión",
                      values=c("#A5A5A5", "#1F4E79"),
                      labels=c("Nulo", "Total"))  +
    scale_x_continuous(breaks = 2012:2019)+
    ylim(4,7.57) +
    theme_hc()+
    theme(plot.title = element_text(size = 30, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777",hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Ubuntu"),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 25),
        legend.position = "top")
ggsave(filename = "04_02_vals_pred.png", path = out, width = 15, height = 10, dpi = 100)  
  

# 7. Relación entre tiempo de vigencia en legislaciones estatales y actitudes hacia el matrimonio igualitario----
# Estimación del modelo
svymoddias <- svyglm(matrimonio ~ dias_matr + dias_lsc + ideologia + 
                       religion + educacion_anio + edad + sex, 
                     design = design)
stargazer::stargazer(svymoddias,type = "text")
# Simulación estadística
coef <- coefficients(svymoddias)
sigma <- vcov(svymoddias)

set.seed(12345)
sim <- mvrnorm(n = 10000, mu = coef, Sigma = sigma)

# Días de vigencia de los dos distintos niveles de inclusión
dias_MATR <- min(data$dias_matr, na.rm = T):max(data$dias_matr, na.rm = T)
dias_LSC <- min(data$dias_lsc, na.rm = T):max(data$dias_lsc, na.rm = T)

# Fijar las demás variables en promedio (redondeado para categóricas)
dias_matr <- mean(data$dias_matr, na.rm =T)
dias_matr <- round(dias_matr, digits = 0)
dias_lsc <- mean(data$dias_lsc, na.rm =T)
dias_lsc <- round(dias_lsc, digits = 0)
ideologia <- mean(data$ideologia, na.rm = TRUE)
ideologia <- round(ideologia, digits = 0)
religion <- mean(data$religion, na.rm = TRUE)
religion <- round(religion, digits = 0)
educacion_anio <- mean(data$educacion_anio, na.rm = TRUE)
educacion_anio <- round(educacion_anio, digits = 0)
edad <- mean(data$edad, na.rm = TRUE)
sex <- mean(data$sex, na.rm = TRUE)
sex <- round(sex, digits = 0)

# Prueba
theta <-  sim[,"(Intercept)"] + sim[,"dias_matr"]*0 +
  sim[,"dias_lsc"]*dias_lsc +
  sim[,"ideologia"]*ideologia +
  sim[,"religion"]*religion +
  sim[,"educacion_anio"]*educacion_anio +
  sim[,"edad"]*edad +
  sim[,"sex"]*sex
quantile(theta, c(0.05, 0.95))

# Funciones para calcular promedios e intervalos de confianza de Código Civil
mi.confianza <- function(i){
  theta <-  sim[,"(Intercept)"] + sim[,"dias_matr"]*i +
    sim[,"dias_lsc"]*dias_lsc +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"sex"]*sex
  quantile(theta, c(0.1, 0.90)) # al 90% de confianza
}

mi.promedio <- function(i){
  theta <-  sim[,"(Intercept)"] + sim[,"dias_matr"]*i +
    sim[,"dias_lsc"]*dias_lsc +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"sex"]*sex
  mean(theta)
}

# Cálculo de promedios e intervalos de confianza de Código Civil
promedios <- sapply(dias_MATR, mi.promedio)
confianza <- sapply(dias_MATR, mi.confianza)
promedios_matr <- data.frame()
for (i in 1:length(dias_MATR)) {
  promedios_matr <- rbind(promedios_matr, 
                        data.frame(dias = i,
                                   intinf= confianza[1,i], 
                                   intsup= confianza[2,i],
                                   promedios=promedios[i] ))
}

# Aplicamos misma metodología a Leyes de Sociedad de Convivencia y su vigencia
mi.confianza <- function(i){
  theta <-  sim[,"(Intercept)"] + sim[,"dias_matr"]*dias_matr +
    sim[,"dias_lsc"]*i +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"sex"]*sex
  quantile(theta, c(0.1, 0.90)) # al 90% de confianza
}

mi.promedio <- function(i){
  theta <-  sim[,"(Intercept)"] + sim[,"dias_matr"]*dias_matr +
    sim[,"dias_lsc"]*i +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"sex"]*sex
  mean(theta)
}

# Cálculo de promedios e intervalos de confianza de Código Civil
promedios <- sapply(dias_LSC, mi.promedio)
confianza <- sapply(dias_LSC, mi.confianza)
promedios_lsc <- data.frame()
for (i in 1:length(dias_LSC)) {
  promedios_lsc <- rbind(promedios_lsc, 
                         data.frame(dias = i,
                                    intinf= confianza[1,i], 
                                    intsup= confianza[2,i],
                                    promedios=promedios[i] ))
}

# Bind de ambos dataframes
promedios_matr$grupo <- 1
promedios_lsc$grupo <- 0
proms <- bind_rows(promedios_matr, promedios_lsc)

# # 7.1 Gráfica: vigencia de legislaciones estatales incluyentes y actitudes hacia el matrimonio igualitario----
fiuf <- "Valores predichos: tiempo de vigencia de legislaciones estatales incluyentes y actitudes hacia el matrimonio igualitario"
fiuff <- "Intervalos de confianza al 85%"
fiuffi <- "Fuente: Elaboración propia con base en encuestas LAPOP (2012-2017)."
anios <- as.character(seq(0,9,1))

ggplot(data = proms,
         aes(x = dias,
             y = promedios,
             group = as.factor(grupo))) +
  geom_line(aes(color=as.factor(grupo),
                linetype = as.factor(grupo))) +
  geom_ribbon(aes(ymin = intinf,
                  ymax = intsup,
                  x = dias,
                  fill = factor(grupo)), alpha = 0.2) +
  labs(x = "Año", y = "Actitud promedio",
       title = str_wrap(fiuf, width = 70),
       subtitle = str_wrap(fiuff, width = 85),
       caption = fiuffi) +
  scale_color_manual(name="Niveles de inclusión",
                     values=c("#5E88AE","#1F4E79"),
                     labels=c("Parcial", "Total")) +
  scale_fill_manual(name="Niveles de inclusión",
                    values=c("#5E88AE","#1F4E79"),
                    labels=c("Parcial", "Total"))  +
  scale_linetype_manual(name="Niveles de inclusión",
                        values=c("longdash", "solid"),
                        labels=c("Parcial", "Total")) +
  scale_x_continuous(breaks = seq(0,3374,365),
                     labels = anios) +
  ylim(4,7.57) +
  theme_hc()+
  theme(plot.title = element_text(size = 30, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777",hjust = 0.5),
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(family = "Ubuntu"),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 25),
        legend.position = "top")
ggsave(filename = "05_vals_pred.png", path = out, width = 15, height = 10, dpi = 100)  

