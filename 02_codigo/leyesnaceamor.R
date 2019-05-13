# Paquetes y directorio ----
require(devtools)
# devtools::install_github('thomasp85/gganimate')
# devtools::install_github("lchiffon/wordcloud2")
require(pacman)
p_load(tidyverse, haven,
       MASS, survey, # análisis de encuestas
       wordcloud, wordcloud2, RColorBrewer, tm, # text mining
       gganimate, ggcorrplot, gridExtra, ggthemes, magick, # Animación y elementos extras para ggplot
       rgeos, transformr, # mapas y hexbins
       webshot, htmlwidgets) # guardar wordcloud2

inp <- "01_datos/"
out <- "03_out/"

# Datos ----
# LAPOP:
# -Base de datos limpia y consolidada de las encuestas LAPOP (2012-2017).
# -Ésta contiene las actitudes (aceptación o rechazo) hacia parejas del mismo sexo.
lapop <- read_dta(paste0(inp, "LAPOP_2012_2017.dta"), encoding = 'latin1')

# LEGIS:
# -Contiene el nivel de inclusión obtenido por la revisión de legislaciones locales.
# -Ésta es una base de datos "ancha"; necesitamos convertirla a una "larga".
legis <- read_csv(paste0(inp, "legislaciones.csv"), 
                  col_types = cols(anio = col_character())) %>% 
  dplyr::select(-c(matr_igua, ley_conv)) %>% 
  gather(anio,
         inclusion,
         inclusion_2019:inclusion_2005) %>% 
  mutate(anio = as.character(case_when(str_detect(anio, "2005") ~ 2005,
                          str_detect(anio, "2006") ~ 2006,
                          str_detect(anio, "2007") ~ 2007,
                          str_detect(anio, "2008") ~ 2008,
                          str_detect(anio, "2009") ~ 2009,
                          str_detect(anio, "2010") ~ 2010,
                          str_detect(anio, "2011") ~ 2011,
                          str_detect(anio, "2012") ~ 2012,
                          str_detect(anio, "2013") ~ 2013,
                          str_detect(anio, "2014") ~ 2014,
                          str_detect(anio, "2015") ~ 2015,
                          str_detect(anio, "2016") ~ 2016,
                          str_detect(anio, "2017") ~ 2017,
                          str_detect(anio, "2018") ~ 2018,
                          str_detect(anio, "2019") ~ 2019)),
         ent = formatC(code_ent, width = 2, format = "d", flag = "0")) %>% 
  dplyr::select(ent, entidad, anio, inclusion)

# Entidades:
# -Claves y abreviaciones de entidades
entidades <- read.csv(paste0(inp,"entidades.csv"))
entidades$ent <- as.character(formatC(entidades$ent,
                       width = 2, format="d", 
                       flag="0"))
legis$abrv <- entidades$abrv[match(legis$ent, entidades$ent)]

# Ponderación ----
# Para analizar encuestas es necesario ponderar y, de esta manera, tener un análisis acertado.
design <- svydesign(id=~upm, # ID de la observación
                    strata=~estratopri, # Estratificación
                    weights=~lapop$wt, # Peso
                    data = lapop, nest = TRUE)

# 1. Mapa de calor del nivel de inclusión en legislaciones estatales----
fiuf <- "Nivel de inclusión en códigos civiles locales"
fiuffi <- "Fuente: Elaboración propia con revisión de legislaciones estatales.
Actualización: mayo de 2019."
inc_tile_plot <- 
  ggplot(data = legis) +
  geom_tile(mapping = aes(x = anio, 
                          y = fct_rev(abrv),
                          fill = as.factor(inclusion)),
            col = "white") +
  scale_fill_manual("Niveles de inclusión",
                    values=c("#DEEBF7", "#5E88AE", "#1F4E79"),
                    labels = c("Nula",
                               "Parcial",
                               "Total")) +
  theme(legend.position="bottom") +
  theme_hc() +
  labs(x = "", y = "",
       title = str_wrap(fiuf, width = 100),
       caption = fiuffi) +
  scale_y_discrete(position = "right") +
  scale_x_discrete(position = "bottom",
                   breaks = as.character(seq(2005,2019,14)),
                   labels = as.character(seq(2005,2019,14))) +
  coord_fixed()
ggsave(filename = "1_inclusion_tile_2005_2019.png", path = out, width = 15, height = 10, dpi = 100)  

# 1.1 Hexbin animado de nivel de inclusión en legislaciones estatales----
# Datos espaciales
load(paste0(inp,"mxhexmap.RData"))
# Para más información acerca de cómo se calcularon los centroides de cada polígono, consulta mapa.R en este repositorio.

hexmatr <- legis %>% 
  left_join(mxhexmap, by = "ent") %>% 
  dplyr::select(ent, state_abbr, anio, inclusion, long, lat, cent_x, cent_y)

# Plot
fiuf <- "Nivel de inclusión en códigos civiles locales"
inc_hexbin_anim <- 
  ggplot(hexmatr,
       aes(long, lat, group=ent)) +
  geom_polygon(color = "gray",
               aes(fill = inclusion)) +
  geom_text(aes(label=state_abbr,x=cent_x,y=cent_y)) +
  scale_fill_gradient2(low = "#DEEBF7", mid = "#5E88AE", high = "#1F4E79",
                       midpoint = 1) +  
  theme_void() +
  theme(legend.position = "none") +
  # Este comando anima por año
  transition_manual(anio) +
  # Éste permite que el título cambie
  labs(title = fiuf,
  subtitle = 'Año: {current_frame}') +
  coord_fixed()
anim_save(filename = paste0(out,"2_inc_hexbin_anim.gif"), inc_hexbin_anim)

# 2. Relación entre nivel de inclusión en legislaciones estatales y actitudes hacia el matrimonio igualitario----
# Estimación del modelo
svymodniv <- svyglm(matrimonio ~ inclusion_total + inclusion_parcial + 
                                 ideologia + religion + educacion_anio + 
                                 edad + genero + anio_2014 + anio_2017,
                    design = design)

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
ideologia <- mean(lapop$ideologia, na.rm = TRUE)
ideologia <- round(ideologia, digits = 0)
religion <- mean(lapop$religion, na.rm = TRUE)
religion <- round(religion, digits = 0)
educacion_anio <- mean(lapop$educacion_anio, na.rm = TRUE)
educacion_anio <- round(educacion_anio, digits = 0)
edad <- mean(lapop$edad, na.rm = TRUE)
genero <- mean(lapop$genero, na.rm = TRUE)
genero <- round(genero, digits = 0)

theta <-  sim[,"(Intercept)"] + sim[,"inclusion_total"]*0 +
  sim[,"ideologia"]*ideologia +
  sim[,"religion"]*religion +
  sim[,"educacion_anio"]*educacion_anio +
  sim[,"edad"]*edad +
  sim[,"genero"]*genero +
  sim[,"anio_2014"]*0 +
  sim[,"anio_2017"]*0

# intervalos de confianza para valores esperados de inclusion nula
# en 2012 (ésta es la prueba)
quantile(theta, c(0.025, 0.975))

# Función para estimar intervalos de confianza
mi.confianza <- function(i,j,k){
  theta <-  sim[,"(Intercept)"] + sim[,"inclusion_total"]*i +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"genero"]*genero +
    sim[,"anio_2014"]*j +
    sim[,"anio_2017"]*k
  quantile(theta, c(0.05, 0.95)) # al 90% de confianza
}

# Función para estimar promedios
mi.promedio <- function(i,j,k){
  theta <-  sim[,"(Intercept)"] + sim[,"inclusion_total"]*i +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"genero"]*genero +
    sim[,"anio_2014"]*j +
    sim[,"anio_2017"]*k
  mean(theta)
}

# La "i" se refiere al nivel de inclusión: si es total o no
# La "j" y la "k" refieren al año

# Un "for" es muy útil para construir el dataframe final que contenga
# promedios e intervalos de confianza, de acuerdo con niveles de inclusión.
niveles <- c("nul", "parc", "tot")
base_final <- data.frame()
for(x in 0:2){
  tempo_prom <- data.frame("2012" = c(2012, mi.promedio(x,0,0)), 
                           "2014" = c(2014, mi.promedio(x,1,0)), 
                           "2017" = c(2017, mi.promedio(x,0,1)))
  tempo <- data.frame("2012" = mi.confianza(x,0,0), 
                      "2014" = mi.confianza(x,1,0), 
                      "2017" = mi.confianza(x,0,1))
  tempo <- rbind(tempo_prom, tempo)
  tempo <- data.frame(tempo, row.names = c("year", "promedios", "intinf", "intsup"))
  tempo <- as.data.frame(t(tempo))
  tempo$grupo <- x
  base_final <- bind_rows(base_final, tempo)
  rm(tempo_prom, tempo)
}

# 2.1 Gráfica: inclusión en legislaciones estatales y actitudes hacia el matrimonio igualitario----
fiuf <- "Valores predichos: nivel de inclusión en legislaciones estatales y actitudes hacia el matrimonio igualitario"
fiuff <- "Intervalos de confianza al 90%"
fiuffi <- "Fuente: Elaboración propia con base en encuestas LAPOP (2012-2017)."
inc_pred_plot <- 
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
       title = str_wrap(fiuf, width = 85),
       subtitle = str_wrap(fiuff, width = 85),
       caption = fiuffi) +
  scale_colour_manual(name="Niveles de inclusión",
                      values=c("#A5A5A5","#5E88AE", "#1F4E79"),
                      labels=c("Nulo", "Parcial", "Total")) +
  scale_fill_manual(name="Niveles de inclusión",
                    values=c("#A5A5A5","#5E88AE", "#1F4E79"),
                    labels=c("Nulo", "Parcial", "Total"))  +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 20, lineheight = 7),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1,"cm"),
        title = element_text(size = 20)) +
  ylim(5,7.57) +
  theme_hc()
plot(inc_pred_plot)
ggsave(filename = "3_inclusion_actitudes_pred.png", path = out, width = 15, height = 10, dpi = 100)

# 3. Relación entre tiempo de vigencia en legislaciones estatales y actitudes hacia el matrimonio igualitario----
# Estimación del modelo
svymoddias <- svyglm(matrimonio ~ diasCC + diasLSC + ideologia + 
                        religion + educacion_anio + edad + genero, 
                      design = design)

# Simulación estadística
coef <- coefficients(svymoddias)
sigma <- vcov(svymoddias)

set.seed(12345)
sim <- mvrnorm(n = 10000, mu = coef, Sigma = sigma)

# Días de vigencia de los dos distintos niveles de inclusión
dias_CC <- min(lapop$diasCC):max(lapop$diasCC)
dias_LSC <- min(lapop$diasLSC):max(lapop$diasLSC)

# Fijar las demás variables en promedio (redondeado para categóricas)
diasCC <- mean(lapop$diasCC, na.rm =T)
diasCC <- round(diasCC, digits = 0)
diasLSC <- mean(lapop$diasLSC, na.rm =T)
diasLSC <- round(diasLSC, digits = 0)
ideologia <- mean(lapop$ideologia, na.rm = TRUE)
ideologia <- round(ideologia, digits = 0)
religion <- mean(lapop$religion, na.rm = TRUE)
religion <- round(religion, digits = 0)
educacion_anio <- mean(lapop$educacion_anio, na.rm = TRUE)
educacion_anio <- round(educacion_anio, digits = 0)
edad <- mean(lapop$edad, na.rm = TRUE)
genero <- mean(lapop$genero, na.rm = TRUE)
genero <- round(genero, digits = 0)

# Prueba
theta <-  sim[,"(Intercept)"] + sim[,"diasCC"]*0 +
  sim[,"diasLSC"]*diasLSC +
  sim[,"ideologia"]*ideologia +
  sim[,"religion"]*religion +
  sim[,"educacion_anio"]*educacion_anio +
  sim[,"edad"]*edad +
  sim[,"genero"]*genero
quantile(theta, c(0.05, 0.95))

# Funciones para calcular promedios e intervalos de confianza de Código Civil
mi.confianza <- function(i){
  theta <-  sim[,"(Intercept)"] + sim[,"diasCC"]*i +
    sim[,"diasLSC"]*diasLSC +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"genero"]*genero
  quantile(theta, c(0.05, 0.95))
}

mi.promedio <- function(i){
  theta <-  sim[,"(Intercept)"] + sim[,"diasCC"]*i +
    sim[,"diasLSC"]*diasLSC +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"genero"]*genero
  mean(theta)
}

# Cálculo de promedios e intervalos de confianza de Código Civil
promedios <- sapply(dias_CC, mi.promedio)
confianza <- sapply(dias_CC, mi.confianza)
promedios_cc <- data.frame()
for (i in 1:length(dias_CC)) {
  promedios_cc <- rbind(promedios_cc, 
                        data.frame(dias = i,
                                   intinf= confianza[1,i], 
                                   intsup= confianza[2,i],
                                   promedios=promedios[i] ))
}

# Aplicamos misma metodología a Leyes de Sociedad de Convivencia y su vigencia
mi.confianza <- function(i){
  theta <-  sim[,"(Intercept)"] + sim[,"diasCC"]*diasCC +
    sim[,"diasLSC"]*i +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"genero"]*genero
  quantile(theta, c(0.05, 0.95))
}

mi.promedio <- function(i){
  theta <-  sim[,"(Intercept)"] + sim[,"diasCC"]*diasCC +
    sim[,"diasLSC"]*i +
    sim[,"ideologia"]*ideologia +
    sim[,"religion"]*religion +
    sim[,"educacion_anio"]*educacion_anio +
    sim[,"edad"]*edad +
    sim[,"genero"]*genero
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
promedios_cc$grupo <- 1
promedios_lsc$grupo <- 0
proms <- bind_rows(promedios_cc, promedios_lsc)

# # 3.1 Gráfica: vigencia de legislaciones estatales incluyentes y actitudes hacia el matrimonio igualitario----
fiuf <- "Valores predichos: tiempo de vigencia de legislaciones estatales incluyentes y actitudes hacia el matrimonio igualitario"
fiuff <- "Intervalos de confianza al 90%"
fiuffi <- "Fuente: Elaboración propia con base en encuestas LAPOP (2012-2017)."
anios <- as.character(seq(0,7,1))
temp_pred_plot <- 
ggplot(data = subset(proms,dias<2656),
       aes(x = dias,
           y = promedios,
           group = as.factor(grupo))) +
  geom_line(aes(color=as.factor(grupo),
                linetype = as.factor(grupo))) +
  geom_ribbon(aes(ymin = intinf,
                  ymax = intsup,
                  x = dias,
                  fill = factor(grupo)), alpha = 0.2) +
  labs(x = "Años", y = "Actitud promedio",
       title = str_wrap(fiuf, width = 85),
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
  scale_x_continuous(breaks = seq(0,2656,365),
                     labels = anios) +
  ylim(5,7.57) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 20, lineheight = 7),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1,"cm"),
        legend.position = "bottom",
        title = element_text(size = 20)) +
  theme_hc()
plot(temp_plot)
ggsave(filename = "4_tiempo_actitudes_pred.png", path = out, width = 15, height = 10, dpi = 100)

# 4. Un muy breve análisis de texto----
file <- read_file(paste0(inp,"matrimonio.txt"))
docs <- Corpus(VectorSource(file))

# limpieza de texto
# a minúsculas
docs <- tm_map(docs, content_transformer(tolower))
# remover números
docs <- tm_map(docs, removeNumbers)
# remover "stopwords"
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# remover stopwords propias
docs <- tm_map(docs, removeWords, c("solo", "asi", "sola")) 

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

# Nube de palabras
nube_matrimonio <- 
  wordcloud2(d, shape = "diamond", size = .7, color = brewer.pal(n = 100, name = "RdBu"),
           fontWeight = "bold",
           minRotation = 1/pi, maxRotation = 1/pi, rotateRatio = 1,
           minSize = "prueba")


# Guardar
# webshot::install_phantomjs()
saveWidget(nube_matrimonio,"5_nube_matrimonio.html",selfcontained = F)
webshot("5_nube_matrimonio.html",paste0(out,"5_nube_matrimonio.pdf"), delay =5, vwidth = 700, vheight=480)
webshot("5_nube_matrimonio.html",paste0(out,"5_nube_matrimonio.png"), delay =5, vwidth = 700, vheight=480)
