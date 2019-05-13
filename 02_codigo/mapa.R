require(pacman)
p_load(tidyverse, rgeos)

load("~/Downloads/mxhexbin.map.RData")
mxhexbin.map$state_abbr <- ifelse(mxhexbin.map$state_abbr=="DF",
                                  "CDMX",as.character(mxhexbin.map$state_abbr))

ggplot(mxhexbin.map,
       aes(long, lat, group=group)) +
  geom_polygon(color = "black") 

mxhexbin.map$idd <- as.integer(mxhexbin.map$id)
centroids <- data.frame()
for(x in 1:32){
  
  tempo <- subset(mxhexbin.map, idd==x) %>% 
    select(id, long, lat) %>% 
    mutate(cent_x = (min(long)+max(long))/2,
           cent_y = mean(lat))
  
  centroids = bind_rows(centroids, tempo) # echamos la base de cada año a la base vacía
  rm(tempo)
  
  centroids <- centroids %>% 
    select(id, cent_x, cent_y)
  
}
mxhexbin.map$idd <- NULL

# Eliminaro duplicados
centroids <- centroids[!duplicated(centroids$id),]

mxhexbin.map <- mxhexbin.map %>% 
  left_join(centroids,by="id")

mxhexmap <- mxhexbin.map %>% 
  select(id, state_abbr, long, lat, cent_x, cent_y)
colnames(mxhexmap)[1] <- "ent"

ggplot(mxhexmap,
       aes(long, lat, group=ent,
           fill=as.integer(ent))) +
  geom_polygon(color = "gray") +
  geom_text(aes(label=state_abbr,x=cent_x,y=cent_y)) +
  theme_void() +
  scale_fill_gradient(low = "red", high = "green")

prueba <- data %>% 
  select(ent, anio, rmm) %>% 
  left_join(mxhexmap, by = "ent")

ggplot(prueba,
       aes(long, lat, group=ent)) +
  geom_polygon(color = "gray",
               aes(fill = rmm,
                   frame = anio)) +
  geom_text(aes(label=state_abbr,x=cent_x,y=cent_y)) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_void() +
  theme(legend.position = "none") +
  transition_time(anio)

mxhexbin_choropleth(data,
                    title = "",
                    num_colors = 1,
                    label_color = "black",
                    legend = "",
                    label_size = 3) +
  scale_fill_gradient2(low = "#fff2e6", mid = "#cc6600", high = "#804000", midpoint = round(mean(prueba$value))) +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = .90, size = 10),
        plot.subtitle = element_text(size = 12),
        plot.title = element_text(size = 17)) +
  transition_time(anio)
