library(tidyverse) #library to make programme run
library(sf)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(htmltools)
library(formattable)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("C:\\Users\\nikla\\programming_projects\\R\\maps")

Scotland_constituencies <- read_excel("../maps/datasets/scotlandConstituencies.xlsx")
fireStations <- read_excel("../maps/datasets//fireStations.xlsx")

#Visualizsation of scotland map with last data sets
data <- st_read("C:/Users/nikla/programming_projects/R/maps/data/shape_files/Data/GB/Scotland_and_Wales_constituencies.shp")
data <- data %>% filter(AREA_CODE != "WAC")
data <- data %>% arrange(NAME)
#C:\Users\nikla\programming_projects\R\maps\data\shape_files\Data\GB\Scotland_and_Wales_constituencies.shp

shp <- cbind(data, Scotland_constituencies)

pop <- sum(Scotland_constituencies[, 'population'])
totall <- sum(Scotland_constituencies[, 'All'])
totAccidental <- sum(Scotland_constituencies[, 'Accidental'])
totNotAccidental <- sum(Scotland_constituencies[, 'NotAccidental'])

a <- c(Scotland_constituencies$All/totall)
b <- c(Scotland_constituencies$population/pop)

plotAll <- ggscatter(Scotland_constituencies, x = "All", y = "population", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "All fires", ylab = "Population")

plotAcci <- ggscatter(Scotland_constituencies, x = "Accidental", y = "population", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "All Accidental fires", ylab = "Population")

plotNotAcci <- ggscatter(Scotland_constituencies, x = "NotAccidental", y = "population", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "All Not Accidental fires", ylab = "Population")
per_fire_pop <- b/a
per_fire_pop <- formattable(per_fire_pop, digits = 2, format = "f")
df <- data.frame(per_fire_pop)
shp <- cbind(shp, df)
pal1 <- colorNumeric(
  palette = brewer.pal(2, "Reds"),
  domain = shp$All)

pal2 <- colorNumeric(
  palette = brewer.pal(6, "Blues"),
  domain = shp$Accidental)

pal3 <- colorNumeric(
  palette = brewer.pal(6, "Oranges"),
  domain = shp$NotAccidental)

pal4 <- colorNumeric(
  palette = brewer.pal(6, "YlOrRd"),
  domain = shp$per_fire_pop)

Retained <- filter(fireStations, CrewingModel == "Retained")  
Wholetime <- filter(fireStations, CrewingModel == "Wholetime") 
Volunteer <- filter(fireStations, CrewingModel == "Volunteer") 
WholetimeandRetained <- filter(fireStations, CrewingModel == "Wholetime and Retained") 

m <-  leaflet(shp) %>%
  addTiles() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(data = Wholetime, radius = 1, color ="Red", label = ~htmlEscape(Station),
                   group = "Fire Stations: Wholetime") %>%
  addCircleMarkers(data = Volunteer, radius = 1, color ="Black",label = ~htmlEscape(Station),
                   group = "Fire Stations: Volunteer") %>%
  addCircleMarkers(data = WholetimeandRetained, radius = 1, label = ~htmlEscape(Station), color ="Green",
                   group = "Fire Stations: Wholetime and Retained") %>%
  addPolygons(weight = 1,
              # add labels that display mean income
              label = ~paste0("All Fires: ", All, ", Consticuency: ", constituencies),
              # highlight polygons on hover
              fillColor = ~pal1(All), highlight = highlightOptions(weight = 5, color = "white",
                                                                   bringToFront = TRUE), group = "All Fires") %>%

addLayersControl( baseGroups = c("All Fires"),
                  overlayGroups = c("Fire Stations: Wholetime", "Fire Stations: Wholetime and Retained", "Fire Stations: Volunteer"))



# dir.create(widget_path)
saveWidget(m, file.path("FireMapAll.html"), selfcontained=FALSE)
