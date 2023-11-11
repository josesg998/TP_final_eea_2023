require("data.table")
require("sf")
require("ggplot2")

# importar todos los csv en molinetes-2022 y combinarlos en un data.table
#molinetes <- rbindlist(lapply(list.files("molinetes-2022",pattern = "csv",full.names = T),fread),use.names = T,fill = T)


#resolve impromer quoting in fread
mol_2023 <- rbind(fread("molinetes-2022/202212_PAX15min-ABC.csv",stringsAsFactors = F,sep=";"),
                  fread("molinetes-2022/202212_PAX15min-DEH.csv",stringsAsFactors = F,sep=";"))

mol_2023$`"FECHA` <- gsub('"','',mol_2023$`"FECHA`)
mol_2023$`pax_TOTAL"` <- gsub('"','',mol_2023$`pax_TOTAL"`)

names(mol_2023)[1] <- "FECHA"
names(mol_2023)[10] <- "pax_TOTAL"

mol_2023$pax_TOTAL <- as.numeric(mol_2023$pax_TOTAL)
mol_2023$ESTACION <- toupper(mol_2023$ESTACION)

unique(mol_2023[,ESTACION,by=LINEA])



estaciones <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-estaciones/estaciones-de-subte.geojson")
lineas <- st_read('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-estaciones/subte_lineas.geojson')
comunas <- st_read('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.geojson')

#garficar estaciones lineas y comunas con geom_sf()
ggplot() + geom_sf(data=comunas)+geom_sf(data=estaciones) + geom_sf(data=lineas)

# obtener media de pax_TOTAL por estacion
pax_estacion <- mol_2023[,.(pax_total=mean(pax_total)),by=.(ESTACION)]

# graficar mismo mapa pero combinando estacion y pax_estacion con left_join que pax_total sea el size de estaciones
ggplot() + geom_sf(data=comunas)+geom_sf(data=estaciones) + 
  geom_sf(data=lineas) + geom_sf(data=merge(estaciones,pax_estacion),aes(size=pax_total))


estaciones <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-estaciones/estaciones-de-subte.geojson")
lineas <- st_read('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-estaciones/subte_lineas.geojson')
comunas <- st_read('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.geojson')

#TODO 2023

#importar y apilar todos los csv de la carpeta molinetes-2022
mol_2022_FULL <- rbindlist(lapply(list.files("molinetes-2022",pattern = "csv",full.names = T),fread,encoding='Latin-1'),use.names = T,fill = T)
#los nulos en fecha imputarlos del valor de la columna DIA
mol_2022_FULL[is.na(`"FECHA`),`"FECHA`:=FECHA]

mol_2022_FULL <- mol_2022_FULL[,1:10]

mol_2022_FULL$`"FECHA` <- gsub('"','',mol_2022_FULL$`"FECHA`)
mol_2022_FULL$`pax_TOTAL"` <- gsub('"','',mol_2022_FULL$`pax_TOTAL"`)

names(mol_2022_FULL)[1] <- "FECHA"
names(mol_2022_FULL)[10] <- "pax_TOTAL"

mol_2022_FULL$pax_TOTAL <- as.numeric(mol_2022_FULL$pax_TOTAL)
mol_2022_FULL$ESTACION <- toupper(mol_2022_FULL$ESTACION)
mol_2022_FULL$LINEA <- toupper(mol_2022_FULL$LINEA)


#·mol_2022_FULL$MES <- month(mol_2022_FULL$FECHA)
lineas$LINEASUB <- gsub(" ","",lineas$LINEA)

colores_lineas <- data.frame(colores=c("#00A0DE","#E4002B","blue","#00933C","#A85298","#FEDD00"),
                             LINEA=c("LINEAA","LINEAB","LINEAC","LINEAD","LINEAE","LINEAH"))

lineas <- merge(lineas,colores_lineas)

# graficar mismo mapa pero combinando estacion y pax_estacion con left_join que pax_total sea el size de estaciones
ggplot() + geom_sf(data=comunas)+geom_sf(data=estaciones) + 
  geom_sf(data=lineas) + 
  geom_sf(data=merge(estaciones,
                     mol_2022_FULL[,.(pax_TOTAL=sum(pax_TOTAL)/1000),by=.(ESTACION)]),aes(size=pax_TOTAL))+
  scale_size_continuous(range = c(1, 10), guide = "legend",name="Pax (miles)")


#grafiar lo de arriba en leaflet
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(mapview)

# graficar el mismo mapa en leaflet
leaflet() %>% 
  addTiles() %>% 
  #addPolygons(data=comunas) %>%  
  addPolylines(data=lineas) %>% 
  addCircles(data=merge(estaciones,
                     mol_2022_FULL[,.(pax_TOTAL=sum(pax_TOTAL)/10000),by=.(ESTACION)]),
             radius = ~pax_TOTAL,color='red',stroke = FALSE,fillOpacity = 0.5) %>% 
  addAwesomeMarkers(data=merge(estaciones,
                               mol_2022_FULL[,.(pax_TOTAL=sum(pax_TOTAL)/1000),by=.(ESTACION)]),
                    popup=~paste(ESTACION,": ",pax_TOTAL, "miles de pasajeros"),
                    label=~paste(ESTACION,": ",pax_TOTAL, "miles de pasajeros")) %>% 
#agregar titulo
  addControl(html = tags$div(HTML("<h1>Movimiento de pasajeros 2022</h1>")),
             position = "topright")


# lo mismo pero removiendo iconos
leaflet() %>% 
  addTiles() %>% 
  #addPolygons(data=comunas) %>%  
  addPolylines(data=lineas) %>% 
  addCircles(data=merge(estaciones,
                     mol_2022_FULL[,.(pax_TOTAL=sum(pax_TOTAL)/10000),by=.(ESTACION)]),
             radius = ~pax_TOTAL,color='red',stroke = FALSE,fillOpacity = 0.5) %>% 
  addAwesomeMarkers(data=merge(estaciones,
                               mol_2022_FULL[,.(pax_TOTAL=sum(pax_TOTAL)/1000),by=.(ESTACION)]),
                    popup=~paste(ESTACION,": ",pax_TOTAL, "miles de pasajeros"),
                    label=~paste(ESTACION,": ",pax_TOTAL, "miles de pasajeros"),
                    icon=awesomeIcons(icon='train',markerColor='red',iconColor='white',library='fa',iconSize=1))

#open the file in molinetes-2019
mol_2019 <- fread("molinetes-2019/datahistorica122019.csv",encoding='Latin-1')
head(mol_2019[month(mol_2019$fecha)==11,])
