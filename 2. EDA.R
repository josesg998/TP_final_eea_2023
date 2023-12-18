require("data.table")
require("sf")
require("tidyverse")
require("rmarkdown")
require("plotly")

ruta <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/"
# importo datasets para generar gráficos
mol_total <- fread("datasets/molinetes.csv.gz")
estaciones <- st_read("datasets/estaciones.geojson")

lineas <- st_read(paste0(ruta,"sbase/subte-estaciones/subte_lineas.geojson"))

barrios <- fread(paste0(ruta,"ministerio-de-educacion/barrios/barrios.csv"))
barrios <- st_as_sf(barrios, wkt = "WKT", crs = 4326)

escuelas <- st_read(paste0(ruta,"ministerio-de-educacion/establecimientos-educativos/escuelas-verdes-wgs84.geojson"))
hospitales <- st_read(paste0(ruta,"ministerio-de-salud/hospitales/hospitales.geojson"))
universidades <- fread(paste0(ruta,"ministerio-de-educacion/universidades/universidades.csv"))
centros_culturales <- st_read(paste0(ruta,"ministerio-de-cultura/espacios-culturales/espacios-culturales.geojson"))
puntos_verdes <- st_read(paste0(ruta,"agencia-de-proteccion-ambiental/puntos-verdes/puntos-verdes.geojson"))

universidades <- universidades[,list(univ_c,universida,unidad_aca,direccion_norm,lat,long)]
# remover duplicados
universidades <- universidades[!duplicated(universidades$direccion_norm),]
universidades <- st_as_sf(universidades, coords = c("long", "lat"), crs = 4326)

centros_culturales <- centros_culturales[!duplicated(centros_culturales$DIRECCION),]

# combino los datasets de edificiso en uno
escuelas$edificio <- 'escuela'
universidades$edificio <- 'universidad'
hospitales$edificio <- 'hospital'
centros_culturales$edificio <- 'centro cultural'
puntos_verdes$edificio <- 'punto verde'

escuelas <- escuelas[c('edificio','geometry')]
universidades <- universidades[c('edificio','geometry')]
hospitales <- hospitales[c('edificio','geometry')]
centros_culturales <- centros_culturales[c('edificio','geometry')]
puntos_verdes <- puntos_verdes[c('edificio','geometry')]

edificios <- rbind(escuelas,universidades,hospitales,centros_culturales,puntos_verdes)



## GRAFICOS---------------------------------------------------------------------------------------------------------------------------------
colores_subte <- c("#00ade2","#ed4337","#0967b4","#06846c","#662c7c","#fedc07")

# pasos por el molinete en el mes de septiembre
mol_total[,.(pax_TOTAL=sum(pax_TOTAL)/1000000),by=list(LINEA,ANIO)] |> 
  ggplot(aes(x=as.factor(ANIO),y=pax_TOTAL,fill=LINEA))+geom_col()+
  scale_fill_manual(values=colores_subte)+
  labs(x=element_blank(),y="Cantidad de pasajeros (millones)",color="Línea",
       title="Pasos por el molinete en el mes de septiembre")+
  theme_minimal()+theme(plot.background = element_rect(fill = "#f5f4ec",colour = "#f5f4ec"))

# cantidad de pasajeos por hora
mol_total[,.(pax_TOTAL=sum(pax_TOTAL)/1000000),by=list(LINEA,ANIO,HORA)] |>
  ggplot(aes(x=HORA,y=pax_TOTAL,fill=LINEA))+geom_col()+
  scale_fill_manual(values=colores_subte)+
  labs(x="Hora",y="Cantidad de pasajeros (millones)",color="Línea",
       title="Pasos por el molinete en el mes de septiembre",subtitle="por hora")+
  facet_wrap(~ANIO)+
  theme_minimal()+theme(plot.background = element_rect(fill = "#f5f4ec",colour = "#f5f4ec"),
                        plot.margin = margin(.5,.5,.5,.5, "cm"))

## ----MAPA DEL SUBTE-------------------------------------------------------------------------------------------------------------

colores_subte_list <- list(A="#00ade2",B="#ed4337",C="#0967b4",D="#06846c",E="#662c7c",H="#fedc07")

estaciones <- estaciones |> separate(LINEA_ESTACION,c("LINEA",'ESTACION'),sep="_",remove = F) 
estaciones$color <- colores_subte_list[estaciones$LINEA]
lineas$LINEA <- gsub("LINEA ","",lineas$LINEA)
lineas$color <- colores_subte_list[lineas$LINEA]
# filtro barrios que tienen subte
barrios$tiene_subte <- ifelse(lengths(st_intersects(barrios,estaciones))>0,1,0)

p <- ggplot()+
  geom_sf(data=barrios)+
  geom_sf(data=estaciones,aes(color=LINEA),size=2)+
  geom_sf(data=lineas,aes(color=LINEA))+
  scale_color_manual(values=colores_subte)+
  theme_void()+theme(legend.position = "left",
                     plot.background = element_rect(fill = "#f5f4ec",colour = "#f5f4ec"))#+labs(title="Mapa del subte de Buenos Aires")

p2 <- ggplot()+
  geom_sf(data=barrios[barrios$tiene_subte==1,])+
  geom_sf(data=estaciones,aes(color=LINEA),size=2.5)+
  geom_sf(data=lineas,aes(color=LINEA))+
  scale_color_manual(values=colores_subte)+
  theme_void()+theme(legend.position = "none",
                     plot.background = element_rect(fill = "#f5f4ec",colour = "#f5f4ec"))#+labs(title="Mapa del subte de Buenos Aires")

gridExtra::grid.arrange(p,p2,nrow=1)

## ------------------------------------------------------------------------------------------------------------------------------------------
mapa_molinetes <- function(){
  check <- mol_total[,.(pax_TOTAL=sum(pax_TOTAL)),by=list(LINEA_ESTACION,ANIO)][,.(pax_TOTAL=median(pax_TOTAL)/1000000),by=LINEA_ESTACION]
  
  estaciones$pax_TOTAL <- check$pax_TOTAL[match(estaciones$LINEA_ESTACION,check$LINEA_ESTACION)]
  
  ggplot()+
    geom_sf(data=barrios[barrios$tiene_subte==1,])+
    geom_sf(data=estaciones,size=estaciones$pax_TOTAL*10,aes(color=LINEA))+
    geom_sf(data=lineas,aes(color=LINEA))+
    scale_color_manual(values=colores_subte)+
    theme_void()+
    labs(title="Paso por los molinetes por estación mediano",subtitle = paste("septiembre"))+
    theme(plot.background = element_rect(fill = "#f5f4ec",colour = "#f5f4ec"))
  
}

# TOP 10 estaciones con mayor paso por los molinetes
mapa_molinetes()

mol_total[,.(pax_TOTAL=sum(pax_TOTAL)),by=list(LINEA_ESTACION,ANIO)][,.(pax_TOTAL=median(pax_TOTAL)/1000000),by=LINEA_ESTACION] |> 
  arrange(-pax_TOTAL) |> separate(LINEA_ESTACION,into = c("Linea","Estación"),sep="_",remove=T) |> 
head(10)


## EDIFICIOS DE BUENOS AIRES--------------------------------------------------------------------------------------------------------------------
ggplot()+
  geom_sf(data=barrios)+
  geom_sf(data=edificios,aes(color=edificio),alpha=.5)+
  theme_void()+
  facet_wrap(~edificio)+scale_color_viridis_d(begin=0,end=.8)+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#f5f4ec",colour = "#f5f4ec"))+
  labs(title="Edificios de Buenos Aires\n")


## BUFFER + EDIFICIOS------------------------------------------------------------------------------------------------------------------------
buffer <- st_buffer(estaciones, dist = 500)

edificios$tiene_subte <- ifelse(lengths(st_intersects(edificios,barrios[barrios$tiene_subte==1,]))>0,1,0)

ggplot()+
  geom_sf(data=barrios[barrios$tiene_subte==1,])+
  geom_sf(data=buffer,size=2)+
  geom_sf(data=edificios[edificios$tiene_subte==1&edificios$edificio!='centro cultural',],aes(color=edificio),alpha=.5)+
  theme_void()+
  labs(title="Edificios cercanos a estaciones",subtitle = '500 metros')+
  scale_color_viridis_d(begin = .2,end=.8)+
  theme(plot.background = element_rect(fill = "#f5f4ec",colour = "#f5f4ec"),
        plot.margin = margin(.5,.5,.5,.5, "cm"))

# CONTEO DE EDIFICIOS POR ESTACION DENTRO DEL BUFFER DE 500 METROS -------------
mapa_edif <- function(edificio){
ggplot()+
  geom_sf(data=barrios[barrios$tiene_subte==1,])+
  geom_sf(data=estaciones,aes(color=LINEA,size=get(edificio)))+
  geom_sf(data=lineas,aes(color=LINEA))+
  scale_color_manual(values=colores_subte)+
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#f5f4ec",colour = "#f5f4ec"),
        plot.margin = margin(.5,.5,.5,.5, "cm"))+
    labs(title=paste(gsub("_"," ",edificio),"por estación"))
}


gridExtra::grid.arrange(mapa_edif("puntos_verdes"),
                        mapa_edif("centros_culturales"),
                        mapa_edif("hospitales"),ncol=3)

gridExtra::grid.arrange(mapa_edif("escuelas"),mapa_edif("universidades"),ncol=2)
