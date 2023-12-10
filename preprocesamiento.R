require("data.table")

correccion <- function(df){
  
  #apply gsub to every column with lapply and return the same dataframe
  df <- as.data.frame(lapply(df, function(x) gsub('"','',x)))
  
  names(df) <- gsub('"|X.|\\.','',names(df))
  
  return(df)
}


#2022
mol_2022_sep <- rbind(correccion(fread("molinetes-2022/202209_PAX15min-ABC.csv",encoding='Latin-1',stringsAsFactors = F,sep=";",quote = "")),
                      correccion(fread("molinetes-2022/202209_PAX15min-DEH.csv",encoding='Latin-1',stringsAsFactors = F,sep=";",quote = "")[,5:14]))
#2023
mol_2023_sep <- correccion(rbind(fread("molinetes-2023/202309_PAX15min-ABC.csv",stringsAsFactors = F,sep=";",quote = "",encoding='Latin-1'),
                                 fread("molinetes-2023/202309_PAX15min-DEH.csv",stringsAsFactors = F,sep=";",quote = "",encoding='Latin-1')))

mol_2022_sep$FECHA <- as.POSIXct(mol_2022_sep$FECHA,format = "%d/%m/%Y")

mol_2023_sep$FECHA <- as.POSIXct(mol_2023_sep$FECHA,format = "%d/%m/%Y")

columnas <- names(mol_2022_sep)
# decompresss molinetes-2018.zip
for (i in 2017:2021){unzip(paste0("molinetes-",i,".zip"))}

#2021
mol_2021_sep <- fread("historico_2021.csv",encoding='Latin-1')
mol_2021_sep$FECHA <- as.POSIXct(mol_2021_sep$FECHA,format = "%d/%m/%Y")
mol_2021_sep <- mol_2021_sep[month(mol_2021_sep$FECHA)==9,2:11]

#2020
mol_2020_sep <- fread("historico2.csv",encoding='Latin-1')
mol_2020_sep$FECHA <- as.POSIXct(mol_2020_sep$FECHA,format = "%d/%m/%Y")
mol_2020_sep <- mol_2020_sep[month(mol_2020_sep$FECHA)==9]

#2019
mol_2019_sep <- fread("datahistorica122019.csv",encoding='Latin-1')
names(mol_2019_sep)[1:7] <- toupper(names(mol_2019_sep)[1:7])
names(mol_2019_sep)[ncol(mol_2019_sep)] <- "pax_TOTAL"
mol_2019_sep$FECHA <- as.POSIXct(mol_2019_sep$FECHA,format = "%d/%m/%Y")
mol_2019_sep <- mol_2019_sep[month(mol_2019_sep$FECHA)==9,2:11]

#2018
mol_2018_sep <- fread("molinetes-subte-18.csv",encoding='Latin-1')
mol_2018_sep <- mol_2018_sep[,c(1:10)]
names(mol_2018_sep)[1:6] <- toupper(names(mol_2018_sep)[1:6])
names(mol_2018_sep)[ncol(mol_2018_sep)] <- "pax_TOTAL"
mol_2018_sep$FECHA <- as.POSIXct(mol_2018_sep$FECHA,format = "%d/%m/%Y")
mol_2018_sep <- mol_2018_sep[month(mol_2018_sep$FECHA)==9,]

#2017
mol_2017_sep <- fread("molinetes_2017.csv",encoding='Latin-1')
mol_2017_sep <- mol_2017_sep[,c(2:6,8:12)]
names(mol_2017_sep)[7:10] <- c("pax_pagos","pax_pases_pagos","pax_franq","pax_TOTAL")
mol_2017_sep$FECHA <- as.POSIXct(mol_2017_sep$FECHA,format = "%d/%m/%Y")
mol_2017_sep <- mol_2017_sep[month(mol_2017_sep$FECHA)==9,]


mol_total <- rbind(mol_2017_sep,mol_2018_sep,mol_2019_sep,mol_2020_sep,mol_2021_sep,mol_2022_sep,mol_2023_sep,fill = T)

mol_total$LINEA <- gsub("Linea","",mol_total$LINEA)

mol_total$DESDE <- as.POSIXct(paste(mol_total$FECHA,mol_total$DESDE,sep=" "),format = "%Y-%m-%d %H:%M:%S")
mol_total$HASTA <- as.POSIXct(paste(mol_total$FECHA,mol_total$HASTA,sep=" "),format = "%Y-%m-%d %H:%M:%S")

#mol_total$DESDE <- as.POSIXct(paste(mol_total$FECHA,mol_total$DESDE,sep=" "),format = "%Y-%m-%d %H:%M:%S")

mol_total$LINEA <- gsub("LINEA_","",mol_total$LINEA)

mol_total$ESTACION <- gsub("\u0081|Â\u0081|Ã¼|ÃƒÂƒÃ‚Â¼|ï¿½","ü",mol_total$ESTACION)
mol_total$ESTACION <- gsub("ü","u",mol_total$ESTACION)
mol_total$ESTACION <- gsub("Ã±|ÃƒÂƒÃ‚Â±","ñ",mol_total$ESTACION)
mol_total$ESTACION <- gsub("Peüa","Peña",mol_total$ESTACION)

mol_total[grepl("Saenz P",mol_total$ESTACION),"ESTACION"] <- "Saenz Peña"
mol_total[grepl("SAENZ P",mol_total$ESTACION),"ESTACION"] <- "SAENZ PEÑA"

mol_total$ESTACION <- gsub('(\\.B)|(\\.C)|(\\.D)|( E)|(\\.H)',"",mol_total$ESTACION)

mol_total$ESTACION <- toupper(mol_total$ESTACION)

for(column in c("pax_pagos","pax_pases_pagos","pax_franq","pax_TOTAL")){
  mol_total[[column]] <- as.numeric(mol_total[[column]])
}

mol_total$ANIO <- year(mol_total$FECHA)
mol_total$MES <- month(mol_total$FECHA)
mol_total$HORA <- hour(mol_total$DESDE)
mol_total$DIA <- wday(mol_total$FECHA)
mol_total$DIA_MES <- mday(mol_total$FECHA)
mol_total$HORA_PICO <- ifelse(mol_total$HORA %in% c(7:9,17:19),1,0)
mol_total$LINEA_ESTACION <- paste(mol_total$LINEA,mol_total$ESTACION,sep="_")

fwrite(mol_total,"molinetes.csv.gz")

estaciones <- fread('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-estaciones/estaciones-de-subte.csv')

#remover tildes
estaciones$estacion <- gsub("Á","A",estaciones$estacion)
estaciones$estacion <- gsub("É","E",estaciones$estacion)
estaciones$estacion <- gsub("Í","I",estaciones$estacion)
estaciones$estacion <- gsub("Ó","O",estaciones$estacion)
estaciones$estacion <- gsub("Ú","U",estaciones$estacion)
estaciones$estacion <- gsub("1°","I",estaciones$estacion)
estaciones$estacion <- gsub("Ü","U",estaciones$estacion)

# agregamos latitud y longitud
estaciones <- fread("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-estaciones/estaciones-de-subte.csv")

estaciones <- estaciones |> tidyr::separate(col = "estacion",into = c("estacion","estacion_nueva"),sep = "( - )|( -)")


estaciones$en_tabla <-  estaciones$estacion %in% mol_total$ESTACION

estaciones$estacion <- gsub("Ü","U",estaciones$estacion)
estaciones$estacion <- gsub("1°","I",estaciones$estacion)
estaciones$estacion <- gsub("É","E",estaciones$estacion)
estaciones$estacion <- gsub("Ó","O",estaciones$estacion)
estaciones$estacion <- gsub("Á","A",estaciones$estacion)
estaciones$estacion <- gsub("Í","I",estaciones$estacion)

estaciones$estacion <- gsub("R\\.","",estaciones$estacion)
estaciones$estacion <- gsub("AV\\.","AVENIDA",estaciones$estacion)
estaciones$estacion <- gsub("MORENO","MARIANO MORENO",estaciones$estacion)
estaciones$estacion <- gsub("C\\.","CARLOS",estaciones$estacion)
estaciones$estacion <- gsub("ALMAGRO","MEDRANO",estaciones$estacion)
estaciones$estacion <- gsub("PLAZA DE MISERERE","PLAZA MISERERE",estaciones$estacion)
estaciones$estacion <- gsub("BELGRANO","GENERAL BELGRANO",estaciones$estacion)
estaciones$estacion <- gsub("JOSE MARIA MARIANO MORENO","JOSE MARIA MORENO",estaciones$estacion)
estaciones$estacion <- gsub("SAN MARTIN","GENERAL SAN MARTIN",estaciones$estacion)
estaciones$estacion <- gsub("PLAZA DE LOS VIRREYES","PZA. DE LOS VIRREYES",estaciones$estacion)
estaciones$estacion <- gsub("DE LOS INCAS","LOS INCAS",estaciones$estacion)
estaciones$estacion <- gsub("PARQUE PATRICIOS","PATRICIOS",estaciones$estacion)
estaciones$estacion <- gsub("JUAN MANUEL DE ROSAS","ROSAS",estaciones$estacion)
estaciones$estacion <- gsub("SAN JOSE DE FLORES","FLORES",estaciones$estacion)

estaciones$LINEA_ESTACION <- paste0(estaciones$linea,"_",estaciones$estacion)
estaciones <- estaciones[,c("id","LINEA_ESTACION","lat","long")]


# detectar si la estación está dentro de un barrio
barrios <- fread("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.csv")

# Crea objetos sf para los dataframes
estaciones <- st_as_sf(estaciones, coords = c("long", "lat"), crs = 4326)
barrios <- st_as_sf(barrios, wkt = "WKT", crs = 4326)

# Realiza la operación de punto en polígono para asignar el barrio
estaciones <- st_join(estaciones, barrios)
estaciones <- estaciones_sf[,c("id","LINEA_ESTACION","BARRIO","COMUNA")]

# escuelas
escuelas <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/establecimientos-educativos/escuelas-verdes-wgs84.geojson")
hospitales <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-salud/hospitales/hospitales.geojson")
universidades <- fread("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/universidades/universidades.csv")
centros_culturales <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-cultura/espacios-culturales/espacios-culturales.geojson")
puntos_verdes <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/agencia-de-proteccion-ambiental/puntos-verdes/puntos-verdes.geojson")

universidades <- universidades[,list(univ_c,universida,unidad_aca,direccion_norm,lat,long)]
# remover duplicados
universidades <- universidades[!duplicated(universidades$direccion_norm),]
universidades <- st_as_sf(universidades, coords = c("long", "lat"), crs = 4326)

centros_culturales <- centros_culturales[!duplicated(centros_culturales$DIRECCION),]

# Función para contar el número de escuelas dentro de 300 metros de una estación
contar_edificios <- function(mapa,distancia) {
  buffer <- st_buffer(estaciones, dist = distancia)  # Crea un buffer de 300 metros alrededor de la estación
  conteo <- lengths(st_intersects(buffer, mapa))
  return(conteo)
}

estaciones$universidades <- contar_edificios(universidades,500)
estaciones$escuelas <- contar_edificios(escuelas,500)
estaciones$hospitales <- contar_edificios(hospitales,500)
estaciones$centros_culturales <- contar_edificios(centros_culturales,500)
estaciones$puntos_verdes <- contar_edificios(puntos_verdes,500)


buffer <- st_buffer(estaciones, dist = 500)

ggplot()+
  geom_sf(data=barrios)+
  geom_sf(data=buffer)+
  geom_sf(data=escuelas,color="red")+
  geom_sf(data=universidades,color="orange")+
  #geom_sf(data=centros_culturales,color="purple",alpha=.2)+
  geom_sf(data=hospitales,color="blue")+
  geom_sf(data=puntos_verdes,color="green")+
  #add legend
  #scale_color_manual(values=c("orange","blue","red","green"),labels=c("Universidades","Hospitales","Escuelas","Puntos verdes"))+
  theme_void()+
  theme(legend.position = "bottom")

fwrite(estaciones,"estaciones.csv.gz")
