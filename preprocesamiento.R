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


# separa la columna estacion por el delimitador " - "

estaciones <- estaciones  |>  tidyr::separate(estacion, c("estacion", "nuevo_nombre"), sep = " - ", remove = FALSE)
estaciones$LINEA_ESTACION <- paste(estaciones$linea,estaciones$estacion,sep="_")

mol_total <- merge(mol_total,estaciones,by="LINEA_ESTACION",all.x=T)

