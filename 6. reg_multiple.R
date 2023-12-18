# Carga las bibliotecas
require("data.table")
require("tidyr")
require("tidymodels")

#########################
# importo datasets + feature engineering

df = fread('datasets/molinetes.csv.gz')
# agrupamos por estación y hora
df <- df[,.(pax_TOTAL=sum(pax_TOTAL)),by=list(LINEA_ESTACION,FECHA,DESDE,HASTA)]

estaciones = fread('datasets/estaciones.csv.gz')

estaciones <-  tidyr::separate(estaciones,geometry,into=c("lat","long"),sep='\\|')
estaciones <- data.table(estaciones)
estaciones$lat <- as.numeric(estaciones$lat)
estaciones$long <- as.numeric(estaciones$long)
estaciones$COMUNA <- as.character(estaciones$COMUNA)

# one hot encoding
barrio_encoded <- dcast(estaciones, LINEA_ESTACION ~ BARRIO, fun.aggregate = length)
comuna_encoded <- dcast(estaciones, LINEA_ESTACION ~ COMUNA, fun.aggregate = length)

names(comuna_encoded)[2:length(names(comuna_encoded))] <- paste0("com_", names(comuna_encoded))

LINEA_ESTACION_encoded <- dcast(estaciones, LINEA_ESTACION ~ LINEA_ESTACION, fun.aggregate = length)

estaciones <- merge(estaciones, barrio_encoded, by = "LINEA_ESTACION")
estaciones <- merge(estaciones, comuna_encoded, by = "LINEA_ESTACION")
estaciones <- merge(estaciones, LINEA_ESTACION_encoded, by = "LINEA_ESTACION")

# Joineamos entre df y estaciones
estaciones[,id:=NULL]

data <- merge(df, estaciones, by = "LINEA_ESTACION")

# volvemos a generar variables temporales
data$ANIO <-            year(data$FECHA)
data$MES <-            month(data$FECHA)
data$HORA_DESDE <-      hour(data$DESDE)
data$HORA_HASTA <-      hour(data$HASTA)
data$DIA <-             wday(data$FECHA)
data$DIA_MES <-         mday(data$FECHA)
data$HORA_PICO <-     ifelse(data$HORA_DESDE %in% c(7:9,17:19),1,0)
data$LINEA_ESTACION <- paste(data$LINEA,data$ESTACION,sep="_")
data$minutos_desde <- lubridate::minute(data$DESDE)
data$minutos_hasta <- lubridate::minute(data$HASTA)

# Descartamos columnas:
columns_to_remove <- c("DESDE", "HASTA", "FECHA",
                       "LINEA_ESTACION","BARRIO","COMUNA")
data[, (columns_to_remove) := NULL]

# corrigo nombres de columnas para que los tome la librería
names(data) <- gsub(" ", "_", names(data))
names(data) <- gsub("Ñ", "N", names(data))

# genero el train y el test
train <- data[ANIO != 2023]
test <- data[ANIO == 2023]

# realizo la regresión múltiple
model <- lm(pax_TOTAL ~ ., data = train)
summary(model)

prediccion_lineal <- predict(
  object = model,
  newdata = test)

resultados <- tidy(model)

# calculo el error
error_lineal <- mean(abs(prediccion_lineal - test$pax_TOTAL))
rmse_lineal <- sqrt(mean((prediccion_lineal - test$pax_TOTAL)^2))
print(error_lineal)
print(rmse_lineal)