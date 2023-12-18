# Carga las bibliotecas
require("mlr")
require("rpart")
require("parallel")
require("data.table")
require("tidyr")


set.seed(123)



df = fread('datasets/molinetes.csv.gz')

df <- df[,.(pax_TOTAL=sum(pax_TOTAL)),by=list(LINEA_ESTACION,FECHA,DESDE,HASTA)]

estaciones = fread('datasets/estaciones.csv.gz')

estaciones <-  tidyr::separate(estaciones,geometry,into=c("lat","long"),sep='\\|')
estaciones <- data.table(estaciones)
estaciones$lat <- as.numeric(estaciones$lat)
estaciones$long <- as.numeric(estaciones$long)
estaciones$COMUNA <- as.character(estaciones$COMUNA)

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

names(data) <- gsub(" ", "_", names(data))
names(data) <- gsub("Ñ", "N", names(data))

data <- data[ANIO!=2023]

# Define el espacio de parámetros
PARAM <- makeParamSet(
  # makeNumericParam("cp", lower = 0.1, upper = 0.1),
  makeIntegerParam("minsplit", lower = 1L, upper = 8000L),
  makeIntegerParam("minbucket", lower = 1L, upper = 4000L),
  makeIntegerParam("maxdepth", lower = 3L, upper = 20L),
  forbidden = quote(minbucket > 0.5 * minsplit)
)

# Define la tarea
task <- makeRegrTask(data = data, target = "pax_TOTAL")

# Define la estrategia de validación cruzada
cv <- makeResampleInstance("CV", task,iters=5)

# Define el modelo de regresión rpart
lrn <- makeLearner("regr.rpart", predict.type = "response")

# Define el diseño de experimento
design <- makeTuneControlRandom(maxit = 50)  # Puedes ajustar el número máximo de iteraciones

# Ruta del archivo para los resultados anteriores
archivo_resultados <- "resultados.txt"

# Realiza la optimización de hiperparámetros
res <- tuneParams(
  learner = lrn,
  task = task,
  resampling = cv,
  measures = list(rmse),
  control = design,
  par.set = PARAM,
)

# Guarda los resultados en un archivo de texto
write.table(res$opt.path, "resultados.txt", quote = FALSE, row.names = FALSE,sep='\t',dec=',')

cat("Resultados guardados en 'resultados.txt'\n")
