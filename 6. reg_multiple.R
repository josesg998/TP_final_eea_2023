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

# calculo el error
error_lineal <- mean(abs(prediccion_lineal - test$pax_TOTAL))
rmse_lineal <- sqrt(mean((prediccion_lineal - test$pax_TOTAL)^2))
print(error_lineal) # MAE: 71.80597
print(rmse_lineal) # RMSE: 126.5835


# train regression model without COMUNA column

model_sin_comuna <- lm(pax_TOTAL ~ . -COMUNA, data = train)
rmse_sin_com <- sqrt(mean((predict(model_sin_comuna,test) - test$pax_TOTAL)^2))
print(rmse_sin_com) # RMSE: 126.5835

# MODELO DEFINITIVO QUE SE UTILIZA EN LA PPT
model_sin_comuna_ni_barrio <- lm(pax_TOTAL ~ . -COMUNA -BARRIO, data = train)
rmse_sin_com_ni_barrio <- sqrt(mean((predict(model_sin_comuna_ni_barrio,test) - test$pax_TOTAL)^2))
print(rmse_sin_com_ni_barrio) # RMSE: 126.5835

############
# MODELOS ALTERNATIVOS QUE SE INTENTARON UTILIZAR


# ELASTIC NET
require("caret")
set.seed(42)
# perform an elastic net regression with 5fold cross validation
elastic_net <- train(
  pax_TOTAL ~ .,
  data = train,
  method = "glmnet",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
)

prediccion_elastic_net <- predict(elastic_net, newdata = test)

rmse_elastic_net <- sqrt(mean((prediccion_elastic_net - test$pax_TOTAL)^2))

print(rmse_elastic_net) # RMSE: 134.2764

# LASSO
require("glmnet")
X <- model.matrix(pax_TOTAL ~ ., data = train)[,-1]
y <- train$pax_TOTAL
lasso <- glmnet(X,y, alpha = 1)

fit_lasso_cv = cv.glmnet(X, y, alpha = 1,nfolds = 5)

# predict test with lasso
prediccion_lasso <- predict(
  object = lasso,
  newx = model.matrix(pax_TOTAL ~ ., data = test)[,-1],
  s = fit_lasso_cv$lambda.min
)

rmse_lasso <- sqrt(mean((prediccion_lasso - test$pax_TOTAL)^2))
print(rmse_lasso) # RMSE: 134:2739

# ridge
ridge <- glmnet(X,y, alpha = 0)

plot(ridge)
tidy(ridge)
prediction_ridge <- predict(
  object = ridge,
  newx = model.matrix(pax_TOTAL ~ ., data = test)[,-1],
  s = fit_lasso_cv$lambda.min
)

fit_ridge_cv = cv.glmnet(X, y, alpha = 0,nfolds = 5)

# predict test with ridge
prediccion_ridge <- predict(
  object = ridge,
  newx = model.matrix(pax_TOTAL ~ ., data = test)[,-1],
  s = fit_ridge_cv$lambda.min
)

rmse_ridge <- sqrt(mean((prediction_ridge - test$pax_TOTAL)^2))
print(rmse_ridge) # RMSE: 133.9727
