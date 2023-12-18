# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require(tidyverse)
require(data.table)
require(rsample)
require(rpart)
require(rpart.plot)

set.seed(428)

df = fread('../molinetes-2023/molinetes.csv.gz')
estaciones = fread('../molinetes-2023/estaciones.csv.gz')

# Creamos LINEA_ESTACION, para el join con estaciones
df$LINEA_ESTACION <- paste(df$LINEA, df$ESTACION, sep = "_")

# Joineamos entre df y estaciones
df <- left_join(df, estaciones %>% select(-id, -geometry), by = "LINEA_ESTACION")
colnames(df)

df[, .(unique_ESTACION = unique(ESTACION)), by = BARRIO]

# Trabajamos sólo para un Año...
df <- df[ANIO == 2022]
df <- df[BARRIO == "BALVANERA"]

# Descartamos columnas:
columns_to_remove <- c("DESDE", "HASTA", "FECHA", "MOLINETE", "HORA_PICO", "DIA_MES",
                       "pax_pagos", "pax_pases_pagos", "pax_franq", 
                       "LINEA_ESTACION", "ANIO", "MES",
                       "BARRIO", "COMUNA",
                       #"ESTACION", 
                       "LINEA", 
                       #"DIA",
                       "universidades", "puntos_verdes", "centros_culturales","escuelas", "hospitales")

df[, (columns_to_remove) := NULL]




# Convertimos a dataframe
df <- as.data.frame(df)

## -- ##

# Vemos la clase de las variables
print(sapply(df, class))

# Convertimos a factor.
variables_to_convert <- c("ESTACION")
for (var in variables_to_convert) {
  df[[var]] <- factor(df[[var]])
}


# Pasamos a dummy algunas categóricas
encoded_estacion <- model.matrix(~ 0 + ESTACION, data = df)
encoded_estacion_df <- as.data.frame(encoded_estacion)

df_final <- cbind(df, encoded_estacion_df)


# Eliminamos últimas columnas.
columns_to_remove <- c("ESTACION")
df_final <- df_final %>% select(-one_of(columns_to_remove))


# Verificamos la clase
print(sapply(df_final, class))

# Pasamos numeric a integer.
#df_final <- df_final %>%
 # mutate_if(is.numeric, as.integer)


trainIndex = caret::createDataPartition(y = df_final$pax_TOTAL,
                                        p = 0.75, 
                                        list = FALSE,
                                        times = 1)
train_set = df_final[ trainIndex,]
test_set = df_final[-trainIndex,]

# Liberamos espacio
rm(df, df_final, estaciones, encoded_estacion_df, encoded_estacion)
gc()

# --- --- --- --- --- --- --- --- --- --- --- Modelos --- --- --- --- --- --- --- --- --- --- --- #

# A continuación generamos distintos modelos para predecir total de pasajeros segun la linea, estacion, hora, dia, y dia del mes.


# --- --- --- --- --- --- --- --- --- --- --- Modelo 1 --- --- --- --- --- --- --- --- --- --- --- #


# Modelo 1: le pasamos cp=0, sin podar.
# Los únicos hiperparámetros que entran en juego son los default: minsplit = 20, minbucket = 7, maxdepth = 30.

m1 <- rpart(pax_TOTAL ~ ., 
            data = train_set, 
            method = "anova",
            control = list(cp=0))

printcp(m1) 
summary(m1)
length(m1$frame$var)
m1$control
m1$control$minbucket

rpart.plot(m1)

# Complexity table
# Tabla: muestra los valores de CP candidatos, y la usamos para determinar cómo podar el árbol a partir de una métrica de error.
# Es decir, usando cross validation "estima" cómo se ve el error para cada valor distinto de CP y, por ende, de complejidad del árbol.
# La idea es examinar el resultado de los errores calculado con cross validation, seleccionar el CP asociado, con el menor error,
# y después podar el árbol. 

# Relative error es una medida de error, ratio entre error de subárbol respecto al árbol original.
# xerror: cross validated error. Estimación de la predicción del error al aplicar nuevos datos.
# xstd: standard error: error estandar del error. Da indicio de cuán robusto es el xerror.

# Conclusiones: el árbol crece en profundidad. Remarcar la cantidad de filas omitidas, que sirve de referencia para entender
# la cantidad de splits del árbol.

# cp: penaliza al modeleo por tener muchos nodos. Cuanto más chico sea cp, más simple es el árbol.
# nsplit: número de splits en el árbol. 
# rel error: ratio de error entre el error del subarbol y del arbol inicial.
# xerror: cross validated error, error basado en cross validation. El modelo elige el cp que minimiza este valor.
# xstd: error estandar del cross validation.



# Predecimos usando los datos de testeo
p1 <- predict(m1, newdata = test_set)

# Creamos un objeto con los datos reales
real_values <- test_set$pax_TOTAL

# Calculamos el Mean Squared Error (MSE)
mse_1 <- mean((real_values - p1)^2)

# Calculamos el Root Mean Squared Error (RMSE)
rmse_1 <- sqrt(mse_1)

print(paste("MSE:", mse_1))
print(paste("RMSE:", rmse_1))

# Scatter plot
plot(real_values, p1, main = "Modelo 1 - Performance del árbol", 
     xlab = "Valores reales", ylab = "Valores predichos") + 
  abline(a = 0, b = 1, col = "red")




# --- --- --- --- --- --- --- --- --- --- --- Modelo 2 --- --- --- --- --- --- --- --- --- --- --- #


# Modelo 2: le pasamos otro cp, sólo para entender la idea de la tabla CP. Luego vamos con el valor por default, cp=0.01.

m2 <- rpart(pax_TOTAL ~ ., 
            data = train_set, 
            method = "anova",
            cp = 0.001) # Asignar luego valores más altos (0.003, 0.004, 0.007), sólo para mostrar cómo cambia la CP table.
                         
# Complexity table
printcp(m2) 

# A medida que hacemos más chico el CP, el árbol crece; si agrandamos CP, el árbol se acerca a su nodo raíz.


# Podamos el arbol
m2_pruned <- prune(m2, cp = 0.01)

summary(m2_pruned)

# Predecimos usando los datos de testeo
p2 <- predict(m2_pruned, newdata = test_set)

# Creamos un objeto con los datos reales
real_values <- test_set$pax_TOTAL

# Calculamos el Mean Squared Error (MSE)
mse_2 <- mean((real_values - p2)^2)

# Calculamos el Root Mean Squared Error (RMSE)
rmse_2 <- sqrt(mse_2)

# Imprimimos resultados hasta ahora.
print(paste("MSE Modelo 1:", mse_1))
print(paste("MSE Modelo 2:", mse_2))

print(paste("RMSE Modelo 1:", rmse_1))
print(paste("RMSE Modelo 2:", rmse_2))


rpart.plot(m2_pruned)
plotcp(m2)

# Scatter plot
plot(real_values, p2, main = "Modelo 2 - Performance del árbol", 
     xlab = "Valores reales", ylab = "Valores predichos") + 
  abline(a = 0, b = 1, col = "red")




# --- --- --- --- --- --- --- --- --- --- --- Modelo 3 --- --- --- --- --- --- --- --- --- --- --- #


# Modelo 3: tomamos el modelo 2, pero lo podamos eligiendo el CP óptimo.

printcp(m2)

# Opción 1: tomamos el alfa que hace mínimo xerror, y luego podamos.

# Tomamos el alfa que minimiza el error.
cp_m3_opt1 <- m1$cptable[which.min(m1$cptable[,"xerror"]),"CP"]
options(scipen = 999)
cp_m3_opt1

# Podamos, construyendo así el modelo 3 opción 1.
m3_opt1 <- prune(m1, cp= cp_m3_opt1)

# Predecimos usando los datos de testeo
p3_opt1 <- predict(m3_opt1, newdata = test_set)

# Creamos un objeto con los datos reales
real_values <- test_set$pax_TOTAL

# Calculamos el Mean Squared Error (MSE)
mse_3_opt1 <- mean((real_values - p3_opt1)^2)

# Calculamos el Root Mean Squared Error (RMSE)
rmse_3_opt1 <- sqrt(mse_3_opt1)

# Imprimimos resultados hasta ahora.
print(paste("MSE Modelo 1:", mse_1))
print(paste("MSE Modelo 2:", mse_2))
print(paste("MSE Modelo 3 Opción 1:", mse_3_opt1))

print(paste("RMSE Modelo 1:", rmse_1))
print(paste("RMSE Modelo 2:", rmse_2))
print(paste("RMSE Modelo 3 Opción 1:", rmse_3_opt1))

# Scatter plot
plot(real_values, p3_opt1, main = "Modelo 3 Opción 1 - Performance del árbol", 
     xlab = "Valores reales", ylab = "Valores predichos") + 
  abline(a = 0, b = 1, col = "red")





# Opción 2: tomamos el Beta óptimo...











rsq.rpart(m2) # Gráfico de error según númber of splits.
plotcp(m2) # Gráficos cross validation
plot(m2) # Grafica el árbol
text(m2)# Agrega texto al árbol graficado

