# Modelo 1: le pasamos un cp muy bajo.
# Los únicos hiperparámetros que entran en juego son los default: minsplit = 20, minbucket = 7, maxdepth = 30.

m1 <- rpart(pax_TOTAL ~ ., 
            data = train_set, 
            method = "anova",
            control = list(cp=0.0000001))

# Principales hiperparámetros utilizados.
m1$control

# Tabla de complejidad.
options(max.print = 200)
printcp(m1)

# plotcp()
plotcp(m1)

# Tomamos el cp que minimiza el error.
cp_table <- printcp(m1)
cp_opt1<-m1$cptable[which.min(m1$cptable[,'xerror']),'CP']
cp_opt1



### Encontramos el mejor parámetro de complejidad

# Podamos el arbol
m1_pruned_V1 <- prune(m1, cp = cp_opt1)

# Creamos el objeto con el contenido del árbol
tree_frame <- m1_pruned_V1$frame

# Nodos terminales
num_terminal_nodes <- sum(tree_frame$var == "<leaf>")
num_terminal_nodes

# Cantidad de splits
num_splits <- sum(tree_frame$var != "<leaf>")
num_splits

# Predecimos usando los datos de testeo
p1 <- predict(m1, newdata = test_set)
p1_pruned_V1 <- predict(m1_pruned_V1, newdata = test_set)

# Creamos un objeto con los datos reales
real_values <- test_set$pax_TOTAL

# Calculamos el Mean Squared Error (MSE)
mse_1 <- mean((real_values - p1)^2)
mse_1_pruned_V1 <- mean((real_values - p1_pruned_V1)^2)


# Calculamos el Root Mean Squared Error (RMSE)
rmse_1 <- sqrt(mse_1)
rmse_1_pruned_V1 <- sqrt(mse_1_pruned_V1)


# Imprimimos resultados
print(paste("MSE Modelo 1:", mse_1))
print(paste("MSE Modelo 1 Pruned:", mse_1_pruned_V1))

print(paste("RMSE Modelo 1:", rmse_1))
print(paste("RMSE Modelo 1 Pruned:", rmse_1_pruned))


# Scatter plot
plot(real_values, p1_pruned_V1, main = "Post Poda - Performance del árbol", 
     xlab = "Valores reales", ylab = "Valores predichos") + 
  abline(a = 0, b = 1, col = "red")