# Uso de árboles de regresión para predecir el caudal de pasajeros en subterráneos de la Ciudad de Buenos Aires

## Introducción

En este trabajo se abordó la problemática del aglomeramiento de pasajeros en el sistema de subterráneos de la Ciudad de Buenos Aires. Para este fin, se generó un modelo basado en árboles de regresión que permita estimar el caudal de pasajeros de cada estación en un momento dado (septiembre) utilizando el algoritmo rpart (lenguaje R) y datos provenientes de la base de datos abierta del Gobierno de la Ciudad de Buenos Aires.

## Códigos en R

`1. preprocesamiento.R`: Código para limpieza y preprocesamiento de los datos provenientes de los datos abiertos del [Gobierno de la Ciudad de Buenos Aires](https://data.buenosaires.gob.ar/dataset/).

`2. EDA.R`: Armado de visualizaciones para el análisis exploratorio de datos.

`3. BO_arbol_pre_poda.R`: Optimización bayesiana para obtener los mejores hiperparámetros del árbol de regresión (pre-poda).
- `resultados_BO_pre_poda.txt`: resultados de la optimización bayesiana.

`4. final_arbol_pre_poda.R`: Corrida final del arbol de decisión y predicción del flujo de pasajeros en septiembre de 2023.

`5. entrenamiento_post_poda.R`: Construcción del árbol de decisión con técnica de post-poda.

`6. reg_multiple.R`: Armado de regresión múltiple como baseline.

## Informe

`Clas_Saint_Germain_Tokman_Propuesta_Trabajo_Final.pdf`: Propuesta de trabajo final en formato pdf.

