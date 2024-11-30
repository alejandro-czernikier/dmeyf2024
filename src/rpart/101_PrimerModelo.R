# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table ,  rpart  y  rpart.plot



# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
getwd()
setwd("D:/Documentos/BACK UP RED INBIRS. 20221011/Doctorado/Cursos/Data Mining/1er año/EyF/rpart") # Establezco el Working Directory

# cargo el dataset que tiene la clase calculada !
#Va de 202101 a 202106
dataset <- fread("../competencia_01_R.csv")

max(dataset$foto_mes)
dataset$clase_ternaria

View(dataset[foto_mes==202106])

dtrain <- dataset[foto_mes <= 202104] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202106] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
    formula = "clase_ternaria ~ .", #"~": es explicado por: Luego iría var1 + var2, etc. EL punto indica "todas las variables"
    data = dtrain, # los datos donde voy a entrenar
    xval = 0, #desactivo CV que haría por defecto
    cp = -1, # esto significa no limitar la complejidad de los splits. Desactiva poda
    minsplit = 250, # minima cantidad de registros para que se haga el split. Obj: prevenir overfiting al evitar divisiones en nodos con pocos datos
    minbucket = 100, # tamaño minimo de una hoja. Previene overfitting al evitar hojas con pocos registros
    maxdepth = 10  # profundidad maxima del arbol. Limito complejidad acá
)


# grafico el arbol
prp(modelo,
    extra = 101, #Define qué info se muestra en los nodos. #obs, %de clase mayoritaria, etc
    digits = -5, #-5 es notacion cientifica. 2 es normal
    branch = 0.5, #angulo de las ramas. 1=angulo recto
    type = 4, #nivel de detalle. 4 es máximo
    varlen = 0, #no trunca longitud del nombre de variables
    faclen = 0 #no trunca longitud de nombre de factores
)

prp()

# aplico el modelo a los datos nuevos
prediccion <- predict(
    object = modelo, #Especifica que modelo se va a aplicar. Aqui el entranado arriba
    newdata = dapply, #Donde lo va a aplicar. newdata debe tener mismas columnas que donde se entrenó
    type = "prob" #Devuelve la probabilidad predicha para cada clase de la variable objetivo
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades que surgen de recorrer el arbol y terminar en una hoja

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)] #si TRUE=1

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_001.csv",
        sep = ","
)
