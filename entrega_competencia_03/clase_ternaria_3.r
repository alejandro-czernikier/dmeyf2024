require( "data.table" )
library(dplyr)
        
getwd()
setwd("D:/Documentos/BACK UP RED INBIRS. 20221011/Doctorado/Cursos/Data Mining/1er año/EyF/Workflow")

# leo el dataset
dataset_crudo <- fread("./competencia_03_crudo.csv" )
#View(dataset)

  # calculo el periodo0 consecutivo
dsimple <- dataset_crudo[, list(
  "pos" = .I,
  numero_de_cliente,
  periodo0 = as.integer(foto_mes/100)*12 +  foto_mes%%100 ) ]

#View(dsimple)

# ordeno
setorder( dsimple, numero_de_cliente, periodo0 )

#View(dsimple)

# calculo topes
periodo_ultimo <- dsimple[, max(periodo0) ]
periodo_anteultimo <- periodo_ultimo - 1

#periodo_ultimo


# calculo los leads de orden 1 y 2
dsimple[, c("periodo1", "periodo2") :=
          shift(periodo0, n=1:2, fill=NA, type="lead"),  numero_de_cliente ]

#View(dsimple)

# assign most common class values = "CONTINUA"
dsimple[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

# calculo BAJA+1
dsimple[ periodo0 < periodo_ultimo &
           ( is.na(periodo1) | periodo0 + 1 < periodo1 ),
         clase_ternaria := "BAJA+1" ]

# calculo BAJA+2
dsimple[ periodo0 < periodo_anteultimo & (periodo0+1 == periodo1 )
         & ( is.na(periodo2) | periodo0 + 2 < periodo2 ),
         clase_ternaria := "BAJA+2" ]


# pego el resultado en el dataset original y grabo
setorder( dsimple, pos )
dataset_crudo[, clase_ternaria := dsimple$clase_ternaria ]

fwrite( dataset_crudo,
        file =  "competencia_03.csv",
        sep = ","
)

dataset2<-fread("./competencia_03.csv")



unique(dataset2$clase_ternaria)
unique(dataset2$clase_ternaria[dataset2$foto_mes == 202109])

