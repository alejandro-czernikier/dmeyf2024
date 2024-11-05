# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("rlang")
require("yaml")
require("data.table")
library(ggplot2)



#Cargamos datasets
setwd("D:/Documentos/BACK UP RED INBIRS. 20221011/Doctorado/Cursos/Data Mining/1er año/EyF/Workflow/Rtados colaborativos")

ganancia_base <- fread("ganancias_log_Base.txt", header = TRUE, sep = "\t")  # Adjust `sep` if needed
ganancia_base_tot <- fread("ganancias_01_054_Base.txt", header=TRUE, sep ="\t")

ganancia_01 <- fread("ganancias_log_exp_01.txt", header = TRUE, sep = "\t")  # Adjust `sep` if needed
ganancia_01_tot <- fread("ganancias_01_034_exp_01.txt", header=TRUE, sep ="\t")

ganancia_02 <- fread("ganancias_log_exp_02.txt", header = TRUE, sep = "\t")  # Adjust `sep` if needed
ganancia_02_tot <- fread("ganancias_01_041_exp_02.txt", header=TRUE, sep ="\t")

ganancia_03 <- fread("ganancias_log_exp_03.txt", header = TRUE, sep = "\t")  # Adjust `sep` if needed
ganancia_03_tot <- fread("ganancias_01_033_exp_03.txt", header=TRUE, sep ="\t")

ganancia_04 <- fread("ganancias_log_exp_04.txt", header = TRUE, sep = "\t")  # Adjust `sep` if needed
ganancia_04_tot <- fread("ganancias_01_046_exp_04.txt", header=TRUE, sep ="\t")

ganancia_05 <- fread("ganancias_log_exp_05.txt", header = TRUE, sep = "\t")  # Adjust `sep` if needed
ganancia_05_tot <- fread("ganancias_01_033_exp_05.txt", header=TRUE, sep ="\t")



#Agrupamos las ganancias de las 20 semillas para el envío donde tuvieron la mejor ganacia promedio
#Establecemos el corte en 11170 que es donde se obtuvo la mejor ganancia promedio en el base
mejor_base <-  ganancia_base_tot[envios == 11170, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_base[, experiment := "base"]

mejor_01 <- ganancia_01_tot[envios == 11170, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_01[, experiment := "01"]

mejor_02 <- ganancia_02_tot[envios == 11170, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_02[, experiment := "02"]

mejor_03 <- ganancia_03_tot[envios == 11170, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_03[, experiment := "03"]

mejor_04 <- ganancia_04_tot[envios == 11170, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_04[, experiment := "04"]

mejor_05 <- ganancia_05_tot[envios == 11170, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_05[, experiment := "05"]


#Juntamos datasets
mejores <- rbind(mejor_base, mejor_01, mejor_02, mejor_03, mejor_04, mejor_05)
mejores_t <- transpose(mejores[, !("experiment"), with=FALSE]) 
setnames(mejores_t, mejores$experiment)

# Usar melt() de data.table
mejores_long <- data.table::melt(mejores_t, variable.name = "experiment", value.name = "ganancia")


# Crear boxplot con puntos usando ggplot2
ggplot(mejores_long, aes(x = experiment, y = ganancia)) +
  geom_boxplot(outlier.shape = NA) +  # Evitar mostrar los outliers en el boxplot
  geom_jitter(color = "red", width = 0.2) +  # Agregar puntos con dispersión
  labs(title = "Ganancias por Experimento", x = "Experimento", y = "Ganancia") +
  theme_minimal()

# Wlcoxon
pairwise_result <- pairwise.wilcox.test(
  x = mejores_long$ganancia,
  g = mejores_long$experiment,
  paired = TRUE,
  p.adjust.method = "bonferroni"  # Ajustamos Bonferroni por comparación múltiple
)

# Display the pairwise comparison results
print(pairwise_result)

#---------------------------------------------------------------------------------------------------------------

#Ahora comparamos tomando las ganancias en el número de envío donde cada modelo tuvo su ganancia promedio máxima

#mejor_base no lo cambio pq 11170 fue su mejor envio

mejor_01_b <- ganancia_01_tot[envios == 11119, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_01_b[, experiment := "01"]

mejor_02_b <- ganancia_02_tot[envios == 12341, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_02_b[, experiment := "02"]

mejor_03_b <- ganancia_03_tot[envios == 10821, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_03_b[, experiment := "03"]

mejor_04_b <- ganancia_04_tot[envios == 11945, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_04_b[, experiment := "04"]

mejor_05_b <- ganancia_05_tot[envios == 11291, !c("envios", "gan_sum_1", "gan_suavizada"), with=FALSE]
mejor_05_b[, experiment := "05"]



#Juntamos datasets
mejores_b <- rbind(mejor_base, mejor_01_b, mejor_02_b, mejor_03_b, mejor_04_b, mejor_05_b)
mejores_b_t <- transpose(mejores_b[, !("experiment"), with=FALSE]) 
setnames(mejores_b_t, mejores_b$experiment)

#Pasamos a formato largo
mejores_b_long <- data.table::melt(mejores_b_t, variable.name = "experiment", value.name = "ganancia")


#Graficamos
ggplot(mejores_b_long, aes(x = experiment, y = ganancia)) +
  geom_boxplot(outlier.shape = NA) +  # Evitar mostrar los outliers en el boxplot
  geom_jitter(color = "red", width = 0.2) +  # Agregar puntos con dispersión
  labs(title = "Ganancias por Experimento", x = "Experimento", y = "Ganancia") +
  theme_minimal()


# Wlcoxon
pairwise_result_b <- pairwise.wilcox.test(
  x = mejores_b_long$ganancia,
  g = mejores_b_long$experiment,
  paired = TRUE,
  p.adjust.method = "bonferroni"  # Ajustamos Bonferroni por comparación múltiple
)

# Display the pairwise comparison results
print(pairwise_result_b)




