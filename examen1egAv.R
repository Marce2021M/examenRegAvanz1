gestacion<-read.csv("https://gente.itam.mx/lnieto/index_archivos/Gestacional.csv",header=TRUE)

# Cargar los datos
datos <- gestacion

# Cargar la librería ggplot2
library(ggplot2)

# crear una tabla de contingencia
tabla <- table(datos$Anestesia, datos$Mortalidad)

barplot(table)
# convertir la tabla a un data frame
df <- as.data.frame.matrix(tabla)

# añadir una columna con la suma de cada fila
df$total <- rowSums(df)

# calcular la proporción de mortalidad para cada categoría de Anestesia
df$prop <- df[2] / df["total"]

#cambiamos nombre de columnas

colnames(df)[1] <- "no_mort"
colnames(df)[2] <- "mortalidad"

colnames(df)[4]<-"prop"


# crear el gráfico de barras apiladas
ggplot( df ,aes(x = "no_mort", y = "mortalidad", fill= "prop")) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Proporción de mortalidad por categoría de Anestesia",
       x = "Anestesia", y = "Proporción") +
  theme_classic()
