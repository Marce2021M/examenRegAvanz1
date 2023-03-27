#install.packages("xtable")
library(xtable)
#--- Usar espejo CRAN del ITAM ---
options(repos="http://cran.itam.mx/")

#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}
# Lectura de los datos 
datos<-read.csv("http://gente.itam.mx/lnieto/index_archivos/Gestacional.csv")
str(datos)
# Resumen de los datos 
# Carga la biblioteca xtable
library(xtable)

# Ejecuta la función summary en tus datos
data_summary <- summary(datos[c("Edad", "Peso_madre", "Peso_prod")])
# Convierte el resultado en una tabla en formato LaTeX
summary_latex <- xtable(data_summary)
# Imprime la tabla en formato LaTeX
print(summary_latex, type = "latex")

library(ggplot2)
categorical_data <- datos[,c("Anestesia", "Complica", "Escolaridad", "Estado_civil", "Forma_nac", "Hospital", "Ind_cesarea", "Monitoreo")]
categorical_data_long <- tidyr::gather(categorical_data, key = "variable", value = "value")
barplot <- ggplot(categorical_data_long, aes(x = value)) + geom_bar(fill = "#4c78a8") + theme_bw()
barplot_faceted <- barplot + facet_wrap(~variable, ncol = 2) + labs(x = NULL, y = "Frecuencia", title = "Gráficas de barras de variables categóricas")
barplot_final <- barplot_faceted + scale_fill_manual(values = c("#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b", "#8172b2", "#b279a2", "#ccb974"))
print(barplot_final)

# Aplicar la estructura condicional ifelse() para crear la variable TipoHosp
datos$TipoHosp <- ifelse(datos$Hospital %in% c(1, 2, 4), 1, 2)
# 1 es privado, 2 es público. 
# Vamos a hacer que los datos de Mortalidad estén en {0, 1}
datos$Mortalidad <- datos$Mortalidad - 1 
# Análisis Bayesiano: Modelos lineales generalizados (Bernoulli)
Anestesia <- datos$Anestesia
Complica <- datos$Complica
Edad <- datos$Edad
Escolaridad <- datos$Escolaridad
Estado_civil <- datos$Estado_civil
Forma_Nac <- datos$Forma_nac
TipoHosp <- datos$TipoHosp
Ind_cesarea <- datos$Ind_cesarea
Monitoreo <- datos$Monitoreo
Mortalidad <- datos$Mortalidad
Peso_madre <- datos$Peso_madre
Peso_prod <- datos$Peso_prod
# Tamaño de la muestra 
n <- length(datos$Anestesia)

# Definimos los datos iniciales 
data<-list("n"=n,"Mortalidad"=Mortalidad,"Anestesia"=Anestesia,"Complica"=Complica,
           "Edad"=Edad,"Escolaridad"=Escolaridad,"Estado_civil"=Estado_civil,"Forma_nac"=Forma_Nac, 
           "TipoHosp"=TipoHosp, "Ind_cesarea"= Ind_cesarea, "Monitoreo"= Monitoreo, "Peso_madre"=Peso_madre, 
           "Peso_prod"= Peso_prod
)

# Parámetros por monitorear 
params <- c("alpha.est", "beta1", "beta3", "beta10", "beta11", 
            "beta1.est", "beta2.est", "beta6.est", "beta7.est", "beta9.est",
            "beta4.est", "beta5.est", "beta8.est")

# Parámetros iniciales
inits <- function(){
  list(
    beta0 = 0 , 
    beta3 = 0 , 
    beta10 = 0 ,
    beta11 = 0 ,
    beta1 = rep(0, 2), 
    beta2 = rep(0, 2),
    beta6 = rep(0, 2),
    beta7 = rep(0, 2),
    beta9 = rep(0, 2),
    beta4 = rep(0, 4),
    beta5 = rep(0, 4),
    beta8 = rep(0, 6)
  )
}

# BUGS 
mod.sim<-bugs(data,inits,params,model.file="ExamModel.txt",
              n.iter=10000,n.chains=2,n.burnin=1000,n.thin=2)
#JAGS
mod.sim<-jags(data,inits,params,model.file="ExamModel.txt",
              n.iter=10000,n.chains=2,n.burnin=1000,n.thin=2)
# Monitoreo de la cadena que acabamos de generar 
#traceplot(mod.sim)
mod.sim$BUGSoutput$DIC

setwd("/home/rstudio/examenRegAvanz1/rstudio-export")
