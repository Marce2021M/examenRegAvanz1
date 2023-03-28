library(R6)
#Creamos la clase
jagsModel <- R6Class("jagsModel",
  public = list(
    data = NULL,
    inits = NULL,
    parms = NULL,
    model.fileText = NULL,
    n.iter = NULL,
    n.chains = NULL,
    n.burnin = NULL,
    n.thin = NULL,
    modelSim = NULL,
    outModelSim = NULL,
    graphModelSim = NULL,
    summModel = NULL,
    dicModel = NULL,
    #Inicializamos
    initialize = function(data, inits, parms, model.fileText, n.iter=5000, n.chains=2, n.burnin=500, n.thin=1) {
      self$data <- data
      self$inits <- inits
      self$parms <- parms
      self$model.fileText <- model.fileText
      self$n.iter <- n.iter
      self$n.chains <- n.chains
      self$n.burnin <- n.burnin
      self$n.thin <- n.thin
      self$modelSim <- NULL
      self$outModelSim <- NULL
      self$graphModelSim <- NULL
      self$summModel <- NULL
      self$dicModel <- NULL
    },
    #Corremos una cadena
    runJagsModel = function() {
      require(R2jags)
      self$modelSim <- jags(model.file = textConnection(self$model.fileText),
      data = self$data,
      parameters.to.sav = self$parms,
      inits = self$inits,
      n.iter=self$n.iter,
      n.chains = self$n.chains,
      n.burnin = self$n.burnin,
      n.thin = self$n.thin)
      
      self$outModelSim <- self$modelSim$BUGSoutput$sims.list
      self$graphModelSim <- self$modelSim$BUGSoutput$sims.array
      self$summModel <- self$modelSim$BUGSoutput$summary
      self$dicModel <- self$modelSim$BUGSoutput$DIC
      
    },
    #Graficamos traceplot
    jagsTraceplot = function() {
      return(traceplot(self$modelSim))
    },
    #Vemos el espacio que recorre la cadena
    viewSpace = function() {
      z <- self$outModelSim$beta
      par(mfrow=c(1,1))
      plot(z)
    },
    #Vemos la convergencia de la cadena
    graphConvergence = function(parama = 1) {
      z1 <- self$graphModelSim[,1,parama]
      z2 <- self$graphModelSim[,2,parama]
      par(mfrow=c(3,2))
      plot(z1,type="l",col="grey50")
      lines(z2,col="firebrick2")
      y1<-cumsum(z1)/(1:length(z1))
      y2<-cumsum(z2)/(1:length(z2))
      ymin<-min(y1,y2)
      ymax<-max(y1,y2)
      plot(y1,type="l",col="grey50",ylim=c(ymin,ymax))
      lines(y2,col="firebrick2",ylim=c(ymin,ymax))
      hist(z1,freq=FALSE,col="grey50")
      hist(z2,freq=FALSE,col="firebrick2")
      acf(z1)
      acf(z2)
    },
    #Vemos el summary
    summaryModel = function() {
      print(self$summModel)
    },
    #Checamos el DIC 
    dicModelfunc = function() {
      print(self$dicModel)
    }
  )
) 

ejercicio3Pred <- function(model1){
  #Predictions
  out.yf<-model1$summModel[grep("yf",rownames(model1$summModel)),]
  or<-order(calif$MO)
  ymin<-min(calif$SP,out.yf[,c(1,3,7)])
  ymax<-max(calif$SP,out.yf[,c(1,3,7)])
  par(mfrow=c(1,1))
  plot(calif$MO,calif$SP,ylim=c(ymin,ymax))
  lines(calif$MO[or],out.yf[or,1],lwd=2,col=2)
  lines(calif$MO[or],out.yf[or,3],lty=2,col=2)
  lines(calif$MO[or],out.yf[or,7],lty=2,col=2)

  plot(calif$SP,out.yf[,1])
  R2<-(cor(calif$SP,out.yf[,1]))^2
  print("Evaluamos la 'R^2'")
  print(R2)
}

ejercicio35Graf <- function(model1){
  out.sum <- model1$summModel
  head(out.sum[,c(1,3,7)])
  write.csv(head(out.sum[,c(1,3,7)]),file="pestim.csv")

  par(mfrow=c(1,1))

  #nu
  out.nu<-out.sum[grep("nu",rownames(out.sum)),]
  out.est<-out.nu
  k<-1
  print(out.est[c(1,3,7)])
  ymin<-min(out.est[c(1,3,7)])
  ymax<-max(out.est[c(1,3,7)])
  plot(1:k,out.est[1],xlab="index",ylab="",ylim=c(ymin,ymax))
  segments(1:k,out.est[3],1:k,out.est[7])
  abline(h=0,col="grey70")
  title("Precio: efecto anticipacion")

  #beta
  out.beta<-out.sum[grep("beta",rownames(out.sum)),]
  out.est<-out.beta
  k<-n
  print(out.est[,c(1,3,7)])
  ymin<-min(out.est[,c(1,3,7)])
  ymax<-max(out.est[,c(1,3,7)])
  plot(1:k,out.est[,1],xlab="index",ylab="",ylim=c(ymin,ymax))
  segments(1:k,out.est[,3],1:k,out.est[,7])
  abline(h=0,col="grey70")
  title("Precio: efecto diario")

  #gama
  out.gama<-out.sum[grep("gama",rownames(out.sum)),]
  out.est<-out.gama
  k<-7
  print(out.est[,c(1,3,7)])
  ymin<-min(out.est[,c(1,3,7)])
  ymax<-max(out.est[,c(1,3,7)])
  plot(1:k,out.est[,1],xlab="dia",ylab="",ylim=c(ymin,ymax))
  segments(1:k,out.est[,3],1:k,out.est[,7])
  abline(h=0,col="grey70")
  title("Precio: efecto dia de la semana")

  #delta
  out.delta<-out.sum[grep("delta",rownames(out.sum)),]
  out.est<-out.delta
  k<-12
  print(out.est[,c(1,3,7)])
  ymin<-min(out.est[,c(1,3,7)])
  ymax<-max(out.est[,c(1,3,7)])
  plot(1:k,out.est[,1],xlab="mes",ylab="",ylim=c(ymin,ymax))
  segments(1:k,out.est[,3],1:k,out.est[,7])
  abline(h=0,col="grey70")
  title("Precio: efecto mensual")

  #epsilon
  out.epsilon<-out.sum[grep("epsilon",rownames(out.sum)),]
  out.est<-out.epsilon
  k<-3
  print(out.est[,c(1,3,7)])
  ymin<-min(out.est[,c(1,3,7)])
  ymax<-max(out.est[,c(1,3,7)])
  plot((1:k)+2016,out.est[,1],xlab="ano",ylab="",ylim=c(ymin,ymax))
  segments((1:k)+2016,out.est[,3],(1:k)+2016,out.est[,7])
  abline(h=0,col="grey70")
  title("Precio: efecto anual")

  #theta
  out.theta<-out.sum[grep("theta",rownames(out.sum)),]
  out.est<-out.theta
  k<-2
  print(out.est[,c(1,3,7)])
  ymin<-min(out.est[,c(1,3,7)])
  ymax<-max(out.est[,c(1,3,7)])
  plot(1:k,out.est[,1],xlab="puente (no - si)",ylab="",ylim=c(ymin,ymax))
  segments(1:k,out.est[,3],1:k,out.est[,7])
  abline(h=0,col="grey70")
  title("Precio: efecto puente")

  #tau
  out.tau<-out.sum[grep("tau",rownames(out.sum)),]
  out.est<-out.tau
  k<-n
  print(out.est[,c(1,3,7)])
  ymin<-min(out.est[,c(1,3,7)])
  ymax<-max(out.est[,c(1,3,7)])
  plot(1:k,out.est[,1],xlab="index- tau",ylab="",ylim=c(ymin,ymax))
  segments(1:k,out.est[,3],1:k,out.est[,7])
  abline(h=0,col="grey70")
  title("Precio: precisiones diarias")

}

canvaMortalidad <- function(){
  #mortality<-read.table("http://gente.itam.mx/lnieto/index_archivos/mortality.txt",header=TRUE)
  out.yf1<-ej5a.sim$summModel[grep("yf1",rownames(ej5a.sim$summModel)),]  #especial
  or<-order(mortality$x)
  ymin<-min(mortality$y,out.yf1[,c(1,3,7)])
  ymax<-max(mortality$y,out.yf1[,c(1,3,7)])

  par(mfrow=c(1,1))
  plot(mortality$x,mortality$y,ylim=c(ymin,ymax))

}

mortalidadGraf <- function(modelo, col=1){
  or<-order(mortality$x)
  out.yf1<-modelo$summModel[grep("yf1",rownames(modelo$summModel)),]  #especial

  lines(mortality$x[or],out.yf1[or,1],lwd=2,col=col)
  lines(mortality$x[or],out.yf1[or,3],lty=2,col=col)
  lines(mortality$x[or],out.yf1[or,7],lty=2,col=col)
}

sextoGrafPred <- function(model){
  out.yf<-model$summModel[grep("yf1",rownames(model$summModel)),]
  ymin<-min(desastres[,2],out.yf[,c(1,3,7)])
  ymax<-max(desastres[,2],out.yf[,c(1,3,7)])

  par(mfrow=c(1,1))
  plot(desastres,type="l",col="grey80",ylim=c(ymin,ymax))
  lines(desastres[,1],out.yf[,1],lwd=2,col=2)
  lines(desastres[,1],out.yf[,3],lty=2,col=2)
  lines(desastres[,1],out.yf[,7],lty=2,col=2)
  lines(desastres[,1],out.yf[,5],lwd=2,col=4)
}

sextoGrafMedia <- function(model){
  out.mu<-model$summModel[grep("mu",rownames(model$summModel)),]
  par(mfrow=c(1,1))
  plot(desastres,type="l",col="grey80")
  lines(desastres[,1],out.mu[,1],lwd=2,col=2)
  lines(desastres[,1],out.mu[,3],lty=2,col=2)
  lines(desastres[,1],out.mu[,7],lty=2,col=2)

}

octavoSeguros <- function(model1,model2, model3){
    #Estimaciones
  outa.p<-model1$summModel[grep("p",rownames(model1$summModel)),]
  outb.p<-model2$summModel[grep("p",rownames(model2$summModel)),]
  outc.p<-model3$summModel[grep("p",rownames(model3$summModel)),]
  outc.eta<-model3$summModel[grep("eta",rownames(model3$summModel)),]

  #x vs. y
  xmin<-0
  xmax<-12
  ymin<-0
  ymax<-1
  par(mfrow=c(1,1))
  plot(reclama$r/reclama$n,type="p",col="grey50",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  #
  out.p<-outb.p
  points(out.p[,1],col=2,pch=16,cex=0.5)
  segments(1:10,out.p[,3],1:10,out.p[,7],col=2)
  #
  out.p<-outc.p
  points((1:10)+0.2,out.p[,1],col=4,pch=16,cex=0.5)
  segments((1:10)+0.2,out.p[,3],(1:10)+0.2,out.p[,7],col=4)
  #
  points(xmax-0.2,sum(reclama$r)/sum(reclama$n))
  #
  out.p<-outa.p
  points(xmax-0.2,out.p[1],col=3,pch=16,cex=0.5)
  segments(xmax-0.2,out.p[3],xmax-0.2,out.p[7],col=3)
  #
  out.p<-outc.eta
  points(xmax,out.p[1],col=4,pch=16,cex=0.5)
  segments(xmax,out.p[3],xmax,out.p[7],col=4)
}

novenoSeguros <- function(model1,model2, model3){
  #Estimaciones
  outa.p<-model1$summModel[grep("theta",rownames(model1$summModel)),]
  outb.p<-model2$summModel[grep("theta",rownames(model2$summModel)),]
  outc.p<-model3$summModel[grep("theta",rownames(model3$summModel)),]
  outc.eta<-model3$summModel[grep("^eta",rownames(model3$summModel)),]

  #x vs. y
  xmin<-0
  xmax<-10
  ymin<-0
  ymax<-5
  par(mfrow=c(1,1))
  plot(leucemia$Obs/leucemia$Pops*10000,type="p",col="grey50",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  #
  out.p<-outb.p
  points(out.p[,1],col=2,pch=16,cex=0.5)
  segments(1:8,out.p[,3],1:8,out.p[,7],col=2)
  #
  out.p<-outc.p
  points((1:8)+0.2,out.p[,1],col=4,pch=16,cex=0.5)
  segments((1:8)+0.2,out.p[,3],(1:8)+0.2,out.p[,7],col=4)
  #
  points(xmax-0.2,sum(leucemia$Obs)/sum(leucemia$Pops)*10000)
  #
  out.p<-outa.p
  points(xmax-0.2,out.p[1],col=3,pch=16,cex=0.5)
  segments(xmax-0.2,out.p[3],xmax-0.2,out.p[7],col=3)
  #
  out.p<-outc.eta
  points(xmax,out.p[1],col=4,pch=16,cex=0.5)
  segments(xmax,out.p[3],xmax,out.p[7],col=4)
}
