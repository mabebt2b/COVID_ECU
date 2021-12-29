# -------------------------------------------------------------------------------
# Análisis estadísitico de Impacto de la vacunación en el número de caso de COVID
#
# Belén Benalcázar
# 28-Dic-2021
# -------------------------------------------------------------------------------

# Se cargan los datasets limpiados, preparados y publicados en GitHub
path_vacunas<-"https://raw.githubusercontent.com/mabebt2b/COVID_ECU/main/vacunas_ecu.csv"
path_casos<-path<-"https://raw.githubusercontent.com/mabebt2b/COVID_ECU/main/covid_ecu.csv"
ds_vacunas<-read.csv(path_vacunas)
ds_casos<-read.csv(path_casos)
ds_vacunas$fecha <- as.Date(ds_vacunas$fecha)
ds_casos$fecha <- as.Date(ds_casos$fecha)

t<-ds_vacunas[,"fecha"]
x<-ds_vacunas[,"segunda_dosis"]/1e06
y<-ds_vacunas[,"positivas"]/1000

# Regresión lineal para los dos grupos
modelo_g1<-lm(y[0:117]~t[0:117])
modelo_g2<-lm(y[118:235]~t[118:235])

# Gráfico de los campos "casos" y "segunda_dosis"
par(mar = c(5, 4, 4, 4) + 0.3)  # Deja espacio para el segundo eje
plot(t,y,type="l", col="darkcyan", lwd=3,xaxt="none", xlab="", ylab="", main="",las = 1,font=7,cex.axis=0.8)
lines(t[0:117],y[0:117],type="l", col="green", lwd=3,xaxt="none", xlab="", ylab="", main="",las = 1,font=7)
abline(modelo_g1,col="green",lwd=1,lty=2)
abline(modelo_g2,col="darkcyan",lwd=1,lty=2)
par(new=TRUE)
plot(t, x, type = "l", col="blue", lwd=3 ,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis.Date(1, at = seq(as.Date("2021-02-01"), as.Date("2021-12-31"),by = "months"),las = 1,font=7,cex.axis=0.8,mgp=c(3,0.5,0))
axis(4, at = pretty(range(x)), las=2,font=7,cex.axis=0.8)
mtext("EVOLUCIÓN CASOS Y SEGUNDA DOSIS EN ECUADOR",side=3, line=.8, cex=1.1, font=2)
mtext("Año 2021",side=1, line=1.5, cex=0.8, font=2)
mtext("Casos (miles)",side=2, line=2.6, cex=0.8, font=2)
mtext("Segunda dosis (millones)", side=4, line=1.9, cex=0.8,font=2)
abline(v=seq(as.Date("2021-02-01"), as.Date("2021-12-31"),by = "months"), lty=3, col="gray")
abline(v=t[117], lty=5, col="black",lwd=2)
legend("topleft", legend=c("Casos", "Segunda dosis"),col=c("green", "blue"), lty=1:1, lwd=3:3,cex=0.8,bty="n")
text(locator(1), "03-Aug",col="black", font=2, cex=0.8)

# Estadística descriptiva de nuevos casos en dos etapas
summary(ds_casos[ds_casos$fecha<='2021-08-03',"nuevos_casos"])
summary(ds_casos[ds_casos$fecha>'2021-08-03',"nuevos_casos"])
mean(ds_casos[ds_casos$fecha<='2021-08-03',"nuevos_casos"])
mean(ds_casos[ds_casos$fecha>'2021-08-03',"nuevos_casos"])

# gráfica de dispersión segunda dosis vs. casos
par(mar = c(5, 5, 4, 2))
plot(x,y, col="darkcyan", lwd=3,xaxt="none", xlab="", ylab="", main="",las = 1,font=7,cex=0.3,cex.axis=0.8)
axis(1, at = pretty(range(x)), las=0,font=7,cex.axis=0.8,mgp=c(3,0.3,0))
mtext("CASOS VS. SEGUNDAS DOSIS",side=3, line=.8, cex=1.1, font=2)
mtext("Casos (miles)",side=2, line=2.6, cex=0.8, font=2)
mtext("Segunda dosis (millones)", side=1, line=1.1, cex=0.8,font=2)
abline(h=seq(from=300,to=550,by=50),v=seq(from=0,to=12), lty=3, col="gray")

# Regresión Logarítmica
modelo<-lm(y~log(x))
summary(modelo)
