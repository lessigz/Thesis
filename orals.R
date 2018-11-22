library(lattice)
par(bg="white")

#site specific non seasonal variables
boxplot(data2$elev,      xlab="Elevation (m)",          cex.lab=1.5,pch=16,group=data2$basin.stream)
boxplot(data2$from.north,xlab="Aspect (Deg. South Facing)",cex.lab=1.5,pch=16,group=data2$basin.stream)
boxplot(data2$slope,     xlab="Slope (%)",         cex.lab=1.5,pch=16,group=data2$basin.stream)
boxplot(data2$bf,        xlab="Bank full (m)",     cex.lab=1.5,pch=16,group=data2$basin.stream)
boxplot(data2$pebble,    xlab="Pebble Median (mm)", cex.lab=1.5,pch=16,group=data2$basin.stream)

# Variables that change over time
interaction.plot(data1$season.yr,data1$stream,data1$width,xlab="Season", ylab="Wetted Width (m)"
,cex.lab=1.5,col="black",lwd=2.5,legend=F)
interaction.plot(data1$season.yr,data1$stream,data1$depth,xlab="Season", ylab="Depth (m)"
,cex.lab=1.5,col="black",lwd=2.5,legend=F)
interaction.plot(data1$season.yr,data1$stream,data1$discharge,xlab="Season", ylab="Discharge (L/s)"
,cex.lab=1.5,col="black",lwd=2.5,legend=F)

interaction.plot(data1$season.yr,data1$stream,data1$carbon,xlab="Season", ylab="Carbon (DOC mg C/L)"
,ylim=c(0,14),cex.lab=1.5,col="black",lwd=2.5,legend=F)
interaction.plot(data1$season.yr,data1$stream,data1$phosphate,xlab="Season", ylab="Phosphate (SRP mg P/L)"
,ylim=c(0,.06),cex.lab=1.5,col="black",lwd=2.5,legend=F)
interaction.plot(data1nitrate_omit$season.yr,data1nitrate_omit$stream,data1nitrate_omit$din,xlab="Season", ylab="Nitrogen (DIN mg N/L)*"
,ylim=c(0,.017),cex.lab=1.5,col="black",lwd=2.5,legend=F) # added.016478 to ammonia and .0012613 to nitrate, dropped outliers

interaction.plot(data1$season.yr,data1$stream,data1$canopy,xlab="Season", ylab="Canopy (% open)"
,ylim=c(0,80),cex.lab=1.5,col="black",lwd=2.5,legend=F)
interaction.plot(data1$season.yr,data1$stream,data1$par.integrative,xlab="Season", ylab="PAR (integrative mol/m2/d)"
,ylim=c(0,3.5),cex.lab=1.5,col="black",lwd=2.5,legend=F) #integrative seemed more informative than mean or max PAR
interaction.plot(data1$season.yr,data1$stream,data1$temp.min,xlab="Season", ylab="Minimum Temp (deg C)"
,ylim=c(0,11),cex.lab=1.5,col="black",lwd=2.5,legend=F) # stream daily minimum temp seemed a better predictor than mean or max temp

interaction.plot(data1omit$season.yr,data1omit$stream,data1omit$gpp,xlab="Season", ylab="GPP (g O2/m2/d)"
,ylim=c(0,.15),cex.lab=1.5,col="black",lwd=2.5,legend=F) # total autotrophic production
interaction.plot(data1omit$season.yr,data1omit$stream,abs(data1omit$er),xlab="Season", ylab="ER (g O2/m2/d)*"
,ylim=c(0,5),cex.lab=1.5,col="black",lwd=2.5,legend=F) # total auto+heterotrophic respiration as negative flux (here as absolute value)
interaction.plot(data1omit$season.yr,data1omit$stream,data1omit$pr.ratio,xlab="Season", ylab="P/R ratio"
,ylim=c(0,.035) ,cex.lab=1.5,col="black",lwd=2.5,legend=F)

interaction.plot(data1cut$season.yr,data1cut$stream,data1cut$sculp.mass.m,xlab="Season", ylab="Sculpin Biomass (g/m)*"
,ylim=c(0,8),cex.lab=1.5,col="black",lwd=2.5,legend=F) # sculpin biomass was additive and no proper estimate was done
interaction.plot(data1cut$season.yr,data1cut$stream,data1cut$cut.mass.m,xlab="Season", ylab="Trout Biomass (g/m)*"
,ylim=c(0,14),cex.lab=1.5,col="black",lwd=2.5,legend=F) # almost entirely CUTT with EBT in Jack summer '18 mixed in

boxplot(data1cut$season.yr,data1cut$cut.mass.m,xlab="Season", ylab="Trout Biomass (g/m)*",ylim=c(0,14),cex.lab=1.5,pch=16) # almost entirely CUTT with EBT in Jack summer '18 mixed in
legend("topleft", legend=c("Paired t-test", "P= 0.30"),bty="n")
t.test(data2$cut.mass.m.sum17,data2$cut.mass.m.sum18,paired=TRUE)

# modeling
# GPP modeling
lmGPP2=lm(t.gpp~pebble+from.north,data=data1omit)
summary(lmGPP2)
lm(formula=lmGPP2)

plot(GPP2resid ~ data1omit$pebble, xlab = "Pebble Median (mm)", ylab = "lm GPP Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)
plot(GPP2resid ~ data1omit$from.north, xlab = "Aspect (Deg. South Facing)", ylab = "lm GPP Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)

plot(data1omit$pebble,data1omit$t.gpp, xlab = "Pebble Median (mm)", ylab = "GPP Transformed",cex.lab=1.5,pch=16)
abline(lm(data1omit$t.gpp~data1omit$pebble))
summary(lm(data1omit$t.gpp~data1omit$pebble))
legend("topright", legend=c("R2= 0.17", "P= 0.022"),bty="n")


plot(data1omit$from.north,data1omit$t.gpp,xlab = "Aspect (Deg. South Facing)", ylab = "GPP Transformed",cex.lab=1.5,pch=16)
abline(lm(data1omit$t.gpp~data1omit$from.north))
summary(lm(data1omit$t.gpp~data1omit$from.north))
legend("topright", legend=c("R2= 0.12", "P= 0.048"),bty="n")

# ER Modeling
lmER2=lm(t.er~depth+from.north,data=data1omit)
summary(lmER2)
lm(formula=lmER2)

plot(ER2resid ~ data1omit$depth, xlab = "Depth (m)", ylab = "lm ER Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)
plot(ER2resid ~ data1omit$from.north, xlab = "Aspect (Deg. South Facing)", ylab = "lm ER Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)

plot(data1omit$depth,data1omit$t.er,xlab = "Depth (m)", ylab = "ER Transformed",cex.lab=1.5,pch=16)
abline(lm(data1omit$t.er~data1omit$depth))
summary(lm(data1omit$t.er~data1omit$depth))
legend("topright", legend=c("R2= 0.59", "P= 3e-6"),bty="n")

plot(data1omit$from.north,data1omit$t.er,xlab = "Aspect (Deg. South Facing)", ylab = "ER Transformed",cex.lab=1.5,pch=16)
abline(lm(data1omit$t.er~data1omit$from.north))
summary(lm(data1omit$t.er~data1omit$from.north))
legend("topright", legend=c("R2= 0.30", "P= 0.002"),bty="n")

# CUTT Modeling
lmCUT2=lm(t.cut.mass.m~width+temp.min,data=data1cut)
summary(lmCUT2)
lm(formula=lmCUT2)
 # why wont these work? they did earlier...
plot(CUT2resid ~ data1cut$width, xlab = "Wetted width (m)", ylab = "lm CUTT Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)
plot(CUT2resid ~ data1cut$temp.min, xlab = "Min Temp (Deg. C)", ylab = "lm CUTT Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)

plot(data1cut$width,data1cut$t.cut.mass.m,xlab = "Wetted Width (m)", ylab = "CUTT Biomass Transformed",cex.lab=1.5,pch=16)
abline(lm(data1cut$t.cut.mass.m~data1cut$width))
summary(lm(data1cut$t.cut.mass.m~data1cut$width))
legend("topright", legend=c("R2= 0.26", "P= 0.013"),bty="n")

plot(data1cut$temp.min,data1cut$t.cut.mass.m,xlab = "Min Temp (Deg. C)", ylab = "CUTT Biomass Transformed",cex.lab=1.5,pch=16)
abline(lm(data1cut$t.cut.mass.m~data1cut$temp.min))
summary(lm(data1cut$t.cut.mass.m~data1cut$temp.min))
legend("topright", legend=c("R2= 0.19", "P= 0.033"),bty="n")


# Additional plots
plot(abs(data1omit$er)~data1omit$gpp,xlab = "GPP (g O2/m2/d)", ylab = "ER (ABS g O2/m2/d)",cex.lab=1.5,pch=16)
abline(lm(abs(data1omit$er)~data1omit$gpp))
summary(lm(abs(data1omit$er)~data1omit$gpp))
legend("topright", legend=c("R2= -0.0035", "P= 0.35"),bty="n")

plot(data1$cut.mass.m~data1$gpp,xlab = "GPP (g O2/m2/d)", ylab = "CUTT Biomass (g/m)",cex.lab=1.5,pch=16)
abline(lm(data1$cut.mass.m~data1$gpp))
summary(lm(data1$cut.mass.m~data1$gpp))
legend("topright", legend=c("R2= -0.045", "P= 0.64"),bty="n")

plot(data1$carbon~data1$gpp,xlab = "GPP (g O2/m2/d)", ylab = "Carbon (mg C/L)",cex.lab=1.5,pch=16)
abline(lm(data1$carbon~data1$gpp))
summary(lm(data1$carbon~data1$gpp))
legend("topright", legend=c("R2= -0.035", "P= 0.70"),bty="n")

plot(data1nitrate_omit$din~data1nitrate_omit$gpp,xlab = "GPP (g O2/m2/d)", ylab = "Nitrogen (DIN mg N/L)*",cex.lab=1.5,pch=16)
abline(lm(data1nitrate_omit$din~data1nitrate_omit$gpp))
summary(lm(data1nitrate_omit$din~data1nitrate_omit$gpp))
legend("topright", legend=c("R2= 0.11", "P= 0.066"),bty="n")

plot(data1$phosphate~data1$gpp,xlab = "GPP (g O2/m2/d)", ylab = "Phosphate (mg P/L)",cex.lab=1.5,pch=16)
abline(lm(data1$phosphate~data1$gpp))
summary(lm(data1$phosphate~data1$gpp))
legend("topright", legend=c("R2= 0.12", "P= 0.047"),bty="n")

plot(abs(data1$er)~data1$cut.mass.m,xlab = "CUTT Biomass (g/m)", ylab = "ER (ABS g O2/m2/d)",cex.lab=1.5,pch=16)
abline(lm(abs(data1$er)~data1$cut.mass.m))
summary((lm(abs(data1$er)~data1$cut.mass.m)))
legend("topright", legend=c("R2= -0.0027", "P= 0.34"),bty="n")

