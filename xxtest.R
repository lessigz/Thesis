setwd("N:/Thesis")
dir()
names(data1)
head(data1)
data1

data1=read.csv("all_trans_short_r.csv",header=TRUE) #load data in short format with NA's
data1metab=read.csv("trans_short_metab_r.csv",header=TRUE)
data1omit=na.omit(data1metab)

data1nitrate=read.csv("trans_short_metab_nitrate_outlier_r.csv",header=TRUE)
data1nitrate_omit=na.omit(data1nitrate)

data1cut=read.csv("trans_short_cut_r.csv",header=TRUE)

data2=read.csv("all_trans_long_r.csv",header=TRUE) #load data in long format

#install packages
install.packages("nlme")
install.packages("lme4")
install.packages("dplyr")
install.packages("nortest")


library(nlme)
library(lme4)
library(dplyr)
library(nortest)
source("AEDgraphingfunctions.R") #we used this alot in advanced biostats

######################################################################################################  
#variables and units

#basin    (Swauk, Teanaway or Taneum)
#stream   (BLue, HOvey, Hurley, IRon, SWauk, JAck, MIller, STandup, FIrst, FRost)
#season	  (Fall or Summer)
#yr       (17 or 18)
#season.yr (season+year)	
#basin.stream (basin+stream)
#basin.season.yr (basin+season+year)
#basin.stream.season.yr (basin+stream+season+year)
# n w	    (GPS coordinates)   
#elev	    (stream site elevation in meters, this variable repeats for each sampling period) 
#aspect   (true stream aspect in degrees, this variable repeats for each sampling period)	
#from.north	(stream aspect in degrees from north, this variable repeats for each sampling period)
#slope	(stream grade in %, this variable repeats for each sampling period)	
#bf	      (stream bank full in meters, this variable repeats for each sampling period)
#pebble	 (median pebble width in mm, this variable repeats for each sampling period)
#width	 (stream wetted width in meters)
#depth	  (mean stream depth in meters)	
#discharge (stream discharge in L/s)
#carbon	  (mg C/L)
#ammonia  (mg N/L)
#nitrate	 (mg N/L)
#phosphate (mg P/L)
#canopy	   (% open)
#par.integrative  (additive PAR for entire day in mol/m2/d)
#par.mean	 (mean PAR for entire day in umol/m2/s)
#par.max	 (maximum PAR in umol/m2/s)
#temp.max	 (stream maximum temp in deg C)
#temp.min	 (stream minimum temp in deg C)
#temp.mean	(stream mean temp in deg C)	
#k600     (estimated gas exchange constant in 1/d)
#gpp      (total autotrophic production in g O2/m2/d)
#er       (total ecosystem respiration in g O2/m2/d)
#pr.ratio    (GPP/ER)
#cut.capt    (probability of cutthroat capture)
#cut.pop      (estimate of number of cutthroat individuals)
#cut.pop.se    (cutthroat population estimate standard error)
#cut.mass     (mean mass of cutthroat individuals in g)
#cut.mass.se  (mean mass of cutthroat individuals standard error)
#cut.mass.m	  (cutthroat biomass per meter of stream in g/m)	 
#sculp.mass.m	(sculpin mass per meter of stream in g/m)
####################################################################################################


############ data exploration starting with likely PREDICTOR variables

#site specific non seasonal variables
dotchart(data2$elev,      xlab="elev (m)",          cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$aspect,    xlab="aspect (deg)",        cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$from.north,xlab="deg from.north",cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$slope,     xlab="slope (%)",         cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$bf,        xlab="bank full (m)",     cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$pebble,    xlab="pebble median (mm)", cex.lab=1.5,pch=16,group=data2$basin.stream)
#variables that are more subject to seasonal change 
op=par(mfrow=c(3,1))
dotchart(data2$width.sum17,    xlab="wetted width sum 17 (m)",  xlim=c(0,2.5),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$width.fall17,    xlab="wetted width fall 17 (m)",  xlim=c(0,2.5),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$width.sum18,    xlab="wetted width sum 18 (m)",  xlim=c(0,2.5),cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$depth.sum17,    xlab="depth sum 17 (m)",xlim=c(.03,.17),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$depth.fall17,    xlab="depth fall 17 (m)",xlim=c(.03,.17),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$depth.sum18,    xlab="depth sum 18 (m)",xlim=c(.03,.17),cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$discharge.sum17,    xlab="discharge sum 17 (L/s)",xlim=c(0,70),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$discharge.fall17,    xlab="discharge fall 17 (L/s)",xlim=c(0,70),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$discharge.sum18,    xlab="discharge sum 18 (L/s)",xlim=c(0,70),cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$carbon.sum17,    xlab="carbon sum 17 (mg C/L)",xlim=c(0,14),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$carbon.fall17,    xlab="carbon fall 17 (mg C/L)",xlim=c(0,14),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$carbon.sum18,    xlab="carbon sum 18 (mg C/L)",xlim=c(0,14),cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$ammonia.sum17,xlab="ammonia sum 17 (mg N/L)",xlim=c(-.018,-.008),  cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$ammonia.fall17,xlab="ammonia fall 17 (mg N/L)",xlim=c(-.018,-.008),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$ammonia.sum18,xlab="ammonia sum 18 (mg N/L)",xlim=c(-.018,-.008),  cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$nitrate.sum17,xlab="nitrate sum 17 (mg N/L)",xlim=c(-.002,.3),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$nitrate.fall17,xlab="nitrate fall 17 (mg N/L)",xlim=c(-.002,.3),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$nitrate.sum18,xlab="nitrate sum 18 (mg N/L)",xlim=c(-.002,.3),  cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(1,1))
dotchart(data1$nitrate,xlab="nitrate (mg N/L)",xlim=c(-.002,.3),  cex.lab=1.5,pch=16,group=data1$basin.stream.season.yr)
#nitrate transformation attempt
data1$t.nitrate=log10((10*(1+data1$nitrate)))
op=par(mfrow=c(1,1))
dotchart(data1$t.nitrate,xlab="nitrate transformed",cex.lab=1.5,pch=16,group=data1$basin.stream.season.yr)
op=par(mfrow=c(3,1))
dotchart(data2$phosphate.sum17,xlab="phosphate sum 17 (mg P/L)",xlim=c(0,.06)   ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$phosphate.fall17,xlab="phosphate fall 17 (mg P/L)",xlim=c(0,.06) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$phosphate.sum18,xlab="phosphate sum 18 (mg P/L)"  ,xlim=c(0,.06) ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$canopy.sum17,xlab="canopy sum 17 (%open)",xlim=c(5,80)   ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$canopy.fall17,xlab="canopy fall 17 (%open)",xlim=c(5,80) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$canopy.sum18,xlab="canopy sum 18 (%open)" ,xlim=c(5,80)  ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$par.integrative.sum17,xlab="integrative PAR sum 17 (mol/m2/d)" ,xlim=c(0,3.5)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.integrative.fall17,xlab="integrative PAR fall 17 (mol/m2/d)",xlim=c(0,3.5) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.integrative.sum18,xlab="integrative PAR sum 18 (mol/m2/d)"  ,xlim=c(0,3.5) ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$par.mean.sum17,xlab="mean PAR sum 17 (umol/m2/s)" ,xlim=c(0,12000)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.mean.fall17,xlab="mean PAR fall 17 (umol/m2/s)",xlim=c(0,12000) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.mean.sum18,xlab="mean PAR sum 18 (umol/m2/s)" ,xlim=c(0,12000)  ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$par.max.sum17,xlab="max PAR sum 17 (umol/m2/s)" ,xlim=c(0,70000)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.max.fall17,xlab="max PAR fall 17 (umol/m2/s)",xlim=c(0,70000) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.max.sum18,xlab="max PAR sum 18 (umol/m2/s)" ,xlim=c(0,70000)  ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$temp.max.sum17,xlab="max temp sum 17 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.max.fall17,xlab="max temp fall 17 (deg C)",xlim=c(0,15) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.max.sum18,xlab="max temp sum 18 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$temp.min.sum17,xlab="min temp sum 17 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.min.fall17,xlab="min temp fall 17 (deg C)",xlim=c(0,15) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.min.sum18,xlab="min temp sum 18 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(3,1))
dotchart(data2$temp.mean.sum17,xlab="mean temp sum 17 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.mean.fall17,xlab="mean temp fall 17 (deg C)",xlim=c(0,15) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.mean.sum18,xlab="mean temp sum 18 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
#I selected believable k600's from my data and replaced what appeared to be erroneous ones, some values were still left out
op=par(mfrow=c(3,1))
dotchart(data2$k600.sum17,xlab="k600 sum 17 (1/d)" ,xlim=c(20,90)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$k600.fall17,xlab="k600 fall 17 (1/d)",xlim=c(20,90) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$k600.sum18,xlab="k600 sum 18 (1/d)" ,xlim=c(20,90)  ,cex.lab=1.5,pch=16,group=data2$basin)
#only 2 fish sampling times instead of 3 for most of the rest of the variables
op=par(mfrow=c(2,1))
dotchart(data2$sculp.mass.m.sum17,xlab="sculpin biomass sum 17 (g/m)"  ,xlim=c(0,8),cex.lab=1,pch=16,group=data2$basin)
dotchart(data2$sculp.mass.m.sum18,xlab="sculpin biomass sum 18 (g/m)",xlim=c(0,8)   ,cex.lab=1,pch=16,group=data2$basin)
#only 2 fish sampling times instead of 3 for most of the rest of the variables
op=par(mfrow=c(2,1))
dotchart(data2$cut.capt.sum17,xlab="cutthroat capture probability sum 17"  ,xlim=c(0,1) ,cex.lab=1,pch=16,group=data2$basin)
dotchart(data2$cut.capt.sum18,xlab="cutthroat capture probability sum 18" ,xlim=c(0,1)  ,cex.lab=1,pch=16,group=data2$basin)

############ exploration of likely RESPONSE variables

#some values left out as NA's
op=par(mfrow=c(3,1))
dotchart(data2$gpp.sum17,xlab="gpp sum 17 (g 02/m2/d)" ,xlim=c(0,.14)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$gpp.fall17,xlab="gpp fall 17 (g 02/m2/d)" ,xlim=c(0,.14),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$gpp.sum18,xlab="gpp sum 18 (g 02/m2/d)" ,xlim=c(0,.14)  ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(1,1))
dotchart(data1$gpp,xlab="all gpp (g 02/m2/d)",xlim=c(0,.14),cex.lab=1.5,pch=16,group=data1$basin)
hist(data1$gpp,xlab="all gpp (g 02/m2/d)",cex.lab=1.5)
#some values left out as NA's
op=par(mfrow=c(3,1))
dotchart(data2$er.sum17,xlab="ER sum 17 (g 02/m2/d)" ,xlim=c(-10,0)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$er.fall17,xlab="ER fall 17 (g 02/m2/d)",xlim=c(-10,0) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$er.sum18,xlab="ER sum 18 (g 02/m2/d)" ,xlim=c(-10,0)  ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(1,1))
dotchart(data1$er,xlab="all ER (g 02/m2/d)",xlim=c(-10,0),cex.lab=1.5,pch=16,group=data1$basin)
hist(data1$er,xlab="all ER (g 02/m2/d)",cex.lab=1.5)
#some values left out as NA's
# really just divide +P by -R ?
op=par(mfrow=c(3,1))
dotchart(data2$pr.ratio.sum17,xlab="P/R sum 17"  ,xlim=c(-.035,0) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$pr.ratio.fall17,xlab="P/R fall 17",xlim=c(-.035,0) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$pr.ratio.sum18,xlab="P/R sum 18" ,xlim=c(-.035,0)  ,cex.lab=1.5,pch=16,group=data2$basin)
op=par(mfrow=c(1,1))
dotchart(data1$pr.ratio,xlab="all P/R (ratio)",xlim=c(-.035,0),cex.lab=1.5,pch=16,group=data1$basin)
hist(data1$pr.ratio,xlab="all P/R (ratio)",cex.lab=1.5)
#only 2 fish sampling times instead of 3 for most of the rest of the variables
op=par(mfrow=c(2,1))
dotchart(data2$cut.mass.m.sum17,xlab="cutthroat biomass sum 17 (g/m)"  ,xlim=c(0,14),cex.lab=1,pch=16,group=data2$basin)
dotchart(data2$cut.mass.m.sum18,xlab="cutthroat biomass sum 18 (g/m)",xlim=c(0,14)   ,cex.lab=1,pch=16,group=data2$basin)
op=par(mfrow=c(1,1))
dotchart(data1$cut.mass.m,xlab="cutthroat biomass (g/m)",xlim=c(0,14),cex.lab=1.5,pch=16,group=data1$basin)
hist(data1$cut.mass.m,xlab="cutthroat biomass (g/m)",cex.lab=1.5)

#################### Transformations of suggested RESPONSE variables
# normality tests
library(nortest)
ad.test(data1$cut.mass.m)
ad.test(data1$gpp.model)
ad.test(data1$gpp.my)
ad.test(data1$er.lit)
ad.test(data1$phosphate)
ad.test(data1$din)
ad.test(data1$carbon)



########### Pairplots of variables

# pairplot of transformed RESPONSE variables
Z=cbind(data1$t.gpp,data1$t.er,data1$t.cut.mass.m)
colnames(Z)<-c("GPP","ER","cutt")
pairs(Z[,1:3], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)

# pairplot most
Z=cbind(data1$basin,data1$elev,data1$from.north,data1$slope,data1$bf,data1$pebble,data1$width,data1$depth,data1$discharge,data1$carbon,data1$ammonia,data1$nitrate,data1$phosphate,data1$canopy,data1$par.integrative,data1$par.mean,data1$par.max,data1$temp.max,data1$temp.min,data1$temp.mean,data1$k600,data1$sculp.mass.m,data1$t.gpp,data1$t.er,data1$pr.ratio,data1$t.cut.mass.m)
colnames(Z)<-c("basin",  "elev",        "deg N",    "slope",    "bf",   "pebble",   "width",      "depth",    "discharge",    "carbon",   "ammonia",    "nitrate",    "phosphate",    "canopy",     "PAR int",            "PAR mean",     "PAR max",    "temp max",   "temp min",   "temp mean",     "k600",    "sculp",            "GPP",      "ER",     "P/R",            "cutt")
pairs(Z[,1:25], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)

# pairplot trimmed down
Z=cbind(data1$elev,data1$from.north,data1$width,data1$carbon,data1$ammonia,data1$nitrate,data1$phosphate,data1$canopy,data1$temp.min,data1$sculp.mass.m,data1$t.gpp,data1$t.er,data1$pr.ratio,data1$t.cut.mass.m)
colnames(Z)<-c("elev",    "deg N",      "width",    "carbon",     "ammonia",    "nitrate",    "phosphate",    "canopy",     "temp min",   "sculp",            "GPP",      "ER",     "P/R",            "cutt")
pairs(Z[,1:14], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)

# pairplot GPP
Z=cbind(data1$elev,data1$from.north,data1$slope,data1$bf,data1$pebble,data1$width,data1$depth,data1$discharge,data1$carbon,data1$canopy,data1$par.integrative,data1$par.mean,data1$par.max,data1$temp.max,data1$temp.min,data1$temp.mean,data1$t.gpp)
colnames(Z)<-c("elev",        "deg N",    "slope",    "bf",   "pebble",   "width",      "depth",    "discharge",    "carbon",    "canopy",     "PAR int",        "PAR mean",     "PAR max",    "temp max",   "temp min",   "temp mean", "GPP")
pairs(Z[,1:17], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)

# pairplot ER
Z=cbind(data1$basin,data1$from.north,data1$width,data1$depth,data1$discharge,data1$phosphate,data1$sculp.mass.m,data1$t.er)
colnames(Z)<-c("basin","deg N",      "width",    "depth",    "discharge",    "phosphate",    "sculp",           "ER")
pairs(Z[,1:8], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)

# pairplot Cutthroat
Z=cbind(data1$basin,data1$elev,data1$width,data1$depth,data1$discharge,data1$nitrate,data1$phosphate,data1$temp.min,data1$temp.mean,data1$sculp.mass.m,data1$t.cut.mass.m)
colnames(Z)<-c("basin", "elev",     "width",  "depth",    "discharge",    "nitrate",    "phosphate",     "temp min",   "temp mean",     "sculp",            "cutt")
pairs(Z[,1:11], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)


##################### model selections

# GPP transformation
dotchart(data1$gpp.lit)
ad.test(data1$gpp.lit)
data1$t.gpp.lit=log10(1+data1$gpp.lit)
ad.test(data1$t.gpp.lit)
hist(data1$t.gpp.lit,xlab="transfromed gpp",cex.lab=1.5)
dotchart(data1$t.gpp.lit,xlab="transfromed gpp",cex.lab=1.5,pch=16)
# GPP LM models
lmGPP=lm(gpp.lit~basin,na.action=na.omit,data=data1)          #p .08   r2 .13 
lmGPP=lm(gpp.lit~stream,na.action=na.omit,data=data1)         #p .05   r2 .4  
lmGPP=lm(gpp.lit~yr,na.action=na.omit,data=data1)             #p .3    r2 .004
lmGPP=lm(gpp.lit~elev,na.action=na.omit,data=data1)           #p .09   r2 .08  
lmGPP=lm(gpp.lit~from.north,na.action=na.omit,data=data1)     #p .8    r2 -.04
lmGPP=lm(gpp.lit~slope,na.action=na.omit,data=data1)          #p .08   r2 .09 
lmGPP=lm(gpp.lit~bf,na.action=na.omit,data=data1)             #p .9    r2 -.04
lmGPP=lm(gpp.lit~pebble,na.action=na.omit,data=data1)         #p .0001 r2 .45
lmGPP=lm(gpp.lit~width,na.action=na.omit,data=data1)          #p .15   r2 .05
lmGPP=lm(gpp.lit~depth,na.action=na.omit,data=data1)          #p .04   r2 .1
lmGPP=lm(gpp.lit~velocity.mean,na.action=na.omit,data=data1)  #p .2    r2 .03
lmGPP=lm(gpp.lit~discharge,na.action=na.omit,data=data1)      #p .9    r2 -.04
lmGPP=lm(gpp.lit~carbon,na.action=na.omit,data=data1)         #p .9    r2 -.04
lmGPP=lm(gpp.lit~ammonia,na.action=na.omit,data=data1)        #p .1    r2 .06
lmGPP=lm(gpp.lit~nitrate,na.action=na.omit,data=data1)        #p .4    r2 -.01
lmGPP=lm(gpp.lit~din,na.action=na.omit,data=data1)            #p .4    r2 -.005
lmGPP=lm(gpp.lit~phosphate,na.action=na.omit,data=data1)      #p .16   r2  .04
lmGPP=lm(gpp.lit~cn.ratio,na.action=na.omit,data=data1)       #p .5    r2  -.02
lmGPP=lm(gpp.lit~canopy,na.action=na.omit,data=data1)         #p .1    r2  .07
lmGPP=lm(gpp.lit~par.integrative,na.action=na.omit,data=data1)#p .26   r2  .01
lmGPP=lm(gpp.lit~par.mean,na.action=na.omit,data=data1)       #p .26   r2  .01
lmGPP=lm(gpp.lit~par.max,na.action=na.omit,data=data1)        #p .8    r2  -.04
lmGPP=lm(gpp.lit~temp.max,na.action=na.omit,data=data1)       #p .07   r2  .1  
lmGPP=lm(gpp.lit~temp.min,na.action=na.omit,data=data1)       #p .07   r2  .09 
lmGPP=lm(gpp.lit~temp.mean,na.action=na.omit,data=data1)      #p .08   r2  .08 
lmGPP=lm(gpp.lit~cut.capt,na.action=na.omit,data=data1)       #p .7    r2 -.05
lmGPP=lm(gpp.lit~cut.pop,na.action=na.omit,data=data1)        #p .5    r2 -.03
lmGPP=lm(gpp.lit~cut.mass,na.action=na.omit,data=data1)       #p .2    r2 .04
lmGPP=lm(gpp.lit~cut.mass.m,na.action=na.omit,data=data1)     #p .6    r2 -.03
lmGPP=lm(gpp.lit~sculp.mass.m,na.action=na.omit,data=data1)   #p .02   r2 .2
summary(lmGPP)
#pairplot
Z=cbind(data1$basin,data1$stream,data1$elev,data1$slope,data1$pebble,data1$depth,data1$temp.max,data1$gpp.lit)
colnames(Z)<-c("basin", "stream",    "elev",    "slope",    "pebble",   "depth",   "temp.max",    "t.gpp.lit")
pairs(Z[,1:8], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)

ad.test(residuals(lmGPP))
hist(rstandard(lmGPP)) 
qqnorm(rstandard(lmGPP)) 

lmGPP1=lm(gpp.lit~pebble+depth+temp.max,data=data1)
lmGPP2=lm(gpp.lit~basin+stream+elev+slope+pebble+depth+temp.max,data=data1)
summary(lmGPP1)
lm(formula=lmGPP1)
anova(lmGPP1)
drop1(lmGPP1,test="F")
step(lmGPP1)
plot(lmGPP1)

plot(rstandard(lmGPP1)~(data1$gpp.lit[!is.na(data1$gpp.lit)]))
abline(0,0)
#transform explanatory
dotchart(data1$pebble)
data1$t.pebble=log10(1+data1$pebble)
dotchart(data1$t.pebble)

dotchart(data1$depth)
data1$t.depth=log10(1+data1$depth)
dotchart(data1$t.depth)

dotchart(data1$temp.max)
data1$t.temp.max=(1+data1$temp.max)^2
dotchart(data1$t.temp.max)
ad.test(data1$t.temp.max)
###############################################################################
#using glm to analyze variables
M.1=gls(t.gpp.lit~pebble+temp.max,na.action=na.omit, 
        data=data1, method="ML") #base model
summary(M.1)
M.2=lme(t.gpp.lit~pebble+temp.max, na.action=na.omit, 
        random = ~1|stream, data=data1, method="ML") #adds random effect

M.3=lme(t.gpp.lit~pebble+temp.max, na.action=na.omit, 
        random = ~1|basin.stream, data=data1, method="ML") #adds nesting

anova(M.1, M.2, M.3)

#Analyze residuals
x.gpp<-data1$t.gpp.lit[!is.na(data1$t.gpp.lit)]#removes na values from column
# M.1 residuals
E.1.gpp<-residuals(M.1,type="normalized")
qqnorm(E.1.gpp)
qqline(E.1.gpp)
ad.test(E.1.gpp)
plot(M.1) 
plot(x.gpp, E.1.gpp)
# M.2 residuals
E.2.gpp<-residuals(M.2,type="normalized")
qqnorm(E.2.gpp)
qqline(E.2.gpp)
ad.test(E.2.gpp)
plot(M.2) 
plot(x.gpp, E.2.gpp)
# M.3 residuals
E.3.gpp<-residuals(M.3,type="normalized")
qqnorm(E.3.gpp)
qqline(E.3.gpp)
ad.test(E.3.gpp)
plot(M.3) 
plot(x.gpp, E.3.gpp)



#residuals somewhat linear? try alternate variance structures?

#####################################################
#Analyze models with alternate variance structures
#####################################################

#base model from above
M.1=gls(t.gpp.lit~pebble+temp.max, na.action=na.omit, 
        data=data1) #run with method="REML" default for comparison

#alternate variance structures
vf1=varIdent(form = ~1|stream)
vf2=varPower(form = ~fitted(.))
vf3=varExp(form = ~fitted(.))
vf4=varConstPower(form = ~fitted(.))
#vf5=varPower(form = ~fitted (.)|site)
#vf6=varExp(form = ~fitted(.)|site)
#vf7=varConstPower(form = ~fitted(.)|site)
#vf8=varIdent(form = ~1|f.sample.event)

#alternate models
M1.1<-gls(t.gpp.lit~pebble+temp.max, na.action=na.omit, 
          data=data1, weights=vf1)

M1.2<-gls(t.gpp.lit~pebble+temp.max, na.action=na.omit, 
          data=data1, weights=vf2)

M1.3<-gls(t.gpp.lit~pebble+temp.max, na.action=na.omit, 
          data=data1, weights=vf3)

M1.4<-gls(t.gpp.lit~pebble+temp.max, na.action=na.omit, 
          data=data1, weights=vf4)

M1.5<-gls(t.gpp.lit~pebble+temp.max, na.action=na.omit, 
          data=data1,  weights=varComb(vf1,vf2))

anova(M.1,M1.1,M1.2,M1.3,M1.4,M1.5)
anova(M.1,M1.1)
#M1.3, expoential variance of fit

#Analyze alternate variance structure model residuals

E1.1.gpp<-residuals(M1.1,type="normalized")
qqnorm(E1.1.gpp)
qqline(E1.1.gpp)
ad.test(E1.1.gpp)
plot(M1.1) 
plot(x.gpp, E1.1.gpp)
abline(0,0)
 
##################################################################################
# GPP model validation
op=par(mfrow = c(2, 2)) 
plot(lmGPP1)
op=par(mfrow = c(2, 2)) 
GPP1resid=rstandard(lmGPP1) 

hist(GPP2resid) 
qqnorm(GPP2resid) 
plot(GPP2resid ~ data1omit$pebble, xlab = "pebble", ylab = "Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)
plot(GPP2resid ~ data1omit$from.north, xlab = "aspect from N", ylab = "Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)

#   plot(x,y) but abline(lm(y~x)) !!!!
# GPP model interpretation
plot(data1$pebble,data1$t.gpp.lit,cex.lab=1.5,pch=16)
abline(lm(data1$t.gpp~data1$pebble))
summary(lm(data1$t.gpp~data1$pebble))
legend("topright", legend=c("R2= 0.45", "P= 1e-4"),bty="n")

plot(data1$temp.max,data1$t.gpp.lit,cex.lab=1.5,pch=16)
abline(lm(data1$t.gpp.lit~data1$temp.max))
summary(lm(data1$t.gpp.lit~data1$temp.max))
legend("topright", legend=c("R2= 0.10", "P=0.07"),bty="n")



# ER transformation
dotchart(data1$er.lit,xlab="ER",cex.lab=1.5,pch=16)
hist(data1$er.lit,xlab="ER",cex.lab=1.5,pch=16)
ad.test(data1$er.lit)
data1$t.er.lit=log10(1+abs(data1$er.lit))
ad.test(data1$t.er.lit)
hist(data1$t.er.lit,xlab="transfromed ER",cex.lab=1.5)
dotchart(data1$t.er.lit,xlab="transfromed ER",cex.lab=1.5,pch=16)

# ER model selsection
lmER=lm(er.lit~basin,na.action=na.omit,data=data1)          #p .02   r2 .2
lmER=lm(er.lit~stream,na.action=na.omit,data=data1)         #p .3e-6 r2 .8
lmER=lm(er.lit~yr,na.action=na.omit,data=data1)             #p .6    r2 -.03
lmER=lm(er.lit~elev,na.action=na.omit,data=data1)           #p .24   r2 .01
lmER=lm(er.lit~from.north,na.action=na.omit,data=data1)     #p .9    r2 -.04
lmER=lm(er.lit~slope,na.action=na.omit,data=data1)          #p 2e-6  r2 .6 
lmER=lm(er.lit~bf,na.action=na.omit,data=data1)             #p .05   r2 .1
lmER=lm(er.lit~pebble,na.action=na.omit,data=data1)         #p .05   r2 .1  
lmER=lm(er.lit~width,na.action=na.omit,data=data1)          #p .09   r2 .07
lmER=lm(er.lit~depth,na.action=na.omit,data=data1)          #p .001  r2 .3  
lmER=lm(er.lit~velocity.mean,na.action=na.omit,data=data1)  #p .9    r2 -.04
lmER=lm(er.lit~discharge,na.action=na.omit,data=data1)      #p .04   r2 .1
lmER=lm(er.lit~carbon,na.action=na.omit,data=data1)         #p .8    r2 -.04
lmER=lm(er.lit~ammonia,na.action=na.omit,data=data1)        #p .6    r2 -.03
lmER=lm(er.lit~nitrate,na.action=na.omit,data=data1)        #p .6    r2 -.03
lmER=lm(er.lit~din,na.action=na.omit,data=data1)            #p .6    r2 -.03
lmER=lm(er.lit~phosphate,na.action=na.omit,data=data1)      #p .5    r2 -.03
lmER=lm(er.lit~cn.ratio,na.action=na.omit,data=data1)       #p .7    r2 -.04
lmER=lm(er.lit~canopy,na.action=na.omit,data=data1)         #p .6    r2 -.03
lmER=lm(er.lit~par.integrative,na.action=na.omit,data=data1)#p .4    r2 -.01
lmER=lm(er.lit~par.mean,na.action=na.omit,data=data1)       #p .4    r2 -.01
lmER=lm(er.lit~par.max,na.action=na.omit,data=data1)        #p .8    r2 -.04
lmER=lm(er.lit~temp.max,na.action=na.omit,data=data1)       #p .2    r2 .02
lmER=lm(er.lit~temp.min,na.action=na.omit,data=data1)       #p .3    r2 .004 
lmER=lm(er.lit~temp.mean,na.action=na.omit,data=data1)      #p .3    r2 .008 
lmER=lm(er.lit~pr.lit,na.action=na.omit,data=data1)         #p .8    r2 -.04
lmER=lm(er.lit~cut.capt,na.action=na.omit,data=data1)       #p .9    r2 -.05
lmER=lm(er.lit~cut.pop,na.action=na.omit,data=data1)        #p .6    r2 -.04
lmER=lm(er.lit~cut.mass,na.action=na.omit,data=data1)       #p .7    r2 -.04
lmER=lm(er.lit~cut.mass.m,na.action=na.omit,data=data1)     #p .9    r2 -.05
lmER=lm(er.lit~sculp.mass.m,na.action=na.omit,data=data1)   #p .004  r2 .4
summary(lmER)
#pairplot
Z=cbind(data1$basin,data1$stream,data1$bf,data1$pebble,data1$depth,data1$discharge,data1$slope,data1$er.lit)
colnames(Z)<-c("basin", "stream",    "bf",   "pebble",    "depth",    "discharge",     "slope" , "er.lit")
pairs(Z[,1:8], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)

lmER1=lm(er.lit~basin+stream+slope+depth+pebble+discharge+bf,data=data1)
lmER2=lm(er.lit~slope+depth+pebble+discharge+bf,data=data1)

summary(lmER1)
lm(formula=lmER1)
anova(lmER2)
drop1(lmER2,test="F")
step(lmER2)

# predictor transformations
dotchart(data1$slope)
data1$t.slope=log10(1+data1$slope)
dotchart(data1$t.slope)

dotchart(data1$depth)
data1$t.depth=log10(1+data1$depth)
dotchart(data1$t.depth)

dotchart(data1$bf)
data1$t.bf=log10(1+data1$bf)
dotchart(data1$t.bf)


###############################################################################
#using glm to analyze variables
Mer.1=gls(t.er.lit~depth+bf,na.action=na.omit, 
        data=data1, method="ML") #base model
Mer.2=lme(t.er.lit~depth+bf, na.action=na.omit, 
        random = ~1|stream, data=data1, method="ML") #adds random effect
Mer.3=lme(t.er.lit~depth+bf, na.action=na.omit, 
        random = ~1|basin.stream, data=data1, method="ML") #adds nesting

anova(Mer.1, Mer.2, Mer.3)

## # # Analyze residuals
x.er<-data1$t.er.lit[!is.na(data1$t.er.lit)]#removes na values from column
# # Mer.1 residuals
E.1.er<-residuals(Mer.1,type="normalized")
qqnorm(E.1.er)
qqline(E.1.er)
ad.test(E.1.er)
plot(Mer.1) 
plot(x.er, E.1.er)
# # # # M.2 residuals
E.2.er<-residuals(Mer.2,type="normalized")
qqnorm(E.2.er)
qqline(E.2.er)
ad.test(E.2.er)
plot(Mer.2) 
plot(x.er, E.2.er)
# # # M.3 residuals
E.3.er<-residuals(Mer.3,type="normalized")
qqnorm(E.3.er)
qqline(E.3.er)
ad.test(E.3.er)
plot(Mer.3) 
plot(x.er, E.3.er)
#residuals somewhat linear? try alternate variance structures?
#####################################################
#Analyze models with alternate variance structures
#####################################################
#base model from above
Mer.1=gls(t.er.lit~depth+bf, na.action=na.omit, 
         data=data1)#base model #run without "ML", method="REML" instead, default for comparison

#####alternate variance structures
vf1=varIdent(form = ~1|stream)
vf2=varPower(form = ~fitted(.))
vf3=varExp(form = ~fitted(.))
vf4=varConstPower(form = ~fitted(.))
####vf5=varPower(form = ~fitted (.)|site)
####vf6=varExp(form = ~fitted(.)|site)
####vf7=varConstPower(form = ~fitted(.)|site)
####vf8=varIdent(form = ~1|f.sample.event)
######alternate models
Mer1.1<-gls(t.er.lit~depth+bf, na.action=na.omit, 
           data=data1, weights=vf1)

Mer1.2<-gls(t.er.lit~depth+bf, na.action=na.omit, 
           data=data1, weights=vf2)

Mer1.3<-gls(t.er.lit~depth+bf, na.action=na.omit, 
           data=data1, weights=vf3)

Mer1.4<-gls(er.lit~depth+bf, na.action=na.omit, 
           data=data1, weights=vf4)

Mer1.5<-gls(t.er.lit~depth+bf, na.action=na.omit, 
           data=data1,  weights=varComb(vf1,vf2))

anova(Mer.1,Mer1.1,Mer1.2,Mer1.3,Mer1.5)
anova(Mer.1,Mer1.4)
anova(Mer.1,Mer1.1)

#M1.1
#Analyze alternate variance structure model residuals
E1.1.er<-residuals(Mer1.1,type="normalized")
qqnorm(E1.1.er)
qqline(E1.1.er)
ad.test(E1.1.er)
plot(Mer1.1) 
plot(x.er,E1.1.er)
plot(data1$depth,E1.1.er) #have to use na.exclude on model for this to work
plot(data1$bf~E1.1.er)#have to use na.exclude on model for this to work


############### ER model validation
op=par(mfrow = c(2, 2)) 
plot(lmER1)
op=par(mfrow = c(2, 2)) 
ER2resid=rstandard(lmER2) 
hist(ER2resid) 
qqnorm(ER2resid) 
plot(ER2resid ~ data1omit$depth, xlab = "depth", ylab = "Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)
plot(ER2resid ~ data1omit$from.north, xlab = "depth", ylab = "Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)

# ER model interpretation
plot(data1omit$depth,data1omit$t.er,cex.lab=1.5,pch=16)
abline(lm(data1omit$t.er~data1omit$depth))
plot(data1omit$from.north,data1omit$t.er,cex.lab=1.5,pch=16)
abline(lm(data1omit$t.er~data1omit$from.north))

# CUT mass/m transformation

dotchart(data1$cut.mass.m)
hist(data1$cut.mass.m)
ad.test(data1$cut.mass.m)
data1$t.cut.mass.m=sqrt(1+data1$cut.mass.m)
ad.test(data1$t.cut.mass.m)
dotchart(data1$t.cut.mass.m)
hist(data1$t.cut.mass.m)

lmCUT=lm(cut.mass.m~basin,na.action=na.omit,data=data1)          #p .02   r2 .3  ###
lmCUT=lm(cut.mass.m~stream,na.action=na.omit,data=data1)         #p .2    r2 .2
lmCUT=lm(cut.mass.m~yr,na.action=na.omit,data=data1)             #p .4    r2 -.004
lmCUT=lm(cut.mass.m~elev,na.action=na.omit,data=data1)           #p .04   r2 .16 ###
lmCUT=lm(cut.mass.m~from.north,na.action=na.omit,data=data1)     #p .8    r2 -.05
lmCUT=lm(cut.mass.m~slope,na.action=na.omit,data=data1)          #p .2    r2 .04
lmCUT=lm(cut.mass.m~bf,na.action=na.omit,data=data1)             #p .2    r2 .05
lmCUT=lm(cut.mass.m~pebble,na.action=na.omit,data=data1)         #p .7    r2 -.05
lmCUT=lm(cut.mass.m~width,na.action=na.omit,data=data1)          #p .02   r2 .2  ###
lmCUT=lm(cut.mass.m~depth,na.action=na.omit,data=data1)          #p .18   r2 .05
lmCUT=lm(cut.mass.m~velocity.mean,na.action=na.omit,data=data1)  #p .6    r2 -.03
lmCUT=lm(cut.mass.m~discharge,na.action=na.omit,data=data1)      #p .3    r2 .02
lmCUT=lm(cut.mass.m~carbon,na.action=na.omit,data=data1)         #p .8    r2 -.05
lmCUT=lm(cut.mass.m~ammonia,na.action=na.omit,data=data1)        #p .3    r2 -.004
lmCUT=lm(cut.mass.m~nitrate,na.action=na.omit,data=data1)        #p .2    r2 .04
lmCUT=lm(cut.mass.m~din,na.action=na.omit,data=data1)            #p .2    r2 .04
lmCUT=lm(cut.mass.m~phosphate,na.action=na.omit,data=data1)      #p .4    r2 -.01
lmCUT=lm(cut.mass.m~cn.ratio,na.action=na.omit,data=data1)       #p .4    r2 -.01
lmCUT=lm(cut.mass.m~canopy,na.action=na.omit,data=data1)         #p .3    r2 .001
lmCUT=lm(cut.mass.m~par.integrative,na.action=na.omit,data=data1)#p .9    r2 -.05
lmCUT=lm(cut.mass.m~par.mean,na.action=na.omit,data=data1)       #p .9    r2 -.05
lmCUT=lm(cut.mass.m~par.max,na.action=na.omit,data=data1)        #p .16   r2 .06
lmCUT=lm(cut.mass.m~temp.max,na.action=na.omit,data=data1)       #p .2    r2 .02
lmCUT=lm(cut.mass.m~temp.min,na.action=na.omit,data=data1)       #p .04   r2 .16 ###
lmCUT=lm(cut.mass.m~temp.mean,na.action=na.omit,data=data1)      #p .07   r2 .13 ###
lmCUT=lm(cut.mass.m~gpp.lit,na.action=na.omit,data=data1)        #p .5    r2 -.03
lmCUT=lm(cut.mass.m~pr.lit,na.action=na.omit,data=data1)         #p .5    r2 -.03
lmCUT=lm(cut.mass.m~cut.capt,na.action=na.omit,data=data1)       #p .5    r2 -.03
lmCUT=lm(cut.mass.m~cut.pop,na.action=na.omit,data=data1)        #p .004  r2 .3  ###
lmCUT=lm(cut.mass.m~cut.mass,na.action=na.omit,data=data1)       #p .17   r2 .05
lmCUT=lm(cut.mass.m~sculp.mass.m,na.action=na.omit,data=data1)   #p .08   r2 .11
summary(lmCUT)
ad.test(residuals(lmGPP))

Z=cbind(data1$basin,data1$elev,data1$width,data1$temp.min,data1$temp.mean,data1$cut.pop,data1$cut.mass.m)
colnames(Z)<-c("basin", "elev",    "width",  "temp.min",      "temp.mean",    "cut pop", "cut biomass")
pairs(Z[,1:7], lower.panel=panel.smooth2,upper.panel=panel.cor,diag.panel=panel.hist)

lmCUT1=lm(cut.mass.m~basin+elev+width+temp.min,data=data1)
lmCUT1=lm(cut.mass.m~temp.min+elev+width,data=data1)

summary(lmCUT1)
lm(formula=lmCUT1)
anova(lmCUT1)
drop1(lmCUT1,test="F")
step(lmCUT1)

#predictor transformations
dotchart(data1$temp.min)
data1$t.temp.min=(1+data1$temp.min)^2
dotchart(data1$t.temp.min)

dotchart(data1$elev)
data1$t.elev=log10(1+data1$elev)
dotchart(data1$t.elev)


###############################################################################
#using glm to analyze variables
Mcut.1=gls(t.cut.mass.m~width+temp.min,na.action=na.omit, 
          data=data1, method="ML") #base model
Mcut.2=lme(t.cut.mass.m~width+temp.min, na.action=na.omit, 
          random = ~1|stream, data=data1, method="ML") #adds random effect
Mcut.3=lme(t.cut.mass.m~width+temp.min, na.action=na.omit, 
          random = ~1|basin.stream, data=data1, method="ML") #adds nesting

anova(Mcut.1, Mcut.2, Mcut.3)

## # # Analyze residuals
x.cut<-data1$t.cut.mass.m[!is.na(data1$t.cut.mass.m)]#removes na values from column
# # Mcut.1 residuals
E.1.cut<-residuals(Mcut.1,type="normalized")
qqnorm(E.1.cut)
qqline(E.1.cut)
ad.test(E.1.cut)
plot(Mcut.1) 
plot(x.cut, E.1.cut)
# # # # M.2 residuals
E.2.cut<-residuals(Mcut.2,type="normalized")
qqnorm(E.2.cut)
qqline(E.2.cut)
ad.test(E.2.cut)
plot(Mcut.2) 
plot(x.cut, E.2.cut)
# # # M.3 residuals
E.3.cut<-residuals(Mcut.3,type="normalized")
qqnorm(E.3.cut)
qqline(E.3.cut)
ad.test(E.3.cut)
plot(Mcut.3) 
plot(x.cut, E.3.cut)
#residuals somewhat linear? try alternate variance structures?
#####################################################
#Analyze models with alternate variance structures
#####################################################
#base model from above
Mcut.1=gls(t.cut.mass.m~width+temp.min, na.action=na.omit, 
          data=data1)#base model #run without "ML", method="REML" instead, default for comparison

#####alternate variance structures
vf1=varIdent(form = ~1|stream)
vf2=varPower(form = ~fitted(.))
vf3=varExp(form = ~fitted(.))
vf4=varConstPower(form = ~fitted(.))
####vf5=varPower(form = ~fitted (.)|site)
####vf6=varExp(form = ~fitted(.)|site)
####vf7=varConstPower(form = ~fitted(.)|site)
####vf8=varIdent(form = ~1|f.sample.event)
######alternate models
Mcut1.1<-gls(t.cut.mass.m~width+temp.min, na.action=na.omit, 
            data=data1, weights=vf1)

Mcut1.2<-gls(t.cut.mass.m~width+temp.min, na.action=na.omit, 
            data=data1, weights=vf2)

Mcut1.3<-gls(t.cut.mass.m~width+temp.min, na.action=na.omit, 
            data=data1, weights=vf3)

Mcut1.4<-gls(t.cut.mass.m~width+temp.min, na.action=na.omit, 
            data=data1, weights=vf4)

Mcut1.5<-gls(t.cut.mass.m~width+temp.min, na.action=na.omit, 
            data=data1,  weights=varComb(vf1,vf2))

anova(Mcut.1,Mcut1.1,Mcut1.2,Mcut1.3,Mcut1.4,Mcut1.5)
anova(Mcut.1,Mcut1.1)
#Mcut1.1
#Analyze alternate variance structure model residuals
E1.1.cut<-residuals(Mcut1.1,type="normalized")
qqnorm(E1.1.cut)
qqline(E1.1.cut)
ad.test(E1.1.cut)
plot(Mcut1.1) 
plot(x.cut, E1.1.cut)
plot(E1.1.cut,data1cut$width)
plot(E1.1.cut,data1cut$temp.min)








# CUT model validation
op=par(mfrow = c(2, 2)) 
plot(lmCUT2)
op=par(mfrow = c(2, 2)) 
CUT2resid=rstandard(lmCUT2) 
hist(CUT2resid) 
qqnorm(CUT2resid) 
plot(CUT2resid ~ data1cut$width, xlab = "width", ylab = "Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)
plot(CUT2resid ~ data1cut$temp.min, xlab = "min temp", ylab = "Residuals",cex.lab=1.5,pch=16) 
abline(0, 0)


############################################## model interpretation
# CUT model interpretation
plot(data1cut$width,data1cut$t.cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$t.cut.mass.m~data1cut$width))
plot(data1cut$temp.min,data1cut$t.cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$t.cut.mass.m~data1cut$temp.min))

############################# possibly meaningful plots
# GPP plots 
plot(data1omit$carbon,data1omit$t.gpp,cex.lab=1.5,pch=16)
abline(lm(data1omit$t.gpp~data1omit$carbon))
plot(data1omit$carbon,data1omit$gpp,cex.lab=1.5,pch=16)
abline(lm(data1omit$gpp~data1omit$carbon))

plot(data1omit$ammonia,data1omit$t.gpp,cex.lab=1.5,pch=16)
abline(lm(data1omit$t.gpp~data1omit$ammonia))
plot(data1omit$ammonia,data1omit$gpp,cex.lab=1.5,pch=16)
abline(lm(data1omit$gpp~data1omit$ammonia))

plot(data1omit$nitrate,data1omit$t.gpp,cex.lab=1.5,pch=16) #to plot t.gpp this data sheet needs the transformation
abline(lm(data1omit$t.gpp~data1omit$nitrate))
plot(data1omit$nitrate,data1omit$gpp,cex.lab=1.5,pch=16) #to plot t.gpp this data sheet needs the transformation
abline(lm(data1omit$gpp~data1omit$nitrate))
plot(data1nitrate_omit$nitrate,data1nitrate_omit$gpp,cex.lab=1.5,pch=16) #to plot t.gpp this data sheet needs the transformation
abline(lm(data1nitrate_omit$gpp~data1nitrate_omit$nitrate))

plot(data1omit$phosphate,data1omit$t.gpp,cex.lab=1.5,pch=16)
abline(lm(data1omit$t.gpp~data1omit$phosphate))
plot(data1omit$phosphate,data1omit$gpp,cex.lab=1.5,pch=16)
abline(lm(data1omit$gpp~data1omit$phosphate))
# ER plots
plot(data1omit$carbon,data1omit$t.er,cex.lab=1.5,pch=16)
abline(lm(data1omit$t.er~data1omit$carbon))
plot(data1omit$carbon,data1omit$er,cex.lab=1.5,pch=16)
abline(lm(data1omit$er~data1omit$carbon))

plot(data1omit$ammonia,data1omit$t.er,cex.lab=1.5,pch=16)
abline(lm(data1omit$t.er~data1omit$ammonia))
plot(data1omit$ammonia,data1omit$er,cex.lab=1.5,pch=16)
abline(lm(data1omit$er~data1omit$ammonia))

plot(data1omit$nitrate,data1omit$t.er,cex.lab=1.5,pch=16) #to plot t.gpp this data sheet needs the transformation
abline(lm(data1omit$t.er~data1omit$nitrate))
plot(data1omit$nitrate,data1omit$er,cex.lab=1.5,pch=16)
abline(lm(data1omit$er~data1omit$nitrate))
plot(data1nitrate_omit$nitrate,data1nitrate_omit$er,cex.lab=1.5,pch=16)
abline(lm(data1nitrate_omit$er~data1nitrate_omit$nitrate))

plot(data1omit$phosphate,data1omit$t.er,cex.lab=1.5,pch=16)
abline(lm(data1omit$t.er~data1omit$phosphate))
plot(data1omit$phosphate,data1omit$er,cex.lab=1.5,pch=16)
abline(lm(data1omit$er~data1omit$phosphate))
# CUT plots
plot(data1cut$carbon,data1cut$t.cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$t.cut.mass.m~data1cut$carbon))
plot(data1cut$carbon,data1cut$cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$cut.mass.m~data1cut$carbon))

plot(data1cut$ammonia,data1cut$t.cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$t.cut.mass.m~data1cut$ammonia))
plot(data1cut$ammonia,data1cut$cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$cut.mass.m~data1cut$ammonia))

plot(data1cut$nitrate,data1cut$t.cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$t.cut.mass.m~data1cut$nitrate))
plot(data1cut$nitrate,data1cut$cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$cut.mass.m~data1cut$nitrate))

plot(data1cut$phosphate,data1cut$t.cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$t.cut.mass.m~data1cut$phosphate))
plot(data1cut$phosphate,data1cut$cut.mass.m,cex.lab=1.5,pch=16)
abline(lm(data1cut$cut.mass.m~data1cut$phosphate))







