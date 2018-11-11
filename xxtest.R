setwd("N:/Thesis")
dir()

data1=read.csv("all_trans_short_r.csv",header=TRUE) #load data in short format with NA's
names(data1)
head(data1)
data1


data2=read.csv("all_trans_long_r.csv",header=TRUE) #load data in long format
names(data2)
head(data2)
data2



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

source("AEDgraphingfunctions.R") #we used this alot in advanced biostats
############ data exploration

#site specific non seasonal variables
dotchart(data2$elev,      xlab="elev (m)",          cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$aspect,    xlab="aspect (deg)",        cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$from.north,xlab="deg from.north",cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$slope,     xlab="slope (%)",         cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$bf,        xlab="bank full (m)",     cex.lab=1.5,pch=16,group=data2$basin.stream)
dotchart(data2$pebble,    xlab="pebble median (mm)", cex.lab=1.5,pch=16,group=data2$basin.stream)
#variables that are more subject to change
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
op=par(mfrow=c(3,1))
dotchart(data2$phosphate.sum17,xlab="phosphate sum 17 (mg P/L)",xlim=c(0,.06)   ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$phosphate.fall17,xlab="phosphate fall 17 (mg P/L)",xlim=c(0,.06) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$phosphate.sum18,xlab="phosphate sum 18 (mg P/L)"  ,xlim=c(0,.06) ,cex.lab=1.5,pch=16,group=data2$basin)

dotchart(data2$canopy.sum17,xlab="canopy sum 17 (%open)",xlim=c(5,80)   ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$canopy.fall17,xlab="canopy fall 17 (%open)",xlim=c(5,80) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$canopy.sum18,xlab="canopy sum 18 (%open)" ,xlim=c(5,80)  ,cex.lab=1.5,pch=16,group=data2$basin)

dotchart(data2$par.integrative.sum17,xlab="integrative PAR sum 17 (mol/m2/d)" ,xlim=c(0,3.5)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.integrative.fall17,xlab="integrative PAR fall 17 (mol/m2/d)",xlim=c(0,3.5) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.integrative.sum18,xlab="integrative PAR sum 18 (mol/m2/d)"  ,xlim=c(0,3.5) ,cex.lab=1.5,pch=16,group=data2$basin)

dotchart(data2$par.mean.sum17,xlab="mean PAR sum 17 (umol/m2/s)" ,xlim=c(0,12000)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.mean.fall17,xlab="mean PAR fall 17 (umol/m2/s)",xlim=c(0,12000) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.mean.sum18,xlab="mean PAR sum 18 (umol/m2/s)" ,xlim=c(0,12000)  ,cex.lab=1.5,pch=16,group=data2$basin)

dotchart(data2$par.max.sum17,xlab="max PAR sum 17 (umol/m2/s)" ,xlim=c(0,70000)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.max.fall17,xlab="max PAR fall 17 (umol/m2/s)",xlim=c(0,70000) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$par.max.sum18,xlab="max PAR sum 18 (umol/m2/s)" ,xlim=c(0,70000)  ,cex.lab=1.5,pch=16,group=data2$basin)

dotchart(data2$temp.max.sum17,xlab="max temp sum 17 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.max.fall17,xlab="max temp fall 17 (deg C)",xlim=c(0,15) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.max.sum18,xlab="max temp sum 18 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)

dotchart(data2$temp.min.sum17,xlab="min temp sum 17 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.min.fall17,xlab="min temp fall 17 (deg C)",xlim=c(0,15) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.min.sum18,xlab="min temp sum 18 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)

dotchart(data2$temp.mean.sum17,xlab="mean temp sum 17 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.mean.fall17,xlab="mean temp fall 17 (deg C)",xlim=c(0,15) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$temp.mean.sum18,xlab="mean temp sum 18 (deg C)" ,xlim=c(0,15)  ,cex.lab=1.5,pch=16,group=data2$basin)

#I selected believable k600's from my data and replaced what appeared to be erroneous ones, some values were still left out

dotchart(data2$k600.sum17,xlab="k600 sum 17 (1/d)" ,xlim=c(20,90)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$k600.fall17,xlab="k600 fall 17 (1/d)",xlim=c(20,90) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$k600.sum18,xlab="k600 sum 18 (1/d)" ,xlim=c(20,90)  ,cex.lab=1.5,pch=16,group=data2$basin)

#some values left out as NA's

dotchart(data2$gpp.sum17,xlab="gpp sum 17 (g 02/m2/d)" ,xlim=c(0,.14)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$gpp.fall17,xlab="gpp fall 17 (g 02/m2/d)" ,xlim=c(0,.14),cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$gpp.sum18,xlab="gpp sum 18 (g 02/m2/d)" ,xlim=c(0,.14)  ,cex.lab=1.5,pch=16,group=data2$basin)

#some values left out as NA's

dotchart(data2$er.sum17,xlab="ER sum 17 (g 02/m2/d)" ,xlim=c(-10,0)  ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$er.fall17,xlab="ER fall 17 (g 02/m2/d)",xlim=c(-10,0) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$er.sum18,xlab="ER sum 18 (g 02/m2/d)" ,xlim=c(-10,0)  ,cex.lab=1.5,pch=16,group=data2$basin)

#some values left out as NA's

dotchart(data2$pr.ratio.sum17,xlab="P/R sum 17"  ,xlim=c(-.035,0) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$pr.ratio.fall17,xlab="P/R fall 17",xlim=c(-.035,0) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$pr.ratio.sum18,xlab="P/R sum 18" ,xlim=c(-.035,0)  ,cex.lab=1.5,pch=16,group=data2$basin)

dotchart(data2$cut.capt.sum17,xlab="cutthroat capture probability sum 17"  ,xlim=c(0,1) ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$cut.capt.sum18,xlab="cutthroat capture probability sum 18" ,xlim=c(0,1)  ,cex.lab=1.5,pch=16,group=data2$basin)














dotchart(data2$.sum17,xlab=" sum 17 ()"   ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$.fall17,xlab=" fall 17 ()" ,cex.lab=1.5,pch=16,group=data2$basin)
dotchart(data2$.sum18,xlab=" sum 18 ()"   ,cex.lab=1.5,pch=16,group=data2$basin)
,xlim=c()



