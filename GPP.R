data1=read.csv("all_trans_short_r.csv",header=TRUE) #load data in short format with NA's
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


##################### model selections

# GPP transformation
dotchart(data1$gpp.lit)
ad.test(data1$gpp.lit)
data1$t.gpp.lit=log10(1+data1$gpp.lit)
ad.test(data1$t.gpp.lit)
hist(data1$t.gpp.lit,xlab="transfromed gpp",cex.lab=1.5)
dotchart(data1$t.gpp.lit,xlab="transfromed gpp",cex.lab=1.5,pch=16)
# GPP LM models
lmGPP=lm(gpp.lit~season,na.action=na.omit,data=data1)         #p .3    r2 .01 
lmGPP=lm(gpp.lit~season.yr,na.action=na.omit,data=data1)      #p .12   r2 .09 
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
M.1=gls(t.gpp.lit~t.pebble,na.action=na.omit, 
        data=data1, method="ML") #base model
M.2=lme(t.gpp.lit~t.depth+t.temp.max, na.action=na.omit, 
        random = ~1|stream, data=data1, method="ML") #adds random effect

M.3=lme(t.gpp.lit~t.depth+t.temp.max, na.action=na.omit, 
        random = ~1|basin.stream, data=data1, method="ML") #adds nesting

anova(M.1, M.2, M.3)

#Analyze residuals
x.gpp<-data1$gpp.lit[!is.na(data1$gpp.lit)]#removes na values from column
x.pebble=data1$t.pebble[!is.na(data1$gpp.lit)]
x.depth=data1$t.depth[!is.na(data1$gpp.lit)]
x.temp.max=data1$t.temp.max[!is.na(data1$gpp.lit)]
# M.1 residuals
E.1.gpp<-residuals(M.1,type="normalized")
qqnorm(E.1.gpp)
qqline(E.1.gpp)
ad.test(E.1.gpp)
plot(M.1) 
plot(x.gpp, E.1.gpp)
plot(data1$gpp.lit,data1$par.max)
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
M.1=gls(t.gpp.lit~t.pebble+t.depth+t.temp.max, na.action=na.omit, 
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
M1.1<-gls(t.gpp.lit~t.pebble, na.action=na.omit, 
          data=data1, weights=vf1)

M1.2<-gls(t.gpp.lit~t.pebble, na.action=na.omit, 
          data=data1, weights=vf2)

M1.3<-gls(t.gpp.lit~t.pebble, na.action=na.omit, 
          data=data1, weights=vf3)

M1.4<-gls(t.gpp.lit~t.pebble, na.action=na.omit, 
          data=data1, weights=vf4)

M1.5<-gls(t.gpp.lit~t.pebble, na.action=na.omit, 
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
plot(x.pebble, E1.1.gpp)
plot(x.depth, E1.1.gpp)
plot(x.temp.max, E1.1.gpp)

E1.2.gpp<-residuals(M1.2,type="normalized")
qqnorm(E1.2.gpp)
qqline(E1.2.gpp)
ad.test(E1.2.gpp)
plot(M1.2) 
plot(x.gpp, E1.2.gpp)

E1.3.gpp<-residuals(M1.3,type="normalized")
qqnorm(E1.3.gpp)
qqline(E1.3.gpp)
ad.test(E1.3.gpp)
plot(M1.3) 
plot(x.gpp, E1.3.gpp)

E1.4.gpp<-residuals(M1.4,type="normalized")
qqnorm(E1.4.gpp)
qqline(E1.4.gpp)
ad.test(E1.4.gpp)
plot(M1.4) 
plot(x.gpp, E1.4.gpp)

E1.5.gpp<-residuals(M1.5,type="normalized")
qqnorm(E1.5.gpp)
qqline(E1.5.gpp)
ad.test(E1.5.gpp)
plot(M1.5) 
plot(x.gpp, E1.5.gpp)

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



