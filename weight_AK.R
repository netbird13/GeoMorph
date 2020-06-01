setwd("F:/Documents/MyRfiles/Rfiles_LeonLab")

# Loading libraries ####
library(lme4)
library(Hmisc) # pour errbar
library(e1071) # pour skewness
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-10.0.1') # for 64-bit version; Check also the jre version and correct if required
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_171') # for 32-bit version; Check also the jre version and correct if re
library(rJava) # pour xlsx
library(xlsx) # pour...

par.dflt = par() # saves par defaults

# Creating file name object: ####
crrntD = paste(Sys.Date(),sep="")
script.name = c("weight_AK")
lme4ver = packageVersion("lme4")
name.ver.date = paste(script.name,lme4ver,crrntD)

# Reading files: ####
weight.df = as.data.frame(read.csv("F:/Documents/Avi_LeonLab/Avi_files/Kairomones_factors.csv"),header=T)

# Renaming columns in the dataframe: ####
names(weight.df)[names(weight.df) == 'X.'] = 'num'
names(weight.df)[names(weight.df) == 'X.Dt'] = 'chkpnt'
names(weight.df)[names(weight.df) == 'Tub..'] = 'tub'
names(weight.df)[names(weight.df) == 'Fem'] = 'fem'
names(weight.df)[names(weight.df) == 'Site.ID'] = 'site'
names(weight.df)[names(weight.df) == 'Treat'] = 'treat'
names(weight.df)[names(weight.df) == 'Weight'] = 'wght'
names(weight.df)[names(weight.df) == 'Dt.Msrd'] = 'DtMsrd'
names(weight.df)[names(weight.df) == 'Dt.Dpst'] = 'DtDpst'

# Converting classes: ####
weight.df$chkpnt = as.factor(weight.df$chkpnt)
weight.df$tub = as.factor(weight.df$tub)

# Normalizing dates: ####
weight.df$DtMsrd = as.Date(paste(substr(weight.df$DtMsrd,1,2),substr(weight.df$DtMsrd,4,5),substr(weight.df$DtMsrd,7,10)), format='%d%m%Y')
weight.df$DtDpst = as.Date(paste(substr(weight.df$DtDpst,1,2),substr(weight.df$DtDpst,4,5),substr(weight.df$DtDpst,7,10)), format='%d%m%Y')

# Subsetting: ####
weight.df.chkpnt0 = subset(weight.df,weight.df$chkpnt==0)
weight.df.chkpnt1 = subset(weight.df,weight.df$chkpnt==1)
weight.df.chkpnt2 = subset(weight.df,weight.df$chkpnt==2)
weight.df.chkpnt3 = subset(weight.df,weight.df$chkpnt==3)
weight.df.chkpnt_3 = subset(weight.df,weight.df$chkpnt!=3)

# Models: ####
weight.null = lmer(wght ~ 1 + (1+chkpnt+treat|site/fem), data = weight.df, REML=F)
weight.null.chkpnt = lmer(wght ~ chkpnt + (1+chkpnt+treat|site/fem), data = weight.df, REML=F)
weight.null.treat = lmer(wght ~ treat + (1+chkpnt+treat|site/fem), data = weight.df, REML=F)
weight.full = lmer(wght ~ chkpnt + treat + (1+chkpnt+treat|site/fem), data = weight.df, REML=F)
weight.full.int = lmer(wght ~ chkpnt * treat + (1+chkpnt+treat|site/fem), data = weight.df, REML=F)
weight.null.site = lm(wght ~ DtDpst + treat + chkpnt, data = weight.df, REML=F)
weight.full.site = lm(wght ~ DtDpst + treat + chkpnt + site, data = weight.df, REML=F)

# Models-chkpnts: ####
weight.chkpnt0.null = lmer(wght ~ 1 + (1+treat|site/fem), data = weight.df.chkpnt0, REML=F)
weight.chkpnt1.null = lmer(wght ~ 1 + (1+treat|site/fem), data = weight.df.chkpnt1, REML=F)
weight.chkpnt2.null = lmer(wght ~ 1 + (1+treat|site/fem), data = weight.df.chkpnt2, REML=F)
weight.chkpnt3.null = lmer(wght ~ 1 + (1+treat|site/fem), data = weight.df.chkpnt3, REML=F)
weight.chkpnt_3.null = lmer(wght ~ 1 + chkpnt + (1+treat+chkpnt|site/fem), data = weight.df.chkpnt_3, REML=F)
weight.chkpnt0.treat = lmer(wght ~ treat + (1+treat|site/fem), data = weight.df.chkpnt0, REML=F)
weight.chkpnt1.treat = lmer(wght ~ treat + (1+treat|site/fem), data = weight.df.chkpnt1, REML=F)
weight.chkpnt2.treat = lmer(wght ~ treat + (1+treat|site/fem), data = weight.df.chkpnt2, REML=F)
weight.chkpnt3.treat = lmer(wght ~ treat + (1+treat|site/fem), data = weight.df.chkpnt3, REML=F)
weight.chkpnt_3.treat = lmer(wght ~ treat + chkpnt + (1+treat+chkpnt|site/fem), data = weight.df.chkpnt_3, REML=F)

# ANOVA: ####
chkpntVSfull = anova(weight.null.chkpnt,weight.full) # treatment effect
treatVSfull = anova(weight.null.treat,weight.full) # chekpoint effect
intVSfull = anova(weight.full.int,weight.full) # Interaction effect vs chkpnt & treat
siteVStreat.chkpnt = anova(weight.null.site,weight.full.site) # site effect
# ANOVA-chkpnts: ####
chkpnt0_nullVStreat = anova(weight.chkpnt0.null,weight.chkpnt0.treat) # treatment effect
chkpnt1_nullVStreat = anova(weight.chkpnt1.null,weight.chkpnt1.treat) # treatment effect
chkpnt2_nullVStreat = anova(weight.chkpnt2.null,weight.chkpnt2.treat) # treatment effect
chkpnt3_nullVStreat = anova(weight.chkpnt3.null,weight.chkpnt3.treat) # treatment effect
chkpnt_3_nullVStreat = anova(weight.chkpnt_3.null,weight.chkpnt_3.treat) # treatment effect

# XLSX: ####
write.xlsx2(x=chkpntVSfull,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="chkpntVSfull", col.names=T, row.names=T, append=T)
write.xlsx2(x=treatVSfull,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="treatVSfull", col.names=T, row.names=T, append=T)
write.xlsx2(x=intVSfull,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="intVSfull", col.names=T, row.names=T, append=T)
write.xlsx2(x=chkpnt0_nullVStreat,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="chkpnt0_nullVStreat", col.names=T, row.names=T, append=T)
write.xlsx2(x=chkpnt1_nullVStreat,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="chkpnt1_nullVStreat", col.names=T, row.names=T, append=T)
write.xlsx2(x=chkpnt2_nullVStreat,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="chkpnt2_nullVStreat", col.names=T, row.names=T, append=T)
write.xlsx2(x=chkpnt3_nullVStreat,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="chkpnt3_nullVStreat", col.names=T, row.names=T, append=T)
write.xlsx2(x=chkpnt_3_nullVStreat,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="chkpnt_3_nullVStreat", col.names=T, row.names=T, append=T)

# summary(intVSfull)
# coef(weight.full)
# summary(weight.full)
# View(weight.df)
# mean(weight.df$wght[weight.df$treat=="0f" & weight.df$chkpnt==0])