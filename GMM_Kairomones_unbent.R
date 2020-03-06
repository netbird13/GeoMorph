# Creating file name object: ####
script.name = c("un_bent")
name.ver.date = paste(script.name,gmrph,crrntD)

# Reading files: ####
# larvae.unbent = readland.tps(file="F:/Documents/Avi_LeonLab/Avi_files/tps temp/PROCCESSED/2018_all_curvesToLM_intersectionsUnited.tps", specID="ID") # bent specimens were NOT replaced
larvae.unbent = readland.tps(file="F:/Documents/Avi_LeonLab/Avi_files/tps temp/PROCCESSED/2018_all_with curves_bentSpecimensReplaced_curvesToLM_intersectionsUnited.tps", specID="ID") # bent specimens WERE replaced

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# larvae.curves doens't contain semi-LM 73-83, so deleting them should happen before GPA (curves are defined in GPA; could be defined in readland.tps but than removal of LM should happen using tpsUtil, unless LM corresponded curves are removed afterwords)
# Removing excess semi-LM: ####
larvae.unbent = larvae.unbent[c(1:72),,]

# Removing bent specimens (that couldn't be replaced with unbent ones)
larvae.unbent = larvae.unbent[,,-c(5, 8, 15, 18, 60, 84, 94, 132, 137, 142, 158, 169, 172, 173, 174, 187, 188, 193, 194, 199, 207, 209, 213, 219, 228, 229, 233, 244, 249, 250, 273)]
l.classifiers = l.classifiers[-c(5, 8, 15, 18, 60, 84, 94, 132, 137, 142, 158, 169, 172, 173, 174, 187, 188, 193, 194, 199, 207, 209, 213, 219, 228, 229, 233, 244, 249, 250, 273),]

# # Removing the eye landmark: ####
# larvae.unbent = larvae.unbent[c(1:3,5:72),,]
# # Correcting larvae.curves accordingly (otherwise GPA returns: "out of bounds"): ####
# larvae.curves[67,3] = 1
# larvae.curves = larvae.curves[1:67,1:3] # deleting row 68
# # Correcting links accordingly: ####
# l.links[70,2] = 1
# l.links = l.links[1:70,1:2] # deleting row 71

# GPA ####
larvae.unbent.gpa = gpagen(larvae.unbent, curves=larvae.curves, ProcD=T, print.progress=F)
layout(1) # reset to 1 graph per plot

# Plotting outliers: ####
# unbent ####
mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_With_Outliers & chkpnt_3.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plotAllSpecimens(larvae.unbent.gpa$coords, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1)) # Plot all specimens with outliers.
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_Outliers.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
outliers = plotOutliers(larvae.unbent.gpa$coords) # function returns dimname and address of outliers
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_No_Outliers.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plotOutliers(larvae.unbent.gpa$coords[,,-c(outliers[1:23])]) # Outliers graph without prominent/protrustive outliers.
dev.off()

# Checkpoint 3 removal: ####
chkpnt3 = which(l.classifiers$chkpnt==3)
larvae.unbent.gpa$coords = larvae.unbent.gpa$coords[,,-c(chkpnt3)]
larvae.unbent.gpa$Csize = larvae.unbent.gpa$Csize[-c(chkpnt3)]
l.classifiers = l.classifiers[-c(chkpnt3),]
larvae.unbent.gpa$data = larvae.unbent.gpa$data[-c(chkpnt3),]

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_With_Outliers, w.o. chkpnt_3.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plotAllSpecimens(larvae.unbent.gpa$coords, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1)) # Plot all specimens with outliers.
dev.off()

# # Outliers removal: ####
# larvae.unbent.gpa$coords = larvae.unbent.gpa$coords[,,-c(outliers[1:23])] # Removing outliers from coords. Be carefull not to remove twice - should be 207 specimens after removal.
# larvae.unbent.gpa$Csize = larvae.unbent.gpa$Csize[-c(outliers[1:23])] # Removing outliers from Centroid size. Be carefull not to remove twice - should be 207 specimens after removal.
# l.classifiers = l.classifiers[-c(outliers[1:23]),] # Removing outliers from classifiers. Be carefull not to remove twice - should be 207 specimens after removal.
# larvae.unbent.gpa$data = larvae.unbent.gpa$data[-c(outliers[1:23]),] # Removing outliers from data. Be carefull not to remove twice - should be 207 specimens after removal.

# # KKB removal: ####
# kkb = which(l.classifiers$site=="KKB")
# larvae.unbent.gpa$coords = larvae.unbent.gpa$coords[,,-c(kkb)]
# larvae.unbent.gpa$Csize = larvae.unbent.gpa$Csize[-c(kkb)]
# l.classifiers = l.classifiers[-c(kkb),]
# larvae.unbent.gpa$data = larvae.unbent.gpa$data[-c(kkb),]

# Creating groups (this has to come only after removing outliers, otherwise it doesn't match coords and Csize in gdf): ####
gp1 = as.factor(paste(l.classifiers$chkpnt, l.classifiers$site, l.classifiers$fem, l.classifiers$treat, l.classifiers$tub)) # chkpnt.site.fem.treat.tub
gp2 = as.factor(paste(l.classifiers$chkpnt, l.classifiers$treat)) # chkpnt.treat
gp3 = as.factor(paste(l.classifiers$chkpnt)) # chkpnt

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_No_Outliers.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
outliers.plot = plotAllSpecimens(larvae.unbent.gpa$coords, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1)) # Plot all specimens with outliers.
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_Without_Outliers & chkpnt_3.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plotAllSpecimens(larvae.unbent.gpa$coords, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1)) # Plot all specimens with outliers.
dev.off()

# GDF ####
# Creating main gdf:
unbent.gdf = geomorph.data.frame(shape=larvae.unbent.gpa$coords, cs=larvae.unbent.gpa$Csize, num=l.classifiers$num, chkpnt=l.classifiers$chkpnt, site=l.classifiers$site, fem=l.classifiers$fem, treat=l.classifiers$treat, tub=l.classifiers$tub, wght=l.classifiers$wght)
# --------------------------------------------------------------------------------------------------------------

# Testing for the explanatory power of centroid size for weight: ####
unbent.cs.wght.df = as.data.frame(cbind(log(unbent.gdf$cs),log(unbent.gdf$wght^(1/3))))
unbent.cs.wght.cov = cov(unbent.cs.wght.df)
unbent.cs.wght.cor = cor(unbent.cs.wght.df)
lm.unbent.cs.wght = lm(log(l.classifiers$wght^(1/3)) ~ log(larvae.unbent.gpa$Csize))
lm.unbent.cs.wght$residuals
sum.unbent.cs.wght = summary(lm.unbent.cs.wght)
eq.unbent.cs.wght.cor = paste('Eq: y=', round(lm.unbent.cs.wght$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght$coefficients[8])

a= paste("Residual standard error:",round(sum.unbent.cs.wght$sigma,digit=4),"on",sum.unbent.cs.wght$df[2],"degrees of freedom")
b =paste("Multiple R-squared:",round(sum.unbent.cs.wght$r.squared,digits=5),"Adjusted R-squared:",round(sum.unbent.cs.wght$adj.r.squared,digits=5))
c= paste("F-statistic:",sum.unbent.cs.wght$fstatistic[1],"on",sum.unbent.cs.wght$fstatistic[2],"and",sum.unbent.cs.wght$fstatistic[3],"DF","p-value:",sum.unbent.cs.wght$coefficients[8])
sum.list = list(c(a,b,c))

lm.size.weight.lst = list(eq.unbent.cs.wght.cor,sum.list)
lm.size.weight.u = rbindlist(lapply(lm.size.weight.lst, data.frame),fill=T)
lm.size.weight.u[is.na(lm.size.weight.u)] = ""

# write.csv(x=lm.size.weight.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/size.weight_",name.ver.date,".csv"))
write.xlsx2(x=lm.size.weight.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="size.weight", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_csVSwght.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
par(mfrow =	c(2,2),mar=c(1,1,8,1)) # set up 4 graphs per plot: Chkpnt
plot(log(larvae.unbent.gpa$Csize),log(l.classifiers$wght^(1/3)))
abline(lm.unbent.cs.wght)
text(0.1,NULL,labels=eq.unbent.cs.wght.cor)
plot(log(larvae.unbent.gpa$Csize),lm.unbent.cs.wght$residuals)
qqnorm(lm.unbent.cs.wght$residuals)
qqline(lm.unbent.cs.wght$residuals)
hist(lm.unbent.cs.wght$residuals)
dev.off()
layout(1) # reset to 1 graph per plot

# --------------------------------------------------------------------------------------------------------------

# subsetting chkpnt (coords + Csize + factors): ####

# Subsetting coords: chkpnt ####
unbent.sub.coords.chkpnt = coords.subset(unbent.gdf$shape, l.classifiers$chkpnt) # chkpnt
unbent.sub.chkpnt.0 = unbent.sub.coords.chkpnt$`0`
unbent.sub.chkpnt.1 = unbent.sub.coords.chkpnt$`1`
unbent.sub.chkpnt.2 = unbent.sub.coords.chkpnt$`2`
unbent.sub.chkpnt.3 = unbent.sub.coords.chkpnt$`3`

# Subsetting coords: chkpnt/treat ####
unbent.sub.coords.chkpnt = coords.subset(unbent.gdf$shape, group=gp2) # chkpnt/treat
unbent.sub.coords.0_0f = unbent.sub.coords.chkpnt$`0 0f`
unbent.sub.coords.0_3f = unbent.sub.coords.chkpnt$`0 3f`
unbent.sub.coords.0_6f = unbent.sub.coords.chkpnt$`0 6f`
unbent.sub.coords.1_0f = unbent.sub.coords.chkpnt$`1 0f`
unbent.sub.coords.1_3f = unbent.sub.coords.chkpnt$`1 3f`
unbent.sub.coords.1_6f = unbent.sub.coords.chkpnt$`1 6f`
unbent.sub.coords.2_0f = unbent.sub.coords.chkpnt$`2 0f`
unbent.sub.coords.2_3f = unbent.sub.coords.chkpnt$`2 3f`
unbent.sub.coords.2_6f = unbent.sub.coords.chkpnt$`2 6f`
unbent.sub.coords.3_0f = unbent.sub.coords.chkpnt$`3 0f`
unbent.sub.coords.3_3f = unbent.sub.coords.chkpnt$`3 3f`
unbent.sub.coords.3_6f = unbent.sub.coords.chkpnt$`3 6f`

# Mean shape:chkpnt: ####
unbent.m.chkpnt = lapply(coords.subset(larvae.unbent.gpa$coords, group=l.classifiers$chkpnt), mshape)
unbent.m.chkpnt.0 = unbent.m.chkpnt$`0`
unbent.m.chkpnt.1 = unbent.m.chkpnt$`1`
unbent.m.chkpnt.2 = unbent.m.chkpnt$`2`
unbent.m.chkpnt.3 = unbent.m.chkpnt$`3`

# Mean shape: chkpnt+treat: ####
unbent.m.chkpnt.treat = lapply(coords.subset(larvae.unbent.gpa$coords, group=gp2), mshape)
unbent.m.chkpnt.treat.0.0f = unbent.m.chkpnt.treat$`0 0f`
unbent.m.chkpnt.treat.0.3f = unbent.m.chkpnt.treat$`0 3f`
unbent.m.chkpnt.treat.0.6f = unbent.m.chkpnt.treat$`0 6f`
unbent.m.chkpnt.treat.1.0f = unbent.m.chkpnt.treat$`1 0f`
unbent.m.chkpnt.treat.1.3f = unbent.m.chkpnt.treat$`1 3f`
unbent.m.chkpnt.treat.1.6f = unbent.m.chkpnt.treat$`1 6f`
unbent.m.chkpnt.treat.2.0f = unbent.m.chkpnt.treat$`2 0f`
unbent.m.chkpnt.treat.2.3f = unbent.m.chkpnt.treat$`2 3f`
unbent.m.chkpnt.treat.2.6f = unbent.m.chkpnt.treat$`2 6f`
unbent.m.chkpnt.treat.3.0f = unbent.m.chkpnt.treat$`3 0f`
unbent.m.chkpnt.treat.3.3f = unbent.m.chkpnt.treat$`3 3f`
unbent.m.chkpnt.treat.3.6f = unbent.m.chkpnt.treat$`3 6f`
# --------------------------------------------------------------------------------------------------------------

# Subsetting Csize: ####
unbent.sub.cs = unbent.gdf$cs
unbent.sub.cs.0.0f = subset(unbent.sub.cs, l.classifiers$chkpnt == 0 & l.classifiers$treat == "0f")
unbent.sub.cs.0.3f = subset(unbent.sub.cs, l.classifiers$chkpnt == 0 & l.classifiers$treat == "3f")
unbent.sub.cs.0.6f = subset(unbent.sub.cs, l.classifiers$chkpnt == 0 & l.classifiers$treat == "6f")
unbent.sub.cs.1.0f = subset(unbent.sub.cs, l.classifiers$chkpnt == 1 & l.classifiers$treat == "0f")
unbent.sub.cs.1.3f = subset(unbent.sub.cs, l.classifiers$chkpnt == 1 & l.classifiers$treat == "3f")
unbent.sub.cs.1.6f = subset(unbent.sub.cs, l.classifiers$chkpnt == 1 & l.classifiers$treat == "6f")
unbent.sub.cs.2.0f = subset(unbent.sub.cs, l.classifiers$chkpnt == 2 & l.classifiers$treat == "0f")
unbent.sub.cs.2.3f = subset(unbent.sub.cs, l.classifiers$chkpnt == 2 & l.classifiers$treat == "3f")
unbent.sub.cs.2.6f = subset(unbent.sub.cs, l.classifiers$chkpnt == 2 & l.classifiers$treat == "6f")
unbent.sub.cs.3.0f = subset(unbent.sub.cs, l.classifiers$chkpnt == 3 & l.classifiers$treat == "0f")
unbent.sub.cs.3.3f = subset(unbent.sub.cs, l.classifiers$chkpnt == 3 & l.classifiers$treat == "3f")
unbent.sub.cs.3.6f = subset(unbent.sub.cs, l.classifiers$chkpnt == 3 & l.classifiers$treat == "6f")

# Calculating Csize average per group: ####
m.sub.cs.chkpnt.treat = aggregate(unbent.sub.cs ~ l.classifiers$chkpnt + l.classifiers$treat, FUN=mean)[,-4]
nam.sub.cs.chkpnt.treat = cbind(l.classifiers$treat,l.classifiers$chkpnt)
# rownames(nam.sub.cs.chkpnt.treat) = levels(gp2) # ain't working!!!

# rownames(m.sub.cs.type.treat.time) = levels(gp3)
# names(m.sub.cs.type.treat.time)[names(m.sub.cs.type.treat.time) == 'sub.cs'] = 'values' # Renaming column in a dataframe

# Calculating Csize sd per group: ####
sd.sub.cs.chkpnt.treat = aggregate(unbent.sub.cs ~ l.classifiers$chkpnt + l.classifiers$treat, FUN=sd)[,-4]
rownames(sd.sub.cs.chkpnt.treat) = levels(gp2)
names(sd.sub.cs.chkpnt.treat)[names(sd.sub.cs.chkpnt.treat) == 'unbent.sub.cs'] = 'values' # Renaming column in a dataframe

# Calculating Csize N per group: ####
n.sub.cs.chkpnt.treat = aggregate(unbent.sub.cs ~ l.classifiers$chkpnt + l.classifiers$treat, FUN=length)[,-4]
rownames(n.sub.cs.chkpnt.treat) = levels(gp2)
names(n.sub.cs.chkpnt.treat)[names(n.sub.cs.chkpnt.treat) == 'unbent.sub.cs'] = 'values' # Renaming column in a dataframe

# Calculating Csize SE: ####
se.sub.cs.chkpnt.treat = sd.sub.cs.chkpnt.treat$values/sqrt(n.sub.cs.chkpnt.treat$values)
# --------------------------------------------------------------------------------------------------------------

# Subsetting weight: #### chkpnt 3 subsets will result "numeric(0)" as long as chkpnt 3 was removed beforehand
unbent.sub.wght = unbent.gdf$wght
unbent.sub.wght.0_0f = subset(unbent.sub.wght, l.classifiers$chkpnt == 0 & l.classifiers$treat == "0f")
unbent.sub.wght.0_3f = subset(unbent.sub.wght, l.classifiers$chkpnt == 0 & l.classifiers$treat == "3f")
unbent.sub.wght.0_6f = subset(unbent.sub.wght, l.classifiers$chkpnt == 0 & l.classifiers$treat == "6f")
unbent.sub.wght.1_0f = subset(unbent.sub.wght, l.classifiers$chkpnt == 1 & l.classifiers$treat == "0f")
unbent.sub.wght.1_3f = subset(unbent.sub.wght, l.classifiers$chkpnt == 1 & l.classifiers$treat == "3f")
unbent.sub.wght.1_6f = subset(unbent.sub.wght, l.classifiers$chkpnt == 1 & l.classifiers$treat == "6f")
unbent.sub.wght.2_0f = subset(unbent.sub.wght, l.classifiers$chkpnt == 2 & l.classifiers$treat == "0f")
unbent.sub.wght.2_3f = subset(unbent.sub.wght, l.classifiers$chkpnt == 2 & l.classifiers$treat == "3f")
unbent.sub.wght.2_6f = subset(unbent.sub.wght, l.classifiers$chkpnt == 2 & l.classifiers$treat == "6f")
unbent.sub.wght.3_0f = subset(unbent.sub.wght, l.classifiers$chkpnt == 3 & l.classifiers$treat == "0f")
unbent.sub.wght.3_3f = subset(unbent.sub.wght, l.classifiers$chkpnt == 3 & l.classifiers$treat == "3f")
unbent.sub.wght.3_6f = subset(unbent.sub.wght, l.classifiers$chkpnt == 3 & l.classifiers$treat == "6f")
# --------------------------------------------------------------------------------------------------------------

# ReTesting for the explanatory power of centroid size for weight - per treat-chkpnt groups: #### chkpnt 3 subsets will result "numeric(0)" as long as chkpnt 3 was removed beforehand
unbent.cs.wght.sub.coords.0_0f.df = as.data.frame(cbind(log(unbent.sub.cs.0.0f),log(unbent.sub.wght.0_0f^(1/3))))
unbent.cs.wght.sub.coords.0_3f.df = as.data.frame(cbind(log(unbent.sub.cs.0.3f),log(unbent.sub.wght.0_3f^(1/3))))
unbent.cs.wght.sub.coords.0_6f.df = as.data.frame(cbind(log(unbent.sub.cs.0.6f),log(unbent.sub.wght.0_6f^(1/3))))
unbent.cs.wght.sub.coords.1_0f.df = as.data.frame(cbind(log(unbent.sub.cs.1.0f),log(unbent.sub.wght.1_0f^(1/3))))
unbent.cs.wght.sub.coords.1_3f.df = as.data.frame(cbind(log(unbent.sub.cs.1.3f),log(unbent.sub.wght.1_3f^(1/3))))
unbent.cs.wght.sub.coords.1_6f.df = as.data.frame(cbind(log(unbent.sub.cs.1.6f),log(unbent.sub.wght.1_6f^(1/3))))
unbent.cs.wght.sub.coords.2_0f.df = as.data.frame(cbind(log(unbent.sub.cs.2.0f),log(unbent.sub.wght.2_0f^(1/3))))
unbent.cs.wght.sub.coords.2_3f.df = as.data.frame(cbind(log(unbent.sub.cs.2.3f),log(unbent.sub.wght.2_3f^(1/3))))
unbent.cs.wght.sub.coords.2_6f.df = as.data.frame(cbind(log(unbent.sub.cs.2.6f),log(unbent.sub.wght.2_6f^(1/3))))
unbent.cs.wght.sub.coords.3_0f.df = as.data.frame(cbind(log(unbent.sub.cs.3.0f),log(unbent.sub.wght.3_0f^(1/3))))
unbent.cs.wght.sub.coords.3_3f.df = as.data.frame(cbind(log(unbent.sub.cs.3.3f),log(unbent.sub.wght.3_3f^(1/3))))
unbent.cs.wght.sub.coords.3_6f.df = as.data.frame(cbind(log(unbent.sub.cs.3.6f),log(unbent.sub.wght.3_6f^(1/3))))

unbent.cs.wght.0_0f.cov = cov(unbent.cs.wght.sub.coords.0_0f.df)
unbent.cs.wght.0_3f.cov = cov(unbent.cs.wght.sub.coords.0_3f.df)
unbent.cs.wght.0_6f.cov = cov(unbent.cs.wght.sub.coords.0_6f.df)
unbent.cs.wght.1_0f.cov = cov(unbent.cs.wght.sub.coords.1_0f.df)
unbent.cs.wght.1_3f.cov = cov(unbent.cs.wght.sub.coords.1_3f.df)
unbent.cs.wght.1_6f.cov = cov(unbent.cs.wght.sub.coords.1_6f.df)
unbent.cs.wght.2_0f.cov = cov(unbent.cs.wght.sub.coords.2_0f.df)
unbent.cs.wght.2_3f.cov = cov(unbent.cs.wght.sub.coords.2_3f.df)
unbent.cs.wght.2_6f.cov = cov(unbent.cs.wght.sub.coords.2_6f.df)
unbent.cs.wght.3_0f.cov = cov(unbent.cs.wght.sub.coords.3_0f.df)
unbent.cs.wght.3_3f.cov = cov(unbent.cs.wght.sub.coords.3_3f.df)
unbent.cs.wght.3_6f.cov = cov(unbent.cs.wght.sub.coords.3_6f.df)

unbent.cs.wght.0_0f.cor = cor(unbent.cs.wght.sub.coords.0_0f.df)
unbent.cs.wght.0_3f.cor = cor(unbent.cs.wght.sub.coords.0_3f.df)
unbent.cs.wght.0_6f.cor = cor(unbent.cs.wght.sub.coords.0_6f.df)
unbent.cs.wght.1_0f.cor = cor(unbent.cs.wght.sub.coords.1_0f.df)
unbent.cs.wght.1_3f.cor = cor(unbent.cs.wght.sub.coords.1_3f.df)
unbent.cs.wght.1_6f.cor = cor(unbent.cs.wght.sub.coords.1_6f.df)
unbent.cs.wght.2_0f.cor = cor(unbent.cs.wght.sub.coords.2_0f.df)
unbent.cs.wght.2_3f.cor = cor(unbent.cs.wght.sub.coords.2_3f.df)
unbent.cs.wght.2_6f.cor = cor(unbent.cs.wght.sub.coords.2_6f.df)
unbent.cs.wght.3_0f.cor = cor(unbent.cs.wght.sub.coords.3_0f.df)
unbent.cs.wght.3_3f.cor = cor(unbent.cs.wght.sub.coords.3_3f.df)
unbent.cs.wght.3_6f.cor = cor(unbent.cs.wght.sub.coords.3_6f.df)

lm.unbent.cs.wght.0_0f = lm(log(unbent.sub.wght.0_0f^(1/3)) ~ log(unbent.sub.cs.0.0f))
lm.unbent.cs.wght.0_3f = lm(log(unbent.sub.wght.0_3f^(1/3)) ~ log(unbent.sub.cs.0.3f))
lm.unbent.cs.wght.0_6f = lm(log(unbent.sub.wght.0_6f^(1/3)) ~ log(unbent.sub.cs.0.6f))
lm.unbent.cs.wght.1_0f = lm(log(unbent.sub.wght.1_0f^(1/3)) ~ log(unbent.sub.cs.1.0f))
lm.unbent.cs.wght.1_3f = lm(log(unbent.sub.wght.1_3f^(1/3)) ~ log(unbent.sub.cs.1.3f))
lm.unbent.cs.wght.1_6f = lm(log(unbent.sub.wght.1_6f^(1/3)) ~ log(unbent.sub.cs.1.6f))
lm.unbent.cs.wght.2_0f = lm(log(unbent.sub.wght.2_0f^(1/3)) ~ log(unbent.sub.cs.2.0f))
lm.unbent.cs.wght.2_3f = lm(log(unbent.sub.wght.2_3f^(1/3)) ~ log(unbent.sub.cs.2.3f))
lm.unbent.cs.wght.2_6f = lm(log(unbent.sub.wght.2_6f^(1/3)) ~ log(unbent.sub.cs.2.6f))

sum.unbent.cs.wght.0_0f = summary(lm.unbent.cs.wght.0_0f)
sum.unbent.cs.wght.0_3f = summary(lm.unbent.cs.wght.0_3f)
sum.unbent.cs.wght.0_6f = summary(lm.unbent.cs.wght.0_6f)
sum.unbent.cs.wght.1_0f = summary(lm.unbent.cs.wght.1_0f)
sum.unbent.cs.wght.1_3f = summary(lm.unbent.cs.wght.1_3f)
sum.unbent.cs.wght.1_6f = summary(lm.unbent.cs.wght.1_6f)
sum.unbent.cs.wght.2_0f = summary(lm.unbent.cs.wght.2_0f)
sum.unbent.cs.wght.2_3f = summary(lm.unbent.cs.wght.2_3f)
sum.unbent.cs.wght.2_6f = summary(lm.unbent.cs.wght.2_6f)

eq.unbent.cs.wght.cor.0_0f = paste("chkpnt:0; 0 fish",'Eq: y=', round(lm.unbent.cs.wght.0_0f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.0_0f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.0_0f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.0_0f$coefficients[8])
eq.unbent.cs.wght.cor.0_3f = paste("chkpnt:0; 3 fish",'Eq: y=', round(lm.unbent.cs.wght.0_3f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.0_3f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.0_3f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.0_3f$coefficients[8])
eq.unbent.cs.wght.cor.0_6f = paste("chkpnt:0; 6 fish",'Eq: y=', round(lm.unbent.cs.wght.0_6f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.0_6f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.0_6f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.0_6f$coefficients[8])
eq.unbent.cs.wght.cor.1_0f = paste("chkpnt:1; 0 fish",'Eq: y=', round(lm.unbent.cs.wght.1_0f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.1_0f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.1_0f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.1_0f$coefficients[8])
eq.unbent.cs.wght.cor.1_3f = paste("chkpnt:1; 3 fish",'Eq: y=', round(lm.unbent.cs.wght.1_3f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.1_3f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.1_3f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.1_3f$coefficients[8])
eq.unbent.cs.wght.cor.1_6f = paste("chkpnt:1; 6 fish",'Eq: y=', round(lm.unbent.cs.wght.1_6f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.1_6f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.1_6f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.1_6f$coefficients[8])
eq.unbent.cs.wght.cor.2_0f = paste("chkpnt:2; 0 fish",'Eq: y=', round(lm.unbent.cs.wght.2_0f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.2_0f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.2_0f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.2_0f$coefficients[8])
eq.unbent.cs.wght.cor.2_3f = paste("chkpnt:2; 3 fish",'Eq: y=', round(lm.unbent.cs.wght.2_3f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.2_3f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.2_3f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.2_3f$coefficients[8])
eq.unbent.cs.wght.cor.2_6f = paste("chkpnt:2; 6 fish",'Eq: y=', round(lm.unbent.cs.wght.2_6f$coefficients[2], digits=3), 'x', '+', round(lm.unbent.cs.wght.2_6f$coefficients[1], digits=3), "; R^2=", round(sum.unbent.cs.wght.2_6f$adj.r.squared, digits=3), "; sig. = ",sum.unbent.cs.wght.2_6f$coefficients[8])

eq.unbent.cs.wght.cor.lst = list(c(eq.unbent.cs.wght.cor.0_0f,eq.unbent.cs.wght.cor.0_3f,eq.unbent.cs.wght.cor.0_6f,eq.unbent.cs.wght.cor.1_0f,eq.unbent.cs.wght.cor.1_3f,eq.unbent.cs.wght.cor.1_6f,eq.unbent.cs.wght.cor.2_0f,eq.unbent.cs.wght.cor.2_3f,eq.unbent.cs.wght.cor.2_6f))

eq.unbent.cs.wght.cor.u = rbindlist(lapply(eq.unbent.cs.wght.cor.lst, data.frame),fill=T)
eq.unbent.cs.wght.cor.u[is.na(eq.unbent.cs.wght.cor.u)] = ""

# write.csv(x=eq.unbent.cs.wght.cor.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/size.weight.subset_",name.ver.date,".csv"))
write.xlsx2(x=eq.unbent.cs.wght.cor.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="size.weight.subset", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# Plots ####

# Plotting shape changes between groups averages: ####
mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "Shape changes between groups averages_chkpnt.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=15)
par(mfrow =	c(3,2),mar=c(1,1,8,1)) # set up 6 graphs per plot: Chkpnt
plotRefToTarget(unbent.m.chkpnt.treat.0.0f,unbent.m.chkpnt.treat.0.3f,method="vector",links=l.links, mag=10, main="chkpnt0: 3f vs. 0f", cex.main=2)
plotRefToTarget(unbent.m.chkpnt.treat.0.0f,unbent.m.chkpnt.treat.0.6f,method="vector",links=l.links, mag=10, main="chkpnt0: 6f vs. 0f", cex.main=2)
plotRefToTarget(unbent.m.chkpnt.treat.1.0f,unbent.m.chkpnt.treat.1.3f,method="vector",links=l.links, mag=10, main="chkpnt1: 3f vs. 0f", cex.main=2)
plotRefToTarget(unbent.m.chkpnt.treat.1.0f,unbent.m.chkpnt.treat.1.6f,method="vector",links=l.links, mag=10, main="chkpnt1: 6f vs. 0f", cex.main=2)
plotRefToTarget(unbent.m.chkpnt.treat.2.0f,unbent.m.chkpnt.treat.2.3f,method="vector",links=l.links, mag=10, main="chkpnt2: 3f vs. 0f", cex.main=2)
plotRefToTarget(unbent.m.chkpnt.treat.2.0f,unbent.m.chkpnt.treat.2.6f,method="vector",links=l.links, mag=10, main="chkpnt2: 6f vs. 0f", cex.main=2)
dev.off()

layout(1) # reset to 1 graph per plot

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_No_Outliers_chkpnt_0-2.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
par(mfrow =	c(2,2),mar=c(1,1,8,1)) # set up 6 graphs per plot: Chkpnt
plotAllSpecimens(unbent.sub.chkpnt.0, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1))
plotAllSpecimens(unbent.sub.chkpnt.1, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1))
plotAllSpecimens(unbent.sub.chkpnt.2, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1))
# plotAllSpecimens(unbent.sub.chkpnt.3, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1)) # Do not run if checkpoint 3 is removed: ####
dev.off()
layout(1) # reset to 1 graph per plot


# Plotting unbent AllSpecimens without Outliers: chekpoints 0-3
mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_No_Outliers_chkpnt_0.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plotAllSpecimens(unbent.sub.chkpnt.0, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1))
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_No_Outliers_chkpnt_1.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plotAllSpecimens(unbent.sub.chkpnt.1, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1))
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_No_Outliers_chkpnt_2.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plotAllSpecimens(unbent.sub.chkpnt.2, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1))
dev.off()

# Do not run if checkpoint 3 is removed: ####
# mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "unbent_AllSpecimens_No_Outliers_chkpnt_3.jpg"))
# jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
# plotAllSpecimens(unbent.sub.chkpnt.3, label=F, mean=T, plot.param=list(mean.bg="yellow", mean.cex=1.1))
# dev.off()

layout(1) # reset to 1 graph per plot
# --------------------------------------------------------------------------------------------------------------

# PCA ####
# treat-checkpoint: ####
larvae.unbent.allo = procD.allometry(f1=shape~cs, f2=~gp2, logsz=T, data=unbent.gdf, iter=999, RRPP=T, print.progress=T)
larvae.unbent.anv = procD.lm(larvae.unbent.allo$formula, data=larvae.unbent.allo$data, iter=999, RRPP=T, print.progress=T) # Obtaining size-adjusted residuals (and allometry-free shapes)
summary(larvae.unbent.anv)
larvae.unbent.shape.resid = arrayspecs(A= larvae.unbent.anv$residuals, p=dim(unbent.gdf$shape)[1], k=dim(unbent.gdf$shape)[2]) # 3D array of size-adjusted residuals
larvae.unbent.adj.shape = larvae.unbent.shape.resid + array(larvae.unbent.gpa$consensus, dim(larvae.unbent.shape.resid)) # make allometry-free shapes

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "PredLine.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plot(larvae.unbent.allo, method="PredLine", label=NULL, gp.label=NULL, pt.col=NULL, shapes=F)
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "RegScore.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
plot(larvae.unbent.allo, method="RegScore", label=NULL, gp.label=NULL, pt.col=NULL, shapes=F)
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "pca.chkpnt.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
pca.chkpnt = plotTangentSpace(larvae.unbent.adj.shape, groups = l.classifiers$chkpnt, legend=T) # PCA of allometry-free shape
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "pca.treat.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
pca.treat = plotTangentSpace(larvae.unbent.adj.shape, groups = l.classifiers$treat, legend=T) # PCA of allometry-free shape
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "pca.chkpnt.treat.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=20)
pca.chkpnt.treat = plotTangentSpace(larvae.unbent.adj.shape, groups = factor(paste(l.classifiers$chkpnt, l.classifiers$treat)), legend=T) # PCA of allometry-free shape
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "pca.treat.chkpnt.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=20)
pca.treat.chkpnt = plotTangentSpace(larvae.unbent.adj.shape, groups = factor(paste(l.classifiers$treat, l.classifiers$chkpnt)), legend=T) # PCA of allometry-free shape
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "pca.site.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=25)
pca.site = plotTangentSpace(larvae.unbent.adj.shape, groups = l.classifiers$site, legend=T) # PCA of allometry-free shape
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "pca.fem.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=20)
pca.fem = plotTangentSpace(larvae.unbent.adj.shape, groups = l.classifiers$fem, legend=T) # PCA of allometry-free shape
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "pca.site.treat.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=20)
pca.site.treat = plotTangentSpace(larvae.unbent.adj.shape, groups = factor(paste(l.classifiers$site, l.classifiers$treat)), legend=T) # PCA of allometry-free shape
dev.off()

mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "pca.site.chkpnt.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100, pointsize=20)
pca.site.chkpnt = plotTangentSpace(larvae.unbent.adj.shape, groups = factor(paste(l.classifiers$site, l.classifiers$chkpnt)), legend=T) # PCA of allometry-free shape
dev.off()

# SITE: graph of the proportion of variance explained by each PC: ####
mypath = file.path("F:/Documents/Avi_LeonLab/Avi_files/pics/Automatic_pics", paste(name.ver.date, "Var.site.jpg"))
jpeg(file=mypath, width=1280, height=960, units="px", quality=100)
unbent.pvar.site = (pca.site$sdev^2)/(sum(pca.site$sdev^2))
names(unbent.pvar.site) = seq(1:length(unbent.pvar.site))
barplot(unbent.pvar.site, xlab= "Principal Components", ylab = "% Variance")
dev.off()

unbent.pvar.site[1:5] # variance of the first five pc
sum(unbent.pvar.site[1:5]) # sum of variance of the first five pc
# --------------------------------------------------------------------------------------------------------------

# TRAJECTORY ANALYSIS ####
unbent.gdf$chkpnt = droplevels(unbent.gdf$chkpnt)
levels(unbent.gdf$chkpnt)
length(unbent.gdf$chkpnt)
dim(unbent.gdf$shape)
larvae.unbent.ta = trajectory.analysis(f1=shape ~ chkpnt, traj.pts=3, data=unbent.gdf, iter=999, print.progress=T)
# str, dim, length, summary, head, tail
# --------------------------------------------------------------------------------------------------------------

# advance.procD.lm ####

# Log(cs) effect given intercept only ####
lm.a00 = advanced.procD.lm(shape ~1, ~log(cs), iter=999, effect.type = "F", data=unbent.gdf, formula=T, print.progress=T)
lm.a00.at = as.data.frame(lm.a00$anova.table)
lm.a00.at1 = cbind(paste("",rownames(lm.a00.at)), lm.a00.at)

lm.a00.lst = list(lm.a00.at1)
lm.a00.u = rbindlist(lapply(lm.a00.lst, data.frame),fill=T)
lm.a00.u[is.na(lm.a00.u)] = ""

# write.csv(x=lm.a00.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/00.size_",name.ver.date,".csv"))
write.xlsx2(x=lm.a00.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="00.size", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# TREAT effect given chkpnt & fem & site effect ####
lm.a01 = advanced.procD.lm(shape ~log(cs)+site/fem+chkpnt, ~log(cs)+site/fem+chkpnt+treat+DtDpst, iter=999, effect.type = "F", groups=~l.classifiers$treat, slope=~log(cs), data=unbent.gdf, formula=F, print.progress=T)
lm.a01.at = as.data.frame(lm.a01$anova.table)
lm.a01.at1 = cbind(paste("",rownames(lm.a01.at)), lm.a01.at)

lm.a01.treat.rslt01 = c("Pairwise: 0f vs. 3f", round(lm.a01$Z.slopes.dist["0f","3f"],digits=3), round(lm.a01$P.slopes.dist["0f","3f"],digits=3), round(lm.a01$Z.slopes.cor["0f","3f"],digits=3),round(lm.a01$P.slopes.cor["0f","3f"],digits=3))
lm.a01.treat.rslt02 = c("Pairwise: 0f vs. 6f", round(lm.a01$Z.slopes.dist["0f","6f"],digits=3), round(lm.a01$P.slopes.dist["0f","6f"],digits=3), round(lm.a01$Z.slopes.cor["0f","6f"],digits=3),round(lm.a01$P.slopes.cor["0f","6f"],digits=3))
lm.a01.treat.rslt03 = c("Pairwise: 3f vs. 6f", round(lm.a01$Z.slopes.dist["3f","6f"],digits=3), round(lm.a01$P.slopes.dist["3f","6f"],digits=3), round(lm.a01$Z.slopes.cor["3f","6f"],digits=3),round(lm.a01$P.slopes.cor["3f","6f"],digits=3))

lm.a01.df = rbind(lm.a01.treat.rslt01, lm.a01.treat.rslt02, lm.a01.treat.rslt03)
colnames(lm.a01.df) = c("Test","Z.dist","P.dist","Z.cor","P.cor")
lm.a01.df = rbind(colnames(lm.a01.df),lm.a01.df)

lm.a01.lst = list(lm.a01.at1,lm.a01.df)
lm.a01.u = rbindlist(lapply(lm.a01.lst, data.frame),fill=T)
lm.a01.u[is.na(lm.a01.u)] = ""

# write.csv(x=lm.a01.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/01.treat_",name.ver.date,".csv"))
write.xlsx2(x=lm.a01.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="01.treat", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# CHKPNT effect given treat effect ####
lm.a02 = advanced.procD.lm(shape ~log(cs)+treat+DtDpst, ~log(cs)+chkpnt+treat+DtDpst, iter=999, effect.type = "F", data=unbent.gdf, print.progress=T)
# groups=~l.classifiers$chkpnt, slope=~log(cs), 
# +site/fem
lm.a02.at = as.data.frame(lm.a02$anova.table)
lm.a02.at1 = cbind(paste("",rownames(lm.a02.at)), lm.a02.at)

# lm.a02.chkpnt.rslt01 = c("Pairwise: 0 vs. 1", round(lm.a02$Z.slopes.dist["0","1"],digits=3), round(lm.a02$P.slopes.dist["0","1"],digits=3), round(lm.a02$Z.slopes.cor["0","1"],digits=3),round(lm.a02$P.slopes.cor["0","1"],digits=3))
# lm.a02.chkpnt.rslt02 = c("Pairwise: 0 vs. 2", round(lm.a02$Z.slopes.dist["0","2"],digits=3), round(lm.a02$P.slopes.dist["0","2"],digits=3), round(lm.a02$Z.slopes.cor["0","2"],digits=3),round(lm.a02$P.slopes.cor["0","2"],digits=3))
# lm.a02.chkpnt.rslt03 = c("Pairwise: 1 vs. 2", round(lm.a02$Z.slopes.dist["1","2"],digits=3), round(lm.a02$P.slopes.dist["1","2"],digits=3), round(lm.a02$Z.slopes.cor["1","2"],digits=3),round(lm.a02$P.slopes.cor["1","2"],digits=3))

# lm.a02.df = rbind(lm.a02.chkpnt.rslt01, lm.a02.chkpnt.rslt02, lm.a02.chkpnt.rslt03)
# colnames(lm.a02.df) = c("Test","Z.dist","P.dist","Z.cor","P.cor")
# lm.a02.df = rbind(colnames(lm.a02.df),lm.a02.df)

lm.a02.lst = list(lm.a02.at1)
# lm.a02.lst = list(lm.a02.at1,lm.a02.df)
lm.a02.u = rbindlist(lapply(lm.a02.lst, data.frame),fill=T)
lm.a02.u[is.na(lm.a02.u)] = ""

# write.csv(x=lm.a02.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/02.chkpnt_",name.ver.date,".csv"))
write.xlsx2(x=lm.a02.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="02.chkpnt", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# FEMALE effect given treat & chkpnt & site effects ####
lm.a03.fem = advanced.procD.lm(shape ~log(cs)+chkpnt+treat+site, ~log(cs)+chkpnt+treat+site/fem, iter=999, effect.type = "F", groups=~l.classifiers$fem, slope=~log(cs), data=unbent.gdf, print.progress=T)
lm.a03.fem.at = as.data.frame(lm.a03.fem$anova.table)
lm.a03.at1 = cbind(paste("",rownames(lm.a03.fem.at)), lm.a03.fem.at)

lm.a03.fem.rslt01 = c("Pairwise: BLD.F2 vs. ALN.F1", round(lm.a03.fem$Z.slopes.dist["BLD.F2","ALN.F1"],digits=3), round(lm.a03.fem$P.slopes.dist["BLD.F2","ALN.F1"],digits=3), round(lm.a03.fem$Z.slopes.cor["BLD.F2","ALN.F1"],digits=3),round(lm.a03.fem$P.slopes.cor["BLD.F2","ALN.F1"],digits=3))
lm.a03.fem.rslt02 = c("Pairwise: ALN.F3 vs. BLD.F2", round(lm.a03.fem$Z.slopes.dist["ALN.F3","BLD.F2"],digits=3), round(lm.a03.fem$P.slopes.dist["ALN.F3","BLD.F2"],digits=3), round(lm.a03.fem$Z.slopes.cor["ALN.F3","BLD.F2"],digits=3),round(lm.a03.fem$P.slopes.cor["ALN.F3","BLD.F2"],digits=3))
lm.a03.fem.rslt03 = c("Pairwise: KKB.F5 vs. BLD.F2", round(lm.a03.fem$Z.slopes.dist["KKB.F5","BLD.F2"],digits=3), round(lm.a03.fem$P.slopes.dist["KKB.F5","BLD.F2"],digits=3), round(lm.a03.fem$Z.slopes.cor["KKB.F5","BLD.F2"],digits=3),round(lm.a03.fem$P.slopes.cor["KKB.F5","BLD.F2"],digits=3))
lm.a03.fem.rslt04 = c("Pairwise: KKB.F7 vs. BLD.F2", round(lm.a03.fem$Z.slopes.dist["KKB.F7","BLD.F2"],digits=3), round(lm.a03.fem$P.slopes.dist["KKB.F7","BLD.F2"],digits=3), round(lm.a03.fem$Z.slopes.cor["KKB.F7","BLD.F2"],digits=3),round(lm.a03.fem$P.slopes.cor["KKB.F7","BLD.F2"],digits=3))
lm.a03.fem.rslt05 = c("Pairwise: BLD.F2 vs. BLD.F3", round(lm.a03.fem$Z.slopes.dist["BLD.F2","BLD.F3"],digits=3), round(lm.a03.fem$P.slopes.dist["BLD.F2","BLD.F3"],digits=3), round(lm.a03.fem$Z.slopes.cor["BLD.F2","BLD.F3"],digits=3),round(lm.a03.fem$P.slopes.cor["BLD.F2","BLD.F3"],digits=3))

lm.a03.fem.rslt06 = c("Pairwise: BLD.F3 vs. ALN.F1", round(lm.a03.fem$Z.slopes.dist["BLD.F3","ALN.F1"],digits=3), round(lm.a03.fem$P.slopes.dist["BLD.F3","ALN.F1"],digits=3), round(lm.a03.fem$Z.slopes.cor["BLD.F3","ALN.F1"],digits=3),round(lm.a03.fem$P.slopes.cor["BLD.F3","ALN.F1"],digits=3))
lm.a03.fem.rslt07 = c("Pairwise: BLD.F3 vs. ALN.F3", round(lm.a03.fem$Z.slopes.dist["BLD.F3","ALN.F3"],digits=3), round(lm.a03.fem$P.slopes.dist["BLD.F3","ALN.F3"],digits=3), round(lm.a03.fem$Z.slopes.cor["BLD.F3","ALN.F3"],digits=3),round(lm.a03.fem$P.slopes.cor["BLD.F3","ALN.F3"],digits=3))
lm.a03.fem.rslt08 = c("Pairwise: BLD.F3 vs. KKB.F5", round(lm.a03.fem$Z.slopes.dist["BLD.F3","KKB.F5"],digits=3), round(lm.a03.fem$P.slopes.dist["BLD.F3","KKB.F5"],digits=3), round(lm.a03.fem$Z.slopes.cor["BLD.F3","KKB.F5"],digits=3),round(lm.a03.fem$P.slopes.cor["BLD.F3","KKB.F5"],digits=3))
lm.a03.fem.rslt09 = c("Pairwise: BLD.F3 vs. KKB.F7", round(lm.a03.fem$Z.slopes.dist["BLD.F3","KKB.F7"],digits=3), round(lm.a03.fem$P.slopes.dist["BLD.F3","KKB.F7"],digits=3), round(lm.a03.fem$Z.slopes.cor["BLD.F3","KKB.F7"],digits=3),round(lm.a03.fem$P.slopes.cor["BLD.F3","KKB.F7"],digits=3))

lm.a03.fem.rslt10 = c("Pairwise: ALN.F1 vs. ALN.F3", round(lm.a03.fem$Z.slopes.dist["ALN.F1","ALN.F3"],digits=3), round(lm.a03.fem$P.slopes.dist["ALN.F1","ALN.F3"],digits=3), round(lm.a03.fem$Z.slopes.cor["ALN.F1","ALN.F3"],digits=3),round(lm.a03.fem$P.slopes.cor["ALN.F1","ALN.F3"],digits=3))
lm.a03.fem.rslt11 = c("Pairwise: KKB.F5 vs. ALN.F1", round(lm.a03.fem$Z.slopes.dist["KKB.F5","ALN.F1"],digits=3), round(lm.a03.fem$P.slopes.dist["KKB.F5","ALN.F1"],digits=3), round(lm.a03.fem$Z.slopes.cor["KKB.F5","ALN.F1"],digits=3),round(lm.a03.fem$P.slopes.cor["KKB.F5","ALN.F1"],digits=3))
lm.a03.fem.rslt12 = c("Pairwise: KKB.F7 vs. ALN.F1", round(lm.a03.fem$Z.slopes.dist["KKB.F7","ALN.F1"],digits=3), round(lm.a03.fem$P.slopes.dist["KKB.F7","ALN.F1"],digits=3), round(lm.a03.fem$Z.slopes.cor["KKB.F7","ALN.F1"],digits=3),round(lm.a03.fem$P.slopes.cor["KKB.F7","ALN.F1"],digits=3))

lm.a03.fem.rslt13 = c("Pairwise: KKB.F5 vs. ALN.F3", round(lm.a03.fem$Z.slopes.dist["KKB.F5","ALN.F3"],digits=3), round(lm.a03.fem$P.slopes.dist["KKB.F5","ALN.F3"],digits=3), round(lm.a03.fem$Z.slopes.cor["KKB.F5","ALN.F3"],digits=3),round(lm.a03.fem$P.slopes.cor["KKB.F5","ALN.F3"],digits=3))
lm.a03.fem.rslt14 = c("Pairwise: KKB.F7 vs. ALN.F3", round(lm.a03.fem$Z.slopes.dist["KKB.F7","ALN.F3"],digits=3), round(lm.a03.fem$P.slopes.dist["KKB.F7","ALN.F3"],digits=3), round(lm.a03.fem$Z.slopes.cor["KKB.F7","ALN.F3"],digits=3),round(lm.a03.fem$P.slopes.cor["KKB.F7","ALN.F3"],digits=3))

lm.a03.fem.rslt15 = c("Pairwise: KKB.F5 vs. KKB.F7", round(lm.a03.fem$Z.slopes.dist["KKB.F5","KKB.F7"],digits=3), round(lm.a03.fem$P.slopes.dist["KKB.F5","KKB.F7"],digits=3), round(lm.a03.fem$Z.slopes.cor["KKB.F5","KKB.F7"],digits=3),round(lm.a03.fem$P.slopes.cor["KKB.F5","KKB.F7"],digits=3))

lm.a03.df = rbind(lm.a03.fem.rslt01, lm.a03.fem.rslt02, lm.a03.fem.rslt03, lm.a03.fem.rslt04, lm.a03.fem.rslt05, lm.a03.fem.rslt06, lm.a03.fem.rslt07, lm.a03.fem.rslt08, lm.a03.fem.rslt09, lm.a03.fem.rslt10, lm.a03.fem.rslt11, lm.a03.fem.rslt12, lm.a03.fem.rslt13, lm.a03.fem.rslt14, lm.a03.fem.rslt15)
colnames(lm.a03.df) = c("Test","Z.dist","P.dist","Z.cor","P.cor")
lm.a03.df = rbind(colnames(lm.a03.df),lm.a03.df)

lm.a03.lst = list(lm.a03.at1,lm.a03.df)
lm.a03.u = rbindlist(lapply(lm.a03.lst, data.frame),fill=T)
lm.a03.u[is.na(lm.a03.u)] = ""

# write.csv(x=lm.a03.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/03.female_",name.ver.date,".csv"))
write.xlsx2(x=lm.a03.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="03.female", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# SITE effect given treat & chkpnt effects ####
# (since females are unique, like they should be, and are nested in sites, you can't enter females in any of the models, reduced and full, while investigaing sites)
lm.a04.site = advanced.procD.lm(shape ~log(cs)+chkpnt+treat, ~log(cs)+chkpnt+treat+site, iter=999, effect.type = "F", groups=~l.classifiers$site, slope=~log(cs), data=unbent.gdf, print.progress=T)
lm.a04.site.at = as.data.frame(lm.a04.site$anova.table)
lm.a04.site.at1 = cbind(paste("",rownames(lm.a04.site.at)), lm.a04.site.at)

lm.a04.site.rslt01 = c("Pairwise: BLD vs. ALN", round(lm.a04.site$Z.slopes.dist["BLD","ALN"],digits=3), round(lm.a04.site$P.slopes.dist["BLD","ALN"],digits=3), round(lm.a04.site$Z.slopes.cor["BLD","ALN"],digits=3),round(lm.a04.site$P.slopes.cor["BLD","ALN"],digits=3))
lm.a04.site.rslt02 = c("Pairwise: BLD vs. KKB", round(lm.a04.site$Z.slopes.dist["BLD","KKB"],digits=3), round(lm.a04.site$P.slopes.dist["BLD","KKB"],digits=3), round(lm.a04.site$Z.slopes.cor["BLD","KKB"],digits=3),round(lm.a04.site$P.slopes.cor["BLD","KKB"],digits=3))
lm.a04.site.rslt03 = c("Pairwise: ALN vs. KKB", round(lm.a04.site$Z.slopes.dist["ALN","KKB"],digits=3), round(lm.a04.site$P.slopes.dist["ALN","KKB"],digits=3), round(lm.a04.site$Z.slopes.cor["ALN","KKB"],digits=3),round(lm.a04.site$P.slopes.cor["ALN","KKB"],digits=3))

lm.a04.site.df = rbind(lm.a04.site.rslt01, lm.a04.site.rslt02, lm.a04.site.rslt03)
colnames(lm.a04.site.df) = c("Test","Z.dist","P.dist","Z.cor","P.cor")
lm.a04.site.df = rbind(colnames(lm.a04.site.df),lm.a04.site.df)

lm.a04.site.lst = list(lm.a04.site.at1,lm.a04.site.df)
lm.a04.site.u = rbindlist(lapply(lm.a04.site.lst, data.frame),fill=T)
lm.a04.site.u[is.na(lm.a04.site.u)] = ""

# write.csv(x=lm.a04.site.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/04.site_",name.ver.date,".csv"))
write.xlsx2(x=lm.a04.site.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="04.site", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# TREAT*CHKPNT effect given treat & chkpnt & fem & site effect ####
lm.a05 = advanced.procD.lm(shape ~log(cs)+site/fem+chkpnt+treat, ~log(cs)+site/fem+chkpnt*treat, iter=999, effect.type = "F", groups=~gp2, slope=~log(cs), data=unbent.gdf, print.progress=T)
lm.a05.at = as.data.frame(lm.a05$anova.table)
lm.a05.at1 = cbind(paste("",rownames(lm.a05.at)), lm.a05.at)

lm.a05.gp2_0.rslt01 = c("Pairwise: chkpnt_0:0f vs. chkpnt_0:3f", round(lm.a05$Z.slopes.dist["0 0f","0 3f"],digits=3), round(lm.a05$P.slopes.dist["0 0f","0 3f"],digits=3), round(lm.a05$Z.slopes.cor["0 0f","0 3f"],digits=3),round(lm.a05$P.slopes.cor["0 0f","0 3f"],digits=3))
lm.a05.gp2_0.rslt02 = c("Pairwise: chkpnt_0:0f vs. chkpnt_0:6f", round(lm.a05$Z.slopes.dist["0 0f","0 6f"],digits=3), round(lm.a05$P.slopes.dist["0 0f","0 6f"],digits=3), round(lm.a05$Z.slopes.cor["0 0f","0 6f"],digits=3),round(lm.a05$P.slopes.cor["0 0f","0 6f"],digits=3))
lm.a05.gp2_0.rslt03 = c("Pairwise: chkpnt_0:3f vs. chkpnt_0:6f", round(lm.a05$Z.slopes.dist["0 3f","0 6f"],digits=3), round(lm.a05$P.slopes.dist["0 3f","0 6f"],digits=3), round(lm.a05$Z.slopes.cor["0 3f","0 6f"],digits=3),round(lm.a05$P.slopes.cor["0 3f","0 6f"],digits=3))

lm.a05.gp2_1.rslt01 = c("Pairwise: chkpnt_1:0f vs. chkpnt_1:3f", round(lm.a05$Z.slopes.dist["1 0f","1 3f"],digits=3), round(lm.a05$P.slopes.dist["1 0f","1 3f"],digits=3), round(lm.a05$Z.slopes.cor["1 0f","1 3f"],digits=3),round(lm.a05$P.slopes.cor["1 0f","1 3f"],digits=3))
lm.a05.gp2_1.rslt02 = c("Pairwise: chkpnt_1:0f vs. chkpnt_1:6f", round(lm.a05$Z.slopes.dist["1 0f","1 6f"],digits=3), round(lm.a05$P.slopes.dist["1 0f","1 6f"],digits=3), round(lm.a05$Z.slopes.cor["1 0f","1 6f"],digits=3),round(lm.a05$P.slopes.cor["1 0f","1 6f"],digits=3))
lm.a05.gp2_1.rslt03 = c("Pairwise: chkpnt_1:3f vs. chkpnt_1:6f", round(lm.a05$Z.slopes.dist["1 3f","1 6f"],digits=3), round(lm.a05$P.slopes.dist["1 3f","1 6f"],digits=3), round(lm.a05$Z.slopes.cor["1 3f","1 6f"],digits=3),round(lm.a05$P.slopes.cor["1 3f","1 6f"],digits=3))

lm.a05.gp2_2.rslt01 = c("Pairwise: chkpnt_2:0f vs. chkpnt_2:3f", round(lm.a05$Z.slopes.dist["2 0f","2 3f"],digits=3), round(lm.a05$P.slopes.dist["2 0f","2 3f"],digits=3), round(lm.a05$Z.slopes.cor["2 0f","2 3f"],digits=3),round(lm.a05$P.slopes.cor["2 0f","2 3f"],digits=3))
lm.a05.gp2_2.rslt02 = c("Pairwise: chkpnt_2:0f vs. chkpnt_2:6f", round(lm.a05$Z.slopes.dist["2 0f","2 6f"],digits=3), round(lm.a05$P.slopes.dist["2 0f","2 6f"],digits=3), round(lm.a05$Z.slopes.cor["2 0f","2 6f"],digits=3),round(lm.a05$P.slopes.cor["2 0f","2 6f"],digits=3))
lm.a05.gp2_2.rslt03 = c("Pairwise: chkpnt_2:3f vs. chkpnt_2:6f", round(lm.a05$Z.slopes.dist["2 3f","2 6f"],digits=3), round(lm.a05$P.slopes.dist["2 3f","2 6f"],digits=3), round(lm.a05$Z.slopes.cor["2 3f","2 6f"],digits=3),round(lm.a05$P.slopes.cor["2 3f","2 6f"],digits=3))

lm.a05.gp2.df = rbind(lm.a05.gp2_0.rslt01, lm.a05.gp2_0.rslt02, lm.a05.gp2_0.rslt03, lm.a05.gp2_1.rslt01, lm.a05.gp2_1.rslt02, lm.a05.gp2_1.rslt03, lm.a05.gp2_2.rslt01, lm.a05.gp2_2.rslt02, lm.a05.gp2_2.rslt03)
colnames(lm.a05.gp2.df) = c("Test","Z.dist","P.dist","Z.cor","P.cor")
lm.a05.gp2.df = rbind(colnames(lm.a05.gp2.df),lm.a05.gp2.df)

lm.a05.lst = list(lm.a05.at1)
lm.a05.u = rbindlist(lapply(lm.a05.lst, data.frame),fill=T)
lm.a05.u[is.na(lm.a05.u)] = ""

# write.csv(x=lm.a05.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/05.treat.chkpnt_",name.ver.date,".csv"))
write.xlsx2(x=lm.a05.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="05.treat.chkpnt", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# Weight effect given intercept effect only: ####
lm.a06 = advanced.procD.lm(shape ~1, ~log((wght)^(1/3)), iter=999, effect.type = "F", data=unbent.gdf, formula=T, print.progress=T)
lm.a06.at = as.data.frame(lm.a06$anova.table)
lm.a06.at1 = cbind(paste("",rownames(lm.a06.at)), lm.a06.at)

lm.a06.lst = list(lm.a06.at1)
lm.a06.u = rbindlist(lapply(lm.a06.lst, data.frame),fill=T)
lm.a06.u[is.na(lm.a06.u)] = ""

# write.csv(x=lm.a06.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/06.Weight_",name.ver.date,".csv"))
write.xlsx2(x=lm.a06.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="06.Weight", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# TREAT*SIZE effect given treat & size & fem & site effect ####
lm.a07 = advanced.procD.lm(shape ~log(cs) + treat + site/fem, ~treat * log(cs) + site/fem, iter=999, effect.type = "F", data=unbent.gdf, formula=T, print.progress=T)
lm.a07.at = as.data.frame(lm.a07$anova.table)
lm.a07.at1 = cbind(paste("",rownames(lm.a07.at)), lm.a07.at)

lm.a07.lst = list(lm.a07.at1)
lm.a07.u = rbindlist(lapply(lm.a07.lst, data.frame),fill=T)
lm.a07.u[is.na(lm.a07.u)] = ""

# write.csv(x=lm.a07.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/07.treat.log(cs)_",name.ver.date,".csv"))
write.xlsx2(x=lm.a07.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="07.treat.log(cs)", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# TREAT*SIZE effect given size effect ####
lm.a08 = advanced.procD.lm(shape ~log(cs), ~ log(cs) * treat, iter=999, effect.type = "F", data=unbent.gdf, formula=T, print.progress=T)
lm.a08.at = as.data.frame(lm.a08$anova.table)
lm.a08.at1 = cbind(paste("",rownames(lm.a08.at)), lm.a08.at)

lm.a08.lst = list(lm.a08.at1)
lm.a08.u = rbindlist(lapply(lm.a08.lst, data.frame),fill=T)
lm.a08.u[is.na(lm.a08.u)] = ""

# write.csv(x=lm.a08.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/08.treat.log(cs).1_",name.ver.date,".csv"))
write.xlsx2(x=lm.a08.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="08.treat.log(cs).1", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# CHKPNT*SIZE effect given size effect ####
lm.a09 = advanced.procD.lm(shape ~log(cs), ~ log(cs) * chkpnt, iter=999, effect.type = "F", data=unbent.gdf, formula=T, print.progress=T)
lm.a09.at = as.data.frame(lm.a09$anova.table)
lm.a09.at1 = cbind(paste("",rownames(lm.a09.at)), lm.a09.at)

lm.a09.lst = list(lm.a09.at1)
lm.a09.u = rbindlist(lapply(lm.a09.lst, data.frame),fill=T)
lm.a09.u[is.na(lm.a09.u)] = ""

# write.csv(x=lm.a09.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/09.chkpnt.log(cs).1_",name.ver.date,".csv"))
write.xlsx2(x=lm.a09.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="09.chkpnt.log(cs).1", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# SITE*SIZE effect given size effect ####
lm.a10 = advanced.procD.lm(shape ~log(cs), ~ log(cs) * site, iter=999, effect.type = "F", data=unbent.gdf, formula=T, print.progress=T)
lm.a10.at = as.data.frame(lm.a10$anova.table)
lm.a10.at1 = cbind(paste("",rownames(lm.a10.at)), lm.a10.at)

lm.a10.lst = list(lm.a10.at1)
lm.a10.u = rbindlist(lapply(lm.a10.lst, data.frame),fill=T)
lm.a10.u[is.na(lm.a10.u)] = ""

# write.csv(x=lm.a10.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/10.chkpnt.log(cs).1_",name.ver.date,".csv"))
write.xlsx2(x=lm.a10.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="10.chkpnt.log(cs).1", col.names=T, row.names=T, append=T)

# --------------------------------------------------------------------------------------------------------------
# SITE/FEM*SIZE effect given size effect ####
lm.a11 = advanced.procD.lm(shape ~log(cs), ~ log(cs) * site/fem, iter=999, effect.type = "F", data=unbent.gdf, formula=T, print.progress=T)
lm.a11.at = as.data.frame(lm.a11$anova.table)
lm.a11.at1 = cbind(paste("",rownames(lm.a11.at)), lm.a11.at)

lm.a11.lst = list(lm.a11.at1)
lm.a11.u = rbindlist(lapply(lm.a11.lst, data.frame),fill=T)
lm.a11.u[is.na(lm.a11.u)] = ""

# write.csv(x=lm.a11.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/CSV/11.chkpnt.log(cs).1_",name.ver.date,".csv"))
write.xlsx2(x=lm.a11.u,file=paste("F:/Documents/Avi_LeonLab/Avi_files/XLSX/",name.ver.date,".xlsx"), sheetName="11.chkpnt.log(cs).1", col.names=T, row.names=T, append=T)
# --------------------------------------------------------------------------------------------------------------

# procD.lm ####

seqSS.treat = procD.lm(shape ~log(cs) * treat, data=unbent.gdf, print.progress=T)
seqSS.chkpnt = procD.lm(shape ~log(cs) * chkpnt, data=unbent.gdf, print.progress=T)
seqSS.site = procD.lm(shape ~log(cs) * site, data=unbent.gdf, print.progress=T)
seqSS.site.fem = procD.lm(shape ~log(cs) * site/fem, data=unbent.gdf, print.progress=T)
# --------------------------------------------------------------------------------------------------------------