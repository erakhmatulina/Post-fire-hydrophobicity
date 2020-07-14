B<-c(7685675,5003325,4928675,3617500,2724775,1914075,1381150,1161950,1099775,1023800)*10^-6
VB<-c(2882775,2773025, 1228250,1180025,1168400,966575,917125,916500,911700,837875)*10^-6

VBcrop1<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/VB_1_bseCrop1.csv', header=T)
NBcrop1<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/NB_2_bseCrop1.csv', header=T)
Bcrop1<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/B_2_bseCrop1.csv', header=T)

VBcrop2<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/VB_1_bseCrop2.csv', header=T)
NBcrop2<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/NB_2_bseCrop2.csv', header=T)
Bcrop2<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/B_2_bseCrop2.csv', header=T)

VBcrop3<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/VB_1_bseCrop3.csv', header=T)
NBcrop3<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/NB_2_bseCrop3.csv', header=T)
Bcrop3<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/B_2_bseCrop3.csv', header=T)

Bx30<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/B_1_bse_x30.csv', header=T)
WDFT<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/1_3_6_bsex100.csv', header=T)
NB50bse<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/NB_2_bsex100.csv', header=T)
B50bse<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/B_2_bsex100.csv', header=T)
VB50bse<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/VB_1_bsex100.csv', header=T)


B60<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/B60_117.csv', header=T)
B50<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/B136_50.csv', header=T)
B40<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/B155_40.csv', header=T)

NB60<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/NB117_60.csv', header=T)
NB50<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/NB137_50.csv', header=T)
NB40<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/NB156_40.csv', header=T)

VB60<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/VB_121_60.csv', header=T)
VB50<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/VB148_50.csv', header=T)
VB40<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/Cropped/VB170_40.csv', header=T)

VBcrop1 <- VBcrop1[order(-VBcrop1$Area),]
NBcrop1 <- NBcrop1[order(-NBcrop1$Area),]
Bcrop1 <- Bcrop1[order(-Bcrop1$Area),]

VBcrop1 <- VBcrop1[VBcrop1$Area>=5000,]
NBcrop1 <- NBcrop1[NBcrop1$Area>=5000,]
Bcrop1 <- Bcrop1[Bcrop1$Area>=5000,]

VBcrop2 <- VBcrop2[order(-VBcrop2$Area),]
NBcrop2 <- NBcrop2[order(-NBcrop2$Area),]
Bcrop2 <- Bcrop2[order(-Bcrop2$Area),]

VBcrop2 <- VBcrop2[VBcrop2$Area>=500000,]
NBcrop2 <- NBcrop2[NBcrop2$Area>=500000,]
Bcrop2 <- Bcrop2[Bcrop2$Area>=500000,]

VBcrop3 <- VBcrop3[order(-VBcrop3$Area),]
NBcrop3 <- NBcrop3[order(-NBcrop3$Area),]
Bcrop3 <- Bcrop3[order(-Bcrop3$Area),]

VBcrop3 <- VBcrop3[VBcrop3$Area>=5000,]
NBcrop3 <- NBcrop3[NBcrop3$Area>=5000,]
Bcrop3 <- Bcrop3[Bcrop3$Area>=5000,]

Bx30 <- Bx30[Bx30$Area>=5000,]
WDFT <- WDFT[WDFT$Area>=5000,]
NB50bse <- NB50bse[NB50bse$Area>=5000,]
B50bse <- B50bse[B50bse$Area>=5000,]
VB50bse <- VB50bse[VB50bse$Area>=5000,]

VB50bse <- VB50bse[order(-VB50bse$Area),]
B50bse <- B50bse[order(-B50bse$Area),]
NB50bse <- NB50bse[order(-NB50bse$Area),]
WDFT <- WDFT[order(-WDFT$Area),]

VB40 <- VB40[order(-VB40$Area),]
VB50 <- VB50[order(-VB50$Area),]
VB60 <- VB60[order(-VB60$Area),]

VB40 <- VB40[VB40$Area>=5000,]
VB50 <- VB50[VB50$Area>=5000,]
VB60 <- VB60[VB60$Area>=5000,]

B40 <- B40[order(-B40$Area),]
B50 <- B50[order(-B50$Area),]
B60 <- B60[order(-B60$Area),]

B40 <- B40[B40$Area>=5000,]
B50 <- B50[B50$Area>=5000,]
B60 <- B60[B60$Area>=5000,]

NB40 <- NB40[order(-NB40$Area),]
NB50 <- NB50[order(-NB50$Area),]
NB60 <- NB60[order(-NB60$Area),]

NB40 <- NB40[NB40$Area>=5000,]
NB50 <- NB50[NB50$Area>=5000,]
NB60 <- NB60[NB60$Area>=5000,]

VB40$Area[1:10]*10^-6
VB50$Area[1:10]*10^-6
VB60$Area[1:10]*10^-6

B40$Area[1:10]*10^-6
B50$Area[1:10]*10^-6
B60$Area[1:10]*10^-6

NB40$Area[1:10]*10^-6
NB50$Area[1:10]*10^-6
NB60$Area[1:10]*10^-6

VB40$Area[1:10]/sum(VB40$Area)*100
VB50$Area[1:10]/sum(VB50$Area)*100
VB60$Area[1:10]/sum(VB60$Area)*100

sum(VB50bse$Area[1:10]/sum(VB50bse$Area)*100)
sum(B50bse$Area[1:10]/sum(B50bse$Area)*100)
sum(NB50bse$Area[1:10]/sum(NB50bse$Area)*100)
sum(WDFT$Area[1:10]/sum(WDFT$Area)*100)

VBcrop1$Area[1:10]/sum(VBcrop1$Area)*100
VBcrop2$Area[1:10]/sum(VBcrop2$Area)*100
VBcrop3$Area[1:10]/sum(VBcrop3$Area)*100
VBcropAvg<-(VBcrop1$Area[1:10]/sum(VBcrop1$Area)*100+
            VBcrop2$Area[1:10]/sum(VBcrop2$Area)*100+
            VBcrop3$Area[1:10]/sum(VBcrop3$Area)*100)/3

B40$Area[1:10]/sum(B40$Area)*100
B50$Area[1:10]/sum(B50$Area)*100
B60$Area[1:10]/sum(B60$Area)*100

NBcrop1$Area[1:10]/sum(NBcrop1$Area)*100
NBcrop2$Area[1:10]/sum(NBcrop2$Area)*100
NBcrop3$Area[1:10]/sum(NBcrop3$Area)*100
NBcropAvg<-(NBcrop1$Area[1:10]/sum(NBcrop1$Area)*100+
              NBcrop2$Area[1:10]/sum(NBcrop2$Area)*100+
              NBcrop3$Area[1:10]/sum(NBcrop3$Area)*100)/3

NB40$Area[1:10]/sum(NB40$Area)*100
NB50$Area[1:10]/sum(NB50$Area)*100
NB60$Area[1:10]/sum(NB60$Area)*100

Bcrop1$Area[1:10]/sum(Bcrop1$Area)*100
Bcrop2$Area[1:10]/sum(Bcrop2$Area)*100
Bcrop3$Area[1:10]/sum(Bcrop3$Area)*100
BcropAvg<-(Bcrop1$Area[1:10]/sum(Bcrop1$Area)*100+
              Bcrop2$Area[1:10]/sum(Bcrop2$Area)*100+
              Bcrop3$Area[1:10]/sum(Bcrop3$Area)*100)/3

ggplot()+geom_histogram(aes(VB40$Area[1:10]*10^-6, fill='red', alpha = 0.2))+
  geom_histogram(aes(B40$Area[1:10]*10^-6, fill='green',alpha = 0.2))+
  geom_histogram(aes(NB40$Area[1:10]*10^-6, fill='orange',alpha = 0.2))

ggplot()+geom_histogram(aes(log10(VB40$Area), y = ..density.., fill='red', alpha = 0.2))+
  geom_histogram(aes(log10(B40$Area),y = ..density.., alpha = 0.2))+
  geom_histogram(aes(log10(NB40$Area),y = ..density.., alpha = 0.2))

ggplot()+geom_histogram(aes(VB40$Area, y = ..density.., fill='red', alpha = 0.2))+
  geom_histogram(aes(B40$Area,y = ..density.., alpha = 0.2))+
  geom_histogram(aes(NB40$Area,y = ..density.., alpha = 0.2))+
  scale_y_log10()
  
plot(B40$Area, log="y", type='h')
barplot(VB40$Area,log="y",add=T)

library('G2Sd')
granplot(B40,x=2)

VB40 <- VB40[order(VB40$Area),]
VB50 <- VB50[order(VB50$Area),]
VB60 <- VB60[order(VB60$Area),]

B40 <- B40[order(B40$Area),]
B50 <- B50[order(B50$Area),]
B60 <- B60[order(B60$Area),]

NB40 <- NB40[order(NB40$Area),]
NB50 <- NB50[order(NB50$Area),]
NB60 <- NB60[order(NB60$Area),]

Bcrop1 <- Bcrop1[order(Bcrop1$Area),]
VBcrop1 <- VBcrop1[order(VBcrop1$Area),]
NBcrop1 <- NBcrop1[order(NBcrop1$Area),]

Bcrop2 <- Bcrop2[order(Bcrop2$Area),]
VBcrop2 <- VBcrop2[order(VBcrop2$Area),]
NBcrop2 <- NBcrop2[order(NBcrop2$Area),]

Bcrop3 <- Bcrop3[order(Bcrop3$Area),]
VBcrop3 <- VBcrop3[order(VBcrop3$Area),]
NBcrop3 <- NBcrop3[order(NBcrop3$Area),]

Bx30 <- Bx30[order(Bx30$Area),]
WDFT <- WDFT[order(WDFT$Area),]
NB50bse <- NB50bse[order(NB50bse$Area),]
B50bse <- B50bse[order(B50bse$Area),]
VB50bse <- VB50bse[order(VB50bse$Area),]

sum.sieve = sum(B50$Area)
class.weight = (B50$Area * 100)/sum.sieve
class.weight.cum = round(cumsum(class.weight),2)

sum.sieve2 = sum(VB50$Area)
class.weight2 = (VB50$Area * 100)/sum.sieve2
class.weight.cum2 = round(cumsum(class.weight2),2)

sum.sieve3 = sum(NB50$Area)
class.weight3 = (NB50$Area * 100)/sum.sieve3
class.weight.cum3 = round(cumsum(class.weight3),2)

sum.sieve3 = sum(NB50$Area)
class.weight3 = (NB50$Area * 100)/sum.sieve3
class.weight.cum3 = round(cumsum(class.weight3),2)

sum.sieve4 = sum(Bx30$Area)
class.weight4 = (Bx30$Area * 100)/sum.sieve4
class.weight.cum4 = round(cumsum(class.weight4),2)

sum.sieve5 = sum(WDFT$Area)
class.weight5 = (WDFT$Area * 100)/sum.sieve5
class.weight.cum5 = round(cumsum(class.weight5),2)


sum.sieve6 = sum(NB50bse$Area)
class.weight6 = (NB50bse$Area * 100)/sum.sieve6
class.weight.cum6 = round(cumsum(class.weight6),2)

sum.sieve7 = sum(B50bse$Area)
class.weight7 = (B50bse$Area * 100)/sum.sieve7
class.weight.cum7 = round(cumsum(class.weight7),2)

sum.sieve8 = sum(VB50bse$Area)
class.weight8 = (VB50bse$Area * 100)/sum.sieve8
class.weight.cum8 = round(cumsum(class.weight8),2)

plot(B50$Area*10^-6,class.weight.cum,pch = 16, cex=0.5, col='#662606', xlim=c(6000*10^-6,max(WDFT$Area,B50$Area,NB50$Area,VB50$Area)*10^-6),log = 'x', xlab=expression(paste('grain size [cm'^'2',']')),ylab='% finer')
points(B50bse$Area*10^-6,class.weight.cum7,pch=16, cex=0.5, col='#662606')
points(NB50$Area*10^-6,class.weight.cum3,pch=16,, cex=0.5, col='#ec7014')
points(NB50bse$Area*10^-6,class.weight.cum6,pch=16,, cex=0.5, col='#ec7014')
points(VB50$Area*10^-6,class.weight.cum2,pch=16, cex=0.5, col='#fec44f')
points(VB50bse$Area*10^-6,class.weight.cum8,pch=16, cex=0.5, col='#fec44f')
points(WDFT$Area*10^-6,class.weight.cum5,pch=16, cex=0.5, col='black')
legend('topleft',pch=16,col=c('#662606','#ec7014','#fec44f'), c('heated hydrophobic\nMED=16.5%\n','non-heated\nMED=6.5%', 'heated hydrophilic\nMED=0%'), cex=0.8,box.lty=0)

pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/GrainSize.pdf",   # The directory you want to save the file in
    width = 6.5, # The width of the plot in inches
    height = 4) # The height of the plot in inches

options(scipen=5)
plot(B50bse$Area*10^-6,class.weight.cum7,pch = 16, cex=0.5, col='#662606', xlim=c(6000*10^-6,max(WDFT$Area,B50bse$Area,NB50bse$Area,VB50bse$Area)*10^-6),log = 'x', xlab=expression(paste('aggregate size [cm'^'2',']')),ylab='% finer')
points(NB50bse$Area*10^-6,class.weight.cum6,pch=16,, cex=0.5, col='#ec7014')
points(VB50bse$Area*10^-6,class.weight.cum8,pch=16, cex=0.5, col='#fec44f')
points(WDFT$Area*10^-6,class.weight.cum5,pch=16, cex=0.5, col='black')
legend('topleft',pch=16,col=c('black','#662606','#ec7014','#fec44f'), c('WDFT Cycle 7\nMED=4%\n','heated hydrophobic\nMED=16.5%','non-heated\nMED=6.5%', 'heated hydrophilic\nMED=0%'), cex=0.8,box.lty=0, bg=NA)

dev.off()
#---------plot sub samples

Bcrop1 <- Bcrop1[order(Bcrop1$Area),]
VBcrop1 <- VBcrop1[order(VBcrop1$Area),]
NBcrop1 <- NBcrop1[order(NBcrop1$Area),]

sum.sieve = sum(Bcrop1$Area)
class.weight = (Bcrop1$Area * 100)/sum.sieve
class.weight.cum = round(cumsum(class.weight),2)

sum.sieve2 = sum(VBcrop1$Area)
class.weight2 = (VBcrop1$Area * 100)/sum.sieve2
class.weight.cum2 = round(cumsum(class.weight2),2)

sum.sieve3 = sum(NBcrop1$Area)
class.weight3 = (NBcrop1$Area * 100)/sum.sieve3
class.weight.cum3 = round(cumsum(class.weight3),2)


Bcrop3 <- Bcrop3[order(Bcrop3$Area),]
VBcrop3 <- VBcrop3[order(VBcrop3$Area),]
NBcrop3 <- NBcrop3[order(NBcrop3$Area),]

sum.sieve1.3 = sum(Bcrop3$Area)
class.weight1.3 = (Bcrop3$Area * 100)/sum.sieve1.3
class.weight.cum1.3 = round(cumsum(class.weight1.3),2)

sum.sieve2.3 = sum(VBcrop3$Area)
class.weight2.3 = (VBcrop3$Area * 100)/sum.sieve2.3
class.weight.cum2.3 = round(cumsum(class.weight2.3),2)

sum.sieve3.3 = sum(NBcrop3$Area)
class.weight3.3 = (NBcrop3$Area * 100)/sum.sieve3.3
class.weight.cum3.3 = round(cumsum(class.weight3.3),2)

Bcrop2 <- Bcrop2[order(Bcrop2$Area),]
VBcrop2 <- VBcrop2[order(VBcrop2$Area),]
NBcrop2 <- NBcrop2[order(NBcrop2$Area),]

sum.sieve1.2 = sum(Bcrop2$Area)
class.weight1.2 = (Bcrop2$Area * 100)/sum.sieve1.2
class.weight.cum1.2 = round(cumsum(class.weight1.2),2)

sum.sieve2.2 = sum(VBcrop2$Area)
class.weight2.2 = (VBcrop2$Area * 100)/sum.sieve2.2
class.weight.cum2.2 = round(cumsum(class.weight2.2),2)

sum.sieve3.2 = sum(NBcrop2$Area)
class.weight3.2 = (NBcrop2$Area * 100)/sum.sieve3.2
class.weight.cum3.2 = round(cumsum(class.weight3.2),2)


plot(Bcrop1$Area*10^-6,class.weight.cum,pch = 16, cex=0.5, col='#662606', xlim=c(5000*10^-6,max(Bcrop1$Area,NBcrop1$Area,VBcrop1$Area,Bcrop2$Area*10^-2,NBcrop2$Area*10^-2,VBcrop2$Area*10^-2)*10^-6),log = 'x', xlab=expression(paste('grain size [cm'^'2',']')),ylab='% finer')
points(NBcrop1$Area*10^-6,class.weight.cum3,pch=16,, cex=0.5, col='#ec7014')
points(VBcrop1$Area*10^-6,class.weight.cum2,pch=16, cex=0.5, col='#fec44f')
points(NBcrop2$Area*10^-8,class.weight.cum3.2,pch=16,, cex=0.5, col='#ec7014')
points(VBcrop2$Area*10^-8,class.weight.cum2.2,pch=16, cex=0.5, col='#fec44f')
points(Bcrop2$Area*10^-8,class.weight.cum1.2,pch=16, cex=0.5, col='#662606')
points(NBcrop3$Area*10^-6,class.weight.cum3.3,pch=16, cex=0.5, col='#ec7014')
points(VBcrop3$Area*10^-6,class.weight.cum2.3,pch=16, cex=0.5, col='#fec44f')
points(Bcrop3$Area*10^-6,class.weight.cum1.3,pch=16, cex=0.5, col='#662606')
legend('topleft',pch=16,col=c('#662606','#ec7014','#fec44f'), c('heated hydrophobic\nMED=16.5%\n','non-heated\nMED=6.5%', 'heated hydrophilic\nMED=0%'), cex=0.8,box.lty=0)

data(granulo)
granplot(B40,xc=2,hist=TRUE,cum=TRUE,main="Grain-size Distribution",
         col.hist="gray",col.cum="red")

granplot(granulo,xc=2:4,main="Grain-size Distribution")

granstat(B40$Area)



##Burned
library(raster)
library(rgdal)
tmp=raster('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/VB_1_bse(x100).tif')

set.seed(18)
x=round(runif(1, 0, extent(tmp)[2]-extent(tmp)[2]/3))
y=round(runif(1, 200, extent(tmp)[4]-extent(tmp)[4]/3))
x2<-x+extent(tmp)[2]/3
y2<-y+extent(tmp)[4]/3
Crop1 <- c(x,x2,y,y2)

set.seed(5)
x=round(runif(1, 0, extent(tmp)[2]-extent(tmp)[2]/3))
y=round(runif(1, 200, extent(tmp)[4]-extent(tmp)[4]/3))
x2<-x+extent(tmp)[2]/3
y2<-y+extent(tmp)[4]/3
Crop2 <- c(x,x2,y,y2)

set.seed(35) #28
x=round(runif(1, 0, extent(tmp)[2]-extent(tmp)[2]/3))
y=round(runif(1, 200, extent(tmp)[4]-extent(tmp)[4]/3))
x2<-x+extent(tmp)[2]/3
y2<-y+extent(tmp)[4]/3
Crop3 <- c(x,x2,y,y2)

# crop to desired extent
Raster_crop1 <- crop(tmp, Crop1)
Raster_crop2 <- crop(tmp, Crop2)
Raster_crop3 <- crop(tmp, Crop3)


image(tmp,col=grey(seq(0, 1, length = 256)))
image(Raster_crop1, col=grey(seq(0.5, 1, length = 256)),add=T)
image(Raster_crop2, col=grey(seq(0.5, 1, length = 256)),add=T)
image(Raster_crop3, col=grey(seq(0.5, 1, length = 256)),add=T)

##--------------------
##Burned
library(raster)
library(rgdal)
tmp=raster('/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis/1_3_6_bse(x100).tif')

set.seed(NULL)
x=round(runif(1, 0, extent(tmp)[2]-extent(tmp)[2]/3))
y=round(runif(1, 200, extent(tmp)[4]-extent(tmp)[4]/3))
x2<-x+extent(tmp)[2]/3
y2<-y+extent(tmp)[4]/3
Crop1 <- c(x,x2,y,y2)

x=round(runif(1, 0, extent(tmp)[2]-extent(tmp)[2]/3))
y=round(runif(1, 200, extent(tmp)[4]-extent(tmp)[4]/3))
x2<-x+extent(tmp)[2]/3
y2<-y+extent(tmp)[4]/3
Crop2 <- c(x,x2,y,y2)

x=round(runif(1, 0, extent(tmp)[2]-extent(tmp)[2]/3))
y=round(runif(1, 200, extent(tmp)[4]-extent(tmp)[4]/3))
x2<-x+extent(tmp)[2]/3
y2<-y+extent(tmp)[4]/3
Crop3 <- c(x,x2,y,y2)

n=1;
while((Crop2[1]>=Crop1[1] & Crop2[1]<=Crop1[2] & Crop2[3]>=Crop1[3] & Crop2[3]<=Crop1[4])|
      (Crop2[1]>=Crop1[1] & Crop2[1]<=Crop1[2] & Crop2[3]>=Crop1[3] & Crop2[3]<=Crop1[4])|
      (Crop2[2]<=Crop1[2] & Crop2[2]>=Crop1[1] & Crop2[4]>=Crop1[3] & Crop2[3]<=Crop1[4])|
      (Crop2[1]>=Crop1[1] & Crop2[1]<=Crop1[2] & Crop2[4]>=Crop1[3] & Crop2[4]<=Crop1[4])|
      (Crop3[1]>=Crop1[1] & Crop3[1]<=Crop1[2] & Crop3[3]>=Crop1[3] & Crop3[3]<=Crop1[4])|
      (Crop3[1]>=Crop1[1] & Crop3[1]<=Crop1[2] & Crop3[3]>=Crop1[3] & Crop3[3]<=Crop1[4])|
      (Crop3[2]<=Crop1[2] & Crop3[2]>=Crop1[1] & Crop3[4]>=Crop1[3] & Crop3[3]<=Crop1[4])|
      (Crop3[1]>=Crop1[1] & Crop3[1]<=Crop1[2] & Crop3[4]>=Crop1[3] & Crop3[4]<=Crop1[4])|
      (Crop3[1]>=Crop2[1] & Crop3[1]<=Crop2[2] & Crop3[3]>=Crop2[3] & Crop3[3]<=Crop2[4])|
      (Crop3[1]>=Crop2[1] & Crop3[1]<=Crop2[2] & Crop3[3]>=Crop2[3] & Crop3[3]<=Crop2[4])|
      (Crop3[2]<=Crop2[2] & Crop3[2]>=Crop2[1] & Crop3[4]>=Crop2[3] & Crop3[3]<=Crop2[4])|
      (Crop3[1]>=Crop2[1] & Crop3[1]<=Crop2[2] & Crop3[4]>=Crop2[3] & Crop3[4]<=Crop2[4])
){


  x=round(runif(1, 0, extent(tmp)[2]-extent(tmp)[2]/3))
  y=round(runif(1, 200, extent(tmp)[4]-extent(tmp)[4]/3))
  x2<-x+extent(tmp)[2]/3
  y2<-y+extent(tmp)[4]/3
  Crop2 <- c(x,x2,y,y2)
  
  x=round(runif(1, 0, extent(tmp)[2]-extent(tmp)[2]/3))
  y=round(runif(1, 200, extent(tmp)[4]-extent(tmp)[4]/3))
  x2<-x+extent(tmp)[2]/3
  y2<-y+extent(tmp)[4]/3
  Crop3 <- c(x,x2,y,y2)}


# crop to desired extent
Raster_crop1 <- crop(tmp, Crop1)
Raster_crop2 <- crop(tmp, Crop2)
Raster_crop3 <- crop(tmp, Crop3)


image(tmp,col=grey(seq(0, 1, length = 256)))
image(Raster_crop1, col=grey(seq(0.5, 1, length = 256)),add=T)
image(Raster_crop2, col=grey(seq(0.5, 1, length = 256)),add=T)
image(Raster_crop3, col=grey(seq(0.5, 1, length = 256)),add=T)

FilePath<-'/Users/katya/Google Drive/PhD/Hydrophobicity/SEM/ParticleSizeAnalysis'
require(rgdal)
writeRaster(Raster_crop3, filename=file.path(FilePath, "1_3_6_bseCrop3.tif"), format="GTiff", overwrite=TRUE)
writeRaster(Raster_crop2, filename=file.path(FilePath, "1_3_6_bseCrop2.tif"), format="GTiff", overwrite=TRUE)
writeRaster(Raster_crop1, filename=file.path(FilePath, "1_3_6_bseCrop1.tif"), format="GTiff", overwrite=TRUE)


#




