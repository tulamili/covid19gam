x75<-read.delim("saitama0303.tsv")
library( mgcv )
library(maps)
attach(x75)
g <- gam ( pat ~ s(lon,lat,k=72), family= poisson()   , offset = log ( pop) ) 

par(mai=c(1,1,0.5,1),xaxs="r",yaxs="r")
vis.gam(g,plot.type="contour", color="terrain",n.grid=120,too.far=0.18,type="link",las=1,nCol=150,levels=NULL,xlim=c(139-.02,139.89),ylim=c(35.77,36.282))
points(xx,type="p",cex=0.1,lwd=0.8,col=rgb(.8,.6,.6,.2))#polygon(map("world", "japan:Honshu",add=T,lwd=5,type="l",fill=F),fillOddEven = T)
rect( 138.7,34.7, 140.9, 36.4,  col=rgb(1,1,1,0.4))
vis.gam(g,plot.type="contour", color="bw",n.grid=120,too.far=0.18,type="response",las=1,add=T)


with ( data = x75[ !is.na(x75$pat) , ] , points(lon,lat,cex=sqrt(pop/1e4),pch=21,bg=rgb(0,1,0,0.5), col="black", lwd=0.3) )
points(lon,lat,cex=sqrt(pat/10e1),pch=21,bg=rgb(1,0,0,0.7),col=rgb(1,0,0,0.5),lwd=0.6) # pch=20で描くと小さい縁となるのでまずい
par(family="HiraKakuProN-W3") # R Studioなら不要かも。R単体なら日本語プロットに必要。]
for(i in 1:2){with ( data = x75[ !is.na(x75$pat) , ] , text(lon,lat,name,cex=0.65,pos=3) )}


xx<- read.delim("tmp.txt")[,c(2,1)]




