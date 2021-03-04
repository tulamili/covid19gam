
library(mgcv)

b<-c("千代田区","中央区","港区","新宿区","文京区","台東区","墨田区","江東区","品川区","目黒区","大田区","世田谷区","渋谷区","中野区","杉並区","豊島区","北区","荒川区","板橋区","練馬区","足立区","葛飾区","江戸川区","八王子市","立川市","武蔵野市","三鷹市","青梅市","府中市","昭島市","調布市","町田市","小金井市","小平市","日野市","東村山市","国分寺市","国立市","福生市","狛江市","東大和市","清瀬市","東久留米市","武蔵村山市","多摩市","稲城市","羽村市","あきる野市","西東京市","瑞穂町","日の出町","檜原村","奥多摩町","大島町","利島村","新島村","神津島村","三宅村","御蔵島村","八丈町","青ヶ島村","小笠原村")
b0 <- b 
b<-substr(b,0,nchar(b0)-1)

x227 <- read.delim("x227.tsv") #attach(x227) #detach(x227)
x49 <- x227[1:49,]
attach(x49)
g <- gam ( i ~ te(lo,la), family=poisson(), offset=log(p))

par(family="HiraKakuProN-W3") # R Studioなら不要かも。R単体なら日本語プロットに必要。]
g <- gam ( i ~ s(lo,la,k=8), family=poisson(), offset=log(p))
summary(g)
plot(g, se=F,las=1)
points(lo,la,cex=sqrt(p/1e4),pch=20,col=rgb(0,1,0,0.5))
points(lo,la,cex=sqrt(i/0.5e3),pch=20,col=rgb(1,0,0,0.5))
text(lo,la,b[1:49])
anova(g)
dev.new()
par(mai=c(1,1,1.4,1.4))
plot ( 1:49, predict ( g ) , ylim=c(-5.7,-3.5), pch=5,las=1)
points ( 1:49 , log (i/p), pch = 15) 
axis ( 3, 1:49 , b[1:49],las=2)
axis ( 4,-11:-7/2 ,paste(round(100* exp (-11:-7/2 ),2),"%" ) ,las=2)
legend("bottomleft", c("実際の割合 (左側の軸は対数,右側で%) ", "補間した場合の値"), pch=c(15,5))
?pch

detach(x49)
x2 <- read.delim("222228.tsv")
x2<-x2[2:46,]
attach(x2)
g2<-gam(week ~ s(la,lo,k=45), family=poisson(), offset=log(pop))
vis.gam(g2,se=F,las=1,color="cm",plot.type="contour",levels= -15:-9, n.grid=80)
rect( 129,31,142,42,col=rgb(1,1,1,0.5))
rect( 132,33.3,142,31,border=NA, col=rgb(1,1,1,0.9))
rect( 136,34.6,142,31,border=NA, col=rgb(1,1,1,0.9))
rect( 136,36,129,42,border=NA, col=rgb(1,1,1,0.9))
rect( 139,38,129,42,border=NA, col=rgb(1,1,1,0.9))
rect( 132,34.5,129,42,border=NA, col=rgb(1,1,1,0.9))
points(la,lo,cex=sqrt(pop/4e5),pch=21,bg=rgb(0,1,0,0.75),col="black")
points(la,lo,cex=sqrt(week/1e2),pch=20,col=rgb(1,0,0,0.75))
points(la,lo,cex=0.1,pch=20,col=rgb(0,0,0,1))
#? vis.gam
#week

? points 