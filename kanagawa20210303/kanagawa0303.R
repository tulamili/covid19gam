x63<-read.delim("kanagawa0303.tsv")
library( mgcv )
library(maps)
attach(x63)
g <- gam ( pat ~ s (lon,lat, k=58,bs="tp",m=0), family= poisson()  , offset = log ( pop) ) # 58
gam.check(g)
par(mai=c(1,1,0.5,1))
vis.gam(g,plot.type="contour", color="terrain",n.grid=120,too.far=0.48,type="link",las=1,nCol=150,levels=c(),xlim=c(139.02,139.73),ylim=c(35.13,35.645))
#levelsの指定は contour()関数と同じものを使ってみると、結果的にうまくいくのだが、コンソール画面にはエラーが表示されるようだ。
polygon(map("world", "japan:Honshu",add=T,lwd=5,type="l",fill=F),fillOddEven = T)
rect( 139.,34.7, 140.9, 36.1,  col=rgb(1,1,1,0.4)) # 半透明の白を重なることで、視覚的な効果(ここから下で描くはっきりさせたいものを目立たせる)
vis.gam(g,plot.type="contour", color="bw",n.grid=120,too.far=0.18,type="response",las=1,add=T)

with ( data = x63[ !is.na(x63$pat) , ] , points(lon,lat,cex=sqrt(pop/1e4),pch=21,bg=rgb(0,1,0,0.5), col="black", lwd=0.5) )
points(lon,lat,cex=sqrt(pat/10e1),pch=21,bg=rgb(1,0,0,0.7),col=rgb(1,0,0,0.5),lwd=0.6) # pch=20で描くと小さい縁となるのでまずい
par(family="HiraKakuProN-W3") # R Studioなら不要かも。R単体なら日本語プロットに必要。]

for(i in 1:2){ with ( data = x63[ !is.na(x63$pat) , ] , text(lon,lat,name,cex=0.75,pos=3,offset=-0.75)) } # 2度描くことで文字がはっきりする。

detach(x63)
