library(mgcv)
b<-c("千代田区","中央区","港区","新宿区","文京区","台東区","墨田区","江東区","品川区","目黒区","大田区","世田谷区","渋谷区","中野区","杉並区","豊島区","北区","荒川区","板橋区","練馬区","足立区","葛飾区","江戸川区","八王子市","立川市","武蔵野市","三鷹市","青梅市","府中市","昭島市","調布市","町田市","小金井市","小平市","日野市","東村山市","国分寺市","国立市","福生市","狛江市","東大和市","清瀬市","東久留米市","武蔵村山市","多摩市","稲城市","羽村市","あきる野市","西東京市","瑞穂町","日の出町","檜原村","奥多摩町","大島町","利島村","新島村","神津島村","三宅村","御蔵島村","八丈町","青ヶ島村","小笠原村")
x227 <- read.delim("x227.tsv") #attach(x227) #detach(x227)
x49 <- x227[1:49,]
attach(x49)
g <- gam ( i ~ s(lo,la), family=poisson(), offset=log(p))
par(family="HiraKakuProN-W3") # R Studioなら不要かも。R単体なら日本語プロットに必要。]
plot(g, se=F,las=1)
points(lo,la,cex=sqrt(p/1e4),pch=20,col=rgb(0,1,0,0.5))
points(lo,la,cex=sqrt(i/0.5e3),pch=20,col=rgb(1,0,0,0.5))
text(lo,la,b[1:49])




https://stopcovid19.metro.tokyo.lg.jp/cards/number-of-confirmed-cases-by-municipalities/ から
https://www.stat.go.jp/data/kokusei/topics/topi102.html
https://www.soumu.go.jp/menu_news/s-news/17216_1.html
