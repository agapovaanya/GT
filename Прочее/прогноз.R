dftest1<-read.csv('01.csv', header = T,sep=";")
dftest2<-read.csv('02.csv', header = T,sep=";")
dftest3<-read.csv('03.csv', header = T,sep=";")
dftest4<-read.csv('04.csv', header = T,sep=";")
dftest5<-read.csv('05.csv', header = T,sep=";")
dftest6<-read.csv('06.csv', header = T,sep=";")
dftest7<-read.csv('07.csv', header = T,sep=";")
dftest8<-read.csv('08.csv', header = T,sep=";")
dftest9<-read.csv('09.csv', header = T,sep=";")
dftest10<-read.csv('10.csv', header = T,sep=";")
dftest11<-read.csv('11.csv', header = T,sep=";")
dftest12<-read.csv('12.csv', header = T,sep=";")


lol=dftest1[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]

mt=apply(lol,2,function(x)gsub('\\s+', '',x))

lol2=dftest2[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol3=dftest3[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol4=dftest4[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol5=dftest5[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol6=dftest6[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol7=dftest7[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol8=dftest8[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol9=dftest9[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol10=dftest10[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol11=dftest11[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]
lol12=dftest12[-c(1,2,3,4,5),-c(1,2,3,5,7,8)]

mt2=apply(lol2,2,function(x)gsub('\\s+', '',x))
mt3=apply(lol3,2,function(x)gsub('\\s+', '',x))
mt4=apply(lol4,2,function(x)gsub('\\s+', '',x))
mt5=apply(lol5,2,function(x)gsub('\\s+', '',x))
mt6=apply(lol6,2,function(x)gsub('\\s+', '',x))
mt7=apply(lol7,2,function(x)gsub('\\s+', '',x))
mt8=apply(lol8,2,function(x)gsub('\\s+', '',x))
mt9=apply(lol9,2,function(x)gsub('\\s+', '',x))
mt10=apply(lol10,2,function(x)gsub('\\s+', '',x))
mt11=apply(lol11,2,function(x)gsub('\\s+', '',x))
mt12=apply(lol12,2,function(x)gsub('\\s+', '',x))

mt5=rbind(mt,mt2,mt3,mt4,mt5,mt6,mt7,mt8,mt9,mt10,mt11,mt12)
