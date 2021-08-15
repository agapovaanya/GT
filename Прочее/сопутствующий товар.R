time1<-Sys.time()
art<-read.csv('art.csv', header = T,sep=";",encoding = "UTF-8")



df1<-read.csv('01.csv', header = T,sep=";",encoding = "UTF-8")
df2<-read.csv('02.csv', header = T,sep=";",encoding = "UTF-8")
df3<-read.csv('03.csv', header = T,sep=";",encoding = "UTF-8")
df4<-read.csv('04.csv', header = T,sep=";",encoding = "UTF-8")
df5<-read.csv('05.csv', header = T,sep=";",encoding = "UTF-8")
df6<-read.csv('06.csv', header = T,sep=";",encoding = "UTF-8")
df7<-read.csv('07.csv', header = T,sep=";",encoding = "UTF-8")
df8<-read.csv('08.csv', header = T,sep=";",encoding = "UTF-8")
df9<-read.csv('09.csv', header = T,sep=";",encoding = "UTF-8")
df10<-read.csv('10.csv', header = T,sep=";",encoding = "UTF-8")
df11<-read.csv('11.csv', header = T,sep=";",encoding = "UTF-8")
df12<-read.csv('12.csv', header = T,sep=";",encoding = "UTF-8")

#удаляем все столбцы, кроме документа продажи и кода инфор
df_t=df1[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t2=df2[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t3=df3[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t4=df4[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t5=df5[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t6=df6[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t7=df7[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t8=df8[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t9=df9[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t10=df10[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t11=df11[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]
df_t12=df12[-c(1,2,3,4,5),-c(2,3,4,5,7,8)]


#убираем пробелы из документа продажи
mt=apply(df_t,2,function(x)gsub('\\s+', '',x))
mt2=apply(df_t2,2,function(x)gsub('\\s+', '',x))
mt3=apply(df_t3,2,function(x)gsub('\\s+', '',x))
mt4=apply(df_t4,2,function(x)gsub('\\s+', '',x))
mt5=apply(df_t5,2,function(x)gsub('\\s+', '',x))
mt6=apply(df_t6,2,function(x)gsub('\\s+', '',x))
mt7=apply(df_t7,2,function(x)gsub('\\s+', '',x))
mt8=apply(df_t8,2,function(x)gsub('\\s+', '',x))
mt9=apply(df_t9,2,function(x)gsub('\\s+', '',x))
mt10=apply(df_t10,2,function(x)gsub('\\s+', '',x))
mt11=apply(df_t11,2,function(x)gsub('\\s+', '',x))
mt12=apply(df_t12,2,function(x)gsub('\\s+', '',x))

#объединяем все mt
mt5=rbind(mt,mt2,mt3,mt4,mt5,mt6,mt7,mt8,mt9,mt10,mt11,mt12)


#оставляем уникальные
uniq_mt5<-unique(mt5)


artics=art[,1]

#создаем пустые таблицы с 21 столбцами и количеством продаваемых артикулов
znach=data.frame(array(1, c(21,length(artics))))
numbers=data.frame(array(1, c(21,length(artics))))


l=524
for (l in (524:length(artics))){
  time_24=Sys.time()
  #соединяем 
  x <- data.frame(match(uniq_mt5[,1],artics[l],nomatch = 0))
  
  du=cbind(uniq_mt5,x)
  
  
  prom_df=data.frame()
  hh=0
  
#собираем все документы продажи  
  for (gg in (1:nrow(du)))
    if (du[gg,3]>0){
      hh=hh+1
      prom_df[hh,1]=du[gg,2]
    }
  
  if (length(prom_df)!=0){
  
    y <- data.frame(match(uniq_mt5[,2],prom_df[,1],nomatch = 0))
    
    
    
    nn=0
    mt5y=cbind(uniq_mt5,y)
    df77=data.frame()
    
    #собираем все позиции проданные вместе с искомой позицией
    for (oo in (1:nrow(mt5y)))
      if (mt5y[oo,3]>0){
        nn=nn+1
        df77[nn,1]=mt5y[oo,1]
      }
    
    final_df=as.data.frame(table(df77))
    
    
    
    znach[l]=data.frame(final_df[order(-rank((final_df$Freq))),][1:21,1])
    numbers[l]=data.frame(final_df[order(-rank((final_df$Freq))),][1:21,2])}
  
  print(l)
  time_23=Sys.time()
  print(time_24-time_23)}

colnames(znach)<-artics
colnames(numbers)<-artics

time2<-Sys.time()
time3=time2-time1
time3

write.table(znach, file="rez.csv ", sep = ";",col.names = NA)
write.table(numbers, file="rezu.csv ", sep = ";",col.names = NA)

