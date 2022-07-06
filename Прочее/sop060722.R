time1<-Sys.time()

#считываем все данные 
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


#приводим код инфор в числовой вид
for (i in (1:dim(art)[1])){
  art[i,1]=as.numeric(as.character(art[i,1]))}

for (i in (1:dim(df1)[1])){
  df1[i,1]=as.numeric(as.character(df1[i,1]))}

for (i in (1:dim(df2)[1])){
  df2[i,1]=as.numeric(as.character(df2[i,1]))}

for (i in (1:dim(df3)[1])){
  df3[i,1]=as.numeric(as.character(df3[i,1]))}

for (i in (1:dim(df4)[1])){
  df4[i,1]=as.numeric(as.character(df4[i,1]))}

for (i in (1:dim(df5)[1])){
  df5[i,1]=as.numeric(as.character(df5[i,1]))}

for (i in (1:dim(df6)[1])){
  df6[i,1]=as.numeric(as.character(df6[i,1]))}

for (i in (1:dim(df7)[1])){
  df7[i,1]=as.numeric(as.character(df7[i,1]))}

for (i in (1:dim(df8)[1])){
  df8[i,1]=as.numeric(as.character(df8[i,1]))}

for (i in (1:dim(df9)[1])){
  df9[i,1]=as.numeric(as.character(df9[i,1]))}

for (i in (1:dim(df10)[1])){
  df10[i,1]=as.numeric(as.character(df10[i,1]))}

for (i in (1:dim(df11)[1])){
  df11[i,1]=as.numeric(as.character(df11[i,1]))}

for (i in (1:dim(df12)[1])){
  df12[i,1]=as.numeric(as.character(df12[i,1]))}


#удаляем ненужные столбцы
df_t=df1[-c(1),-c(2,3,4,5,7,8)]
df_t2=df2[-c(1),-c(2,3,4,5,7,8)]
df_t3=df3[-c(1),-c(2,3,4,5,7,8)]
df_t4=df4[-c(1),-c(2,3,4,5,7,8)]
df_t5=df5[-c(1),-c(2,3,4,5,7,8)]
df_t6=df6[-c(1),-c(2,3,4,5,7,8)]
df_t7=df7[-c(1),-c(2,3,4,5,7,8)]
df_t8=df8[-c(1),-c(2,3,4,5,7,8)]
df_t9=df9[-c(1),-c(2,3,4,5,7,8)]
df_t10=df10[-c(1),-c(2,3,4,5,7,8)]
df_t11=df11[-c(1),-c(2,3,4,5,7,8)]
df_t12=df12[-c(1),-c(2,3,4,5,7,8)]

#удаляем пробелы из названия документа продаж
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

объединяем все таблицы в одну
mt5=rbind(mt,mt2,mt3,mt4,mt5,mt6,mt7,mt8,mt9,mt10,mt11,mt12)


#удаляем дубликаты
uniq_mt5<-unique(mt5)

#отделяем код инфор
artics=art[,1]

#создаем таблицы для дальнейшего заполнения
znach=data.frame(array(0, c(41,length(artics))))
numbers=data.frame(array(0, c(41,length(artics))))


#определяем самые частые попадания в общий чек для каждой позиции
for (l in (1:length(artics))){
  time_24=Sys.time()
  x <- data.frame(match(uniq_mt5[,1],artics[l],nomatch = 0))
  
  du=cbind(uniq_mt5,x)
  
  
  prom_df=data.frame()
  hh=0
  
  
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
    
    for (oo in (1:nrow(mt5y)))
      if (mt5y[oo,3]>0){
        nn=nn+1
        df77[nn,1]=mt5y[oo,1]
      }
    
    final_df=as.data.frame(table(df77))
    
    
    
    znach[l]=data.frame(final_df[order(-rank((final_df$Freq))),][1:41,1])
    numbers[l]=data.frame(final_df[order(-rank((final_df$Freq))),][1:41,2])
    print(l)
    time_23=Sys.time()
    print(time_24-time_23)
    
    
    
    }}

colnames(znach)<-artics
colnames(numbers)<-artics



df1rez<-znach
df2rez<-numbers


df1_trez=t(df1rez)
df2_trez=t(df2rez)

df4rez=data.frame(array(0, c(length(df1rez),41)))
df5rez=data.frame(array(0, c(length(df1rez),41)))

df3=cbind(df1_trez,df2_trez,df4rez,df5rez)

a=(42*2)
for (l in (1:41))
{
  print(l)
  for (m in (1:dim(df3rez)[1])){
    df3rez[m,l+a]=df3rez[m,43+l]/df3rez[m,44]}}



for (l in (1:41))
{
  
  for (m in (1:dim(df3rez)[1])){
    df3rez[m,l+a]=df3rez[m,43+l]/df3rez[m,44]}}    



tyu=l+a



for (l in (1:41))
{
  
  for (m in (1:dim(df3rez)[1])){
    
    if (df3rez[m,44]>2 & df3rez[m,43+l]>2 & df3rez[m,l+a]>0.15){
      df3rez[m,tyu+l]=1}}}


for (m in (1:dim(df3rez)[1])){    
  df3rez$summa[m]=sum(df3rez[m,(126:(126+40))])-1}




for (l in (1:41))
{
  
  for (m in (1:dim(df3rez)[1])){
    if (df3rez$summa[m] < 3){
      if (df3rez[m,125+l]==0){
        if (df3rez[m,43+l]>10 & df3rez[m,l+a]>0.05){
          df3rez[m,125+l]=1}}}}}


df5rez=data.frame(array(0, c(length(df3rez),2)))
u=1
for (l in (1:41))
{
  
  for (m in (1:dim(df3rez)[1])){
    if (df3rez[m,125+l]==1){
      
      df5rez[u,1]=df3rez[m,1]
      df5rez[u,2]=df3rez[m,l]
      u=u+1}
  }}

df5rez$it=0
df5rez[is.na(df5rez)] <- 0 

for (m in (1:dim(df5rez)[1])){
  if (df5rez[m,1]==df5rez[m,2]){
    df5rez$it[m]=1
    
  }
}


for (m in (1:dim(df5rez)[1])){
  if (df5rez[m,1]==0 || df5rez[m,2]==0){
    df5rez$it[m]=1
    
  }
}
df_final23=df5rez[df5rez$it==0,]


write.table(df_final23[,1:2], file=paste0('сопутствующий товар_',format(Sys.time(), "%d %m %Y"),'.csv'), sep = ";",col.names = NA)


time2<-Sys.time()
time3=time2-time1
time3