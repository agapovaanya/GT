library(dplyr)

sale_fact<-read.delim("продажи_факт.csv",header=T,sep=";",dec=",",encoding = "UTF-8",stringsAsFactors=F)

prev_forecast<-read.delim("Предыдущий прогноз_июнь.csv",header=T,sep=";",dec=",",encoding = "UTF-8",stringsAsFactors=F)

methods_df<-read.delim("лучшие методы_июнь.csv",header=T,sep=";",dec=",",encoding = "UTF-8",stringsAsFactors=F)

#создаем новый столбец с 0 значениями для дальнейшего заполнения
prev_forecast[,7]=0

#подтягиваем фактические продажи к предыдущему прогнозу
for (i in (1:nrow(prev_forecast))){
  print(i)
  for (j in (1:nrow(sale_fact)))
    if (prev_forecast[i,2]==sale_fact[j,1]){
      #print(i)
      #print(sale_fact[j,2])
      k=sale_fact[j,2]
      prev_forecast[i,7]=k
      k=0
    }
    
  }

#заменяем пустые и na значения на 0
prev_forecast[,7][prev_forecast[,7]==""] <- 0 
prev_forecast[is.na(prev_forecast)]<-0


#считаем точность каждого метода
for (i in (1:nrow(prev_forecast))){
  
  if (as.integer(prev_forecast[i,7])>prev_forecast[i,3]){
    prev_forecast$toch_arima[i]=prev_forecast[i,3]/as.integer(prev_forecast[i,7])
    
  }else{
    prev_forecast$toch_arima[i]=as.integer(prev_forecast[i,7])/prev_forecast[i,3]
  }
  
  if (as.integer(prev_forecast[i,7])>prev_forecast[i,4]){
    prev_forecast$toch_HW[i]=prev_forecast[i,4]/as.integer(prev_forecast[i,7])
    
  }else{
    prev_forecast$toch_HW[i]=as.integer(prev_forecast[i,7])/prev_forecast[i,4]
  }
  
  if (as.integer(prev_forecast[i,7])>prev_forecast[i,5]){
    prev_forecast$toch_avg[i]=prev_forecast[i,5]/as.integer(prev_forecast[i,7])
    
  }else{
    prev_forecast$toch_avg[i]=as.integer(prev_forecast[i,7])/prev_forecast[i,5]
  }
  
  if (as.integer(prev_forecast[i,7])>prev_forecast[i,6]){
    prev_forecast$toch_lr[i]=prev_forecast[i,6]/as.integer(prev_forecast[i,7])
    
  }else{
    prev_forecast$toch_lr[i]=as.integer(prev_forecast[i,7])/prev_forecast[i,6]
  }
  
}

prev_forecast[is.na(prev_forecast)]<-0  

#выясняем у какого метода максимальная точность
for (i in (1:nrow(prev_forecast))){
  max_toch=max(prev_forecast$toch_lr[i],prev_forecast$toch_avg[i],prev_forecast$toch_HW[i],prev_forecast$toch_arima[i])
  if (max_toch==0){
    prev_forecast$best_method[i]=''
  }else {
    if ((max_toch)==prev_forecast$toch_lr[i]){
      prev_forecast$best_method[i]='lr'
    }else {
      if ((max_toch)==prev_forecast$toch_avg[i]){
        prev_forecast$best_method[i]='avg'
      }else {
        if ((max_toch)==prev_forecast$toch_HW[i]){
          prev_forecast$best_method[i]='HW'
        }else {
          prev_forecast$best_method[i]='arima'
        }
    }
    
  }
  }}

prev_forecast_final=data.frame(prev_forecast[,2],prev_forecast[,12])
names(methods_df)[2]='infor'
names(prev_forecast_final)[1]='infor'  


spl=full_join(methods_df[,2:8],prev_forecast_final,by = "infor") 



itog=data.frame(spl[,1],spl[,3:8])

itog[is.na(itog)]<-""

#подсчитываем какой метод оказывался лучшим большее количество раз
for (i in (1:nrow(itog))){
  #i=1
  count_arima=0
  count_HW=0
  count_lr=0
  count_avg=0
  for (j in (2:7))
    if (itog[i,j]=='arima'){
      count_arima=count_arima+1
    }else{
      if (itog[i,j]=='HW'){
        count_HW=count_HW+1
      }else{
        if (itog[i,j]=='lr'){
          count_lr=count_lr+1
        }else{
          if (itog[i,j]=='avg'){
            count_avg=count_avg+1
          
          }}}}
  max_method = max(count_arima,count_HW,count_lr,count_avg)
  
  if (max_method==0){
    itog$method[i]='not'
  }else{
    
    if (max_method==count_arima){
      itog$method[i]='arima'
    }else{
      if (max_method==count_HW){
        itog$method[i]='HW'
      }else{
        if (max_method==count_lr){
          itog$method[i]='lr'
        }else{
          if (max_method==count_avg){
            itog$method[i]='avg'}
          }}}}}
  

names(itog)=c('infor','1','2','3','4','5','6','method')

#удаляем пустые строки
itog = itog[itog$method != "not",]

write.table(itog, file="лучшие методы_июль.csv ", sep = ";",dec=",",col.names = NA)


