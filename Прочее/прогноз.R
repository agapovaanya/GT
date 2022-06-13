# записываем время начала работы модели
start_time<-Sys.time() 

#считываем библиотеки
library(forecast) 
library(reshape2)
library(reshape)
library(dplyr)
library(stringr)
library(stringi)
library(tibble)

#файл с продажами за 4 года
df_sales1<-read.delim("apr120622.csv",header=T,sep=";",dec=",",encoding = "UTF-8")


#файл с дефицитом
df_def<-read.delim("def0.csv",header=T,sep=";",dec=",")

#файл с лучшими методами
methods_df<-read.delim("лучшие методы_июнь.csv",header=T,sep=";",dec=",",encoding = "UTF-8",stringsAsFactors=F)


predict14=data.frame()
options(scipen=999) #чтобы десятичные числа не записывались в виде 1,96е5

#считываем файлы по продажам и по дефициту



number_of_columns_df_sales=dim(df_sales1)[1]-2 #количество позиций

# убираем пустые строки (первая и 2 последние)
df_sales1=df_sales1[2:number_of_columns_df_sales,]

#считываем файлы по дефициту


chanks=100

for (i in (1:(as.integer(dim(df_sales1)[1]/chanks)+1)))
  
{ 
  print(i)
  
  if ((dim(df_sales1)[1]-((i-1)*chanks))<chanks){
    abc=dim(df_sales1)[1]
  }else{
    abc=i*chanks
  }
  
  df_sales=data.frame(df_sales1[((i-1)*chanks+1):abc,])
  
  
  
  
  
  number_of_columns_df_sales=dim(df_sales)[1]
  years=dim(df_sales)[2]-19 #количество столбцов 
  
  dim_sales=dim(df_sales)[2]
  
  #совмещаем 2 таблицы (дефицит и продажи)
  for (j in (1:years))
    for (i in (1:number_of_columns_df_sales))
      df_sales[i,(j+dim_sales)]=df_def[match(df_sales[,2],df_def[,1])[i],j+1]
  
  
  
  #все ячейки делаем целыми 
  for (i in (18:(dim(df_sales)[2]-1)))
    df_sales[,i]<-as.integer(as.character(df_sales[,i]))
  
  #заменяем пустые ячейки на 0
  df_sales[is.na(df_sales)] <- 0 
  
  #восстанавливаем продажи (складываем дефицит и продажи)
  for (j in (1:number_of_columns_df_sales))
    for (i in (1:years))
      df_sales[j,(i+years*2+19)]=df_sales[j,(i+17)]+df_sales[j,(i+years+19)]
  
  
  
  v=df_sales[,1:16]
  vv=df_sales[,((years*2+20):(years*3+19))]
  
  #формируем таблицу 
  df_1=data.frame(v,vv)
  
  
  #ARIMA
  #Основная идея этой модели в том, что цена в будущем зависит от цен в прошлом (авторегрессионная часть AR) и возврата к среднему (MA часть).
  
  
  #считываем файл
  
  chhot<-df_1
  
  a= dim(chhot)[1] #число строк
  b=dim(chhot)[2] #число столбцов
  c=dim(chhot)[2]%/%2 #число месяцев
  
  
  #создаем пустые таблицы для записи прогнозных значений
  predict_4=data.frame()
  
  
  #переводим все значения в целые числа
  for (i in (1:b))
    chhot[,i]<-as.numeric(as.character(chhot[,i]))
  
  #транспонируем
  vv<-t(chhot)
  
  #убираем первую строку (шапка)
  sales<-vv[17:(16+years),]
  
  
  #строим для переодичности - 4
  for (i in 1:number_of_columns_df_sales)
  {
    
    res.auto<-auto.arima(ts(sales[,i],frequency = 4,start = c(1994,1)),approximation = T,trace=F,allowdrift = T)
    
    pre <- forecast(sales[,i],model=res.auto, h=12)
    
    progn<-pre$mean
    
    for (j in (1:12))
      predict_4[j,i]<-progn[j]
  }
  
  
  for (i in (1:dim(predict_4)[1])){
    for (j in (1:dim(predict_4)[2]))
      if (predict_4[i,j]<0){
        predict_4[i,j]=0
      }else {
        predict_4[i,j]=predict_4[i,j]
      }}
  
  #записываем результат
  arima_predict=data.frame(chhot[,1],t(predict_4))
  
  
  
  
  chhot<-df_1
  
  colnames=c('code2','infor','art','art_supplyer', 'isstop', 'brand', 'name', 'isnew', 'pricetype', 'supplier', 'delivery', 'production', 'unitpack',
             'category', 'parent', 'comments')
  
  period=dim(df_1)[2]-16
  
  number_of_items=dim(chhot)[1]
  
  number_of_columns=dim(chhot)[2]
  
  
  #переименовываем колонки
  
  names(chhot)=c((c('code2','infor','art','art_supplyer', 'isstop', 'brand', 'name', 'isnew', 'pricetype', 'supplier', 'delivery', 'production', 'unitpack',
                    'category', 'parent', 'comments')),seq(as.Date("2015/01/01"), by = "month", length.out = period))
  
  for (i in (18:number_of_columns))
    chhot[,i]<-as.numeric(as.character(chhot[,i]))
  
  #заменяем значения na на 0
  chhot[is.na(chhot)] <- 0 
  
  #задаем формат таблицы
  chhot<-as.data.frame(chhot)
  
  df2=melt.data.frame(chhot, id.vars = 1:16 ,measure.vars=17:number_of_columns,variable.names=c('date'),value.name = c('sht'))
  
  
  
  df3=sort_df(df2,vars=c('code2','variable'))
  
  number_of_columns_df3= dim(df3)[1]
  
  ma5=data.frame()
  ma5[1,1]=0
  
  
  for (i in (2:(number_of_columns_df3-1)))
    ma5[i,1]=(df3$value[i-1]+df3$value[i]+df3$value[i+1])/3
  
  
  ma5[number_of_columns_df3,1]=0     
  
  
  for (i in (1:(number_of_columns_df3-1)))
    if (i%%period==0){
      ma5[i,1]=0
      ma5[i+1,1]=0}
  
  
  
  df_w=data.frame(df3,ma5)
  
  nears=data.frame()
  
  nears[1:2,1]=0
  
  for (i in (3:(number_of_columns_df3-1)))
    nears[i,1]=(df3$value[i-2]+df3$value[i-1]+df3$value[i+2]+df3$value[i+1])/4
  
  nears[(number_of_columns_df3-1):number_of_columns_df3,1]=0 
  
  
  for (i in (1:(number_of_columns_df3-1)))
    if (i%%period==0){
      nears[i-1,1]=0
      nears[i,1]=0
      nears[i+1,1]=0
      nears[i+2,1]=0}
  
  
  df_w=data.frame(df_w,nears)
  
  
  
  model=list()
  m=data.frame()
  
  for (i in (1:(number_of_columns_df3/period)))
  {model<-lm(data=df_w[(1+period*(i-1)):(period*i),],value~as.numeric(variable))
  for (j in (1:period))
  {m[j,i]=as.numeric(model$fitted.values[j])
  }}
  
  trend=data.frame()
  
  for (i in (1:(number_of_columns_df3/period)))
    trend=c(trend,m[,i])
  
  trend1=t(as.data.frame(trend))
  df_w3=data.frame(df_w,trend1)
  
  for (i in (1:number_of_columns_df3))
    df_w3$distance[i]=abs(df_w3$value[i]-df_w3$trend1[i])
  
  mean_dist=df_w3 %>% group_by(code2) %>% summarise(Mean_sales = mean(distance))
  
  mean_dist=as.data.frame(mean_dist)
  
  
  
  for (i in 1:number_of_columns_df3)
    df_w3$distance[i]=df_w3$distance[i]/(mean_dist[match(df_w3$code2,mean_dist[,1])[i],2])
  
  
  
  
  
  
  mean_value=df_w3 %>% group_by(code2) %>% summarise(Mean_sales = mean(value))
  
  
  smooth=data.frame()
  smooth[1,1]=0
  
  for (j in (1:(number_of_columns_df3/period)))
    
    for (i in (2:period))
      smooth[i,j]=round((df_w3$value[(period*j+i-(period+1))]+df_w3$value[(period*j+i-period)]+df_w3$value[(period*j+i-(period-1))]+mean_value[j,2])/4,digits=0)
  
  
  for (j in (1:(number_of_columns_df3/period))){
    smooth[1,j]=round(mean_value[j,2]/4,digits=0)
    smooth[period,j]=round(mean_value[j,2]/4,digits=0)}
  
  smooth1=data.frame()
  
  for (i in (1:(number_of_columns_df3/period)))
    smooth1=c(smooth1,smooth[,i])
  
  
  smooth1=t(as.data.frame(smooth1))
  df_w4=data.frame(df_w3,smooth1)
  
  for (i in (1:(dim(df_w4)[1])))  
    if (df_w4$distance[i]>0.5 & ((df_w4$value[i]>df_w4$trend1[i])& (df_w4$smooth1[i]<df_w4$value[i])|(df_w4$value[i]<df_w4$trend1[i])& (df_w4$smooth1[i]>df_w4$value[i]))){
      df_w4$smooth[i]=df_w4$smooth1[i]
    }else {
      df_w4$smooth[i]=df_w4$value[i]
    }
  
  
  
  
  for (i in (1:(dim(df_w4)[1])))
    df_w4$month[i]=format(as.Date(as.numeric(as.character(df_w4$variable[i])), origin = "1970-01-01"),"%m")
  
  piu=df_w4 %>% group_by(code2,month) %>% summarise(Mean_sales = mean(smooth))
  
  bbju=piu %>% group_by(code2) %>% summarise(Mean_sales_year = mean(Mean_sales))
  
  
  piu=as.data.frame(piu)
  bbju=as.data.frame(bbju)
  
  
  for (i in (1:(dim(piu)[1])))
    piu$mean[i]=bbju[match(piu[,1],bbju[,1])[i],2]
  
  for (i in (1:(dim(piu)[1])))
    piu$season[i]=piu$Mean_sales[i]/as.numeric(piu$mean[i])
  
  
  piu$season[is.na(piu$season)] <- 1 
  
  for (i in (1:dim(piu)[1]))
    piu$scep[i]=paste(piu[i,1],piu[i,2])
  
  for (i in (1:number_of_columns_df3))
    df_w4$scep[i]=paste(df_w4$code2[i],df_w4$month[i])
  
  
  for (i in (1:(dim(df_w4)[1])))
    df_w4$season[i]=piu[match(df_w4$scep,piu$scep)[i],5]
  
  
  
  #model willemein
  
  pivot_for_w=dcast(df_w,code2~variable)
  
  
  total_length=length(pivot_for_w)
  
  d=data.frame()
  
  d[1,]=0
  
  number_of_columns_pivot=dim(pivot_for_w)[2]
  
  

  
  
  #model Holt-Winters
  
  
  tt=t(pivot_for_w[,2:number_of_columns_pivot])
  months_HW=12
  predict_HW=data.frame()
  
  for (i in (1:number_of_items))
  {
    for (j in (1:12))
      predict_HW[j,i]=0
  }
  
  for (i in (1:number_of_items))
    try({
      ser.g.02<-ts(data=tt[,i],frequency=12,start=c(2018,1))
      ser.g.HW=HoltWinters(ser.g.02,seasonal="add")
      ser.g.predict=predict(ser.g.HW,n.ahead=months_HW)
      for (j in (1:months_HW))
        predict_HW[j,i]=as.data.frame(ser.g.predict)[j,1]
      
    },silent=T)
  
  
  for (i in (1:dim(predict_HW)[1])){
    for (j in (1:dim(predict_HW)[2]))
      if (predict_HW[i,j]<0){
        predict_HW[i,j]=0
      }else {
        predict_HW[i,j]=predict_HW[i,j]
      }}
  
  predict_HW=data.frame(pivot_for_w[,1],t(predict_HW))
  
  
  #model avg
  
  sred=pivot_for_w[,2:number_of_columns_pivot]
  
  
  
  pp=data.frame()  
  
  for (j in (1:number_of_items)){
    as=mean(as.numeric(sred[j,]))
    for (i in (1:period)){
      if (as==0){
        sred[j,i]=sred[j,i]
      } else{
        if ((sred[j,i]/(as+1)<0.9)|  (sred[j,i]/as>1.1)){
          sred[j,i]=as
        }}}
    pp[j,1]=mean(as.numeric(sred[j,]))}    
  
  
  predict_avg=data.frame(pivot_for_w[,1],pp)   
  
  
  #lineal
  
  k=data.frame()
  l=data.frame()
  for (j in (1:number_of_columns_df_sales)){
    model<-lm(data=df_w4[(1+period*(j-1)):(period*j),],value~c(1:period))
    
    for (i in (1:(period+12))){
      k[i,j]=model$coefficients[1]+i*model$coefficients[2]
      
    }}
  
  
  
  rep=data.frame()
  bb=k[period:(period+12),]
  
  for (j in (1:number_of_columns_df_sales)){
    for (i in (1:12)){
      rep[j,i]=as.integer(bb[1,j]*piu[(j*i),5])
    }
  }
  
  
  
  for (i in (1:dim(rep)[1])){
    for (j in (1:dim(rep)[2]))
      if (rep[i,j]<0){
        rep[i,j]=0
      }else {
        rep[i,j]=rep[i,j]
      }}
  
  
  predict_lm=data.frame(unique(df_w4$code2),rep)
  
  
  
  chhot_sort=sort_df(chhot,vars='code2')
  
  
  
  for (i in 1:number_of_columns_df_sales)
    arima_predict[i,1]<-as.character(arima_predict[i,1])
  
  
  
  names(arima_predict)[1]=c('code2')
  arima_sort=sort_df(arima_predict,vars='code2')
  
  
  
  predict_all=data.frame(chhot_sort,arima_sort[,2:13],predict_HW[,2:13],predict_avg[,2],predict_lm[,2:13])
  
  
  
  
  predict_all=data.frame(predict_all[,1:14],predict_all[,17:(dim(predict_all)[2])])
  
  for (j in (15:(dim(predict_all)[2]))){
    for (i in (1:(dim(predict_all)[1])))
      predict_all[i,j]=as.integer(predict_all[i,j])
  }
  
  
  
  
  
  dates_predict=seq(as.Date("2020/03/01"), by = "month", length.out = 12)
  
  
  
  
  colnames_pred=c('code2','infor','art','art_supplyer', 'isstop', 'brand', 'name', 'isnew', 'pricetype', 'supplier', 'delivery', 'production', 'unitpack',
                  'category', 
                  seq(as.Date("2015/01/01"), by = "month", length.out = period), paste('ARIMA',dates_predict[1]),paste('ARIMA',
                                                                                                                       dates_predict[2]),paste('ARIMA',dates_predict[3]),paste('ARIMA',dates_predict[4]),
                  paste('ARIMA',dates_predict[5]),paste('ARIMA',dates_predict[6]),paste('ARIMA',
                                                                                        dates_predict[7]),paste('ARIMA',dates_predict[8]),paste('ARIMA',dates_predict[9]),
                  paste('ARIMA',dates_predict[10]),paste('ARIMA',dates_predict[11]),paste('ARIMA',dates_predict[12]),
                  paste('HW',dates_predict[1]),paste('HW',
                                                     dates_predict[2]),paste('HW',dates_predict[3]),paste('HW',dates_predict[4]),
                  paste('HW',dates_predict[5]),paste('HW',dates_predict[6]),paste('HW',
                                                                                  dates_predict[7]),paste('HW',dates_predict[8]),paste('HW',dates_predict[9]),
                  paste('HW',dates_predict[10]),paste('HW',dates_predict[11]),paste('HW',dates_predict[12]),'avg',paste('lr',dates_predict[1]),paste('lr',
                                                                                                                                                     dates_predict[2]),paste('lr',dates_predict[3]),paste('lr',dates_predict[4]),
                  paste('lr',dates_predict[5]),paste('lr',dates_predict[6]),paste('lr',
                                                                                  dates_predict[7]),paste('lr',dates_predict[8]),paste('lr',dates_predict[9]),
                  paste('lr',dates_predict[10]),paste('lr',dates_predict[11]),paste('lr',dates_predict[12]))
  
  names(predict_all)=colnames_pred
  
  predict12=predict_all
  
  
  for (i in (1:number_of_items)){
    summ_arima=0
    for (j in ((number_of_columns-1):(number_of_columns+4))){
      summ_arima=summ_arima+predict12[i,j]
      
      predict12$summ_arima[i]=summ_arima}}
  
  

  for (i in (1:number_of_items)){
    summ_HW=0
    for (j in ((number_of_columns+11):(number_of_columns+16))){
      summ_HW=summ_HW+predict12[i,j]
      
      predict12$summ_HW[i]=summ_HW}}
  
  
  for (i in (1:number_of_items))
    predict12$summ_avg[i]=predict12$avg[i]*6
  
  
  for (i in (1:number_of_items)){
    summ_lm=0
    for (j in ((number_of_columns+24):(number_of_columns+29))){
      summ_lm=summ_lm+predict12[i,j]
      
      predict12$summ_lm[i]=summ_lm}}
  
  

  
  for (i in (1:number_of_items)){
    summ_fact=0
    for (j in ((number_of_columns-7):(number_of_columns-2))){
      summ_fact=summ_fact+predict12[i,j]
      
      predict12$summ_fact[i]=summ_fact}}
  
  
  
  for (i in (1:number_of_items)){
    if (predict12$summ_fact[i]==0){
      predict12$error_arima[i]=0
    }else{
      predict12$error_arima[i]=abs((predict12$summ_arima[i]-predict12$summ_fact[i])/predict12$summ_fact[i])
      
    }}
  

  
  for (i in (1:number_of_items)){
    if (predict12$summ_fact[i]==0){
      predict12$error_HW[i]=0
    }else{
      predict12$error_HW[i]=abs((predict12$summ_HW[i]-predict12$summ_fact[i])/predict12$summ_fact[i])}}
  
  for (i in (1:number_of_items)){
    if (predict12$summ_fact[i]==0){
      predict12$error_avg[i]=0
    }else{
      predict12$error_avg[i]=abs((predict12$summ_avg[i]-predict12$summ_fact[i])/predict12$summ_fact[i])}}
  
  
  for (i in (1:number_of_items)){
    if (predict12$summ_fact[i]==0){
      predict12$error_lm[i]=0
    }else{
      predict12$error_lm[i]=abs((predict12$summ_lm[i]-predict12$summ_fact[i])/predict12$summ_fact[i])}}
  
  
 
  
  for (i in (1:number_of_items))
    predict12$min_error[i]=min(predict12[i,(number_of_columns+41):(number_of_columns+44)])
  
  
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$method[i]='arima'
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$method[i]='HW'
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$method[i]='avg'
        }else { 
          
          predict12$method[i]='lm'
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_1[i]=predict12[i,(number_of_columns-1)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_1[i]=predict12[i,(number_of_columns+11)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_1[i]=predict12[i,(number_of_columns+23)]
        }else {
          predict12$predict_1[i]=predict12[i,(number_of_columns+24)]
        }}}}
  
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_2[i]=predict12[i,(number_of_columns)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_2[i]=predict12[i,(number_of_columns+12)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_2[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_2[i]=predict12[i,(number_of_columns+25)]
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_3[i]=predict12[i,(number_of_columns+1)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_3[i]=predict12[i,(number_of_columns+13)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_3[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_3[i]=predict12[i,(number_of_columns+26)]
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_4[i]=predict12[i,(number_of_columns+2)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_4[i]=predict12[i,(number_of_columns+14)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_4[i]=predict12[i,(number_of_columns+23)]
        }else { 
          predict12$predict_4[i]=predict12[i,(number_of_columns+27)]
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_5[i]=predict12[i,(number_of_columns+3)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_5[i]=predict12[i,(number_of_columns+15)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_5[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_5[i]=predict12[i,(number_of_columns+28)]
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_6[i]=predict12[i,(number_of_columns+4)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_6[i]=predict12[i,(number_of_columns+16)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_6[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_6[i]=predict12[i,(number_of_columns+29)]
        }}}}
  
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_7[i]=predict12[i,(number_of_columns+5)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_7[i]=predict12[i,(number_of_columns+17)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_7[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_7[i]=predict12[i,(number_of_columns+30)]
        }}}}
  
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_8[i]=predict12[i,(number_of_columns+6)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_8[i]=predict12[i,(number_of_columns+18)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_8[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_8[i]=predict12[i,(number_of_columns+31)]
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_9[i]=predict12[i,(number_of_columns+7)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_9[i]=predict12[i,(number_of_columns+19)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_9[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_9[i]=predict12[i,(number_of_columns+32)]
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_10[i]=predict12[i,(number_of_columns+8)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_10[i]=predict12[i,(number_of_columns+20)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_10[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_10[i]=predict12[i,(number_of_columns+33)]
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_11[i]=predict12[i,(number_of_columns+9)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_11[i]=predict12[i,(number_of_columns+21)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_11[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_11[i]=predict12[i,(number_of_columns+34)]
        }}}}
  
  for (i in (1:number_of_items)){
    if(predict12$min_error[i]==predict12$error_arima[i]){
      predict12$predict_12[i]=predict12[i,(number_of_columns+10)]
    }else { 
      if(predict12$min_error[i]==predict12$error_HW[i]){
        predict12$predict_12[i]=predict12[i,(number_of_columns+22)]
      }else { 
        if(predict12$min_error[i]==predict12$error_avg[i]){
          predict12$predict_12[i]=predict12[i,(number_of_columns+23)]
        }else { 
          
          predict12$predict_12[i]=predict12[i,(number_of_columns+35)]
        }}}}
  
  predict13=predict12
  
  dff=tibble(mn = predict13$infor) %>% 
    mutate(mn_new = str_pad(mn, 10,  pad = "0"))
  
  for (i in (1:number_of_items))
    predict13[i,2]=dff[i,2]
  
  
  predict14=rbind(predict14,predict13)
}




#write.table(predict14, file="prog_июнь.csv ", sep = ";",dec=",",col.names = NA)













#записываем файл, который будем применять в следующем прогнозе, для выявления лучшего прогноза 
#записываем прогноз на следующий месяц по каждому месяцу
itog_table=data.frame(predict14[2],predict14[63],predict14[75],predict14[87],predict14[88])




itog_table22=data.frame(predict14[2],predict14[110:121])


#itog_table22$predict_1[1]=100

for (i in (1:nrow(itog_table22)))
  itog_table22[i,14]=methods_df[match(as.integer(itog_table22[,1]),methods_df[,2])[i],9]

itog_table22[is.na(itog_table22)]<-0



for (i in (1:nrow(itog_table22))){
  if (itog_table22$V14[i]==0){
    itog_table22$pred_1[i]=predict14[i,110]
    itog_table22$pred_2[i]=predict14[i,111]
    itog_table22$pred_3[i]=predict14[i,112]
    itog_table22$pred_4[i]=predict14[i,113]
    itog_table22$pred_5[i]=predict14[i,114]
    itog_table22$pred_6[i]=predict14[i,115]
    itog_table22$pred_7[i]=predict14[i,116]
    itog_table22$pred_8[i]=predict14[i,117]
    itog_table22$pred_9[i]=predict14[i,118]
    itog_table22$pred_10[i]=predict14[i,119]
    itog_table22$pred_11[i]=predict14[i,120]
    itog_table22$pred_12[i]=predict14[i,121]
    
    }else{
    if (itog_table22$V14[i]=="arima"){
      itog_table22$pred_1[i]=predict14[i,62]
      itog_table22$pred_2[i]=predict14[i,63]
      itog_table22$pred_3[i]=predict14[i,64]
      itog_table22$pred_4[i]=predict14[i,65]
      itog_table22$pred_5[i]=predict14[i,66]
      itog_table22$pred_6[i]=predict14[i,67]
      itog_table22$pred_7[i]=predict14[i,68]
      itog_table22$pred_8[i]=predict14[i,69]
      itog_table22$pred_9[i]=predict14[i,70]
      itog_table22$pred_10[i]=predict14[i,71]
      itog_table22$pred_11[i]=predict14[i,72]
      itog_table22$pred_12[i]=predict14[i,73]
      
    }else{
      if (itog_table22$V14[i]=="HW"){
        itog_table22$pred_1[i]=predict14[i,74]
        itog_table22$pred_2[i]=predict14[i,75]
        itog_table22$pred_3[i]=predict14[i,76]
        itog_table22$pred_4[i]=predict14[i,77]
        itog_table22$pred_5[i]=predict14[i,78]
        itog_table22$pred_6[i]=predict14[i,79]
        itog_table22$pred_7[i]=predict14[i,80]
        itog_table22$pred_8[i]=predict14[i,81]
        itog_table22$pred_9[i]=predict14[i,82]
        itog_table22$pred_10[i]=predict14[i,83]
        itog_table22$pred_11[i]=predict14[i,84]
        itog_table22$pred_12[i]=predict14[i,85]
      }else{
        if (itog_table22$V14[i]=="avg"){
          itog_table22$pred_1[i]=predict14[i,86]
          itog_table22$pred_2[i]=predict14[i,86]
          itog_table22$pred_3[i]=predict14[i,86]
          itog_table22$pred_4[i]=predict14[i,86]
          itog_table22$pred_5[i]=predict14[i,86]
          itog_table22$pred_6[i]=predict14[i,86]
          itog_table22$pred_7[i]=predict14[i,86]
          itog_table22$pred_8[i]=predict14[i,86]
          itog_table22$pred_9[i]=predict14[i,86]
          itog_table22$pred_10[i]=predict14[i,86]
          itog_table22$pred_11[i]=predict14[i,86]
          itog_table22$pred_12[i]=predict14[i,86]
        }else{
          itog_table22$pred_1[i]=predict14[i,87]
          itog_table22$pred_2[i]=predict14[i,88]
          itog_table22$pred_3[i]=predict14[i,89]
          itog_table22$pred_4[i]=predict14[i,90]
          itog_table22$pred_5[i]=predict14[i,91]
          itog_table22$pred_6[i]=predict14[i,92]
          itog_table22$pred_7[i]=predict14[i,93]
          itog_table22$pred_8[i]=predict14[i,94]
          itog_table22$pred_9[i]=predict14[i,95]
          itog_table22$pred_10[i]=predict14[i,96]
          itog_table22$pred_11[i]=predict14[i,97]
          itog_table22$pred_12[i]=predict14[i,98]
        }
    }
  }}}


write.table(itog_table22, file="итог_июнь_2022.csv ", sep = ";",dec=",",col.names = NA)  
write.table(itog_table, file="Предыдущий прогноз_июнь_2022.csv ", sep = ";",dec=",",col.names = NA)


end_time<-Sys.time()


end_time-start_time
