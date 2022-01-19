
setwd("C:/Users/danie/Desktop/2021-2/PARCIAL")

library(readxl)
library(readr)
library(survey)
library(dplyr)



THOGAR_DBF <- read_excel("THOGAR.DBF.xlsx")

TSDEM_DBF <- read_excel("TSDEM.DBF.xlsx")

TVIVIENDA_DBF <- read_excel("TVIVIENDA.DBF.xlsx")

Tviajes_modificado <- read_delim("Tviajes_modificado.csv",";", escape_double = FALSE, trim_ws = TRUE)

TTRANSPORTE_DBF <- read_delim("TTRANSPORTE.DBF.csv",";", escape_double = FALSE, trim_ws = TRUE)



################################################################################
################################################################################

#Viajes realizados el día observado entre semana por la población de 6 años y más

### unimos las tres bases
Base<-TSDEM_DBF%>%
  left_join(THOGAR_DBF,by=("ID_HOG"="ID_HOG"))%>%
  left_join(TVIVIENDA_DBF,by=("ID_VIV"="ID_VIV"))%>%
  filter(EDAD>=6)%>%
  select(ID_VIV,ID_HOG,HOGAR,P5_4,FACTOR,UPM_DIS,EST_DIS,ENT,DISTRITO,EDAD)
 

  ### Se agrupan por identificador de vivienda y se suman los numeros de viajes por persona en la vivienda
# se obtiene el numero de viajes por vivienda
  
  T_viajes<-Base%>%
  group_by(ID_VIV,UPM_DIS,EST_DIS,DISTRITO,FACTOR)%>%
  summarise(suma=sum(P5_4))


### Se agrupa por unidad primaria de muestreo y se multiplica por el factor de expansión
  
  T_viajes<-T_viajes%>%
  group_by(UPM_DIS,EST_DIS,DISTRITO,FACTOR)%>%
  summarise(suma=sum(suma))%>%
  mutate(prod=suma*FACTOR)

### Se agrupa por Estratos de diseño
 
   T_viajes<-T_viajes%>%
  group_by(EST_DIS,DISTRITO)%>%
  summarise(suma=sum(prod))

## suma los estimadores de los estratos

tpi_est<-sum(T_viajes$suma)


##### VARIANZA ESTIMADA DEL ESTIMADOR

T_viajes_var<-TVIVIENDA_DBF%>%
  left_join(THOGAR_DBF,by=("ID_VIV"="ID_VIV"))%>%
  left_join(TSDEM_DBF,by=("ID_HOG"="ID_HOG"))%>%
  filter(EDAD>=6)%>%
  select(ID_VIV,ID_HOG,HOGAR,P5_4,FACTOR,UPM_DIS,EST_DIS,ENT,DISTRITO)


E1<-T_viajes_var%>%
  group_by(UPM_DIS,EST_DIS,DISTRITO)%>%
  summarise(total=sum(P5_4*FACTOR))  ## total ponderado por su factor
  
E2<-T_viajes_var%>%
  group_by(EST_DIS,DISTRITO)%>%
  summarise(tot_ponderado=sum(P5_4*FACTOR)/n_distinct(UPM_DIS),n_upms=n_distinct(UPM_DIS),
            term=(n_upms/(n_upms-1)))   ## total poderado por su factor / # de UPMs en estrato

E<-E1%>%
  left_join(E2,by=("EST_DIS "="EST_DIS" ))%>%
  mutate(suma=(total-tot_ponderado)**2)



E_distrito<-E%>%
  group_by(DISTRITO.x)%>%
  summarise(suma=sum(term*suma))

var_est<-sum(E_distrito$suma)


cv<-sqrt(var_est)/tpi_est

cv*100


100-(54593*100)/66625

####################################################################################
####################################################################################
####################################################################################





##
## 4 ESTIMADORES


# TOTAL DE VIAJES (SEMANA Y SABADO)x 
# TOTAL MODOS DE TRANSPORTE (SEMANA Y SABADO) Ulpiano
# Total viajes por propósito de viaje x
# Viajes dia obs por hora x

### PRESENTACION

# Introdución 

# Replica

# Sugerencias 

# Conclusiones propias


### Johan Viernes
### Sabado noche (Resultados de los calculos)
### Domingo tarde/noche (Resultados para presentacion)






################################################################################
################################################################################
################################################################################
# Número de viajes realizados un día entre semana por la población de 6 años y más según hora
# inicio del viaje1 (Miles)


## Filtramos los viajes realizados entre semana por los que se iniciar en los primeros 15 min de cada hora

Base3<-Tviajes_modificado%>%
  filter(P5_9_2<15)
  
## Unimos las bases

t_horas<-Base3%>%
  left_join(TSDEM_DBF,by=("ID_SOC"="ID_SOC"))%>%
  left_join(THOGAR_DBF,by=("ID_HOG"="ID_HOG"))%>%
  left_join(TVIVIENDA_DBF,by=("ID_VIV"="ID_VIV"))%>%
  filter(EDAD>=6)%>%
  select(ID_SOC,ID_VIV,ID_HOG,HOGAR,P5_9_1,P5_9_2,P5_4,FACTOR.x,UPM_DIS.x,EST_DIS.x,ENT,DISTRITO,EDAD)

  rename(t_horas,EST_DIS=EST_DIS.x)



## 
x<-c("Hora","Total_estimado","CV%")


for (i in sort(unique(Base3$P5_9_1)) ){
   Base3.1<-t_horas%>%
    filter(P5_9_1==i)%>%
    group_by(ID_SOC,ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO,FACTOR.x)%>%
    summarise(suma=n())%>%
    
    group_by(ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO,FACTOR.x)%>%
    summarise(suma=sum(suma))%>%
    ### Se agrupa por unidad primaria de muestreo y se multiplica por el factor de expansion
    group_by(UPM_DIS.x,EST_DIS.x,DISTRITO,FACTOR.x)%>%
    summarise(suma=sum(suma))%>%
    mutate(prod=suma*FACTOR.x)%>%
    group_by(EST_DIS.x,DISTRITO)%>%
    summarise(suma=sum(prod))%>%
    group_by(DISTRITO)%>%
    summarise(suma=sum(suma))
  
  
    tpi_est<-sum(Base3.1$suma)
  
  

      
############ CALCULO DE VARIANZA ESTIMADA DEL ESTIMADOR
    n_upm<-Base%>%
      group_by(EST_DIS)%>%
      summarise(n_UPm=sum(n_distinct(UPM_DIS)))
    
    E1<-t_horas%>%
      filter(P5_9_1==i)%>%
      group_by(ID_SOC,ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO,FACTOR.x)%>%
      summarise(suma=n())%>%
      group_by(UPM_DIS.x,EST_DIS.x,DISTRITO)%>%
      summarise(total=sum(suma*FACTOR.x))%>%
      rename("EST_DIS"="EST_DIS.x")
      ## total ponderado por su factor
   

      E2<-t_horas%>%
        filter(P5_9_1==i)%>%
        group_by(UPM_DIS.x,EST_DIS.x,FACTOR.x)%>%
        summarise(suma=n())%>%
        group_by(EST_DIS.x)%>%
        summarise(suma=sum(suma*FACTOR.x))%>%
        rename("EST_DIS"="EST_DIS.x")%>%
        left_join(n_upm,by=("EST_DIS"="EST_DIS"))%>%
        mutate(tot_ponderado=suma/n_UPm,term=(n_UPm/(n_UPm-1)))
    ## total poderado por su factor / # de UPMs en estrato
    
    E<-E1%>%
      left_join(E2,by=("EST_DIS"="EST_DIS" ))%>%
      mutate(suma=(total-tot_ponderado)**2)
    
    E_distrito<-E%>%
      group_by(DISTRITO)%>%
      summarise(suma=sum(term*suma))
    
    var_est<-sum(E_distrito$suma)
    
    cv<-sqrt(var_est)/tpi_est
    
    cv_porc<-cv*100
    
    x<-rbind(x,c(i,tpi_est,round(cv_porc,4)))
}

# estimador del de viajes por hora
x

  
#####################################################################################
#####################################################################################
#####################################################################################
####  Totales por  Propósito del viaje


t_proposito<-Tviajes_modificado%>%
  left_join(TSDEM_DBF,by=("ID_SOC"="ID_SOC"))%>%
  left_join(THOGAR_DBF,by=("ID_HOG"="ID_HOG"))%>%
  left_join(TVIVIENDA_DBF,by=("ID_VIV"="ID_VIV"))%>%
  filter(EDAD>=6)%>%
  mutate(P5_13=case_when(P5_13=="01"~"01",
                         P5_13=="02"~"02",
                         P5_13=="03"~"03",
                         TRUE ~ "10"))%>%
  select(ID_SOC,ID_VIV,ID_HOG,HOGAR,P5_13,FACTOR.x,UPM_DIS.x,EST_DIS.x,ENT.x,DISTRITO.x,EDAD)


y <- data.frame(matrix(ncol = 3, nrow = 0))
nombres_columnas <- c("proposito", "Total_Estimado", "CV_porc")
colnames(y) <- nombres_columnas


j<-1

for (i in sort(unique(t_proposito$P5_13)) ){
 
  Base4.1<-t_proposito%>%
    filter(P5_13==i)%>%
    group_by(ID_SOC,ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
    summarise(suma=n())%>%
    
    group_by(ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
    summarise(suma=sum(suma))%>%
    ### Se agrupa por unidad primaria de muestreo y se multiplica por el factor de expansion
    group_by(UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
    summarise(suma=sum(suma))%>%
    mutate(prod=suma*FACTOR.x)%>%
    group_by(EST_DIS.x,DISTRITO.x)%>%
    summarise(suma=sum(prod))%>%
    group_by(DISTRITO.x)%>%
    summarise(suma=sum(suma))
  
  
  tpi_est<-sum(Base4.1$suma)
  
  
  
  ############ CALCULO DE VARIANZA ESTIMADA DEL ESTIMADOR
  n_upm<-Base%>%
    group_by(EST_DIS)%>%
    summarise(n_UPm=sum(n_distinct(UPM_DIS)))
  
  E1<-t_proposito%>%
    filter(P5_13==i)%>%
    group_by(ID_SOC,ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
    summarise(suma=n())%>%
    group_by(UPM_DIS.x,EST_DIS.x,DISTRITO.x)%>%
    summarise(total=sum(suma*FACTOR.x))%>%
    rename("EST_DIS"="EST_DIS.x")  ## total ponderado por su factor
  
  E2<-t_proposito%>%
    filter(P5_13==i)%>%
    group_by(UPM_DIS.x,EST_DIS.x,FACTOR.x)%>%
    summarise(suma=n())%>%
    group_by(EST_DIS.x)%>%
    summarise(suma=sum(suma*FACTOR.x))%>%
    rename("EST_DIS"="EST_DIS.x")%>%
    left_join(n_upm,by=("EST_DIS"="EST_DIS"))%>%
    mutate(tot_ponderado=suma/n_UPm,term=(n_UPm/(n_UPm-1)))
  ## total poderado por su factor / # de UPMs en estrato
  
  E<-E1%>%
    left_join(E2,by=("EST_DIS"="EST_DIS" ))%>%
    mutate(suma=(total-tot_ponderado)**2)
  
  E_distrito<-E%>%
    group_by(DISTRITO.x)%>%
    summarise(suma=sum(term*suma))
  
  var_est<-sum(E_distrito$suma)
  
  
  cv<-sqrt(var_est)/tpi_est
  
  cv_porc<-cv*100
  
  y[j,]<-c(i,as.double(tpi_est),round(as.double(cv_porc),4))
  j<-j+1
}

# estimador del total de propositos de viaje

y

y<-y%>%
  left_join(y_1,by=("proposito"="id_proposito"))
  

id_proposito <-c("01","02","03","04","05","06","07","08","09","10","99")
proposito<-c(  
	"Ir al hogar",
	"Ir al trabajo",
	"Ir a estudiar",
	"Ir de compras (bienes y servicios)",
	"Convivir a(amigos o familiares), deportes o recreación",
	"Llevar o recoger a alguien",
	"Hacer un trámite",
	"Ir al médico o recibir atención de salud",
	"Ir a acto religioso",
	"Otro",
	"No sabe") 
y_1<-data.frame(id_proposito ,proposito)
  




#################################################################################
########################  Viajes realizados en un día entre semana en automovil por numero de personas




t_viajes_auto<-Tviajes_modificado%>%
  left_join(TSDEM_DBF,by=("ID_SOC"="ID_SOC"))%>%
  left_join(THOGAR_DBF,by=("ID_HOG"="ID_HOG"))%>%
  left_join(TVIVIENDA_DBF,by=("ID_VIV"="ID_VIV"))%>%
  select(ID_SOC,ID_VIV,ID_HOG,HOGAR,P5_19,FACTOR.x,UPM_DIS.x,EST_DIS.x,ENT.x,DISTRITO.x,EDAD)



z<-c("Número de personas","Total_estimado","CV%")

for (i in sort(unique(t_viajes_auto$P5_19)) ){

  
   Base5.1<-t_viajes_auto%>%
    filter(P5_19==i)%>%
     group_by(ID_SOC,ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
     summarise(suma=n())%>%
     group_by(ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
     summarise(suma=sum(suma))%>%
     group_by(UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
     summarise(suma=sum(suma))%>%
     mutate(prod=suma*FACTOR.x)%>%
     group_by(EST_DIS.x,DISTRITO.x)%>%
     summarise(suma=sum(prod))%>%
     group_by(DISTRITO.x)%>%
     summarise(suma=sum(suma))
  
  
    tpi_est<-sum(Base5.1$suma)
  
  
  
  ############ CALCULO DE VARIANZA ESTIMADA DEL ESTIMADOR
     E1<-t_viajes_auto%>%
      filter(P5_19==i)%>%
      group_by(ID_SOC,ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
      summarise(suma=n())%>%
      group_by(UPM_DIS.x,EST_DIS.x,DISTRITO.x)%>%
      summarise(total=sum(suma*FACTOR.x))%>%
      rename("EST_DIS"="EST_DIS.x")  ## total ponderado por su factor
  
    n_upm<-Base%>%
      group_by(EST_DIS)%>%
      summarise(n_UPm=sum(n_distinct(UPM_DIS)))

    E2<-t_viajes_auto%>%
      filter(P5_19==i)%>%
      group_by(UPM_DIS.x,EST_DIS.x,FACTOR.x)%>%
      summarise(suma=n())%>%
      group_by(EST_DIS.x)%>%
      summarise(suma=sum(suma*FACTOR.x))%>%
      rename("EST_DIS"="EST_DIS.x")%>%
      left_join(n_upm,by=("EST_DIS"="EST_DIS"))%>%
      mutate(tot_ponderado=suma/n_UPm,term=(n_UPm/(n_UPm-1)))
    ## total poderado por su factor / # de UPMs en estrato
    
    
    E<-E1%>%
      left_join(E2,by=("EST_DIS"="EST_DIS" ))%>%
      mutate(suma=(total-tot_ponderado)**2)
    
    E_distrito<-E%>%
      group_by(DISTRITO.x)%>%
      summarise(suma=sum(term*suma))
    
    var_est<-sum(E_distrito$suma)
    
    cv<-sqrt(var_est)/tpi_est
    
    cv_porc<-cv*100
  z<-rbind(z,c(i,tpi_est,round(cv_porc,4)))
  
}

# estimador del total de propositos de viaje

z



################################################################################
################################################################################
# Modos de transporte

T_modos_transporte<-Tviajes_modificado%>%
  left_join(TSDEM_DBF,by=("ID_SOC"="ID_SOC"))%>%
  left_join(THOGAR_DBF,by=("ID_HOG"="ID_HOG"))%>%
  left_join(TVIVIENDA_DBF,by=("ID_VIV"="ID_VIV"))%>%
  left_join(TTRANSPORTE_DBF,by=("ID_VIA"="ID_VIA"))%>%
  rename("EDAD"="EDAD.x")%>%
  filter(EDAD>=6)%>%
  select(ID_SOC,ID_VIV,ID_HOG,HOGAR,P5_14,FACTOR.x,UPM_DIS.x,EST_DIS.x,ENT.x,DISTRITO.x)


w <- data.frame(matrix(ncol = 3, nrow = 0))
nombres_columnas <- c("Modo_Transporte", "Total_Estimado", "CV_porc")
colnames(w) <- nombres_columnas


j<-1

for (i in sort(unique(T_modos_transporte$P5_14)) ){
  
  Base6.1<-T_modos_transporte%>%
    filter(P5_14==i)%>%
    group_by(ID_SOC,ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
    summarise(suma=n())%>%
    
    group_by(ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
    summarise(suma=sum(suma))%>%
    ### Se agrupa por unidad primaria de muestreo y se multiplica por el factor de expansion
    group_by(UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
    summarise(suma=sum(suma))%>%
    mutate(prod=suma*FACTOR.x)%>%
    group_by(EST_DIS.x,DISTRITO.x)%>%
    summarise(suma=sum(prod))%>%
    group_by(DISTRITO.x)%>%
    summarise(suma=sum(suma))
  
  
  tpi_est<-sum(Base6.1$suma)
  
  
  
  ############ CALCULO DE VARIANZA ESTIMADA DEL ESTIMADOR
  n_upm<-Base%>%
    group_by(EST_DIS)%>%
    summarise(n_UPm=sum(n_distinct(UPM_DIS)))
  
  E1<-T_modos_transporte%>%
    filter(P5_14==i)%>%
    group_by(ID_SOC,ID_VIV,UPM_DIS.x,EST_DIS.x,DISTRITO.x,FACTOR.x)%>%
    summarise(suma=n())%>%
    group_by(UPM_DIS.x,EST_DIS.x,DISTRITO.x)%>%
    summarise(total=sum(suma*FACTOR.x))%>%
    rename("EST_DIS"="EST_DIS.x")  ## total ponderado por su factor
  
  E2<-T_modos_transporte%>%
    filter(P5_14==i)%>%
    group_by(UPM_DIS.x,EST_DIS.x,FACTOR.x)%>%
    summarise(suma=n())%>%
    group_by(EST_DIS.x)%>%
    summarise(suma=sum(suma*FACTOR.x))%>%
    rename("EST_DIS"="EST_DIS.x")%>%
    left_join(n_upm,by=("EST_DIS"="EST_DIS"))%>%
    mutate(tot_ponderado=suma/n_UPm,term=(n_UPm/(n_UPm-1)))
  ## total poderado por su factor / # de UPMs en estrato
  
  E<-E1%>%
    left_join(E2,by=("EST_DIS"="EST_DIS" ))%>%
    mutate(suma=(total-tot_ponderado)**2)
  
  E_distrito<-E%>%
    group_by(DISTRITO.x)%>%
    summarise(suma=sum(term*suma))
  
  var_est<-sum(E_distrito$suma)
  
  
  cv<-sqrt(var_est)/tpi_est
  
  cv_porc<-cv*100
  
  w[j,]<-c(i,as.double(tpi_est),round(as.double(cv_porc),4))
  j<-j+1
}

# estimador del total de modos de viaje

w




##################################################################################

DISTRITOS<-Base%>%
  group_by(DISTRITO,EST_DIS,UPM_DIS,FACTOR)%>%
  summarise(upms=n())




