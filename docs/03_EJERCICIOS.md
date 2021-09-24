# Ejercicio fuera de muestra 

## Aplicación sobre la inflación de Honduras




Los pronósticos de inflación a determinado horizonte h, realizados en el momento t a partir de una determinada especificación econométrica j ($\hat{\pi}_{t+h\mid t}^{j}$)  son comparados con los datos efectivos ($\pi_{t+h}$), deduciendo los errores de pronósticos ($E_{t+h}^{j}$) en conformidad a la ecuación (\#eq:1) la cual aplica para cualquier tipo de ventana $h$.

\begin{equation} 
 ECM_{h}^{j}=\frac{\sum\limits_{n=0}^{N=g_{h}-1} (E_{t+h+n}^{j})^{2}}{g_{h}}
 (\#eq:1)
\end{equation} 


En el siguiente código, se seleccionará la mejor específicación según el criterio de información Akaike (AIC) para la inflación de Honduras tomando como muestra enero 1994 a diciembre 2006. 


```r
library("xts") 
library("zoo")
library("astsa")
library("forecast")
library("ggplot2")
library("forecast")
library("ggfortify")
library("stargazer")
library("urca")
library("dynlm")
library("scales")
library("quantmod")
library("dplyr")
library("sandwich")
library("knitr") 
library("dynlm")
library("stargazer")
MES<-as.xts(read.zoo("MES_HN.csv", index.column = 1, sep = ";", header=TRUE, format = "%d/%m/%Y"))
INFLA <-(log(MES$IPC)-stats::lag(log(MES$IPC), n=12))/stats::lag(log(MES$IPC), n=12)
INFLA1<-INFLA["1994-01-01/2006-12-01"]
INFLA_fit <-auto.arima(INFLA1, ic = c("aic"))
INFLA_fit
```

```
## Series: INFLA1 
## ARIMA(1,1,2) with drift 
## 
## Coefficients:
##           ar1      ma1      ma2  drift
##       -0.6503  -0.1120  -0.7132  0e+00
## s.e.   0.1504   0.1212   0.1035  1e-04
## 
## sigma^2 estimated as 2.125e-06:  log likelihood=793.57
## AIC=-1577.13   AICc=-1576.73   BIC=-1561.91
```
La mejor específicación seleccionada es un a ARIMA(1,1,2). El que formalmente puede escribirse como:

\begin{equation} 
\Phi(L)(1-L)\pi_{t}=\Theta(L)\epsilon_{t}
(\#eq:3)
\end{equation} 

Donde $\Phi(L)=(1+0.65L),$ $\Theta(L)=(1-0.11L-0.71L^{2})$. 


Luego de tener esa específicación podemos comparar su desempeño predictivo con respecto a los datos observados que se encuentran *fuera de la muestra de estimación*. 

Para ejemplificar se selecciona como horizonte predictivo 3 meses.


```r
FECHA            <-index(MES)
t_inicial        <-first(FECHA,'1 month')
index_final      <-last(index(FECHA))
fecha_contador   <-seq(as.Date(t_inicial), length =index_final, by = "months")
counter          <-c(1:index_final)  
contador         <-xts(x=counter, order.by = fecha_contador)
inicio_estimacion<-coredata(contador["1994-01-01"])[1]
final_estimacion <-coredata(contador["2006-12-01"])[1]
final_muestra    <-coredata(contador["2019-02-01"])[1]
H                <-3 #Horizonte predictivo
DENTRO           <- seq(as.Date(FECHA[inicio_estimacion]), 
length =final_estimacion+H-inicio_estimacion, by = "months")
FUERA            <- seq(as.Date(FECHA[final_estimacion+1]), 
length =final_muestra-H-final_estimacion, by = "months")
assign(paste('PI_', H, sep=''), xts(x=window(INFLA, start=FECHA[inicio_estimacion], end=FECHA[final_estimacion+H-1]),
order.by = DENTRO))
Y <-window(INFLA, start=FECHA[inicio_estimacion], end=FECHA[final_estimacion])
for(i in 1:length(FUERA)){
Y_F<-sarima.for(Y,H,1,1,2, xreg=NULL, 
                newxreg=NULL, plot= FALSE) 
dates_out<-as.Date(FECHA[final_estimacion+i+H-1])
Y_F_P<-xts(x=Y_F$pred[H], order.by = dates_out)
DENTRO<-seq(as.Date(FECHA[inicio_estimacion]), 
length =final_estimacion+1+i-inicio_estimacion, by = "months")
Y <-window(INFLA, start=FECHA[inicio_estimacion], end=FECHA[final_estimacion+i])
assign(paste('PI_', H, sep=''),rbind(get(paste('PI_', H, sep='')), 
Y_F_P))
}
```

![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-1.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-2.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-3.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-4.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-5.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-6.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-7.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-8.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-9.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-10.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-11.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-12.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-13.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-14.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-15.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-16.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-17.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-18.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-19.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-20.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-21.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-22.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-23.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-24.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-25.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-26.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-27.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-28.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-29.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-30.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-31.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-32.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-33.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-34.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-35.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-36.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-37.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-38.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-39.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-40.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-41.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-42.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-43.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-44.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-45.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-46.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-47.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-48.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-49.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-50.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-51.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-52.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-53.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-54.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-55.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-56.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-57.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-58.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-59.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-60.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-61.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-62.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-63.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-64.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-65.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-66.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-67.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-68.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-69.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-70.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-71.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-72.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-73.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-74.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-75.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-76.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-77.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-78.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-79.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-80.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-81.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-82.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-83.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-84.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-85.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-86.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-87.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-88.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-89.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-90.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-91.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-92.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-93.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-94.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-95.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-96.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-97.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-98.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-99.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-100.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-101.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-102.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-103.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-104.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-105.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-106.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-107.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-108.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-109.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-110.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-111.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-112.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-113.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-114.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-115.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-116.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-117.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-118.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-119.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-120.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-121.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-122.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-123.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-124.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-125.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-126.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-127.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-128.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-129.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-130.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-131.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-132.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-133.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-134.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-135.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-136.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-137.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-138.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-139.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-140.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-141.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-142.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-2-143.png)<!-- -->

```r
compara<-merge(INFLA,get(paste('PI_', H, sep='')),join='inner') 
compara<-data.frame(date=index(compara), coredata(compara))
compara$date<-as.Date(compara$date)
compara<-filter(compara, date>="2007-03-01" & date<="2019-02-01")
compara<-mutate(compara, DIFF =(IPC-IPC.1)^2)
ECM_1<-mean(compara$DIFF)
ECM_1
```

```
## [1] 5.891045e-07
```



Luego de tener esa específicación podemos comparar su desempeño predictivo con respecto a otra específicación. De acuerdo a la literura un benchmark natural es un simple *random walk* (ver @Ohanian2001 y @Rogoff1983 entre otros).

En efecto, con un modelo *random walk* podemos calcular para el mismo horizonte de pronóstico (3) su ECM y compararlo con el obtenido por el modelo ARIMA(1,1,2).


```r
DENTRO<- seq(as.Date(FECHA[inicio_estimacion]), 
length =final_estimacion+H-inicio_estimacion, by = "months")
assign(paste('PI_', H, sep=''), xts(x=window(INFLA, start=FECHA[inicio_estimacion], end=FECHA[final_estimacion+H-1]),
order.by = DENTRO))
Y <-window(INFLA, start=FECHA[inicio_estimacion], end=FECHA[final_estimacion])
for(i in 1:length(FUERA)){
Y_F<-sarima.for(Y,H,1,0,0, xreg=NULL, 
                newxreg=NULL, plot= FALSE) 
dates_out<-as.Date(FECHA[final_estimacion+i+H-1])
Y_F_P<-xts(x=Y_F$pred[H], order.by = dates_out)
DENTRO<-seq(as.Date(FECHA[inicio_estimacion]), 
length =final_estimacion+1+i-inicio_estimacion, by = "months")
Y <-window(INFLA, start=FECHA[inicio_estimacion], end=FECHA[final_estimacion+i])
assign(paste('PI_', H, sep=''),rbind(get(paste('PI_', H, sep='')), 
Y_F_P))
}
```

![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-1.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-2.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-3.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-4.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-5.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-6.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-7.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-8.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-9.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-10.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-11.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-12.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-13.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-14.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-15.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-16.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-17.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-18.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-19.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-20.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-21.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-22.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-23.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-24.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-25.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-26.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-27.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-28.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-29.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-30.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-31.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-32.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-33.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-34.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-35.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-36.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-37.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-38.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-39.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-40.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-41.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-42.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-43.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-44.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-45.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-46.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-47.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-48.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-49.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-50.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-51.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-52.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-53.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-54.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-55.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-56.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-57.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-58.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-59.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-60.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-61.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-62.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-63.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-64.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-65.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-66.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-67.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-68.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-69.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-70.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-71.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-72.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-73.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-74.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-75.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-76.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-77.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-78.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-79.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-80.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-81.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-82.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-83.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-84.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-85.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-86.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-87.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-88.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-89.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-90.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-91.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-92.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-93.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-94.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-95.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-96.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-97.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-98.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-99.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-100.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-101.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-102.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-103.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-104.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-105.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-106.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-107.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-108.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-109.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-110.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-111.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-112.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-113.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-114.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-115.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-116.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-117.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-118.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-119.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-120.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-121.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-122.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-123.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-124.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-125.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-126.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-127.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-128.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-129.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-130.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-131.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-132.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-133.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-134.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-135.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-136.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-137.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-138.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-139.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-140.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-141.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-142.png)<!-- -->![](03_EJERCICIOS_files/figure-epub3/unnamed-chunk-3-143.png)<!-- -->

```r
compara2<-merge(INFLA,get(paste('PI_', H, sep='')),join='inner') 
compara2<-data.frame(date=index(compara2), coredata(compara2))
compara2$date<-as.Date(compara2$date)
compara2<-filter(compara2, date>="2007-03-01" & date<="2019-02-01")
compara2<-mutate(compara2, DIFF =  (IPC-IPC.1)^2)
ECM_2<-mean(compara2$DIFF)
ECM_2
```

```
## [1] 1.314962e-06
```

Una forma muy usual para comparar dos específicaciones es definir un ratio entre cada uno de los ECM, sí este es mayor a 1 la específicación del que resulta el ECM del denominador sería la mejor.

```r
ECM_1/ECM_2
```

```
## [1] 0.4480013
```

Sin embargo, un criterio *formal* para la deducción de referentes predictivos, consiste en conocer sí las diferencias en el ECM entre distintas especificaciones econométricas son significativas desde la perspectiva estadística, ello se realiza a través del test propuesto por @Giacomini2006.

La estructura de este test consiste en definir la hipótesis nula  que la diferencia en la métrica ECM de una especificación respecto a otra es cero. Luego se construye el estadístico que se denomina Giacomini y White (GW) de acuerdo a la ecuación (\#eq:2), el cuál se distribuye asintóticamente normal y se utiliza para contrastar la hipótesis nula.

\begin{equation} 
GW_{h}^{i,j}= \left\{ \begin{array}{lcll}
 \frac{\bar{\Delta L}^{i,j}_{h}}{\hat{\sigma}^{i,j}_{g_{h}}} & \mbox{si} & h=1 & \\
 & & & \forall\;i\neq j  \\
  \frac{\bar{\Delta L}^{i,j}_{h}}{\frac{\hat{\sigma}^{i,j}_{g_h}}{\sqrt{g_{h}}}} & \mbox{ si } & h = \{3,6,9,12\} & 
 \end{array}
 \right.
 (\#eq:2)
\end{equation} 

La aplicación en R sería de la forma siguiente:

```r
GW_H<-cbind(compara$date, compara$DIFF, compara2$DIFF)
GW_H<-data.frame(GW_H)
colnames(GW_H)<-c("date", "ARIMA","RW")
GW_H$date<-as.Date(GW_H$date)
GW_H<-mutate(GW_H, delta=ARIMA-RW)
GW_ts<-xts(GW_H[, -1], order.by=as.Date(GW_H$date))
GW_MODEL <- lm(delta ~ 1, data=GW_ts)
list(sqrt(diag(sandwich(GW_MODEL))))
```

```
## [[1]]
##  (Intercept) 
## 1.295287e-07
```
