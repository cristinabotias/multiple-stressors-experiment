#cargar los datos 
>weight<-read.table("C:/Users/Usuario/Desktop/STATS MULTIPLE STRESSORS/CUMMULATIVE WEIGHT GAIN BBEE NESTS_MULT STRESSORS.txt", head=T)
#2. Representa gr?ficamente los datos. 
>xyplot(WEIGHT~WEEK|NEST, group=GROUP, data=weight) 

#3. Selecciona la estructura de efectos aleatorios (para ello recuerda que tienes que utilizar la estructura m?s compleja de efectos fijos) 
#?Qu? efectos aleatorios son posibles y qu? significan?

> 
  lme1 <- lme(peso~marca*dia, random=~1|ind, data=perdiz2,na.action=na.omit )
> lme0 <- gls(weight~group,data=weight_gain, na.action=na.omit)
> lme1 <- lme(weight~group*week, random=~1|nest, data=weight_gain,na.action=na.omit)
> lme2<-lme(weight~group*week, random=list(~1|nest, ~1|week), data=weight_gain, na.action=na.omit)
>anova(lme0, lme1, lme2) 
#     Model df      AIC      BIC    logLik   Test  L.Ratio p-value
lme0     1 64 5606.691 5884.021 -2739.345                        
lme1     2 65 5418.925 5700.588 -2644.462 1 vs 2 189.7659  <.0001
lme2     3 66 5420.925 5706.921 -2644.462 2 vs 3   0.0000       1
> #Los AIC indican que el modelo lme1, en el que entra la interacci?n entre group y semana, y poniendo la colonia como aleatorio, es el modelo que explica m?s varianza
  #Por tanto elegiremos esa estructura de los efectos aleatorios
  
#4. Una vez que has seleccionado la estructura de efectos aleatorios, selecciona la estructura de efectos fijos. 
#Para ello utiliza el m?todo de m?xima verosimilitud.  
  > lme3 <- lme(weight~group*week, random=~1|nest, data=weight_gain, na.action=na.omit, method="ML")
> lme4 <- lme(weight~group+week, random=~1|nest, data=weight_gain, na.action=na.omit, method="ML")
> lme5 <- lme(weight~group, random=~1|nest, data=weight_gain, na.action=na.omit, method="ML")
> lme6 <- lme(WEIGHT~WEEK, random=~1|NEST, data=weight, na.action=na.omit, method="ML")
> anova(lme3,lme4,lme5,lme6)
Model df      AIC      BIC    logLik   Test  L.Ratio p-value
lme3     1 65 5783.488 6072.046 -2826.744                        
lme4     2 17 5761.011 5836.480 -2863.506 1 vs 2  73.5232  0.0103
lme5     3  9 6471.634 6511.588 -3226.817 2 vs 3 726.6231  <.0001
lme6     4 11 5761.184 5810.016 -2869.592 3 vs 4 714.4506  <.0001
>  #el model lme4 es el m?s parsimonioso, incluye el grupo y la semana como efectos fijos y no la interacci?n.

#5. Ajusta el modelo final con m?xima verosimilitud y expl?calo.

 > lme.weight <- lme(WEIGHT~GROUP + WEEK, random=~1|NEST, data=weight, na.action=na.omit)
#Aplicamos test post hoc para hacer comparaciones dos a dos
> library(multcomp)
> mmod.t.test <- glht(lme.weight, linfct=mcp(GROUP="Tukey"))
> summary(mmod.t.test)

Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts


Fit: lme.formula(fixed = WEIGHT ~ GROUP + WEEK, data = weight, random = ~1 | 
                   NEST, na.action = na.omit)

Linear Hypotheses:
  Estimate Std. Error z value Pr(>|z|)  
T2 - T1 == 0    4.291      8.702   0.493    0.999  
T3 - T1 == 0    1.784      8.705   0.205    1.000  
T4 - T1 == 0   -7.656      8.698  -0.880    0.976  
T5 - T1 == 0   -1.756      8.698  -0.202    1.000  
T6 - T1 == 0   11.676      8.702   1.342    0.832  
T7 - T1 == 0   18.289      8.698   2.103    0.351  
T3 - T2 == 0   -2.507      8.709  -0.288    1.000  
T4 - T2 == 0  -11.946      8.702  -1.373    0.816  
T5 - T2 == 0   -6.046      8.702  -0.695    0.993  
T6 - T2 == 0    7.385      8.705   0.848    0.980  
T7 - T2 == 0   13.998      8.702   1.609    0.677  
T4 - T3 == 0   -9.439      8.705  -1.084    0.933  
T5 - T3 == 0   -3.539      8.705  -0.407    1.000  
T6 - T3 == 0    9.893      8.709   1.136    0.917  
T7 - T3 == 0   16.505      8.705   1.896    0.483  
T5 - T4 == 0    5.900      8.698   0.678    0.994  
T6 - T4 == 0   19.332      8.702   2.222    0.284  
T7 - T4 == 0   25.944      8.698   2.983    0.045 *
  T6 - T5 == 0   13.432      8.702   1.544    0.718  
T7 - T5 == 0   20.044      8.698   2.304    0.242  
T7 - T6 == 0    6.613      8.702   0.760    0.989  
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
(Adjusted p values reported -- single-step method)

Warning message:
  In RET$pfunction("adjusted", ...) : Completion with error > abseps
> mmod.t.test2 <- glht(lme.weight, linfct=mcp(WEEK="Tukey"))
> summary(mmod.t.test2)

Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts


Fit: lme.formula(fixed = WEIGHT ~ GROUP + WEEK, data = weight, random = ~1 | 
                   NEST, na.action = na.omit)

Linear Hypotheses:
  Estimate Std. Error z value Pr(>|z|)    
week2 - week1 == 0   25.557      3.587   7.126  < 0.001 ***
  week3 - week1 == 0   53.543      3.587  14.928  < 0.001 ***
  week4 - week1 == 0   79.771      3.587  22.241  < 0.001 ***
  week5 - week1 == 0  110.971      3.587  30.940  < 0.001 ***
  week6 - week1 == 0   75.057      3.587  20.927  < 0.001 ***
  week7 - week1 == 0   49.729      3.587  13.865  < 0.001 ***
  week8 - week1 == 0   34.757      3.587   9.691  < 0.001 ***
  week9 - week1 == 0   18.165      3.646   4.982  < 0.001 ***
  week3 - week2 == 0   27.986      3.587   7.803  < 0.001 ***
  week4 - week2 == 0   54.214      3.587  15.116  < 0.001 ***
  week5 - week2 == 0   85.414      3.587  23.815  < 0.001 ***
  week6 - week2 == 0   49.500      3.587  13.801  < 0.001 ***
  week7 - week2 == 0   24.171      3.587   6.739  < 0.001 ***
  week8 - week2 == 0    9.200      3.587   2.565  0.20152    
week9 - week2 == 0   -7.392      3.646  -2.027  0.52421    
week4 - week3 == 0   26.229      3.587   7.313  < 0.001 ***
  week5 - week3 == 0   57.429      3.587  16.012  < 0.001 ***
  week6 - week3 == 0   21.514      3.587   5.998  < 0.001 ***
  week7 - week3 == 0   -3.814      3.587  -1.063  0.97930    
week8 - week3 == 0  -18.786      3.587  -5.238  < 0.001 ***
  week9 - week3 == 0  -35.378      3.646  -9.702  < 0.001 ***
  week5 - week4 == 0   31.200      3.587   8.699  < 0.001 ***
  week6 - week4 == 0   -4.714      3.587  -1.314  0.92734    
week7 - week4 == 0  -30.043      3.587  -8.376  < 0.001 ***
  week8 - week4 == 0  -45.014      3.587 -12.551  < 0.001 ***
  week9 - week4 == 0  -61.607      3.646 -16.895  < 0.001 ***
  week6 - week5 == 0  -35.914      3.587 -10.013  < 0.001 ***
  week7 - week5 == 0  -61.243      3.587 -17.075  < 0.001 ***
  week8 - week5 == 0  -76.214      3.587 -21.249  < 0.001 ***
  week9 - week5 == 0  -92.807      3.646 -25.452  < 0.001 ***
  week7 - week6 == 0  -25.329      3.587  -7.062  < 0.001 ***
  week8 - week6 == 0  -40.300      3.587 -11.236  < 0.001 ***
  week9 - week6 == 0  -56.892      3.646 -15.602  < 0.001 ***
  week8 - week7 == 0  -14.971      3.587  -4.174  0.00102 ** 
  week9 - week7 == 0  -31.564      3.646  -8.656  < 0.001 ***
  week9 - week8 == 0  -16.592      3.646  -4.550  < 0.001 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
(Adjusted p values reported -- single-step method)

Warning messages:
  1: In RET$pfunction("adjusted", ...) : Completion with error > abseps
2: In RET$pfunction("adjusted", ...) : Completion with error > abseps
> #En el modelo se mete como efectos fijos el grupo de tratamiento y la semana en la que pesan las colonias
  #La interacci?n entre los efectos fijos no es significativa, y como efecto aleatorio se introduce cada colonia.
  #vemos que s?lo el grupo 4 crece significativamente menos que el grupo control a lo largo del tiempo.
  

#COMIENZO TODO DESDE EL PRINCIPIO PARA QUE QUEDE M?S CLARO:
#CARGO LOS DATOS DE NUEVO:
> weight<-read.table("C:/Users/Usuario/Desktop/STATS MULTIPLE STRESSORS/CUMULATIVE WEIGHT GAIN BBEE NESTS_MULT STRESSORS.txt", na.strings="-",head=T)
#Antes de ajustar un modelo lineal vamos a obtener un gr?fico exploratorio de la variable respuesta frente a la explicativa:
>plot(weight$weight ~ weight$group, xlab= "GROUP", ylab="WEIGHT GAIN",pch=weight$week)
#Tambi?n podemos explorar los datos con la funci?n xyplot()
library(lattice)
xyplot(weight~week|nest, group=group, data=weight)
xyplot(weight~group|week,group=group,data=weight)
xyplot(weight~group, group=group, data=weight)
xyplot(weight~week,group=group, data=weight)
#Ahora ajustamos un modelo lineal mixto para ver qu? efectos son significativos:
#Selecciona la estructura de efectos aleatorios (para ello recuerda que tienes que utilizar la estructura m?s compleja de efectos fijos) ?Qu? efectos aleatorios son posibles y qu? significan?
library(nlme)
lme0 <- gls(weight~group*week*workers,data=weight, na.action=na.omit)
lme1 <- lme(weight~group*week*workers, random=~1|nest, data=weight,na.action=na.omit )
lme2 <- lme(weight~group*week*workers, random=list(~1|nest, ~1|week), data=weight,na.action=na.omit )
anova(lme0,lme1,lme2)
Model  df      AIC      BIC    logLik   Test  L.Ratio p-value
lme0     1 127 5441.529 5976.784 -2593.764                        
lme1     2 128 5277.457 5816.927 -2510.729 1 vs 2 166.0718  <.0001
lme2     3 129 5279.457 5823.141 -2510.729 2 vs 3   0.0000       1

#Vemos que el modelo que mejor se ajusta es el lme1, que es el que s?lo mete nest como efecto aleatorio.
#Ahora quiero ver qu? efectos son significativos en este modelo:
anova(lme1)
numDF denDF   F-value p-value
(Intercept)            1   444 1031.0375  <.0001
group                  6    56    1.9326  0.0914
week                   8   444  182.4960  <.0001
workers                1    56    0.0700  0.7923
group:week            48   444    1.3942  0.0473
group:workers          6    56    0.7454  0.6155
week:workers           8   444    0.7034  0.6887
group:week:workers    48   444    0.4948  0.9983

#El efecto fijo que influye significativamente sobre weight es week, y la interacci?n group:week es significativa.
#Elimino las interacciones y el efecto fijo de workers, ya que no son significativas, y as? simplificar el modelo
#Veo como queda el modelo sin incluir workers:
lme0 <- gls(weight~group*week,data=weight, na.action=na.omit)
lme1 <- lme(weight~group*week, random=~1|nest, data=weight,na.action=na.omit )
lme2 <- lme(weight~group*week, random=list(~1|nest, ~1|week), data=weight,na.action=na.omit )
anova(lme0,lme1,lme2)
Model df      AIC      BIC    logLik   Test  L.Ratio p-value
lme0     1 64 5606.691 5884.021 -2739.345                        
lme1     2 65 5418.925 5700.588 -2644.462 1 vs 2 189.7659  <.0001
lme2     3 66 5420.925 5706.921 -2644.462 2 vs 3   0.0000       1

#el modelo lme1 sigue siendo el m?s parsimonioso.Vemos los efectos significativos:
anova(lme1)
> anova(lme1)
numDF denDF   F-value p-value
(Intercept)     1   500 1072.9674  <.0001
group           6    63    2.0111  0.0773
week            8   500  192.7618  <.0001
group:week     48   500    1.4727  0.0246

#Week y la interacci?n group:week son significativos. Esto quiere decir que el peso de las colonias crecen con el tiempo de distinta manera en cada grupo.
#Ahora una vez que has seleccionado la estructura de efectos aleatorios, selecciona la estructura de efectos fijos. 
#Para ello utiliza el m?todo de m?xima verosimilitud:
lmc <- lmeControl(niterEM=5000, msMaxIter=5000)
lme3 <- lme(weight~group*week*workers, random=~1|nest, data=weight, control=lmc, na.action=na.omit, method="ML")
lme4 <- lme(weight~group*week + workers, random=~1|nest, data=weight, control=lmc, na.action=na.omit, method="ML")
lme5 <- lme(weight~group*workers+week, random=~1|nest, data=weight, control=lmc, na.action=na.omit, method="ML")
lme6 <- lme(weight~group, random=~1|nest, data=weight, control=lmc, na.action=na.omit, method="ML")
lme7<- lme(weight~week,random=~1|nest, data=weight, control=lmc, na.action=na.omit, method="ML" )
lme8<- lme(weight~workers,random=~1|nest, data=weight, control=lmc, na.action=na.omit, method="ML" )
lme9 <- lme(weight~group*week, random=~1|nest, data=weight, control=lmc, na.action=na.omit, method="ML")
lme10 <- lme(weight~group*workers, random=~1|nest, data=weight, control=lmc, na.action=na.omit, method="ML")
anova(lme3, lme4, lme5, lme6, lme7, lme8, lme9, lme10)
Model  df      AIC      BIC    logLik   Test  L.Ratio p-value
lme3      1 128 5868.411 6436.648 -2806.205                        
lme4      2  66 5785.413 6078.410 -2826.706 1 vs 2  41.0016  0.9818
lme5      3  24 5769.498 5876.042 -2860.749 2 vs 3  68.0850  0.0066
lme6      4   9 6471.634 6511.588 -3226.817 3 vs 4 732.1366  <.0001
lme7      5  11 5761.184 5810.016 -2869.592 4 vs 5 714.4506  <.0001
lme8      6   4 6474.068 6491.825 -3233.034 5 vs 6 726.8843  <.0001
lme9      7  65 5783.488 6072.046 -2826.744 6 vs 7 812.5800  <.0001
lme10     8  16 6480.569 6551.599 -3224.285 7 vs 8 795.0816  <.0001

#El modelo m?s parsimonioso es el lme7, pero dejar?amos fuera la interacci?n significativa de group:week
#Por tanto elegimos el que incluye la interaccion, lme9

# Ajustamos el modelo final con m?xima verosimilitud y lo explicamos.
lme.weight <- lme(weight~group*week, random=~1|nest, data=weight, control=lmc, na.action=na.omit)
summary(lme.weight)


Linear mixed-effects model fit by REML
Data: weight 
AIC      BIC    logLik
5418.925 5700.588 -2644.462

Random effects:
  Formula: ~1 | nest
(Intercept) Residual
StdDev:    18.17548 20.79248

Fixed effects: weight ~ group * week 
Value Std.Error  DF   t-value p-value
(Intercept)        25.00000  8.733129 500  2.862662  0.0044
groupT1             2.20000 12.350509  63  0.178130  0.8592
groupT2             4.00000 12.350509  63  0.323873  0.7471
groupT3             3.20000 12.350509  63  0.259099  0.7964
groupT4            -0.30000 12.350509  63 -0.024290  0.9807
groupT5            -0.70000 12.350509  63 -0.056678  0.9550
groupT6             0.30000 12.350509  63  0.024290  0.9807
weekweek2          30.40000  9.298681 500  3.269281  0.0012
weekweek3          57.70000  9.298681 500  6.205181  0.0000
weekweek4          87.90000  9.298681 500  9.452954  0.0000
weekweek5         132.60000  9.298681 500 14.260087  0.0000
weekweek6         120.40000  9.298681 500 12.948073  0.0000
weekweek7          76.00000  9.298681 500  8.173202  0.0000
weekweek8          56.10000  9.298681 500  6.033114  0.0000
weekweek9          28.00000  9.298681 500  3.011180  0.0027
groupT1:weekweek2  -4.60000 13.150321 500 -0.349801  0.7266
groupT2:weekweek2  -6.60000 13.150321 500 -0.501889  0.6160
groupT3:weekweek2  -8.10000 13.150321 500 -0.615955  0.5382
groupT4:weekweek2  -8.60000 13.150321 500 -0.653976  0.5134
groupT5:weekweek2  -4.70000 13.150321 500 -0.357406  0.7209
groupT6:weekweek2  -1.30000 13.150321 500 -0.098857  0.9213
groupT1:weekweek3  -5.90000 13.150321 500 -0.448658  0.6539
groupT2:weekweek3  -6.10000 13.150321 500 -0.463867  0.6429
groupT3:weekweek3  -6.60000 13.150321 500 -0.501889  0.6160
groupT4:weekweek3  -7.00000 13.150321 500 -0.532306  0.5948
groupT5:weekweek3  -3.50000 13.150321 500 -0.266153  0.7902
groupT6:weekweek3   0.00000 13.150321 500  0.000000  1.0000
groupT1:weekweek4 -16.00000 13.150321 500 -1.216700  0.2243
groupT2:weekweek4  -8.80000 13.150321 500 -0.669185  0.5037
groupT3:weekweek4  -6.60000 13.150321 500 -0.501889  0.6160
groupT4:weekweek4 -14.70000 13.150321 500 -1.117843  0.2642
groupT5:weekweek4 -10.90000 13.150321 500 -0.828877  0.4076
groupT6:weekweek4   0.10000 13.150321 500  0.007604  0.9939
groupT1:weekweek5 -30.00000 13.150321 500 -2.281313  0.0229
groupT2:weekweek5 -22.10000 13.150321 500 -1.680567  0.0935
groupT3:weekweek5 -25.90000 13.150321 500 -1.969534  0.0494
groupT4:weekweek5 -35.20000 13.150321 500 -2.676741  0.0077
groupT5:weekweek5 -29.30000 13.150321 500 -2.228083  0.0263
groupT6:weekweek5  -8.90000 13.150321 500 -0.676790  0.4989
groupT1:weekweek6 -60.40000 13.150321 500 -4.593044  0.0000
groupT2:weekweek6 -49.50000 13.150321 500 -3.764167  0.0002
groupT3:weekweek6 -57.50000 13.150321 500 -4.372517  0.0000
groupT4:weekweek6 -69.90000 13.150321 500 -5.315460  0.0000
groupT5:weekweek6 -53.10000 13.150321 500 -4.037924  0.0001
groupT6:weekweek6 -27.00000 13.150321 500 -2.053182  0.0406
groupT1:weekweek7 -38.00000 13.150321 500 -2.889663  0.0040
groupT2:weekweek7 -33.50000 13.150321 500 -2.547466  0.0111
groupT3:weekweek7 -34.50000 13.150321 500 -2.623510  0.0090
groupT4:weekweek7 -40.10000 13.150321 500 -3.049355  0.0024
groupT5:weekweek7 -28.90000 13.150321 500 -2.197665  0.0284
groupT6:weekweek7  -8.90000 13.150321 500 -0.676790  0.4989
groupT1:weekweek8 -22.70000 13.150321 500 -1.726194  0.0849
groupT2:weekweek8 -26.00000 13.150321 500 -1.977138  0.0486
groupT3:weekweek8 -27.70000 13.150321 500 -2.106412  0.0357
groupT4:weekweek8 -33.90000 13.150321 500 -2.577884  0.0102
groupT5:weekweek8 -29.20000 13.150321 500 -2.220478  0.0268
groupT6:weekweek8  -9.90000 13.150321 500 -0.752833  0.4519
groupT1:weekweek9  -6.80000 13.150321 500 -0.517098  0.6053
groupT2:weekweek9  -9.05724 13.351053 500 -0.678392  0.4978
groupT3:weekweek9  -9.53728 13.597802 500 -0.701384  0.4834
groupT4:weekweek9 -21.40000 13.150321 500 -1.627337  0.1043
groupT5:weekweek9 -14.50000 13.150321 500 -1.102635  0.2707
groupT6:weekweek9  -6.98545 13.351053 500 -0.523214  0.6011

Standardized Within-Group Residuals:
  Min           Q1          Med           Q3          Max 
-3.101707998 -0.486343495 -0.006593256  0.458764010  3.695058425 

Number of Observations: 626
Number of Groups: 70 

#Ahora comprobamos si se cumplen los supuestos del modelo (normalidad, homocedasticidad, linealidad:
Res <- residuals (lme.weight, type="normalized")
Fit <- fitted(lme.weight)
par(mfrow = c(2,2))
plot(Res~Fit)
boxplot(Res~na.omit(weight)$group)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)

#Vemos que se cumple todo bastante bien. 


#Comienzo con el an?lisis de datos sobre peso y tama?o de obreras y machos de las colonias:

wksmls<-read.table("C:/Users/Usuario/Desktop/STATS MULTIPLE STRESSORS/weights and widths workers and males.txt", na.strings="-",head=T)

#Como los datos no siguen una distribuci?n normal y hay muchos datos que faltan, utilizamos tests no param?tricos
#Primero vemos los datos en gr?ficas:

boxplot(wweight~group, data=wksmls)
boxplot(wwidth~group, data=wksmls)
boxplot(mweight~group, data=wksmls)
boxplot(mwidth~group, data=wksmls)

#Hacemos test Kruskal Wallis con cada variable:

>kruskal.test(wweight~group, data=wksmls)


Kruskal-Wallis rank sum test

data:  wweight by group
Kruskal-Wallis chi-squared = 6.471, df = 6, p-value = 0.3725


>kruskal.test(wwidth~group, data=wksmls)

Kruskal-Wallis rank sum test

data:  wwidth by group
Kruskal-Wallis chi-squared = 7.1943, df = 6, p-value = 0.3032

>kruskal.test(mweight~group, data=wksmls)

Kruskal-Wallis rank sum test

data:  mweight by group
Kruskal-Wallis chi-squared = 10.75, df = 6, p-value = 0.09642

> kruskal.test(mwidth~group, data=wksmls)

Kruskal-Wallis rank sum test

data:  mwidth by group
Kruskal-Wallis chi-squared = 11.816, df = 6, p-value = 0.0662

# Los tratamientos no parecen tener un efecto en el peso y tama?o de obreras y machos en las colonias.

