#cargo los datos 
weight_gain_2

#2. Represento gráficamente los datos. 
xyplot(weight~week|nest, group=group, data=weight_gain_2)

#3. Selecciono la estructura de efectos aleatorios (para ello utilizo la estructura más compleja de efectos fijos) 
#¿Qué efectos aleatorios son posibles y qué significan?
# Como efectos fijos meto el grupo de tratamiento (group), el momento de medición (week), week al cuadrado para tener en cuenta la relación curvilínea del peso a lo largo del tiempo
#y por último el número de obreras al inicio del experimento (workers)
#añado interacciones entre semana y tratamiento, y semana y número de obreras, y las colonias (nest) se meten como efecto aleatorio.

lme0 <- gls(weight~group*week*workers*week^2,data=weight_gain_2, na.action=na.omit)
lme1 <- lme(weight~group*week*workers*week^2, random=~1|nest, data=weight_gain_2,na.action=na.omit)
lme2 <-lme(weight~group*week*workers*week^2, random=list(~1|nest, ~1|week), data=weight_gain_2, na.action=na.omit)
anova(lme0, lme1, lme2) 

#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
lme0     1 29 6409.648 6537.014 -3175.824                        
lme1     2 30 6395.537 6527.294 -3167.768 1 vs 2 16.11096   1e-04
lme2     3 31 6397.537 6533.686 -3167.768 2 vs 3  0.00000   1e+00


#Los AIC indican que el modelo lme1, en el que entra la interacción entre group y semana, y grupo y número de obreras, y poniendo la colonia como aleatorio, es el modelo que explica más varianza
#Por tanto elegiremos esa estructura de los efectos aleatorios. Ahora quiero ver qué efectos son significativos en este modelo:

anova(lme1)

numDF denDF   F-value p-value
(Intercept)            1   541 1078.0002  <.0001
group                  6    56    2.0053  0.0802
week                   1   541    7.3199  0.0070
workers                1    56    0.0254  0.8740
group:week             6   541    0.8900  0.5018
group:workers          6    56    0.6998  0.6508
week:workers           1   541    0.9276  0.3359
group:week:workers     6   541    0.6003  0.7302

##El efecto fijo que influye significativamente sobre weight es week.
#Elimino las interacciones y el efecto fijo de workers, ya que no son significativas, y así simplificar el modelo
#Veo como queda el modelo sin incluir workers:
lme0 <- gls(weight~group*week*week^2,data=weight_gain_2, na.action=na.omit)
lme1 <- lme(weight~group*week*week^2, random=~1|nest, data=weight_gain_2,na.action=na.omit )
lme2 <- lme(weight~group*week*week^2, random=list(~1|nest, ~1|week), data=weight_gain_2,na.action=na.omit )
anova(lme0,lme1,lme2)

Model df      AIC      BIC    logLik   Test  L.Ratio p-value
lme0     1 15 6417.811 6484.037 -3193.905                        
lme1     2 16 6404.080 6474.722 -3186.040 1 vs 2 15.73046   1e-04
lme2     3 17 6406.080 6481.137 -3186.040 2 vs 3  0.00000   1e+00

# el modelo lme1 sigue siendo el m?s parsimonioso.Vemos los efectos significativos:
anova(lme1)

numDF denDF   F-value p-value
(Intercept)     1   548 1127.4377  <.0001
group           6    63    2.0975  0.0659
week            1   548    7.3692  0.0068
group:week      6   548    0.8942  0.4987

#El efecto de Week es  significativo. Esto quiere decir que el peso de las colonias crecen con el tiempo de distinta manera en cada colonia.

#4. Una vez seleccionada la estructura de efectos aleatorios, selecciono la estructura de efectos fijos. 
#Para ello utiliza el método de máxima verosimilitud.

lmc <- lmeControl(niterEM=5000, msMaxIter=5000)
lme3 <- lme(weight~group*week*workers*week^2, random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit, method="ML")
lme4 <- lme(weight~group*week + workers + week^2, random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit, method="ML")
lme5 <- lme(weight~group*workers + week + week^2, random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit, method="ML")
lme6 <- lme(weight~group, random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit, method="ML")
lme7<- lme(weight~week + week^2,random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit, method="ML" )
lme8<- lme(weight~workers,random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit, method="ML" )
lme9 <- lme(weight~group*week + week^2, random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit, method="ML")
lme10 <- lme(weight~group*workers, random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit, method="ML")
anova(lme3, lme4, lme5, lme6, lme7, lme8, lme9, lme10)
Model df      AIC      BIC    logLik   Test   L.Ratio p-value
lme3      1 30 6479.919 6613.052 -3209.960                         
lme4      2 17 6463.614 6539.056 -3214.807 1 vs 2  9.694928  0.7187
lme5      3 17 6463.992 6539.433 -3214.996                         
lme6      4  9 6460.446 6500.386 -3221.223 3 vs 4 12.454774  0.1320
lme7      5  4 6455.803 6473.554 -3223.902 4 vs 5  5.356644  0.3739
lme8      6  4 6462.999 6480.750 -3227.500                         
lme9      7 16 6461.644 6532.648 -3214.822 6 vs 7 25.355010  0.0132
lme10     8 16 6469.303 6540.307 -3218.652                         

#el modelo que explica más varianza es el lme7, que incluye sólo "week" como efecto fijo, sin ninguna interacción
# Ajustamos el modelo final con máxima verosimilitud y lo explicamos.

lme.weight <- lme(weight~group*week*week^2, random=~1|nest, data=weight_gain_2, control=lmc, na.action=na.omit)
summary(lme.weight)

Linear mixed-effects model fit by REML
Data: weight_gain_2 
AIC      BIC   logLik
6404.08 6474.722 -3186.04

Random effects:
  Formula: ~1 | nest
(Intercept) Residual
StdDev:    13.37968 40.36823

Fixed effects: weight ~ group * week * week^2 
Value Std.Error  DF   t-value p-value
(Intercept)  68.93889 10.193524 548  6.763009  0.0000
groupT1      -3.82761 14.446375  63 -0.264953  0.7919
groupT2       0.92814 14.446375  63  0.064247  0.9490
groupT3      -1.25746 14.479098  63 -0.086847  0.9311
groupT4      -2.36944 14.415819  63 -0.164364  0.8700
groupT5      -1.33611 14.415819  63 -0.092684  0.9264
groupT6       0.08733 14.446375  63  0.006045  0.9952
week          4.30333  1.648026 548  2.611205  0.0093
groupT1:week -2.75422  2.347626 548 -1.173193  0.2412
groupT2:week -2.88427  2.347626 548 -1.228592  0.2198
groupT3:week -2.84693  2.365700 548 -1.203419  0.2293
groupT4:week -4.71500  2.330661 548 -2.023032  0.0436
groupT5:week -3.74167  2.330661 548 -1.605410  0.1090
groupT6:week -1.21037  2.347626 548 -0.515570  0.6064
Correlation: 
  (Intr) gropT1 gropT2 gropT3 gropT4 gropT5 gropT6 week   grpT1: grpT2: grpT3: grpT4:
  groupT1      -0.706                                                                             
groupT2      -0.706  0.498                                                                      
groupT3      -0.704  0.497  0.497                                                               
groupT4      -0.707  0.499  0.499  0.498                                                        
groupT5      -0.707  0.499  0.499  0.498  0.500                                                 
groupT6      -0.706  0.498  0.498  0.497  0.499  0.499                                          
week         -0.808  0.570  0.570  0.569  0.572  0.572  0.570                                   
groupT1:week  0.567 -0.809 -0.400 -0.400 -0.401 -0.401 -0.400 -0.702                            
groupT2:week  0.567 -0.400 -0.809 -0.400 -0.401 -0.401 -0.400 -0.702  0.493                     
groupT3:week  0.563 -0.397 -0.397 -0.809 -0.398 -0.398 -0.397 -0.697  0.489  0.489              
groupT4:week  0.572 -0.403 -0.403 -0.402 -0.808 -0.404 -0.403 -0.707  0.496  0.496  0.493       
groupT5:week  0.572 -0.403 -0.403 -0.402 -0.404 -0.808 -0.403 -0.707  0.496  0.496  0.493  0.500
groupT6:week  0.567 -0.400 -0.400 -0.400 -0.401 -0.401 -0.809 -0.702  0.493  0.493  0.489  0.496
grpT5:
  groupT1            
groupT2            
groupT3            
groupT4            
groupT5            
groupT6            
week               
groupT1:week       
groupT2:week       
groupT3:week       
groupT4:week       
groupT5:week       
groupT6:week  0.496

Standardized Within-Group Residuals:
  Min         Q1        Med         Q3        Max 
-2.1353520 -0.7611434 -0.1011664  0.6414720  2.7615141 

Number of Observations: 625
Number of Groups: 70 

#Ahora comprobamos si se cumplen los supuestos del modelo (normalidad, homocedasticidad, linealidad:
Res <- residuals (lme.weight, type="normalized")
Fit <- fitted(lme.weight)
par(mfrow = c(2,2))
plot(Res~Fit)
boxplot(Res~na.omit(weight_gain_2)$group)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)


#Parece que se cumple todo bastante bien. 


#Aplico test post hoc para hacer comparaciones dos a dos
library(multcomp)
mmod.t.test <- glht(lme.weight, linfct=mcp(group ="Tukey"))

Error in mcp2matrix(model, linfct = linfct) : 
  Variable(s) 'group' of class 'character' is/are not contained as a factor in 'model'.

summary(mmod.t.test)

Error in summary(mmod.t.test) : objeto 'mmod.t.test' no encontrado

