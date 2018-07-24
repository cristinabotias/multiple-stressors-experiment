#cargo los datos 
weight<-read.table("C:/Users/Usuario/Desktop/STATS MULTIPLE STRESSORS/CUMMULATIVE WEIGHT GAIN BBEE NESTS_MULT STRESSORS.txt", head=T)
#2. Represento gráficamente los datos. 
xyplot(weight~week|nest, group=group, data=weight_gain_2)

#3. Selecciono la estructura de efectos aleatorios (para ello utilizo la estructura más compleja de efectos fijos) 
#¿Qué efectos aleatorios son posibles y qué significan?
# Como efectos fijos meto el grupo de tratamiento (group), el momento de medición (week), week al cuadrado para tener en cuenta la relación curvilínea del peso a lo largo del tiempo
#y por último el número de obreras al inicio del experimento (workers)
#añado interacciones entre semana y tratamiento, y semana y número de obreras, y las colonias (nest) se meten como efecto aleatorio.

lme0 <- gls(weight~group*week + workers + group:workers + week^2,data=weight_gain_2, na.action=na.omit)
lme1 <- lme(weight~group*week + workers + group:workers + week^2, random=~1|nest, data=weight_gain_2,na.action=na.omit)
lme2 <-lme(weight~group*week + workers + group:workers + week^2, random=list(~1|nest, ~1|week), data=weight_gain_2, na.action=na.omit)
anova(lme0, lme1, lme2) 

#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
lme0     1 22 6405.278 6502.156 -3180.639                        
lme1     2 23 6390.881 6492.163 -3172.440 1 vs 2 16.39716   1e-04
lme2     3 24 6392.881 6498.566 -3172.440 2 vs 3  0.00000   1e+00

#Los AIC indican que el modelo lme1, en el que entra la interacción entre group y semana, y grupo y número de obreras, y poniendo la colonia como aleatorio, es el modelo que explica más varianza
#Por tanto elegiremos esa estructura de los efectos aleatorios

#4. Una vez seleccionada la estructura de efectos aleatorios, selecciono la estructura de efectos fijos. 
#Para ello utiliza el método de máxima verosimilitud.  
lme3 <- lme(weight~group*week + workers + group:workers + week^2, random=~1|nest, data=weight_gain_2, na.action=na.omit, method="ML")
lme4 <- lme(weight~group*week+ workers+ week^2, random=~1|nest, data=weight_gain_2, na.action=na.omit, method="ML")
lme5 <- lme(weight~group*workers + week + week^2, random=~1|nest, data=weight_gain_2, na.action=na.omit, method="ML")
lme6 <- lme(weight~group + week + workers + week^2, random=~1|nest, data=weight_gain_2, na.action=na.omit, method="ML")

anova(lme3,lme4,lme5,lme6)
Model df      AIC      BIC    logLik   Test  L.Ratio p-value
lme3     1 23 6470.556 6572.625 -3212.278                        
lme4     2 17 6463.614 6539.056 -3214.807 1 vs 2 5.057847  0.5364
lme5     3 17 6463.992 6539.433 -3214.996                        
lme6     4 11 6457.028 6505.843 -3217.514 3 vs 4 5.036394  0.5392

#el modelo lme6 es el más parsimonioso, incluye el grupo y la semana y las obreras como efectos fijos y no la interacción entre ellos.

#5. Ajusto el modelo final con máxima verosimilitud 

lme.weight <- lme(weight~group + week + workers + week^2, random=~1|nest, data=weight_gain_2, na.action=na.omit)

#Aplico test post hoc para hacer comparaciones dos a dos

library(multcomp)
mmod.t.test <- glht(lme.weight, linfct=mcp(group ="Tukey"))

Error in mcp2matrix(model, linfct = linfct) : 
  Variable(s) 'group' of class 'character' is/are not contained as a factor in 'model'.

summary(mmod.t.test)

Error in summary(mmod.t.test) : objeto 'mmod.t.test' no encontrado



  