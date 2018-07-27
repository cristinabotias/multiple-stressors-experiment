#Análisis de datos sobre peso y tamaño de obreras y machos de las colonias:

#Como los datos no siguen una distribución normal y hay muchos datos que faltan, utilizamos tests no paramétricos
#Primero vemos los datos en gráficas:

boxplot(wweight~group, data=worker_male_size)
boxplot(wwidth~group, data=worker_male_size)
boxplot(mweight~group, data=worker_male_size, na.action = na.omit)
boxplot(mwidth~group, data=worker_male_size, na.action = na.omit)

# al correr los boxplots, en los casos de mweight y mwidth he tenido que indicar que son variables númericas, ya que se consideraban categóricas (segurmante porque hay NA)
#para esto:

worker_male_size$mweight <- as.numeric(as.character(worker_male_size$mweight))
worker_male_size$mwidth <- as.numeric(as.character(worker_male_size$mwidth))

# y tras hacer esto ya no ha habido problema para correr los boxplots

#Hacemos test Kruskal Wallis con cada variable:

kruskal.test(wweight~group, data=worker_male_size)
Error in kruskal.test.default(c(0.103, 0.086, 0.059, 0.073, 0.059, 0.094,  : all group levels must be finite

#tenemos que especificar que group es una variable categórica para poder correr el análisis.
worker_male_size$group <- as.factor(worker_male_size$group)
kruskal.test(wweight~group, data=worker_male_size)

Kruskal-Wallis rank sum test
                                
data:  wweight by group
Kruskal-Wallis chi-squared = 5.9792, df = 6, p-value = 0.4255

# sin diferencias significativas entre grupos para el peso de las obreras

kruskal.test(wwidth~group, data=worker_male_size)

Kruskal-Wallis rank sum test

data:  wwidth by group
Kruskal-Wallis chi-squared = 8.1816, df = 6, p-value = 0.2251

# sin diferencias significativas para el tamaño de las obreras
kruskal.test(mweight~group, data=worker_male_size, na.action = na.omit)

Kruskal-Wallis rank sum test

data:  mweight by group
Kruskal-Wallis chi-squared = 7.3313, df = 6, p-value = 0.2913

# sin diferencias significativas entre grupos para el peso de los machos

kruskal.test(mwidth~group, data=worker_male_size, na.action = na.omit)

Kruskal-Wallis rank sum test

data:  mwidth by group
Kruskal-Wallis chi-squared = 10.639, df = 6, p-value = 0.1002


# Los tratamientos no parecen tener un efecto en el peso y tama?o de obreras y machos en las colonias.

#Ahora miramos si los tratamientos tienen efecto en el número de obreras, machos, reinas, celdillas de cría y realeras

View(offspring)

#Echamos un vistazo a los datos:

boxplot(no.wrk~group, data=offspring)
boxplot(no.mls~group, data=offspring)
boxplot(no.brd~group, data=offspring)
boxplot(no.qcells~group, data=offspring)

#Corremos test Kruskal Wallis con cada variable:
offspring$group <- as.factor(offspring$group)
kruskal.test(no.wrk~group, data=offspspring)

Kruskal-Wallis rank sum test

data:  no.wrk by group
Kruskal-Wallis chi-squared = 2.417, df = 6, p-value = 0.8776

# el número de obreras no es significativamente diferente entre grupos

kruskal.test(no.mls~group, data=offspring)

Kruskal-Wallis rank sum test

data:  no.mls by group
Kruskal-Wallis chi-squared = 7.6512, df = 6, p-value = 0.2648

# el número de machos tampoco es significativamente diferente entre grupos

kruskal.test(no.brd~group, data=offspring)

Kruskal-Wallis rank sum test

data:  no.brd by group
Kruskal-Wallis chi-squared = 16.561, df = 6, p-value = 0.01104

# la cantidad de cría sí se ve afectada por los tratamientos, así que voy a hacer post hoc para comparar dos a dos:

library(PMCMRplus)
posthoc.kruskal.nemenyi.test(no.brd ~ group, data=offspring, dist="Tukey")

Pairwise comparisons using Tukey and Kramer (Nemenyi) test	
with Tukey-Dist approximation for independent samples

data: no.brd by group

   C     T1    T2    T3    T4    T5   
T1 0.087 -     -     -     -     -    
T2 0.066 1.000 -     -     -     -    
T3 0.988 0.429 0.363 -     -     -    
T4 0.025 0.999 1.000 0.193 -     -    
T5 0.491 0.978 0.961 0.924 0.854 -    
T6 0.728 0.887 0.841 0.989 0.647 1.000

P value adjustment method: none
Warning message:
In posthoc.kruskal.nemenyi.test.default(c(71L, 63L, 66L, 82L, 39L,  :
    Ties are present, p-values are not corrected.
    
    
# según el test post hoc, la diferencia se da entre el grupo T4 (neonics+piretroides+Nosema) y el control


kruskal.test(no.qcells~group, data=offspring)

Kruskal-Wallis rank sum test

data:  no.qcells by group
Kruskal-Wallis chi-squared = 15.931, df = 6, p-value = 0.01413

# hay diferencias significativas entre grupos para el número de realeras. Hacemos post hoc para ver qué grupos son diferentes:

posthoc.kruskal.nemenyi.test(no.qcells ~ group, data=offspring, dist="Tukey")

Pairwise comparisons using Tukey and Kramer (Nemenyi) test	
with Tukey-Dist approximation for independent samples

data: no.qcells by group

   C     T1    T2    T3    T4    T5   
T1 0.113 -     -     -     -     -    
T2 0.321 0.999 -     -     -     -    
T3 1.000 0.148 0.387 -     -     -    
T4 0.079 1.000 0.996 0.104 -     -    
T5 0.771 0.899 0.993 0.830 0.838 -    
T6 0.827 0.856 0.985 0.878 0.783 1.000

P value adjustment method: none
Warning message:
  In posthoc.kruskal.nemenyi.test.default(c(16L, 11L, 15L, 18L, 0L,  :
  Ties are present, p-values are not corrected.
  
#pruebo el post hoc utilizando el test Wilcoxon con corrección de p-values (Bonferroni-Holm method): 

pairwise.wilcox.test(offspring$no.qcells, offspring$group, p.adjust.method = p.adjust.methods, paired = F)
  
Pairwise comparisons using Wilcoxon rank sum test 
  
  data:  offspring$no.qcells and offspring$group 
  
   C     T1    T2    T3    T4    T5   
T1 0.091 -     -     -     -     -    
T2 0.276 1.000 -     -     -     -    
T3 1.000 0.266 0.606 -     -     -    
T4 0.051 1.000 1.000 0.229 -     -    
T5 1.000 1.000 1.000 1.000 1.000 -    
T6 1.000 1.000 1.000 1.000 1.000 1.000
  
  P value adjustment method: holm  
  
#parece que es el grupo T4 el que ha producido menos reinas que el control.
  
