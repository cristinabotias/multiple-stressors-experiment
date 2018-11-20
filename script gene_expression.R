#Analysis of multivariate data using mvabund package.
#Gene expression data:

library(mvabund)
  
gene_expression <- read.csv("gene expression.csv")
head(gene_expression)

#First I will use just the fold-change expression data (in columns 2 to 10) 
#and convert it to an mvabund object format used by the mvabund package.
genes <- mvabund(gene_expression[,2:10])

# Quick look at the spread of our data using the boxplot function.

par(mar=c(2,10,2,2)) # adjusts the margins
boxplot(gene_expression[,2:10],horizontal = TRUE,las=2, main="Fold-change in gene expression")

#It looks like some genes (e.g. Hemomucin) are more overexpressed and variable than others 
#(e.g. Defensin). It is probably a good idea to check our mean-variance relationship then! 
#We can do this using the meanvar.plot function:

meanvar.plot(genes)

#We can clearly see that the genes with high means (on the x axis) also have high variances (y axis).
#We can deal with this relationship by choosing a family of GLMs with an appropriate mean-variance assumption. 
#The default family used by mvabund when fitting multivariate GLMs is negative binomial which assumes 
#a quadratic mean-variance relationship and a log-linear relationship between the response variables 
#and any continuous variables. 
#We can check our model fit later.

#And going back to our research questions: Are there differences in the gene expression of the different 
#treatment groups? Are some genes particularly over or under-expressed in some treatment groups? Which genes?
#I will start by having a look at the data.

# To contrast gene expression against GROUP, we would use:

plot(genes~gene_expression$GROUP, cex.axis=0.8, cex=0.8)

#this is the message I get:

# PIPING TO 1st MVFORMULA 
#Error in do.call("default.plot.mvformula", foo, allargs, dots) : 
#  second argument must be a list

#IB: I don't get the error, but this is just an ugly plot, Ignore.

#I will now contrast the gene expression fold-change across treatment groups to see if the models 
#support the observations in the above graph (when I manage to plot it).

#The model syntax below fits our response variable (the mvabund object gene with the 50 values of 9 
#genes) to the predictor variable GROUP (treatment).

mod1 <- manyglm(genes ~ gene_expression$GROUP, family="poisson")

#Warning message:
#In manyglm(genes ~ gene_expression$GROUP, family = "poisson") :
#  Non-integer data are fitted to the poisson model.
#Indeed, you can only use counts with poison

#For count data which does not fit the 'poisson distribution, we can use the negative_binomial distribution.

mod2 <- manyglm(genes ~ gene_expression$GROUP, family="negative_binomial")

#Warning message:
#In manyglm(genes ~ gene_expression$GROUP, family = "negative_binomial") :
#  Non-integer data are fitted to the negative.binomial model.
#Same here, non counts can be problematic.

#If you check the assumptions you see they suck.
plot.manyglm(mod1) #really bad fit!
plot.manyglm(mod2) #really bad fit!
#Let's check the data
hist(gene_expression$HEMOM.)
hist(gene_expression$ABAEC.)
hist(gene_expression$PELLE)
hist(gene_expression$RELISH) #ok, so quite skewed
#gene_expression

#try lm's

mod3 <- manylm(genes ~ gene_expression$GROUP)
plot.manylm(mod3) #not as bad, but it opens up a bit

mod4 <- manylm(log(genes) ~ gene_expression$GROUP)
plot.manylm(mod4) #really nice. I think this is a fair choice.

#let's see the results
summary.manylm(mod3) #so, 3 and 6 are clearly different from C, and 4 is slightly different.
summary.manylm(mod4) #all sig.

#let's check uni p-values
summary.manylm(mod3, p.uni = "adjusted")  # DEFENS. and DETOX are the more different, 
#you can see all combinations too.
summary.manylm(mod4, p.uni = "adjusted")  # HEMOM, DEFENS. and DETOX are the more different, 

#Probably I'll go with logged values, but inference is similar.

#Alternative testing based on anovas.
anova.manylm(mod3, p.uni = "adjusted") #very similar output as above
anova.manylm(mod4, p.uni = "adjusted") #idem.
#summary seems more complete to me whan comparing to a control.
