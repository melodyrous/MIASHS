
library(lme4)
library(ggplot2)


### read the data

ldesign <- read.csv2("lattice.design.csv")
summary(ldesign)
str(ldesign)
# 'data.frame':	1778 obs. of  5 variables:
#   $ yield    : num  5.42 6.05 5.49 5.98 7.13 ...
# $ family   : Factor w/ 25 levels "F1","F10","F11",..: 24 24 24 24 24 24 24 24 24 24 ...
# $ replicate: Factor w/ 6 levels "R1","R2","R3",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ block    : Factor w/ 5 levels "B1","B2","B3",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ plot     : Factor w/ 5 levels "P1","P2","P3",..: 1 1 1 1 1 1 1 1 1 1 ...

### dispositif experimental

ftable(replicate ~ family, data = ldesign)            # entre 9 et 12 palmiers par famille et par repet
ftable(replicate ~ block, data = ldesign)              # 5 blocs par repet
ftable(replicate ~ block + plot, data = ldesign)       # 5 plot par bloc par repet

### plot 

ggplot(ldesign, aes(reorder(family, yield), yield)) + geom_boxplot()
ggplot(ldesign, aes(replicate, yield)) + geom_boxplot(aes(fill = replicate)) + facet_wrap(~family)

mod0<- lm(yield ~ family, data = ldesign)
anova(mod0)
plot(ldesign$replicate, resid(mod0))

attach( ldesign ) 
interaction.plot(family, replicate, yield, las = 1) 
detach() 


#-----------------------------#
### modele mixte
#-----------------------------#

mod <- lmer(yield ~ family + (1|replicate), data = ldesign) # avec replicate en aleatoire
summary(mod)

VarCorr(mod) # variance covariance
fixef(mod)   # coefficients associ?s aux effets fixes (fixed effect)
# (Intercept)   familyF10  ...  
# 5.11370770  1.54588600  ...
ranef(mod)   # blup : effets al?atoires.


# note: par d?faut, la famille F1 est consid?r?e comme modalit? de r?f?rence (contr.treatment).
options()$contrasts
# unordered           ordered 
# "contr.treatment"      "contr.poly" 
contrasts(ldesign$family)

# pour changer le contrats
# options(contrasts = c("contr.sum", "contr.poly"))
contrasts(ldesign$family)

mod <- lmer(yield ~ family + (1|replicate), data = ldesign)
summary(mod)
fixef(mod)
# (Intercept)      family1      
# 5.696244877 -0.582537178

# dans le modele precedent, intercept = mu + effet associe a la famille F1 = 5.114
# dans ce modele l?, intercept = mu=5.696244877 et effet associe a la famille F1 = -0.582537178 => ok, 5.696 + (-0.582) = 5.114

### etude des residus
plot(mod)

plot(resid(mod, type = "pearson") ~ family, data = ldesign)
plot(resid(mod, type = "pearson") ~ replicate, data = ldesign)
ggplot(ldesign, aes(factor(paste(replicate, family)), resid(mod))) + geom_point() + geom_hline(yintercept = 0, colour = "red") + theme(axis.text.x = element_text(angle = 90))


### modele avec famille dans replicate (nested random effect)

mod2 <- lmer(yield ~ family + (1|replicate/family), data = ldesign)
mod2.bis <- lmer(yield ~ family + (1|replicate) +(1|replicate:family), data = ldesign)
summary(mod2)
VarCorr(mod2)

### choix de modele
anova(mod, mod2)


### prise en compte du bloc ?
mod3 <- lmer(yield ~ family + (1|replicate/block/family), data = ldesign)
mod3.bis <- lmer(yield ~ family + (1|replicate/block) +(1|replicate:family), data = ldesign)

summary(mod3)
anova(mod3, mod2)





### test de l'effet famille
library(lmerTest)  # pseudo F test bas? sur l'approximation de Satterthwaite
mod2 <- lmer(yield ~ family + (1|replicate/family), data = ldesign)
anova(mod2)   # ok, test significatif au seuil de 5%.
summary(mod2)

# autre possibilit?
# => test de modele embo?te avec effet fixe estime par ML (et non par REML)

mod0 <- lmer(yield ~ 1 + (1|replicate/family), data = ldesign, REML = FALSE)
anova(mod0, mod2)


## autre modeles possible

mod4 <- lmer(yield ~ family + (1|replicate/block), data = ldesign)
summary(mod4)

anova(mod3, mod4)  # p value < 0.05 


#-----------------------------#
### remarque sur le codage des facteurs
#-----------------------------#


# on considere ici un modele avec les 3 effets aleatoires replicate, block et family

mod3 <- lmer(yield ~ family + (1|replicate/block/family), data = ldesign)
VarCorr(mod3)

mod3b <- lmer(yield ~ family + (1|replicate/block/plot), data = ldesign)
VarCorr(mod3b)
# effet plot/block/replicate = effet famille/block/replicate => c'est normal. la famille de la repet = plot

### codage des levels
levels(ldesign$replicate)  # R1 ? R6
levels(ldesign$block)      # B1 ? B5
levels(ldesign$plot)       # P1 ? P5.

# => comme block est emboite dans repet, on pourrait recoder les modalites de bloc (pour faire la distinction entre bloc 1 de la repet 1 et bloc 1 de la repet 2 par exemple)
# => idem pour la parcelle elementaire, on peut recoder les modalites de plot (pour distinguer plot 1 du bloc 1 de la repet 1 et plot 1 du bloc 2 de la repet 1 par exemple)

ldesign$block2 <- factor(paste(ldesign$replicate, ldesign$block, sep = "_"))
ldesign$plot2 <- factor(paste(ldesign$replicate, ldesign$block, ldesign$plot, sep = "_"))

ftable(replicate ~ block, data = ldesign)
ftable(replicate ~ block2, data = ldesign)
ftable(replicate ~ block + plot, data = ldesign)
ftable(replicate ~ plot2, data = ldesign)

### on refait le modele predecent

mod3 <- lmer(yield ~ family + (1|replicate/block/family), data = ldesign)
VarCorr(mod3)

mod3b <- lmer(yield ~ family + (1|replicate/block/plot), data = ldesign)
VarCorr(mod3b)

mod3c <- lmer(yield ~ family + (1|replicate/block2/plot2), data = ldesign)
VarCorr(mod3c) # ok, on a bien les memes resultats

mod3d <- lmer(yield ~ family + (1|replicate) + (1|block2) + (1|plot2), data = ldesign)  # possible d'ecrire comme cela car on a recoder bloc et plot
VarCorr(mod3d) # ok, on a bien les memes resultats



#-----------------------------#
### remarque sur l'independance des effets al?atoires
#-----------------------------#


# quand on ecrit (1|replicate/family) ou (1|replicate) + (1|replicate:family), on suppose que les deux effets al?atoires sont ind?pendants.
# si on suppose que ces deux effets ne sont pas ind?pedants => plus de param?tres ? estimer.

### exemple avec la fonction lme du package nlme

library(nlme)

data.test <- droplevels(subset(ldesign, family %in% c("F18","F7","F10")))


#---------#
# Compound Symmetry Model (voir livre de Pinheiro et Bates ou ?pdMat dans R)
#---------#

mod1.test <- lme(yield ~ family, data = data.test ,
                 random =list(replicate = pdCompSymm(~ family -1)))
summary(mod1.test)  
VarCorr(mod1.test)

mod1.xxx <- lmer(yield ~ family + (1|replicate) + (1|replicate:family), data = data.test)
summary(mod1.xxx)  
VarCorr(mod1.xxx)

# ok on a bien de parametres : 
# => var(uij, uij')=0.30 
# => cov(uij, uij')=0.096

#---------#
# Heterogeneous Compound Symmetry Model (voir livre de Pinheiro et Bates ou ?pdMat dans R)
#---------#

mod2.test <- lme(yield ~ family, data = data.test ,
                 random =list(replicate = pdBlocked(list(pdIdent(~ 1),
                                                  pdDiag(~ family - 1)))))
summary(mod2.test)
VarCorr(mod2.test)


#---------#
# Unstructured Model (voir livre de Pinheiro et Bates ou ?pdMat dans R)
#---------#

mod3.test <- lme(yield ~ family, data = data.test ,
                 random = ~family - 1|replicate)
summary(mod3.test)
VarCorr(mod3.test)





