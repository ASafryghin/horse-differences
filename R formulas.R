#Generalised linear model analysis - Loading ata file and required packages  
DataManu <- read.csv(file.choose(), header = T)
library(glmmADMB)

#GLMM 1 - Generalised linear model testing the influence of experiment, testing period, and behavioural category and their interaction on the mean 10s HR of the horses 
GLMM1 <- glmmadmb(mean10s ~ Experiment + Test.Period + Experiment*Beh.Cat + (1|Name), data = DataManu, zeroInflation = FALSE, family ="gaussian")
summary(GLMM1)

#GLMM 2 - Generalised linear model testing the influence of experiment, testing period, and behavioural category and their interaction on the HR increase of the horses 
GLMM2 <- glmmadmb(HR_Increase ~ Experiment + Test.Period + Experiment*Beh.Cat + (1|Name), data = DataManu.na, zeroInflation = FALSE, family ="gaussian")
summary(GLMM2)


#Post Hoc Tests (Tukey) - Testing differences in mean 10s HR between experimental conditions (NO, PF and Control)
library (multcomp)
summary(glht(GLMM1, mcp(Experiment="Tukey")))



#Random effect analysis - Anova - Test the effect of the random effect on the model, by comparing model without the random effect to the model with the random effect
GLMM1o <- glmmadmb(mean10s ~ Experiment + Test.Period + Experiment*Beh.Cat, data = DataManu, zeroInflation = FALSE, family ="gaussian")
anova(GLMM1o, GLMM1) #mean 10s HR

GLMM2o <- glmmadmb(HR_Increase ~ Experiment + Test.Period + Experiment*Beh.Cat, data = DataManu.na, zeroInflation = FALSE, family ="gaussian")
anova(GLMM2o, GLMM2) #HR increase


#Repeatability Analysis - Load Packages 
library(rptR)

#mean 10s repeatability
rep1 <- rpt(mean10s ~ (1 | Name), grname = "Name", data = DataManu, 
            datatype = "Gaussian", nboot = 1000, npermut = 1000, CI=0.95)
summary(rep1)


#HR Increase repeatability
rep2 <- rpt(HR_Increase ~ (1 | Name), grname = "Name", data = DataManu, 
            datatype = "Gaussian", nboot = 1000, npermut = 1000, CI=0.95)
summary(rep2)




