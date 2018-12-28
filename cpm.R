# read in file
# critical variables are marked red in original excel
creativity <- read_excel("~/desktop/creativeity_2018_parents.xlsx")
creativity <- creativity[,-28] # exclude score 

creativity_yismean<- creativity[, -c(9:14, 17:24,29:32)]
# multicollinerality for xs 
require(psych)
pairs.panels(creativity_yismean[,-14], cex = 1.5) 

# model selection: 
# use BIC and CP mallow since they tend to choose the correct model (smaller model)
# not transforming the data, losing interpretablity
library(leaps)
colnames(creativity_yismean)[14] <- "Y" 
all.models = regsubsets(Y ~., data = creativity_yismean)
some.stuff = summary(all.models)
names.of.data = c("Y",colnames(some.stuff$which)[-1])
n= nrow(creativity_yismean) 
K = nrow(some.stuff$which)
nicer = lapply(1:K,function(i){
  model = paste(names.of.data[some.stuff$which[i,]],collapse = ",")
  p = sum(some.stuff$which[i,])
  BIC = some.stuff$bic[i]
  CP = some.stuff$cp[i]
  results = data.frame(model,p,CP,BIC)
  return(results)
})
nicer = Reduce(rbind,nicer)
nicer
# model selected by BIC 
BIC_from_full <- lm(Y ~age+home_nature_chance+parents_behavior_support_mean+
                      concentration_in_activity+willingness_in_nature_env, 
                    data = creativity_yismean)
# get p value for each variable, r square, adjusted r square...
summary(BIC_from_full)
anova(BIC_from_full)
CP_Mallow_full <- lm(Y ~age+home_nature_chance+area_of_nature_exposure+
                     nature_elememt_diversity_mean+
                     parents_behavior_support_mean+
                     concentration_in_activity+willingness_in_nature_env,
                     data = creativity_yismean)
summary(CP_Mallow_full)
anova(CP_Mallow_full)
# outlier removal
# Semi-studentized/standardized residuals,  ei.s = ei/sqrt(MSE)
ei.s = BIC_from_full$residuals/sqrt(sum(BIC_from_full$residuals^2)/
      (nrow(creativity_yismean) - length(BIC_from_full$coefficients)))
# Studentized/Standardized residuals
ri = rstandard(BIC_from_full)
# Deleted Residuals
ti = rstudent(BIC_from_full)
# calculating cutoff and identifying outliers 
alpha = 0.1 ; n = nrow(creativity_yismean); p = length(BIC_from_full$coefficients)
cutoff = qt(1-alpha/(2*n), n -p )
cutoff.deleted = qt(1-alpha/(2*n), n -p -1 )
outliers = which(abs(ei.s)> cutoff | abs(ri) > cutoff | abs(ti) > cutoff.deleted)
# remove outliers
creativity_yismean_ <- creativity_yismean[-outliers, ]
# rerun 
all.models = regsubsets(Y ~., data = creativity_yismean_)
some.stuff = summary(all.models)
names.of.data = c("Y",colnames(some.stuff$which)[-1])
n= nrow(creativity_yismean_) 
K = nrow(some.stuff$which)
nicer = lapply(1:K,function(i){
  model = paste(names.of.data[some.stuff$which[i,]],collapse = ",")
  p = sum(some.stuff$which[i,])
  BIC = some.stuff$bic[i]
  CP = some.stuff$cp[i]
  results = data.frame(model,p,CP,BIC)
  return(results)
})
nicer_ = Reduce(rbind,nicer)
nicer_

# model selected after outlier removal
BIC_after_removal <- lm (Y ~ age+area_of_nature_exposure+nature_elememt_diversity_mean
  +parents_behavior_support_mean+concentration_in_activity+
willingness_in_nature_env, data = creativity_yismean_)
summary(BIC_after_removal)
anova(BIC_after_removal)

# diagnostics
# normality of eis 
# QQ plot
qqnorm(BIC_after_removal$residuals)
qqline(BIC_after_removal$residuals)
# SW test 
the.SWtest = shapiro.test(BIC_after_removal$residuals)
the.SWtest
# constant varainces 
# FK test
Group = rep("Lower",nrow(creativity_yismean_)) #Creates a vector that repeats "Lower" n times
Group[creativity_yismean_$Y > median(creativity_yismean_$Y)] = "Upper" #Changing the appropriate values to "Upper"
Group = as.factor(Group) #Changes it to a factor, which R recognizes as a grouping variable.
creativity_yismean_$Group = Group
the.FKtest= fligner.test(BIC_after_removal$residuals, Group)
the.FKtest
# BF test
the.BFtest = leveneTest(BIC_after_removal$residuals~Group, data=creativity_yismean_, center=median)
p.val = the.BFtest[[3]][1]
p.val
# Lasso
require(glmnet)
creativity_yismean_matrix <- as.matrix(creativity_yismean)
set.seed(9999)
CV <- cv.glmnet(x=creativity_yismean_matrix[, -14], y = creativity_yismean_matrix[,14],
                family="gaussian", alpha=1,nlambda=100, type.measure = "mse")
fit <- glmnet(x=creativity_yismean_matrix[, -14], y = creativity_yismean_matrix[,14], family = "gaussian", alpha = 1,
              lambda = CV$lambda.1se)
fit$beta[,1]

# anova
factor_creativity <- creativity_yismean[, c(1,2,3,14)]
factor_creativity <- as.data.frame(factor_creativity)
factor_creativity[,1] <- as.factor(factor_creativity[,1])
factor_creativity[,2] <- as.factor(factor_creativity[,2])
factor_creativity$age[which(factor_creativity$age < 4)] <- "3-4"
factor_creativity$age[which(factor_creativity$age > 4 & factor_creativity$age <5)] <- "4-5"
factor_creativity$age[which(factor_creativity$age >5)] <- "5-6"
factor_creativity[,3] <- as.factor(factor_creativity[,3])
fm <- aov(Y ~ Kindergarden_index * sex * age, data =factor_creativity)
summary(fm)

