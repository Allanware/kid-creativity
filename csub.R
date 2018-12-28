library(leaps)
creativity_yisinquisitive <-  creativity[, -c(9:14, 17:24,28, 30:32)]
creativity_yisimagin <-  creativity[, -c(9:14, 17:24,28, 29,31, 32)]
creativity_yisrisk_mean <- creativity[, -c(9:14, 17:24,28:30,32)]
creativity_yischallenge_mean <-  creativity[, -c(9:14, 17:24,28:31)]

colnames(creativity_yischallenge_mean)[14] <- "Y" 
colnames(creativity_yisinquisitive)[14] <- "Y" 
colnames(creativity_yisimagin)[14] <- "Y" 
colnames(creativity_yisrisk_mean)[14] <- "Y" 

all.models = regsubsets(Y ~., data = dataset)
some.stuff = summary(all.models)
names.of.data = c("Y",colnames(some.stuff$which)[-1])
n= nrow(dataset) 
K = nrow(some.stuff$which)
nicer = lapply(1:K,function(i){
  model = paste(names.of.data[some.stuff$which[i,]],collapse = "+")
  p = sum(some.stuff$which[i,])
  BIC = some.stuff$bic[i]
  CP = some.stuff$cp[i]
  results = data.frame(model,p,CP,BIC)
  return(results)
})



# Multiple R-squared:  0.3283,	Adjusted R-squared:  0.3263 
dataset <- creativity_yisinquisitive
nicer_inquisitiveness <- Reduce(rbind, nicer)
BIC_inquitiveness <- lm (Y ~ parents_behavior_support_mean+concentration_in_activity+willingness_in_nature_env, data = creativity_yisinquisitive)

#Multiple R-squared:  0.272,	Adjusted R-squared:  0.2685 
dataset <- creativity_yischallenge_mean
nicer_challenge_mean <- Reduce(rbind, nicer)
BIC_challenge_mean <- lm (Y ~ age+educational_level+parents_behavior_support_mean
      +concentration_in_activity+willingness_in_nature_env,data=creativity_yischallenge_mean)
summary(BIC_challenge_mean)

#Multiple R-squared:  0.2423,	Adjusted R-squared:  0.2371 
dataset <- creativity_yisimagin
nicer_imagin <- Reduce(rbind, nicer)
BIC_imagin <- lm(Y~sex+age+home_nature_chance+area_of_nature_exposure+
nature_elememt_diversity_mean+parents_behavior_support_mean+
  willingness_in_nature_env, data=creativity_yisimagin)
summary(BIC_imagin)

#Multiple R-squared:  0.3562,	Adjusted R-squared:  0.3518 
dataset <- creativity_yisrisk_mean
nicer_risk <- Reduce(rbind, nicer)
BIC_risk <- lm(Y~age+home_nature_chance+area_of_nature_exposure+nature_elememt_diversity_mean+parents_behavior_support_mean
+concentration_in_activity+willingness_in_nature_env, data=creativity_yisrisk_mean)
summary(BIC_risk)

# Multiple R-squared:  0.327,	Adjusted R-squared:  0.3224 
c_mix_ii <- creativity_yisimagin
c_mix_ii[,14] <- (creativity_yisimagin[,14] + creativity_yisinquisitive[,14]) /2
dataseet <- c_mix_ii
nicer_ii <- Reduce(rbind, nicer)
BIC_ii <- lm( Y~age+home_nature_chance+area_of_nature_exposure+nature_elememt_diversity_mean+parents_behavior_support_mean+concentration_in_activity+willingness_in_nature_env,
              data=c_mix_ii)
summary(BIC_ii)

# Multiple R-squared:  0.3749,	Adjusted R-squared:  0.3725 
c_mix_cr <- creativity_yischallenge_mean
c_mix_cr[,14] <- (creativity_yischallenge_mean[,14] + creativity_yisrisk_mean[,14]) /2
dataset <- c_mix_cr 
nicer_cr <- Reduce(rbind, nicer)
BIC_cr <- lm(Y~age+parents_behavior_support_mean+concentration_in_activity+willingness_in_nature_env
             ,data=c_mix_cr)
summary(BIC_cr)


