# creativity test from teacher
tcreativity <- read_excel("~/desktop/creativeity_2018_teacher.xlsx")
tcreativity <- tcreativity[,-28] # exclude score 

tcreativity_yismean<- tcreativity[, -c(9:14, 17:24,29:32)]

library(leaps)
colnames(tcreativity_yismean)[14] <- "Y" 
all.models = regsubsets(Y ~., data = tcreativity_yismean)
some.stuff = summary(all.models)
names.of.data = c("Y",colnames(some.stuff$which)[-1])
n= nrow(tcreativity_yismean) 
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

BIC_from_full <- lm(Y ~ Kindergarden_index + age, data = tcreativity_yismean)

