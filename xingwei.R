library(readxl)
library(glmnet)

# xw stands for xingwei
xw <- read_excel("~/desktop/creativity/creativeity_2018_xingwei.xlsx")
# remove rows that contain any NA
xw <- xw[complete.cases(xw), ]
xw <- xw[, -c(9:14, 17:24)]

library(leaps)
colnames(xwm)[14] <- "Y" 
all.models = regsubsets(Y ~., data = xwm)
some.stuff = summary(all.models)
names.of.data = c("Y",colnames(some.stuff$which)[-1])
n= nrow(xwm) 
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

# Lasso
getLasso <- function(data){
  require(glmnet)
  data_matrix <- as.matrix(data)
  set.seed(9999)
  CV <- cv.glmnet(x=data_matrix[, -14], y = data_matrix[,14],
                family="gaussian", alpha=1,nlambda=100, type.measure = "mse")
  fit <- glmnet(x=data_matrix[, -14], y = data_matrix[,14], family = "gaussian", alpha = 1,
              lambda = CV$lambda.1se)
  fit
}
# xwm: y is the mean of xingwei (新颖是mean)
xwm <- c()
for (i in 1:nrow(xw))
  xwm[i] <- sum (xw[i,16],xw[i,17],xw[i,18],xw[i,19]) / 4
xwm <- cbind(xw, xwm)
xwm <- xwm[, -(14:19)]
xwm_fit <- getLasso(xwm)
xwm_fit$beta
BIC_from_full_xwm <- lm(Y ~ Kindergarden_index + age + educational_level, data = xwm)
summary(BIC_from_full_xwm)
#xwx： y is 新颖度 (0-1)
xwx <- xw[,-(15:19)]

xwx_fit <- getLasso(xwx)
xwx_fit$beta
# r2 = 0.1106
BIC_from_full_xwx <- lm(Y ~ Kindergarden_index + sex + `mothers' _educatioanl_level`, data = xwx)
summary(BIC_from_full_xwx)
# xwx2: y is 新颖度（0-3）
xwx2 <- xw[, -c(14,16:19)]
BIC_from_full_xwx2 <- lm(Y ~ Kindergarden_index + age, data=xwx2)
# r2 = 0.09
summary(BIC_from_full_xwx2)
xwx2_fit <- getLasso(xwx2)
xwx2_fit$beta
 
#xwx3: y is 新颖度 （sub1-5)
xwx3 <- xw[, -c(14,15,17:19)]
BIC_from_full_xwx3 <- lm(Y ~Kindergarden_index+age+educational_level,data=xwx3)
summary(BIC_from_full_xwx3)
xwx3_fit <- getLasso(xwx3)
xwx3_fit$beta
  
  
  
  
  
# xw4: y is y4. 
xw4 <- xw[, -c(14:16,18,19)]
xw4_fit <- getLasso(xw4)
xw4_fit$beta
BIC_from_full_xw4 <- lm(Y ~  Kindergarden_index + age, data=xw4)
# r2 = 0.1378
summary(BIC_from_full_xw4)

# xw5: y is y5. At column 18
xw5 <- xw[, -c(14:17,19)]
xw5_fit <- getLasso(xw5)
xw5_fit$beta
BIC_from_full_xw5 <- lm(Y ~  Kindergarden_index + sex + age + parents_view_in_nature, data=xw5)
#r2 = 0,1181
summary(BIC_from_full_xw5)

# xw6: y is y6, 19
xw6 <- xw[, -c(14:18)]
xw6_fit <- getLasso(xw6)
xw6_fit$beta
BIC_from_full_xw6 <- lm(Y ~  Kindergarden_index+sex+age+
`mothers' _educatioanl_level`+parents_behavior_support_mean+
  parents_view_in_nature, data=xw6)
# r2 = 0.2632
summary(BIC_from_full_xw6)












