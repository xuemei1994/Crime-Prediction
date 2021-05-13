
rm(list = ls()) #delete objects
cat("\014") # clear console

library(dplyr)
library(tidyr)
library(glmnet)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(grid)

### this part is for raw data cleaning only

df = read.csv("CommViolPredUnnormalizedData.txt", header=F) # the original raw data
df[df == '?'] = NA
sum(is.na(df[,6:103])) # 1 na in column#31
df[2006,31] = 0
sum(is.na(df[,6:103]))

names = c('communityname',
          'state',
          'countyCode',
          'communityCode',
          'fold',
          'population',
          'householdsize',
          'racepctblack',
          'racePctWhite',
          'racePctAsian',
          'racePctHisp',
          'agePct12t21',
          'agePct12t29',
          'agePct16t24',
          'agePct65up',
          'numbUrban',
          'pctUrban',
          'medIncome',
          'pctWWage',
          'pctWFarmSelf',
          'pctWInvInc',
          'pctWSocSec',
          'pctWPubAsst',
          'pctWRetire',
          'medFamInc',
          'perCapInc',
          'whitePerCap',
          'blackPerCap',
          'indianPerCap',
          'AsianPerCap',
          'OtherPerCap',
          'HispPerCap',
          'NumUnderPov',
          'PctPopUnderPov',
          'PctLess9thGrade',
          'PctNotHSGrad',
          'PctBSorMore',
          'PctUnemployed',
          'PctEmploy',
          'PctEmplManu',
          'PctEmplProfServ',
          'PctOccupManu',
          'PctOccupMgmtProf',
          'MalePctDivorce',
          'MalePctNevMarr',
          'FemalePctDiv',
          'TotalPctDiv',
          'PersPerFam',
          'PctFam2Par',
          'PctKids2Par',
          'PctYoungKids2Par',
          'PctTeen2Par',
          'PctWorkMomYoungKids',
          'PctWorkMom',
          'NumKidsBornNeverMar',
          'PctKidsBornNeverMar',
          'NumImmig',
          'PctImmigRecent',
          'PctImmigRec5',
          'PctImmigRec8',
          'PctImmigRec10',
          'PctRecentImmig',
          'PctRecImmig5',
          'PctRecImmig8',
          'PctRecImmig10',
          'PctSpeakEnglOnly',
          'PctNotSpeakEnglWell',
          'PctLargHouseFam',
          'PctLargHouseOccup',
          'PersPerOccupHous',
          'PersPerOwnOccHous',
          'PersPerRentOccHous',
          'PctPersOwnOccup',
          'PctPersDenseHous',
          'PctHousLess3BR',
          'MedNumBR',
          'HousVacant',
          'PctHousOccup',
          'PctHousOwnOcc',
          'PctVacantBoarded',
          'PctVacMore6Mos',
          'MedYrHousBuilt',
          'PctHousNoPhone',
          'PctWOFullPlumb',
          'OwnOccLowQuart',
          'OwnOccMedVal',
          'OwnOccHiQuart',
          'OwnOccQrange',
          'RentLowQ',
          'RentMedian',
          'RentHighQ',
          'RentQrange',
          'MedRent',
          'MedRentPctHousInc',
          'MedOwnCostPctInc',
          'MedOwnCostPctIncNoMtg',
          'NumInShelters',
          'NumStreet',
          'PctForeignBorn',
          'PctBornSameState',
          'PctSameHouse85',
          'PctSameCity85',
          'PctSameState85',
          'LemasSwornFT',
          'LemasSwFTPerPop',
          'LemasSwFTFieldOps',
          'LemasSwFTFieldPerPop',
          'LemasTotalReq',
          'LemasTotReqPerPop',
          'PolicReqPerOffic',
          'PolicPerPop',
          'RacialMatchCommPol',
          'PctPolicWhite',
          'PctPolicBlack',
          'PctPolicHisp',
          'PctPolicAsian',
          'PctPolicMinor',
          'OfficAssgnDrugUnits',
          'NumKindsDrugsSeiz',
          'PolicAveOTWorked',
          'LandArea',
          'PopDens',
          'PctUsePubTrans',
          'PolicCars',
          'PolicOperBudg',
          'LemasPctPolicOnPatr',
          'LemasGangUnitDeploy',
          'LemasPctOfficDrugUn',
          'PolicBudgPerPop',
          'murders',
          'murdPerPop',
          'rapes',
          'rapesPerPop',
          'robberies',
          'robbbPerPop',
          'assaults',
          'assaultPerPop',
          'burglaries',
          'burglPerPop',
          'larcenies',
          'larcPerPop',
          'autoTheft',
          'autoTheftPerPop',
          'arsons',
          'arsonsPerPop',
          'ViolentCrimesPerPop',
          'nonViolPerPop')


colnames(df) = names
df2 = df[, c(6:103,131)]
df2[,26] = as.numeric(df2[,26])

### raw data cleaning done
############################################################################

#dim(df2) # 2215x99 #99th variable murdPerPop is y 
#write.csv(df2, 'crime2.csv')
#summary(df2)
#str(df2)

### df2 is ready to use
df2 = read.csv("https://raw.githubusercontent.com/soniashih/STA9890/main/crime.csv") # read data with github url
df2 = read.csv('crime.csv') # or read the same data with local file
dim(df2) # 2215x99

set.seed(1)
M = 100
n = dim(df2)[1]
p = dim(df2)[2]-1

X = as.matrix(df2[,1:p])
y = as.matrix(df2["murdPerPop"])

############################################################################
############################### PART 3-4 ###################################
############################################################################

n.train = 0.8*n

train_Rsq = matrix(NA,M,4) # order: en, lasso, ridge, rf
test_Rsq = matrix(NA,M,4) # order: en, lasso, ridge, rf

time.rid = matrix(NA,M,1)
time.las = matrix(NA,M,1)
time.eln = matrix(NA,M,1)

Rsq.rf = matrix(NA,M,2) # Rsq matrix for random forest model
Rsq.las = matrix(NA,M,2) # Rsq matrix for lasso
Rsq.en = matrix(NA,M,2) # Rsq matrix for elastic-net
Rsq.rid = matrix(NA,M,2) # Rsq matrix for ridge

for (m in 1:M) {
  
  ###############################################
  ###############################################
  shuffled_indexes =  sample(n)
  train            =  shuffled_indexes[1:n.train]
  test             =  shuffled_indexes[(1+n.train):n]
  
  X.train          =  X[train, ]
  y.train          =  y[train]
  X.test           =  X[test, ]
  y.test           =  y[test]
  
  ###############################################
  ###############################################
  
  # -----elastic-net model-----#
  time0.eln = proc.time()
  cv.fit        =     cv.glmnet(X.train, y.train, intercept = TRUE, alpha = 0.5, nfolds = 10)
  time1.eln = proc.time()
  time.eln[m,1] = (time1.eln - time0.eln)[['elapsed']]
  
  fit           =     glmnet(X.train, y.train,intercept = TRUE, alpha = 0.5, lambda = cv.fit$lambda.min)
  # train Rsq
  y.train.hat   =     predict(fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  res.train.en  =     y.train - y.train.hat # residuals
  Rsq.train.en  =     1-mean((res.train.en)^2)/mean((y.train - mean(y.train))^2)  
  # test Rsq
  y.test.hat    =     predict(fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  res.test.en   =     y.test - y.test.hat # residuals
  Rsq.test.en   =     1-mean((res.test.en)^2)/mean((y.test - mean(y.test))^2)
  
  # recording Rsq
  cat("elastic-net", m, ":","Rsq",Rsq.train.en, Rsq.test.en, "\n")
  Rsq.en[m,] = c(Rsq.train.en, Rsq.test.en)
  train_Rsq[m,1] = Rsq.train.en
  test_Rsq[m,1] = Rsq.test.en
  
  if (m==91) {
  #plotting cv curve
  par(mfrow = c(1,3))
  plot.en = plot(cv.fit, main='elastic-net')
  }
  
  
  # -----lasso model----- #
  
  time0.las = proc.time()
  cv.fit         =     cv.glmnet(X.train, y.train, intercept = TRUE, alpha = 1, nfolds = 10)
  time1.las = proc.time()
  time.las[m,1] = (time1.las - time0.las)[['elapsed']]
  
  
  fit            =     glmnet(X.train, y.train,intercept = TRUE, alpha = 1, lambda = cv.fit$lambda.min)
  # train Rsq
  y.train.hat    =     predict(fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  res.train.las  =     y.train - y.train.hat # residuals
  Rsq.train.las  =     1-mean((res.train.las)^2)/mean((y.train - mean(y.train))^2)  
  # test Rsq
  y.test.hat     =     predict(fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  res.test.las   =     y.test - y.test.hat # residuals
  Rsq.test.las   =     1-mean((res.test.las)^2)/mean((y.test - mean(y.test))^2)
  time1.las = proc.time()
  # recording Rsq
  cat("lasso", m, ":","Rsq",Rsq.train.las, Rsq.test.las, "\n")
  Rsq.las[m,] = c(Rsq.train.las, Rsq.test.las)
  train_Rsq[m,2] = Rsq.train.las
  test_Rsq[m,2] = Rsq.test.las
  
  if (m==91) {
  #plotting cv curve
  plot.las = plot(cv.fit, main='lasso')
  }
  
  
  # -----ridge model----- #
  time0.rid = proc.time()
  cv.fit         =     cv.glmnet(X.train, y.train, intercept = TRUE, alpha = 0, nfolds = 10)
  time1.rid = proc.time()
  time.rid[m,1] = (time1.rid - time0.rid)[['elapsed']]
  
  fit            =     glmnet(X.train, y.train,intercept = TRUE, alpha = 0, lambda = cv.fit$lambda.min)
  # train error
  y.train.hat    =     predict(fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  res.train.rid  =     y.train - y.train.hat # residuals
  Rsq.train.rid  =     1-mean((res.train.rid)^2)/mean((y.train - mean(y.train))^2)  
  # test error
  y.test.hat     =     predict(fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  res.test.rid   =     y.test - y.test.hat # residuals
  Rsq.test.rid   =     1-mean((res.test.rid)^2)/mean((y.test - mean(y.test))^2)
  
  # recording Rsq
  cat("ridge", m, ":", "Rsq",Rsq.train.rid, Rsq.test.rid, "\n")
  Rsq.rid[m,] = c(Rsq.train.rid, Rsq.test.rid)
  train_Rsq[m,3] = Rsq.train.rid
  test_Rsq[m,3] = Rsq.test.rid
  
  if (m==91) {
  #plotting cv curve
  plot.rid = plot(cv.fit, main='ridge')
  }
  
  # -----random forest model----- #
  rf.df2  =  randomForest(murdPerPop~., data=df2, subset = train, mtry=floor(sqrt(p)), importance=F)
  # train Rsq
  y.train.hat   = predict(rf.df2, newdata = df2[train,])
  res.train.rf  = y.train - y.train.hat # residuals
  Rsq.train.rf  = 1-mean((res.train.rf)^2)/mean((y.train - mean(y.train))^2)  
  # test Rsq
  y.test.hat    = predict(rf.df2, newdata = df2[test,])
  res.test.rf   = y.test - y.test.hat # residuals
  Rsq.test.rf   = 1-mean((res.test.rf)^2)/mean((y.test - mean(y.test))^2)
  time1.rf =  proc.time()
  # recording Rsq
  cat("random forest", m, ":", "Rsq",Rsq.train.rf, Rsq.test.rf, "\n")
  Rsq.rf[m,] = c(Rsq.train.rf, Rsq.test.rf)
  train_Rsq[m,4] = Rsq.train.rf
  test_Rsq[m,4] = Rsq.test.rf
  
  
  ##################################################
  ##################################################
  ##################################################
  if (m==91) {
  # train residual plot
  train_residual = cbind(c(res.train.en), c(res.train.las), c(res.train.rid), c(res.train.rf))
  colnames(train_residual) = c('EN', 'Lasso', 'Ridge', 'RF')
  # test residual plot
  test_residual = cbind(c(res.test.en), c(res.test.las), c(res.test.rid), c(res.test.rf))
  colnames(test_residual) = c('EN', 'Lasso', 'Ridge', 'RF')
  #residual box plot for this sample
  par(mfrow = c(1,2))
  lower_residual = floor(min(train_residual, test_residual))
  upper_residual = ceiling(max(train_residual, test_residual))
  boxplot(train_residual, main='Train Residuals', ylim=c(lower_residual,upper_residual))
  boxplot(test_residual, main='Test Residuals', ylim=c(lower_residual,upper_residual))
  }
}



# ----- PART4(b) -----
par(mfrow = c(1,2)) # arrange plots into 1x2 format

colnames(train_Rsq) = c('EN', 'Lasso', 'Ridge', 'RF')
colnames(test_Rsq) = c('EN', 'Lasso', 'Ridge', 'RF')
lowest_Rsq = min(train_Rsq, test_Rsq)
boxplot(train_Rsq, main='Train Rsq', ylim=c(0.3,1))
boxplot(test_Rsq, main='Test Rsq', ylim=c(0.3,1))


# ----- PART4(c) -----
# single model running time recording - 100 samples average
ave.time.eln    = mean(time.eln)   # one elastic-net running time
ave.time.las   = mean(time.las)   # one lasso running time
ave.time.rid   = mean(time.rid)   # one ridge model running time





#####################################################################
############################### PART5 ###############################
#####################################################################

############     LASSO - All DATA ############
set.seed(1)
timeLas0    =      proc.time()
cv.lasso    =      cv.glmnet(X, y, alpha=1, nfolds = 10)
lasso.fit   =      glmnet(X,y,alpha=1, lambda=cv.lasso$lambda.min)
timeLas1    =      proc.time()

#######coefficient. lasso 
df.lasso    =      cv.lasso$nzero[which.min(cv.lasso$cvm)]
a0.hat.lasso      =   lasso.fit$a0[lasso.fit$lambda==cv.lasso$lambda.min]
beta.hat.lasso    =   lasso.fit$beta[ ,lasso.fit$lambda==cv.lasso$lambda.min]

############     RIDGE - All DATA ############
set.seed(1)
timeRid0    =      proc.time()
cv.ridge    =      cv.glmnet(X, y, alpha=0, nfolds = 10)
ridge.fit   =      glmnet(X,y,alpha=0, lambda=cv.ridge$lambda.min)
timeRid1    =      proc.time()

#######coefficient. ridge
df.ridge    =      cv.ridge$nzero[which.min(cv.ridge$cvm)]
a0.hat.ridge      =   ridge.fit$a0[ridge.fit$lambda==cv.ridge$lambda.min]
beta.hat.ridge    =   ridge.fit$beta[ ,ridge.fit$lambda==cv.ridge$lambda.min]

############     ELNET - All DATA ############
set.seed(1)
timeEln0    =      proc.time()
cv.elnet    =      cv.glmnet(X, y, alpha=0.5, nfolds = 10)
elnet.fit   =      glmnet(X,y,alpha=0.5, lambda=cv.elnet$lambda.min)
timeEln1    =      proc.time()

#######coefficient. elnet
df.elnet    =      cv.elnet$nzero[which.min(cv.elnet$cvm)]
a0.hat.elnet      =   elnet.fit$a0[elnet.fit$lambda==cv.elnet$lambda.min]
beta.hat.elnet    =   elnet.fit$beta[ ,elnet.fit$lambda==cv.elnet$lambda.min]



############     RANDOM FOREST - ALL data #########
set.seed(1)
timeRf0 = proc.time()
rf.full  =  randomForest(murdPerPop~., data=df2, mtry=floor(sqrt(p)), importance=T)
timeRf1 = proc.time()
timeRf = timeRf1 - timeRf0





betaS.en               =     data.frame(colnames(df2)[1:p], as.vector(beta.hat.elnet))
colnames(betaS.en)     =     c( "feature", "Coefficient")

betaS.ls               =     data.frame(colnames(df2)[1:p], as.vector(beta.hat.lasso))
colnames(betaS.ls)     =     c( "feature", "Coefficient")

betaS.rg               =     data.frame(colnames(df2)[1:p], as.vector(beta.hat.ridge))
colnames(betaS.rg)     =     c( "feature", "Coefficient")

betaS.rf               =     data.frame(colnames(df2)[1:p], as.vector(rf.full$importance[,1]))
colnames(betaS.rf)     =     c( "feature", "PctIncMSE")



betaS.ls$feature     =  factor(betaS.ls$feature, levels = betaS.en$feature[order(betaS.en$Coefficient, decreasing = TRUE)])
betaS.en$feature     =  factor(betaS.en$feature, levels = betaS.en$feature[order(betaS.en$Coefficient, decreasing = TRUE)])
betaS.rg$feature     =  factor(betaS.rg$feature, levels = betaS.en$feature[order(betaS.en$Coefficient, decreasing = TRUE)])
betaS.rf$feature     =  factor(betaS.rg$feature, levels = betaS.en$feature[order(betaS.en$Coefficient, decreasing = TRUE)])


L = min(betaS.ls$Coefficient, betaS.en$Coefficient, betaS.rg$Coefficient)
U = max(betaS.ls$Coefficient, betaS.en$Coefficient, betaS.rg$Coefficient)
U_rf = max(betaS.rf$PctIncMSE)


lsPlot =  ggplot(betaS.ls, aes(x=feature, y=Coefficient)) +
  geom_bar(stat = "identity", fill=ifelse(betaS.ls$Coefficient>0,"steelblue","red"), colour="black") +
  ggtitle(("Lasso")) + ylim(c(L,U))+theme(axis.text.x = element_text(size = 0,angle=90),
                                                                                          axis.title.x = element_blank())


enPlot =  ggplot(betaS.en, aes(x=feature, y=Coefficient)) +
  geom_bar(stat = "identity", fill=ifelse(betaS.en$Coefficient>0,"steelblue","red"), colour="black")  + 
  ggtitle("Elastic-Net")+ ylim(c(L,U))+ theme(axis.text.x = element_text(size = 0,angle=90),
                                                                                              axis.title.x = element_blank())


rgPlot =  ggplot(betaS.rg, aes(x=feature, y=Coefficient)) +
  geom_bar(stat = "identity", fill=ifelse(betaS.rg$Coefficient>0,"steelblue","red"), colour="black") + 
  ggtitle("Ridge")+ ylim(c(L,U)) + theme(axis.text.x = element_text(size = rel(0),angle=90),
                                                                                         axis.title.x = element_blank())  


rfPlot =  ggplot(betaS.rf, aes(x=feature, y=PctIncMSE)) +
  geom_bar(stat = "identity", fill=ifelse(betaS.rf$PctIncMSE>3,"green","steelblue"), colour="black")  + 
  ggtitle("Random Forest")+ ylim(c(0,U_rf))+ theme(axis.text.x = element_text(size =rel(0.7),angle=90))


grid.arrange(enPlot, lsPlot, rgPlot, rfPlot, nrow = 4)

######################################
timeLas = timeLas1 - timeLas0
timeRid = timeRid1 - timeRid0
timeEln = timeEln1 - timeEln0
timeRf  = timeRf1 - timeRid0

answer_table = matrix(NA,4,3)
colnames(answer_table) = c("CI R-square Lower","CI R-square Higher","time to fit All model")
rownames(answer_table) = c("Elastic-Net","Lasso","Ridge","Random Forest")
answer_table[1,3] = timeEln["elapsed"]
answer_table[2,3] = timeLas["elapsed"]
answer_table[3,3] = timeRid["elapsed"]
answer_table[4,3] = timeRf["elapsed"]

test.Rsq.eln = sort(Rsq.en[,2])
test.Rsq.rid = sort(Rsq.rid[,2])
test.Rsq.las = sort(Rsq.las[,2])
test.Rsq.rf  = sort(Rsq.rf[,2])

ci.eln = quantile(test.Rsq.eln, probs = c(0.05,0.95))
ci.las = quantile(test.Rsq.las, probs = c(0.05,0.95))
ci.rid = quantile(test.Rsq.rid, probs = c(0.05,0.95))
ci.rf  = quantile(test.Rsq.rf, probs = c(0.05,0.95))

answer_table[1,1] = ci.eln["5%"]
answer_table[2,1] = ci.las["5%"]
answer_table[3,1] = ci.rid["5%"]
answer_table[4,1] = ci.rf["5%"]

answer_table[1,2] = ci.eln["95%"]
answer_table[2,2] = ci.las["95%"]
answer_table[3,2] = ci.rid["95%"]
answer_table[4,2] = ci.rf["95%"]
quantile(test.Rsq.las, probs = c(0.05,0.95))
ci.rid = quantile(test.Rsq.rid, probs = c(0.05,0.95))
ci.rf  = quantile(test.Rsq.rf, probs = c(0.05,0.95))

answer_table[1,1] = ci.eln["5%"]
answer_table[2,1] = ci.las["5%"]
answer_table[3,1] = ci.rid["5%"]
answer_table[4,1] = ci.rf["5%"]

answer_table[1,2] = ci.eln["95%"]
answer_table[2,2] = ci.las["95%"]
answer_table[3,2] = ci.rid["95%"]
answer_table[4,2] = ci.rf["95%"]
answer_table
