library(psych)
library(plyr)
library(VIM)
library(MASS)

telech = read.csv(file.choose(),header = T,stringsAsFactors = F)
str(telech)
attach(telech)
summary(telech)


table(PhoneService,InternetService)
table(Contract)
table(gender)
table(InternetService)
table(MultipleLines)
table(SeniorCitizen)
table(OnlineSecurity)
table(OnlineBackup)
table(DeviceProtection)
table(StreamingMovies)
table(StreamingTV)
table(TechSupport)
table(PaperlessBilling)
table(PaymentMethod)



cols_edit<- c(10:15)
for(i in 1:ncol(telech[,cols_edit])) {
  telech[,cols_edit][,i] <- as.factor(mapvalues(telech[,cols_edit][,i],  
                                  from =c("No internet service"),to=c("No")))
}

telech$MultipleLines <- as.factor(mapvalues(telech$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))



table(telech$MultipleLines)

telech$SeniorCitizen <- as.factor(mapvalues(telech$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))
telech$Churn <- as.factor(mapvalues(telech$Churn,from=c("Yes","No") , to=c("0", "1")))
table(telech$Churn)

grp_tenure = function(tenure){
  if (tenure > 0 && tenure<= 6){return('0-6 month')}
  else if (tenure > 6 && tenure<= 12){return('6-12 month')}
  else if (tenure > 12 && tenure<= 24){return('12-24 month')}
  else if (tenure > 24 && tenure<=36){return('24-36 month')}
  else if (tenure > 36 && tenure<= 48){return('36-48 month')}
  else if (tenure > 48 && tenure<= 62){return('48 - 62 month')}
  else if(tenure > 62){return('>62 month')}
  
}

telech$tenurenew <- lapply(telech$tenure,grp_tenure)
telech$tenurenew <- as.factor(as.character(telech$tenurenew))
table(telech$tenurenew)

telech$customerID <- NULL
telech$tenure <- NULL
str(telech)

pairs.panels(telech[1:10])
pairs.panels(telech[11:18],pch = 21)

#to reduce multicolinearity
telech$TotalCharges = NULL
telech$StreamingTV = NULL


#to check outliers
boxplot(MonthlyCharges)

# factorize all the categorical attributes atonce using apply method and convert back to dataframe
df_cat = data.frame(apply(telech[-c(16)], MARGIN = 2, FUN = function(x) as.factor(as.character(x))))
str(df_cat)

# bind together the categorical col and numerical colms to get the final dataset
MonthlyCharges = telech$MonthlyCharges
df = cbind(df_cat,MonthlyCharges)
str(df)
sum(is.na(df))


sp = sample(2,prob = c(0.8,0.2),replace = T,nrow(df))
tr = df[sp == 1,]
ts = df[sp == 2,]


library(caret)
lg = glm(Churn ~ ., data = tr,family = binomial(link = "logit"))
summary(lg)

lg1 = glm(Churn ~ InternetService+StreamingMovies+Contract
          +PaymentMethod+MonthlyCharges+tenurenew+TechSupport+OnlineSecurity
         +PaperlessBilling, data = tr,family = binomial(link = "logit"))
summary(lg1)
anova(lg,lg1,test = "Chisq")
anova(lg1,test = "Chisq")

pr = predict(lg1,ts,type = "response")
head(pr)

pr = ifelse(pr>0.5 ,1,0)
t = table(predicted = pr,actual = ts$Churn)
correct = mean(pr == ts$Churn)
correct
error = 1-correct
error

#goodness of fit test
with(lg1,pchisq(null.deviance - deviance,df.null - df.residual,lower.tail = F))


library(party)
tree = ctree(Churn ~ SeniorCitizen+InternetService+StreamingMovies+
               Contract+PaymentMethod+MonthlyCharges, data = tr,
             controls = ctree_control(mincriterion = 0.9,minsplit = 800))
plot(tree,type = "simple")

pred_tree = predict(tree,ts)
confusionMatrix(pred_tree,ts$Churn)


#random forest
library(randomForest)
rf1  = randomForest(Churn~.,data = tr) 
print(rf1)
prf = predict(rf1,tr)
confusionMatrix(prf,tr$Churn)
plot(rf1)

#tuning random forest model
t <- tuneRF(tr[, -18], tr[, 18], stepFactor = 0.5, plot = TRUE, 
            ntreeTry = 200, trace = TRUE, improve = 0.05)

rf  = randomForest(Churn~.,data = tr,mtry = 5,ntree = 200,importance = T,
                   proximity =T) 

print(rf)

prt = predict(rf,ts)
confusionMatrix(prt,ts$Churn)


#variable importance
varImpPlot(rf, sort=T, n.var = 10, main = 'Top 10 Feature Importance')


#accuracies of diff models
logistic:0.8092199
decisiontree:0.7844
randomForest:0.8035

