install.packages("haven")
install.packages("dplyr")
install.packages("readxl")
install.packages("labelled")
install.packages("arsenal")
install.packages("knitr")
install.packages("htmlTable")
install.packages("table1")
install.packages("lubridate")
install.packages("tidyr")
install.packages("pROC")
install.packages("PRROC")
install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")
install.packages("MASS")
install.packages("MLmetrics")
install.packages("broom")
install.packages("car")
library(broom)
library("MLmetrics")
library(MASS)
library(tidyverse)
library(caret)
library(leaps)

library(PRROC)
library(pROC)
library(tidyr)
library(lubridate)
library(table1)
library(knitr)
library(arsenal)
library(haven)
library(dplyr)
library(readxl)
library(labelled)
library(htmlTable)
library(lmtest)


setwd("~/IDfurkan")
moeder <- read_sav("DISIII Dataset .sav")
b1 <- read_sav("DIS3_blok1.sav")
b2 <- read_sav("DIS3_blok2.sav")
b3 <- read_sav("DIS3_blok3.sav")
b4 <- read_sav("DIS3_blok4.sav")
b5 <- read_sav("DIS3_blok5.sav")
b6 <- read_sav("DIS3_blok6.sav")
b7 <- read_sav("DIS3_blok7.sav")
ffq <- read_sav("FFQ.sav")
sys <- read_sav("Sysmex.sav")
pbac <- read_sav("DIS III with PBAC scores.sav")
#columns uit moederfile

set.seed(123)

columns <- c("KeyID","studienummer","Sex","Age","Q31.1","Q31.2","Ferritin","EIN","Donatiedatum","HBB","Q44","IndexDonatie","Donation5YearPriorDIS","Length","Weight","DagenLaatsteDonatie","FreqDonatie2Jaar","Hb","BMI", "Donatiesoortcode")

vader <- moeder %>% select(columns) %>%
  rename(HA_Lowfeoranemia = Q31.1, HA_Prescript = Q31.2, MenL4M = Q44)

columns <- c("KeyID", "MSK_Score")

pbac <- pbac %>% select(columns)


columns <- c("KeyID","Q36","Q37","Q38","Q39","Q40","Q41","Q42")

b6 <- b6 %>% select(columns) %>% rename(HeavyADays = Q36, HeavyAHours = Q37, ModADays = Q38, ModAHours = Q39, DaysW10Min = Q40, TotalWTime = Q41, TotalSTime= Q42)

#blood parameters#
kolums<- c("Voor_RBC104uL", "Voor_EIN","Voor_RET#102uL","Voor_MCV101fL" )
blood <- sys %>% select(kolums) %>% rename(EIN = Voor_EIN, RBC = Voor_RBC104uL,RET =  "Voor_RET#102uL", MCV = Voor_MCV101fL)
blood$RET[blood$RET==""]<-NA
blood$RET[blood$RET=="----"]<-NA
#RET =  "Voor_RET#102uL
#MCV = Voor_MCV101fL


rffq <- read_xlsx("ffqdata.xlsx", col_names= F)

rffq <- rffq %>% rename(studienummer = ...1, totaal_ijzer = ...2, haem = ...3, non_haem = ...4)  %>% filter(studienummer != 'nienke') %>% mutate(paper = grepl('p', studienummer), studienummer = gsub("p","",studienummer)) %>% mutate(duplicate = duplicated(studienummer)) %>% filter(!(duplicate == T & paper == T) ) %>% select(-duplicate, -paper)

# 2552 naar 2395 84 van ffq zit niet in vader of moeder, en 157 van vader/moeder zit niet in rffq waarvan 7 wel keyid had maar geen studienummer
#157 zonder ffq


semi <- merge(vader,rffq, by="studienummer", all.x = F, all.y = F)
semif<- merge(semi,b6, by="KeyID")
final<- merge(semif,pbac,by="KeyID")
bloodtest <- merge(final,blood, by= "EIN")

final2 <- bloodtest %>% mutate(irondef = case_when(Ferritin<15~1, Ferritin >=15 ~0)) %>% filter(!(irondef != 0 & irondef != 1)) %>% mutate(HeavyADays = replace_na(HeavyADays,0)) %>% mutate(ModADays = replace_na(ModADays,0)) %>% mutate(DaysW10Min = replace_na(DaysW10Min,0))  %>% drop_na(Sex) %>% drop_na(BMI) %>% drop_na(Hb) %>%  filter(!(Age<0)) %>% filter(!(HeavyADays == 0 & ModADays == 0 & DaysW10Min == 0))  %>% mutate(HeavyADays = replace(HeavyADays, HeavyADays == 8,0)) %>% mutate(ModADays = replace(ModADays, ModADays == 8,0)) %>% mutate(DaysW10Min = replace(DaysW10Min, DaysW10Min == 8,0))


final3 <- final2 %>% mutate(MenL4M = replace(MenL4M,(is.na(final2$MenL4M) & final2$MSK_Score>0 & final2$Sex==1),1))  %>%
mutate(MenL4M = replace(MenL4M,(is.na(final2$MenL4M) & final2$Sex==1 & final2$Age < 48),1)) %>%
mutate(MenL4M = replace(MenL4M,(is.na(final2$MenL4M) & final2$Sex==1 & final2$Age >= 48),3)) %>% 
mutate(MenL4M = replace_na(MenL4M,0)) %>% mutate(MSK_Score = replace(MSK_Score, MenL4M == 0, 0)) %>%
mutate(MSK_Score = replace(MSK_Score, (MenL4M != 1 & MenL4M != 0), 0)) %>% mutate(Pfreqdonatie = replace(final2$FreqDonatie2Jaar, final2$Donatiesoortcode == "P", 0)) %>%  mutate(HPfreqdonatie = replace(final2$FreqDonatie2Jaar, (final2$Donatiesoortcode == "P" | (final2$Donatiesoortcode == "H" & final2$FreqDonatie2Jaar>8)), 0)) %>% mutate(irondef = as.factor(irondef)) 

final4 <- final3 %>% select(-c("TotalSTime","TotalWTime", "ModAHours", "HeavyAHours","Donatiesoortcode", "Weight", "Length", "Donation5YearPriorDIS", "IndexDonatie", "HBB", "EIN", "HA_Prescript", "HA_Lowfeoranemia","studienummer", "KeyID","DagenLaatsteDonatie")) %>% mutate(highmens = case_when(MSK_Score>= 150~1, MSK_Score <150~0)) %>% mutate(MenL4M = ifelse(MenL4M == 1, 1, 0)) %>% mutate(highmens = as.factor(highmens)) %>% mutate(Sex = as.factor(Sex)) %>% mutate(MenL4M = as.factor(MenL4M)) %>% mutate(RET = as.numeric(RET)) %>% mutate(testseizoen = ifelse(month(Donatiedatum)>=4 & month(Donatiedatum)<=9,"warm","cold")) %>% select(!(highmens) & !(Pfreqdonatie) & !(FreqDonatie2Jaar) & !(totaal_ijzer) & !(Donatiedatum))  %>% drop_na(RET,HPfreqdonatie,MSK_Score)

final5 <- final4 %>% mutate(HPfreqdonatiem = replace(final4$HPfreqdonatie,(final4$HPfreqdonatie <= 2 & final4$Sex==0),"low")) %>% mutate(HPfreqdonatiem = replace(HPfreqdonatiem,(between(final4$HPfreqdonatie, 3,6) &Sex==0),"mid")) %>% mutate(HPfreqdonatiem = replace(HPfreqdonatiem,(HPfreqdonatie >6 & Sex==0),"high")) %>% mutate(HPfreqdonatiem = replace(HPfreqdonatiem, Sex== 1,NA)) %>% mutate(HPfreqdonatiem= as.factor(HPfreqdonatiem)) %>% mutate(RBCm= case_when(RBC<= 500 & Sex==0 ~'low',RBC>500 &Sex==0 ~'high')) %>% mutate(RBCm = replace(RBCm, Sex== 1,NA)) %>% mutate(RBCm= as.factor(RBCm)) 

final5$HPfreqdonatiem <- factor(final5$HPfreqdonatiem, levels = c("low","mid","high"))
final5$RBCm <- factor(final5$RBCm,levels = c("low","high"))

  
# df$cat <- cut(df$points,
               # breaks=c(70, 80, 90, 100, 110),
               # labels=c('Bad', 'OK', 'Good', 'Great'))


 



# na 2395-2382= 13 irondef NA omitted
# replace NAs for activity variables
# drop  1 NA's for sex 
# drop 42 NA's for BMI = 2339
# drop 8 NA's for Hb
# filter negative age one person 1 = 2330 observations
# als HeavyADays, ModADays en DaysW10Min = 0, removed 2 observations
# 8 en 0 gemaakt bij activiteit
# drop 5 NA's bij frequentie
 

saveRDS(final5,"alldata4.RDS")

#final4 <- readRDS("final4.RDS")

mannen <- subset(final4, final4$Sex==0)
vrouwen <- subset(final4, final4$Sex==1)
#split data into 70 training 30 test
dtm = sort(sample(nrow(mannen), nrow(mannen)*.7))
trainm<-mannen[dtm,]
testm<-mannen[-dtm,]

dtv = sort(sample(nrow(vrouwen),nrow(vrouwen)*.7))
trainv<-vrouwen[dtv,]
testv<-vrouwen[-dtv,]

modelMan <- glm(irondef~Age+Hb+FreqDonatie2Jaar + BMI + haem + non_haem + HeavyADays + ModADays + DaysW10Min,data = train, subset = train$Sex==0, family = "binomial")

bloodman <- glm(irondef~Age+Hb+HPfreqdonatie + BMI + haem + non_haem + HeavyADays + ModADays + DaysW10Min + RBC + RET + MCV + testseizoen,data = trainm, family = "binomial")
predbloodman <- predict(bloodman,testm, type = "response")

modelMan2 <- glm(irondef~Age+Hb+HPfreqdonatie + BMI + haem + non_haem + HeavyADays + ModADays + DaysW10Min,data = trainm, family = "binomial")
predictionsMan2 <- predict(modelMan2, testm,  type = "response")


#zonder dummies voor menstruatie
modelVrouw <- glm(irondef~Age+Hb+FreqDonatie2Jaar + BMI + haem + non_haem + HeavyADays + ModADays + DaysW10Min + MSK_Score ,data = train, subset = train$Sex==1, family = "binomial")

modelVrouw2<-glm(irondef~Age+Hb+HPfreqdonatie + BMI + haem + non_haem + HeavyADays + ModADays + DaysW10Min + highmens,data = trainv, family = "binomial")
predictionsWomen2 <- predict(modelVrouw2, testv, type = "response")

# met dummies voor menstruatie
nmodelVrouw <- glm(irondef~Age+Hb+FreqDonatie2Jaar + BMI + haem + non_haem + HeavyADays + ModADays + DaysW10Min + MSK_Score + highmens ,data = train, subset = train$Sex==1, family = "binomial")

nmodelVrouw2<-glm(irondef~Age+Hb+HPfreqdonatie + BMI + haem + non_haem  + HeavyADays + ModADays + DaysW10Min + (MSK_Score:MenL4M) ,data = vrouwen, family = "binomial")
predictionsnWomen2 <- predict(nmodelVrouw2,testv, type = "response")


#blood women# 

bloodwomen<-glm(irondef~Age+Hb+HPfreqdonatie + BMI + haem + non_haem  + HeavyADays + ModADays + DaysW10Min + (MSK_Score:MenL4M) + RBC + MCV + RET,data = vrouwen, family = "binomial")


bloodstepw <- stepAIC(bloodwomen, direction = "backward", 
                     trace = FALSE)

summary(bloodstepw)
pred <- predict(bloodstepw,testv, type = "response")

fgzz <- pred[testv$irondef == 1]
bgzz <- pred[testv$irondef == 0]

# ROC Curve    0.946
roc <- roc.curve(scores.class0 = fgzz, scores.class1 = bgzz, curve = T)
plot(roc)

# PR Curve = 0.464
pr <- pr.curve(scores.class0 = fgzz, scores.class1 = bgzz, curve = T)
plot(pr)







####################evaluation methods################

# create roc curve
rocmodelVrouw2 <- roc(testv$irondef, predictionsWomen2)
rocnmodelVrouw2 <- roc(testv$irondef, predictionsnWomen2)
rocmodelMan2 <- roc(testm$irondef, predictionsMan2)
# calculate area under curve
auc(rocmodelMan2)  #= 0.857
auc(rocnmodelVrouw2) #=0.882
auc(rocmodelVrouw2) #=0.880


#MEN#
require(PRROC)
fg <- predictionsMan2[testm$irondef == 1]
bg <- predictionsMan2[testm$irondef == 0]

# ROC Curve    0.913
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

# PR Curve = 0.396
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)


#MEN blood#
fgg <- predbloodman[testm$irondef == 1]
bgg <- predbloodman[testm$irondef == 0]

# ROC Curve    0.95
roc <- roc.curve(scores.class0 = fgg, scores.class1 = bgg, curve = T)
plot(roc)

# PR Curve = 0.465
pr <- pr.curve(scores.class0 = fgg, scores.class1 = bgg, curve = T)
plot(pr)



#WOMEN without dummies#
require(PRROC)
fgw <- predictionsWomen2[testv$irondef == 1]
bgw <- predictionsWomen2[testv$irondef == 0]

# ROC Curve    = 0.861
roc <- roc.curve(scores.class0 = fgw, scores.class1 = bgw, curve = T)
plot(roc)

# PR Curve = 0.394
pr <- pr.curve(scores.class0 = fgw, scores.class1 = bgw, curve = T)
plot(pr)


#WOMEN with dummies for MSK #
require(PRROC)
fgwn <- predictionsnWomen2[testv$irondef == 1]
bgwn <- predictionsnWomen2[testv$irondef == 0]

# ROC Curve    = 0.814/0.912/0.946
roc <- roc.curve(scores.class0 = fgwn, scores.class1 = bgwn, curve = T)
plot(roc)

# PR Curve = 0.472/0.394/0.464
pr <- pr.curve(scores.class0 = fgwn, scores.class1 = bgwn, curve = T)
plot(pr)







#backwards stepwise selection
step.model <- stepAIC(bloodman, direction = "backward", 
                      trace = FALSE)


summary(step.model)
pred <- predict(step.model,testm, type = "response")

fgz <- pred[testm$irondef == 1]
bgz <- pred[testm$irondef == 0]

# ROC Curve    0.883
roc <- roc.curve(scores.class0 = fgz, scores.class1 = bgz, curve = T)
plot(roc)

# PR Curve = 0.289
pr <- pr.curve(scores.class0 = fgz, scores.class1 = bgz, curve = T)
plot(pr)

predn <- pred %>% as.factor(pred) %>% (mutate(pred = case_when(pred <= 0.5~0, pred> 0.5~1)))

FN <- (testm$irondef == 1 & pred <= 0.5)
FP <- testm[testm$irondef == 0 & pred >0.5]
F1 = 2*(testm$irondef == 1)/(2*(testm$irondef == 1)+(testm$irondef == 0)+ FP + FN)


sink("bloodmanRBC.txt")
print(summary(step.model))
sink()  # returns output to the console


png("test1.png")
roc <- roc.curve(scores.class0 = fgz, scores.class1 = bgz, curve = T)
plot(roc)
dev.off()

png("prtest1.png")
pr <- pr.curve(scores.class0 = fgz, scores.class1 = bgz, curve = T)
plot(pr)
dev.off()

#hemeslow test
pred <- as.factor(pred)

F1_Score(pred,testm$irondef)

confusionMatrix(pred,testm$irondef,
                mode = "everything",
                positive="1")


#false positive/false negative etc.
predictions <- bind_cols(testm, pred) %>% rename(prediction = `...22`)
predictions$predicted_label <- 0 
predictions$predicted_label[predictions$prediction>0.8] <- 1 
table(predictions$irondef, predictions$predicted_label)




library(broom)
model1man_coef<- as.data.frame(tidy(model1man))

library("writexl")
write_xlsx(model1man_coef, "model1mancoef.xls")
