library(plyr)
library(ggplot2)
library(condvis2)
library(lme4)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gdata)
library(psych)
library(visdat)
library(naniar)
library(simputation)
library(mice)
library(nnet)
library(haven)
library(jmv)
library(foreign)
library(misty)
library(rstatix)
library(reshape2)


# WAVE 2 DATA - FOR FRAILTY INDEX

ph2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_ph.sav")
gvimp2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_gv_imputations.sav")
hc2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_hc.sav")
mh2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_mh.sav")
dv2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_dn.sav")
gvh2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_gv_health.sav")

#subset all the variables
ph4sub <- subset(ph2, select = c(mergeid, ph049d3, ph049d1, ph048d3, ph049d2, ph049d4, ph048d7, ph049d6, ph048d5, ph048d9, ph048d1, ph049d5, ph006d10, ph006d2, ph006d1, ph006d5, ph006d8, ph006d6, ph006d16, ph006d12, ph010d1, ph010d7, ph004_))
gvimp4sub <- subset(gvimp2, select = c(mergeid, orienti, maxgrip, sphus, bmi, phinact, hearing))
hc4sub <- subset(hc2, select = c(mergeid, hc012_, hc029_))
mh4sub <- subset(mh2, select = c(mergeid, mh011_))
dv4sub <- subset(dv2, select = c(mergeid, dn042_))

gvimp4sub <- unique(gvimp4sub)

#merging dataframes
merge4.1 <- merge (ph4sub, gvimp4sub, by = "mergeid")
merge4.2 <- merge (merge4.1, hc4sub, by = "mergeid")
merge4.3 <- merge (merge4.2, mh4sub, by = "mergeid")
merge4.4 <- merge (merge4.3, dv4sub, by = "mergeid")

names(merge4.4) <- c("id", "iadl1", "iadl2", "iadl3", "iadl4", "iadl5", "iadl6", "iadl7", "iadl8", "iadl9", "iadl10", "iadl11", "cancer", "hypertension", "heartattack", "diabetes", "arthritis", "lungdisease", "alzheimers", "parkinsons", "pain", "falls", "longtermillness", "orientation", "gripstrength", "srh", "bmi", "activity", "hearing", "hospitalisation", "nursinghome", "appetite", "gender")

#iadl1
merge4.4$iadl1 = as.factor(merge4.4$iadl1)
levels(merge4.4$iadl1) <- c(NA,NA,0,1)
merge4.4$iadl1 = as.numeric(merge4.4$iadl1)

#iadl2
merge4.4$iadl2 = as.factor(merge4.4$iadl2)
levels(merge4.4$iadl2) <- c(NA,NA,0,1)
merge4.4$iadl2 = as.numeric(merge4.4$iadl2)

#iadl3
merge4.4$iadl3 = as.factor(merge4.4$iadl3)
levels(merge4.4$iadl3) <- c(NA,NA,0,1)
merge4.4$iadl3 = as.numeric(merge4.4$iadl3)

#iadl4
merge4.4$iadl4 = as.factor(merge4.4$iadl4)
levels(merge4.4$iadl4) <- c(NA,NA,0,1)
merge4.4$iadl4 = as.numeric(merge4.4$iadl4)

#iadl5
merge4.4$iadl5 = as.factor(merge4.4$iadl5)
levels(merge4.4$iadl5) <- c(NA,NA,0,1)
merge4.4$iadl5 = as.numeric(merge4.4$iadl5)

#iadl6
merge4.4$iadl6 = as.factor(merge4.4$iadl6)
levels(merge4.4$iadl6) <- c(NA,NA,0,1)
merge4.4$iadl6 = as.numeric(merge4.4$iadl6)

#iadl7
merge4.4$iadl7 = as.factor(merge4.4$iadl7)
levels(merge4.4$iadl7) <- c(NA,NA,0,1)
merge4.4$iadl7 = as.numeric(merge4.4$iadl7)

#iadl8
merge4.4$iadl8 = as.factor(merge4.4$iadl8)
levels(merge4.4$iadl8) <- c(NA,NA,0,1)
merge4.4$iadl8 = as.numeric(merge4.4$iadl8)

#iadl9
merge4.4$iadl9 = as.factor(merge4.4$iadl9)
levels(merge4.4$iadl9) <- c(NA,NA,0,1)
merge4.4$iadl9 = as.numeric(merge4.4$iadl9)

#iadl10
merge4.4$iadl10 = as.factor(merge4.4$iadl10)
levels(merge4.4$iadl10) <- c(NA,NA,1,2)
merge4.4$iadl10 = as.numeric(merge4.4$iadl10)

#iadl11
merge4.4$iadl11 = as.factor(merge4.4$iadl11)
levels(merge4.4$iadl11) <- c(NA,NA,0,1)
merge4.4$iadl11 = as.numeric(merge4.4$iadl11)

#cancer 
merge4.4$cancer = as.factor(merge4.4$cancer)
levels(merge4.4$cancer) <- c(NA,NA,0,1)
merge4.4$cancer = as.numeric(merge4.4$cancer)

#hypertension 
merge4.4$hypertension = as.factor(merge4.4$hypertension)
levels(merge4.4$hypertension) <- c(NA,NA,0,1)
merge4.4$hypertension = as.numeric(merge4.4$hypertension)

#heart attack
merge4.4$heartattack = as.factor(merge4.4$heartattack)
levels(merge4.4$heartattack) <- c(NA,NA,0,1)
merge4.4$heartattack = as.numeric(merge4.4$heartattack)

#diabetes
merge4.4$diabetes = as.factor(merge4.4$diabetes)
levels(merge4.4$diabetes) <- c(NA,NA,0,1)
merge4.4$diabetes = as.numeric(merge4.4$diabetes)

#arthritis
merge4.4$arthritis = as.factor(merge4.4$arthritis)
levels(merge4.4$arthritis) <- c(NA,NA,0,1)
merge4.4$arthritis = as.numeric(merge4.4$arthritis)

#chronic lung disease
merge4.4$lungdisease = as.factor(merge4.4$lungdisease)
levels(merge4.4$lungdisease) <- c(NA,NA,0,1)
merge4.4$lungdisease = as.numeric(merge4.4$lungdisease)

#parkinsons disease - remove those with parkinsons
merge4.4 <- merge4.4[which(merge4.4$parkinsons==0),]

#pain
merge4.4$pain = as.factor(merge4.4$pain)
levels(merge4.4$pain) <- c(NA,NA,0,1)
merge4.4$pain = as.numeric(merge4.4$pain)

#falls
merge4.4$falls = as.factor(merge4.4$falls)
levels(merge4.4$falls) <- c(NA,NA,0,1)
merge4.4$falls = as.numeric(merge4.4$falls)

#longtermillness
merge4.4$longtermillness = as.factor(merge4.4$longtermillness)
levels(merge4.4$longtermillness) <- c(NA,NA,1,0)
merge4.4$longtermillness = as.numeric(merge4.4$longtermillness)

#orientation
merge4.4$orientation = as.numeric(merge4.4$orientation)
merge4.4$orientation <- cut(merge4.4$orientation, breaks=c(0,2,5), labels = c(0,1), right=FALSE)
summary(merge4.4$orientation)
merge4.4$orientation = as.factor(merge4.4$orientation)
levels(merge4.4$orientation) <- c(2,1)
merge4.4$orientation = as.numeric(merge4.4$orientation)

#gender 
merge4.4$gender = as.factor(merge4.4$gender)
levels(merge4.4$gender) <- c(1, 2)

#grip strength
merge4.4$gripstrength = as.numeric(merge4.4$gripstrength)
merge4.4$bmi = as.numeric(merge4.4$bmi)

merge4.4 <-merge4.4 %>% mutate(New_Column = case_when(
  bmi<=24 & gripstrength<=29 ~ 1,
  bmi>24 & gripstrength<=28 ~ 1,
  bmi>28 & gripstrength<= 32 ~ 1,
  bmi<=23 & gripstrength<=17 ~ 2,
  bmi>23 & gripstrength<=18 ~ 2,
  bmi>29 & gripstrength<=21 ~ 2,
  
  TRUE~bmi
))##takes in the conditions and gives the newcolumn that value. True~ bmi means it keeps the bmi values as it is. 1 for men and 2 for women. 

merge4.4%>%select(bmi, gripstrength, New_Column)

merge4.4$newgrip <- merge4.4$New_Column 
merge4.4$newgrip[merge4.4$newgrip < 2] <- 0
merge4.4$newgrip[merge4.4$newgrip > 2] <- 1

merge4.4$newgrip = as.numeric(merge4.4$newgrip)

#hospitalisation
merge4.4$hospitalisation = as.factor(merge4.4$hospitalisation)
levels(merge4.4$hospitalisation) <- c(NA, NA, 1, 0, NA)
merge4.4$hospitalisation = as.numeric(merge4.4$hospitalisation)

#hearing
merge4.4$hearing = as.factor(merge4.4$hearing)
levels(merge4.4$hearing) <- c(NA,0,0,0,1,1)
merge4.4$hearing = as.numeric(merge4.4$hearing)

#physicalactivity
merge4.4$activity = as.factor(merge4.4$activity)
levels(merge4.4$activity) <- c(NA,0,1)
merge4.4$activity = as.numeric(merge4.4$activity)

#appetite
merge4.4$appetite = as.factor(merge4.4$appetite)
levels(merge4.4$appetite) <- c(NA,NA,1,0,NA)
merge4.4$appetite = as.numeric(merge4.4$appetite)

#nursinghome
merge4.4$nursinghome = as.factor(merge4.4$nursinghome)
levels(merge4.4$nursinghome) <- c(NA,NA,1,1,0)
merge4.4$nursinghome = as.numeric(merge4.4$nursinghome)

# --------- REMOVING NOW UNWANTED COLUMNS --------- #

wave4 <- merge4.4[c(-20,-25,-27,-34)]

# --------- MISSINGNESS ---------------

sapply(wave4, function(x) sum(is.na(x)))

# --------- IMPUTATION ----------------

unimputed = wave4

init = mice(unimputed, maxit=0) 
print(init$method)
meth = init$method
predM = init$predictorMatrix

predM[, c("id")]=0 #remove id variable from the imputation, not needed to predict other values

meth[c("iadl1","iadl2","iadl3","iadl4","iadl5","iadl6","iadl7","iadl8","iadl9","iadl10", "iadl11", "cancer", "hypertension", "heartattack", "diabetes", "arthritis", "lungdisease", "alzheimers", "pain", "falls", "longtermillness", "activity", "hearing", "hospitalisation", "nursinghome", "appetite")]=""

meth[c("iadl1")]="pmm" 
meth[c("iadl2")]="pmm"
meth[c("iadl3")]="pmm"
meth[c("iadl4")]="pmm"
meth[c("iadl5")]="pmm"
meth[c("iadl6")]="pmm"
meth[c("iadl7")]="pmm"
meth[c("iadl8")]="pmm"
meth[c("iadl9")]="pmm"
meth[c("iadl10")]="pmm"
meth[c("iadl11")]="pmm"
meth[c("cancer")]="pmm"
meth[c("hypertension")]="pmm"
meth[c("diabetes")]="pmm"
meth[c("arthritis")]="pmm"
meth[c("lungdisease")]="pmm"
meth[c("alzheimers")]="pmm"
meth[c("pain")]="pmm"
meth[c("falls")]="pmm"
meth[c("longtermillness")]="pmm"
meth[c("activity")]="pmm"
meth[c("hearing")]="pmm"
meth[c("hospitalisation")]="pmm"
meth[c("nursinghome")]="pmm"
meth[c("appetite")]="pmm"

set.seed(103)
dfunimputed = mice(unimputed, method=meth, predictorMatrix=predM, m=6)

df <- complete(dfunimputed)

sapply(df, function(x) sum(is.na(x)))

#--------REMOVING NAs------------#

df <- na.omit(df)

df <- unique(df)

write.csv(df, "Desktop/Chapter 3/dataframes/df.csv", row.names = F)

# ------- COMPILING FRAILTY INDEX -------#

str(df)

df$srh = as.factor(df$srh)

df$iadl1 <- df$iadl1 - 1
df$iadl2 <- df$iadl2 - 1
df$iadl3 <- df$iadl3 - 1
df$iadl4 <- df$iadl4 - 1
df$iadl5 <- df$iadl5 - 1
df$iadl6 <- df$iadl6 - 1
df$iadl7 <- df$iadl7 - 1
df$iadl8 <- df$iadl8 - 1
df$iadl9 <- df$iadl9 - 1
df$iadl10 <- df$iadl10 - 1
df$iadl11 <- df$iadl11 - 1
df$cancer <- df$cancer - 1
df$hypertension <- df$hypertension - 1
df$heartattack <- df$heartattack - 1
df$diabetes <- df$diabetes - 1
df$arthritis <- df$arthritis - 1
df$lungdisease <- df$lungdisease - 1
df$alzheimers <- df$alzheimers - 1
df$pain <- df$pain - 1
df$falls <- df$falls - 1
df$longtermillness <- df$longtermillness - 1
df$orientation <- df$orientation - 1
df$activity <- df$activity - 1
df$hearing <- df$hearing - 1
df$hospitalisation <- df$hospitalisation - 1
df$nursinghome <- df$nursinghome - 1
df$appetite <- df$appetite - 1

#summing frailty deficits
df$frailtyx <- apply(df[,-c(1,24,30)], 1, sum, na.rm = TRUE)

#dividing by the number of frailty deficits
df$frailty <- df$frailtyx / 28

psych::describe(df$frailty)


# ADDING COVARIATES #

# --------- SUBSETTING WAVE 2 ----------

gvh2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_gv_health.sav")
dn2<- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_dn.sav")
gvimp2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_gv_imputations.sav")
gvisced <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_gv_isced.sav")
gvsp2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_sp.sav")
gvhh2 <- read.sav ("Desktop/Chapter 3/share_wave2/sharew2_rel8-0-0_hh.sav")

gv2hs <- subset(gvh2, select = c(mergeid, eurod, country))
dn2s <- subset(dn2, select = c(mergeid, dn042_))
gvimp2s <- subset(gvimp2, select = c(mergeid, age, thinc))
gvisced2 <- subset(gvisced, select = c(mergeid, isced1997_r))

mergex <- merge(gv2hs, dn2s, by = "mergeid")
mergexx <- merge(mergex, gvisced2, by = "mergeid")
merge2 <- merge(mergexx, gvimp2s, by = "mergeid")

names(merge2) <- c("mergeid", "depression2", "country", "gender","education","age", "income")

sapply(merge2, function(x) sum(is.na(x)))


# --------- SUBSETTING WAVE 4 ----------

gvh4 <- read.sav ("Desktop/Chapter 3/share_wave4/sharew4_rel7-1-0_gv_health.sav")
dn4 <- read.sav ("Desktop/Chapter 3/share_wave4/sharew4_rel7-1-0_dn.sav")
gvimp4 <- read.sav ("Desktop/Chapter 3/share_wave4/sharew4_rel7-1-0_gv_imputations.sav")
gvisced <- read.sav ("Desktop/Chapter 3/share_wave4/sharew4_rel7-1-0_gv_isced.sav")
gvsn4 <- read.sav ("Desktop/Chapter 3/share_wave4/sharew4_rel7-1-0_gv_networks.sav")

gv4hs <- subset(gvh4, select = c(mergeid, eurod))
names(gv4hs) <- c("mergeid", "depression4")
merge4 <- merge(merge2, gv4hs, by = "mergeid")

# ------ SUBETTING WAVE 5 -------- #

gvh5 <- read.sav ("Desktop/Chapter 3/share_wave5/sharew5_rel7-1-0_gv_health.sav")
merge5 <- subset(gvh5, select = c(mergeid, eurod))
names(merge5) <- c("mergeid", "depression5")

merge4 <- merge(merge4, merge5, by = "mergeid")

# ------ SUBSETTING WAVE 6 ------ #

gvh6 <- read.sav ("Desktop/Chapter 3/share_wave6/sharew6_rel7-1-0_gv_health.sav")
merge6 <- subset(gvh6, select = c(mergeid, eurod))
names(merge6) <- c("mergeid", "depression6")

merge4 <- merge(merge4, merge6, by = "mergeid")

# ------- SUBSETTING WAVE 7 -------- #

gvh7 <- read.sav ("Desktop/Chapter 3/share_wave7/sharew7_rel7-1-1_gv_health.sav")
merge7 <- subset(gvh7, select = c(mergeid, eurod))
names(merge7) <- c("mergeid", "depression7")

merge4 <- merge(merge4, merge7, by = "mergeid")

# ------ SUBSETTING WAVE 8 --------- #

gvh8 <- read.sav ("Desktop/Chapter 3/share_wave8/sharew8_rel1-0-0_gv_health.sav")
merge8 <- subset(gvh8, select = c(mergeid, eurod))
names(merge8) <- c("mergeid", "depression8")

merge4 <- merge(merge4, merge8, by = "mergeid")

merge4 <- unique(merge4)
sapply(merge4, function(x) sum(is.na(x)))

merge <- subset(merge4, select = c(mergeid, depression2, depression4, depression5, depression6, depression7, depression8, age, gender, education, income, country))



#---------- LOAD IN DATAFRAMES -------------

frailty <- read.csv("Desktop/Chapter 3/dataframes/frailty.csv")
merge4 <- read.csv("Desktop/Study 1b/dataframes/merge4.csv")

frailty <- frailty[,c(1,24,33)]

names(frailty) <- c("mergeid", "srh", "frailty")

#merge with 'merge' dataframe from covariates!!
frailty <- merge(frailty, merge, by = "mergeid")
frailty$srh <- as.numeric(frailty$srh)
frailty <- unique(frailty)

#participants 60+ years only 
frailty <- frailty[ which(frailty$age > 59),]

t1 <- frailty %>% group_by(mergeid) %>% filter (!duplicated(mergeid))
frailty <- t1


# --------- CREATING THE HEALTH ASSYMETRY VARIABLE ------------
frailty$frailty <- scale (frailty$frailty, center = TRUE, scale = TRUE) #scale frailty index
frailty$srh <- scale(frailty$srh, center = TRUE, scale = TRUE) #scale selfrated health index

frailty$frailty_minus_self <- (frailty$frailty - frailty$srh) #minus one from other
frailty$frailty_minus_self = as.numeric(frailty$frailty_minus_self) #make sure its numeric

psych::describe (frailty$frailty_minus_self) #get the SD from this 

#sd=1.12

#create Health Asymmetry categories: the consistent group are those whose self-rated health and objective health scores are within one SD of each other, health pessimistic are those whose self-rated health lies more than one SD below objective health, health optimistic is those whose self-rated health lies one SD above their objective health
frailty$HealthAsymmetry [frailty$frailty_minus_self < -1.12] <- "Health Pessimistic"
frailty$HealthAsymmetry [frailty$frailty_minus_self > -1.12 & frailty$frailty_minus_self < 1.12 ] <- "Health Realistic" 
frailty$HealthAsymmetry [frailty$frailty_minus_self > 1.12] <- "Health Optimistic"

# convert to ordered factor
frailty$HA = factor (frailty$HealthAsymmetry, order = TRUE, levels = c("Health Realistic", "Health Pessimistic", "Health Optimistic")) 

summary(frailty$HA)

frailtydep <- frailty 
write.csv(frailtydep, "Desktop/Chapter 3/dataframes/frailtydep.csv", row.names = F)


frailtydep <- frailtydep[c(-2,-3,-15,-16)]
ha <- frailtydep

str(ha)

#numeric variables
ha$age = as.numeric(ha$age)
ha$income = as.numeric(ha$income)
ha$depression2 = as.numeric(ha$depression2)
ha$depression4 = as.numeric(ha$depression4)
ha$depression5 = as.numeric(ha$depression5)
ha$depression6 = as.numeric(ha$depression6)
ha$depression7 = as.numeric(ha$depression7)
ha$depression8 = as.numeric(ha$depression8)

#categorical variables
ha$gender = as.factor(ha$gender)
ha$education = as.factor(ha$education)
ha$country = as.factor(ha$country)

#reformatting
levels(ha$gender) <- c("Male", "Female")
summary(ha$education)
levels(ha$education) <- c(NA, "None", "Primary", "Secondary", "Secondary","Tertiary", "Tertiary", "Tertiary", NA,NA)

# ------ IMPUTATION FOR GROWTH CURVE MODEL ------ #
sum(is.na(ha)) # 639
dim(ha) # 13 x 3428
# 639 / 44,564

sapply(ha, function(x) sum(is.na(x)))

unimputedha = ha

init = mice(unimputedha, maxit=0) 
print(init$method)
meth = init$method
predM = init$predictorMatrix

predM[, c("mergeid", "HA")]=0 #remove id variable and health asymmetry from the imputation, not needed to predict other values

meth[c("education", "depression2", "depression4", "depression5", "depression6", "depression7","depression8")]=""

meth[c("depression2")]="pmm"
meth[c("depression4")]="pmm"
meth[c("depression5")]="pmm"
meth[c("depression6")]="pmm"
meth[c("depression7")]="pmm"
meth[c("depression8")]="pmm"
meth[c("education")]="polyreg"

unimputedha2 = mice(unimputedha, method=meth, predictorMatrix=predM, m=6)

final <- complete(unimputedha2)


# cutting income into 4 groups
quantile(final$income, prob=c(.25,.5,.75))

final$incomecat <- cut(final$income,
                       breaks=c(0, 12000, 22000, 37000, 1000000),
                       labels=c('A', 'B', 'C', 'D'))

# melting data into long format
final$mergeid = as.factor(final$mergeid)
names(final)[2] <- "2"
names(final)[3] <- "4"
names(final)[4] <- "5"
names(final)[5] <- "6"
names(final)[6] <- "7"
names(final)[7] <- "8"

depression_long <- melt(final,
                        # ID variables - all the variables to keep but not split apart on
                        id.vars=c("mergeid", "gender", "age", "incomecat","country","education", "HA"),
                        # The source columns
                        measure.vars=c("2", "4", "5", "6", "7", "8"),
                        # Name of the destination column that will identify the original
                        # column that the measurement came from
                        variable.name="time",
                        value.name="score"
)

str(depression_long)

depression_long$time = as.numeric(depression_long$time)
depression_long$gender = as.factor(depression_long$gender)
depression_long$age = as.numeric(depression_long$age)
depression_long$score = as.numeric(depression_long$score)
depression_long$HA = as.factor(depression_long$HA)
depression_long$country = as.factor(depression_long$country)
depression_long$incomecat = as.factor(depression_long$incomecat)

dep_optimistic <- final[final$HA == "Health Optimistic",]
dep_consistent <- final[final$HA == "Health Realistic",]
dep_pessimistic <- final[final$HA == "Health Pessimistic",]


depression_long$HA = factor (depression_long$HA, order = TRUE, levels = c("Health Realistic", "Health Pessimistic", "Health Optimistic")) # convert to ordered factor. 

# ------------ KRUSKAL WALLIS RANK SUM TEST ----

kruskal.test(final$'2' ~ HA, data = final)

final %>% kruskal_effsize(final$'2' ~ HA)

pairwise.wilcox.test(final$'2', final$HA,
                     p.adjust.method = "BH")

# ----------- GROWTH CURVE MODEL ------------- 

#unconditional model with linear time term
dep1 <- lmer(score ~ time + (time||mergeid) + (time||country), depression_long)
summary(dep1)
tab_model(dep1)

#unconditional model with quadratic time term
dep2 <- lmer(score ~ time + I(time^2) + (time||mergeid) + (time||country), depression_long)
summary(dep2)
tab_model(dep2)

anova(dep1, dep2)
drop1(dep2, test = "Chisq")

#model with HA and time interactions
dep3 <- lmer(score ~ (time||mergeid) + (time||country) + time + I(time^2) + time*HA + I(time^2)*HA + HA, depression_long)
summary(dep3)
tab_model(dep3)

anova(dep3,dep2)

#full model
dep4 <- lmer(score ~ (time||mergeid) + (time||country) + time + I(time^2) + time*HA + I(time^2)*HA + HA + gender + age + education + incomecat, depression_long)
summary(dep4)
tab_model(dep4)

drop1(dep4, test = "Chisq")

anova(dep3,dep4)
