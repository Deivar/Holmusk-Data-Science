

###reading the data

bill_amount <- read.csv("bill_amount.csv")
summary(bill_amount)
head(bill_amount,3)


bill_id <- read.csv("bill_id.csv")
summary(bill_id)
head(bill_id,3)


clinical_data <- read.csv("clinical_data.csv")
summary(clinical_data)
head(clinical_data,3)

demographics <- read.csv("demographics.csv")
summary(demographics)
head(demographics)


###data cleaning

bill_amount$bill_id <- as.factor(bill_amount$bill_id)
summary(bill_amount)

bill_id$bill_id <- as.factor(bill_id$bill_id)
bill_id$date_of_admission <- as.Date(as.character(bill_id$date_of_admission))
summary(bill_id)

clinical_data$date_of_admission <- as.Date(as.character(clinical_data$date_of_admission))
clinical_data$date_of_discharge <- as.Date(as.character(clinical_data$date_of_discharge))

clinical_data$medical_history_1 <- as.factor(clinical_data$medical_history_1)
clinical_data$medical_history_2 <- as.factor(clinical_data$medical_history_2)
clinical_data$medical_history_3 <- as.factor(clinical_data$medical_history_3)
clinical_data$medical_history_4 <- as.factor(clinical_data$medical_history_4)
clinical_data$medical_history_5 <- as.factor(clinical_data$medical_history_5)
clinical_data$medical_history_6 <- as.factor(clinical_data$medical_history_6)
clinical_data$medical_history_7 <- as.factor(clinical_data$medical_history_7)
clinical_data$preop_medication_1 <- as.factor(clinical_data$preop_medication_1)
clinical_data$preop_medication_2 <- as.factor(clinical_data$preop_medication_2)
clinical_data$preop_medication_3 <- as.factor(clinical_data$preop_medication_3)
clinical_data$preop_medication_4 <- as.factor(clinical_data$preop_medication_4)
clinical_data$preop_medication_5 <- as.factor(clinical_data$preop_medication_5)
clinical_data$preop_medication_6 <- as.factor(clinical_data$preop_medication_6)
clinical_data$symptom_1 <- as.factor(clinical_data$symptom_1)
clinical_data$symptom_2 <- as.factor(clinical_data$symptom_2)
clinical_data$symptom_3 <- as.factor(clinical_data$symptom_3)
clinical_data$symptom_4 <- as.factor(clinical_data$symptom_4)
clinical_data$symptom_5 <- as.factor(clinical_data$symptom_5)

summary(clinical_data)
head(clinical_data,5)

demographics$date_of_birth <- as.Date(as.character(demographics$date_of_birth))
demographics$gender <- ifelse(demographics$gender == 'f','Female',ifelse(demographics$gender == 'm','Male',ifelse(demographics$gender == 'Female','Female','Male')))
head(demographics,3)
demographics$gender <- as.factor(demographics$gender)
summary(demographics)


###creating new variables

#period of stay
clinical_data$PeriodOfStay <- as.numeric(clinical_data$date_of_discharge - clinical_data$date_of_admission)
summary(clinical_data)
head(clinical_data)




###Basic data discovery
library("sqldf")

#number of patients
sqldf("select count(distinct patient_id) from demographics")
sqldf("select count(distinct id) from clinical_data")

#number of times a patient visited
patient_visits <- sqldf("select count(distinct date_of_admission) as visit_count, id from clinical_data group by id order by count(distinct date_of_admission) desc")
summary(patient_visits)
patient_visits

#maximum bill amount, average bill amount
summary(bill_amount)

#patient demographics
summary(demographics)
levels(demographics$race)

#period of stay average
summary(clinical_data)



###joining the tables

#using bill id
bill_complete <- sqldf("select a.amount, b.* from bill_amount a, bill_id b where a.bill_id = b.bill_id")
summary(bill_complete)
colnames(bill_complete)

subset(bill_complete_agg, bill_complete$patient_id == "4e46fddfa404b306809c350aecbf0f6a")

bill_complete_agg <- aggregate(amount ~ patient_id + date_of_admission, data = bill_complete ,sum)
summary(bill_complete_agg)

#using patient id and date of admission
patient_complete <- sqldf("select a.*, b.gender, b.race, b.resident_status, b.date_of_birth from clinical_data a, demographics b where a.id = b.patient_id")
summary(patient_complete)
colnames(patient_complete)

patient_bill_complete <- sqldf("select a.*, b.* from bill_complete_agg a, patient_complete b where a.patient_id = b.id and a.date_of_admission = b.date_of_admission")
summary(patient_bill_complete)

head(patient_bill_complete)
summary(patient_bill_complete)

#some more data cleaning
patient_bill_complete$medical_history_2 <- as.factor(patient_bill_complete$medical_history_2)
patient_bill_complete$medical_history_5 <- as.factor(patient_bill_complete$medical_history_5)
patient_bill_complete$medical_history_3[patient_bill_complete$medical_history_3 == 'Yes'] <- 1
patient_bill_complete$medical_history_3[patient_bill_complete$medical_history_3 == 'No'] <- 0
patient_bill_complete$medical_history_3 <- droplevels(patient_bill_complete$medical_history_3)

patient_bill_complete$race[patient_bill_complete$race == 'India'] <- 'Indian'
patient_bill_complete$race[patient_bill_complete$race == 'chinese'] <- 'Chinese'
patient_bill_complete$race <- droplevels(patient_bill_complete$race)

patient_bill_complete$resident_status[patient_bill_complete$resident_status=='Singapore citizen'] <- 'Singaporean'
patient_bill_complete$resident_status <- droplevels(patient_bill_complete$resident_status)


###exploratory analysis####

# Multiple plot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



library(ggplot2)
library(plyr)
head(arrange(patient_bill_complete,desc(amount)), n = 10)
summary(patient_bill_complete)

#distribution of amount
ggplot(patient_bill_complete, aes("",amount)) + geom_boxplot()

subset(patient_bill_complete, patient_id == "0eacfb2daed1f3ba2adf32e293bc05a6")

#race
ggplot(patient_bill_complete, aes(race,amount)) + geom_boxplot()
sqldf("select count(distinct patient_id),race from patient_bill_complete group by race")
sqldf("select count(distinct patient_id),race, resident_status from patient_bill_complete group by race, resident_status")

#gender
plot(patient_bill_complete$gender,patient_bill_complete$amount)

#residential status
plot(patient_bill_complete$resident_status, patient_bill_complete$amount)
ggplot(patient_bill_complete, aes(resident_status,amount)) + geom_boxplot()
sqldf("select count(distinct patient_id),resident_status from patient_bill_complete group by resident_status")


#period of stay
patient_bill_complete$StayBracket <- ifelse(patient_bill_complete$PeriodOfStay < 5, "01--05", ifelse(patient_bill_complete$PeriodOfStay < 10, "05--10", ifelse(patient_bill_complete$PeriodOfStay < 15, "10--15", "15+")))
patient_bill_complete$StayBracket <- as.factor(patient_bill_complete$StayBracket)

ggplot(patient_bill_complete,aes(StayBracket,amount)) + geom_boxplot()

sqldf("select count(distinct patient_id), avg(Weight), avg(height),StayBracket from patient_bill_complete group by StayBracket")


#age of patient
patient_bill_complete$Age <- as.numeric(format(patient_bill_complete$date_of_admission, '%Y')) - as.numeric(format(patient_bill_complete$date_of_birth, '%Y'))
summary(patient_bill_complete)

patient_bill_complete$AgeBracket <- ifelse(patient_bill_complete$Age < 30, "0--30", ifelse(patient_bill_complete$Age < 45, "30--45", ifelse(patient_bill_complete$Age < 60, "45--60", ifelse(patient_bill_complete$Age < 75, "60--75", "75+"))))
patient_bill_complete$AgeBracket <- as.factor(patient_bill_complete$AgeBracket)

p <- ggplot(patient_bill_complete,aes(AgeBracket,amount))
p+geom_boxplot()

plot(patient_bill_complete$AgeBracket, patient_bill_complete$amount)

#weight wise analysis
patient_bill_complete$WeightBracket <- ifelse(patient_bill_complete$weight < 50, "0--50", ifelse(patient_bill_complete$weight < 60, "50--60", ifelse(patient_bill_complete$weight < 70, "60--70" , ifelse(patient_bill_complete$weight < 80, "70-80", "80+"))))
patient_bill_complete$WeightBracket <- as.factor(patient_bill_complete$WeightBracket)

p <- ggplot(patient_bill_complete,aes(WeightBracket,amount))
p+geom_boxplot()

plot(patient_bill_complete$AgeBracket, patient_bill_complete$amount)

plot(patient_bill_complete$gender,patient_bill_complete$amount)

plot(patient_bill_complete$race,patient_bill_complete$amount)


#bmi variables - to standardise weight to height
patient_bill_complete$bmi <- patient_bill_complete$weight/(patient_bill_complete$height/100)^2
summary(patient_bill_complete)


patient_bill_complete$bmiBracket <- ifelse(patient_bill_complete$bmi < 20, " 0--20", ifelse(patient_bill_complete$bmi < 25, "20--25", ifelse(patient_bill_complete$bmi < 30, "25--30" , ifelse(patient_bill_complete$bmi < 35,
"30--35",  "35+"))))

patient_bill_complete$bmiBracket <- as.factor(patient_bill_complete$bmiBracket)

p <- ggplot(patient_bill_complete,aes(bmiBracket,amount))
p+geom_boxplot()


pbcnona <- na.omit(patient_bill_complete)[-2]
summary(pbc.no.na)
sqldf("select count(distinct patient_id), avg(Weight), avg(height),agebracket from pbcnona group by agebracket")


#medical history
plot(patient_bill_complete$medical_history_1, patient_bill_complete$amount)

p1 <- ggplot(patient_bill_complete,aes(medical_history_1,amount)) +geom_boxplot()
p2 <- ggplot(patient_bill_complete,aes(medical_history_2,amount)) +geom_boxplot()
p3 <- ggplot(patient_bill_complete,aes(medical_history_3,amount)) +geom_boxplot()
p4 <- ggplot(patient_bill_complete,aes(medical_history_4,amount)) +geom_boxplot()
p5 <- ggplot(patient_bill_complete,aes(medical_history_5,amount)) +geom_boxplot()
p6 <- ggplot(patient_bill_complete,aes(medical_history_6,amount)) +geom_boxplot()
p7 <- ggplot(patient_bill_complete,aes(medical_history_7,amount)) +geom_boxplot()

multiplot(p1, p2, p3, p4, p5, p6, p7,cols=2)

sqldf("select count(distinct patient_id), avg(amount), medical_history_1 from pbcnona group by medical_history_1")
sqldf("select count(distinct patient_id), avg(amount), medical_history_2 from pbcnona group by medical_history_2")
sqldf("select count(distinct patient_id), avg(amount), medical_history_3 from pbcnona group by medical_history_3")
sqldf("select count(distinct patient_id), avg(amount), medical_history_4 from pbcnona group by medical_history_4")
sqldf("select count(distinct patient_id), avg(amount), medical_history_5 from pbcnona group by medical_history_5")
sqldf("select count(distinct patient_id), avg(amount), medical_history_6 from pbcnona group by medical_history_6")
sqldf("select count(distinct patient_id), avg(amount), medical_history_7 from pbcnona group by medical_history_7")

# no of histories
patient_bill_complete$NoOfHist <- (as.numeric(as.character(patient_bill_complete$medical_history_1)) 
                                  + as.numeric(as.character(patient_bill_complete$medical_history_2)) 
                                  + as.numeric(as.character(patient_bill_complete$medical_history_3)) 
                                  + as.numeric(as.character(patient_bill_complete$medical_history_4)) 
                                  + as.numeric(as.character(patient_bill_complete$medical_history_5))
+ as.numeric(as.character(patient_bill_complete$medical_history_6))
+ as.numeric(as.character(patient_bill_complete$medical_history_7)))

summary(patient_bill_complete)

patient_bill_complete$NoOfHist <- as.factor(patient_bill_complete$NoOfHist)
ggplot(patient_bill_complete,aes(NoOfHist,amount)) +geom_boxplot()

sqldf("select count(distinct patient_id),NoOfHist from patient_bill_complete group by NoOfHist")


#symptoms!!

p1 <- ggplot(patient_bill_complete,aes(symptom_1,amount, fill = symptom_1)) +geom_boxplot()
p2 <- ggplot(patient_bill_complete,aes(symptom_2,amount,fill = symptom_2)) +geom_boxplot()
p3 <- ggplot(patient_bill_complete,aes(symptom_3,amount,fill = symptom_3)) +geom_boxplot()
p4 <- ggplot(patient_bill_complete,aes(symptom_4,amount,fill = symptom_4)) +geom_boxplot()
p5 <- ggplot(patient_bill_complete,aes(symptom_5,amount,fill = symptom_5)) +geom_boxplot()

multiplot(p1, p2, p3, p4, p5,cols=2)

sqldf("select count(distinct patient_id), avg(amount), symptom_1 from pbcnona group by symptom_1")
sqldf("select count(distinct patient_id), avg(amount), symptom_2 from pbcnona group by symptom_2")
sqldf("select count(distinct patient_id), avg(amount), symptom_3 from pbcnona group by symptom_3")
sqldf("select count(distinct patient_id), avg(amount), symptom_4 from pbcnona group by symptom_4")
sqldf("select count(distinct patient_id), avg(amount), symptom_5 from pbcnona group by symptom_5")

#no of symptoms

nosymp <- subset(patient_bill_complete, patient_bill_complete$symptom_1 == 0 & patient_bill_complete$symptom_2 == 0 & patient_bill_complete$symptom_3 == 0 & patient_bill_complete$symptom_4 == 0 & patient_bill_complete$symptom_5 == 0)
summary(nosymp)

patient_bill_complete$NoOfSymp <- (as.numeric(as.character(patient_bill_complete$symptom_1)) 
+ as.numeric(as.character(patient_bill_complete$symptom_2)) 
+ as.numeric(as.character(patient_bill_complete$symptom_3)) 
+ as.numeric(as.character(patient_bill_complete$symptom_4)) 
+ as.numeric(as.character(patient_bill_complete$symptom_5)))
             
summary(patient_bill_complete)

patient_bill_complete$NoOfSymp <- as.factor(patient_bill_complete$NoOfSymp)
ggplot(patient_bill_complete,aes(NoOfSymp,amount)) +geom_boxplot()

patient_bill_complete <- patient_bill_complete[-2]
sqldf("select count(distinct patient_id),NoOfSymp from patient_bill_complete group by NoOfSymp")

#preop medication

p1 <- ggplot(patient_bill_complete,aes(preop_medication_1,amount)) +geom_boxplot()
p2 <- ggplot(patient_bill_complete,aes(preop_medication_2,amount)) +geom_boxplot()
p3 <- ggplot(patient_bill_complete,aes(preop_medication_3,amount)) +geom_boxplot()
p4 <- ggplot(patient_bill_complete,aes(preop_medication_4,amount)) +geom_boxplot()
p5 <- ggplot(patient_bill_complete,aes(preop_medication_5,amount)) +geom_boxplot()
p6 <- ggplot(patient_bill_complete,aes(preop_medication_6,amount)) +geom_boxplot()

multiplot(p1, p2, p3, p4, p5, p6,cols=2)

sqldf("select count(distinct patient_id), avg(amount), preop_medication_1 from pbcnona group by preop_medication_1")
sqldf("select count(distinct patient_id), avg(amount), preop_medication_2 from pbcnona group by preop_medication_2")
sqldf("select count(distinct patient_id), avg(amount), preop_medication_3 from pbcnona group by preop_medication_3")
sqldf("select count(distinct patient_id), avg(amount), preop_medication_4 from pbcnona group by preop_medication_4")
sqldf("select count(distinct patient_id), avg(amount), preop_medication_5 from pbcnona group by preop_medication_5")
sqldf("select count(distinct patient_id), avg(amount), preop_medication_6 from pbcnona group by preop_medication_6")

# no of preop medication
patient_bill_complete$NoOfMed <- (as.numeric(as.character(patient_bill_complete$preop_medication_1)) 
                                   + as.numeric(as.character(patient_bill_complete$preop_medication_2)) 
                                   + as.numeric(as.character(patient_bill_complete$preop_medication_3)) 
                                   + as.numeric(as.character(patient_bill_complete$preop_medication_4)) 
                                   + as.numeric(as.character(patient_bill_complete$preop_medication_5))
                                   + as.numeric(as.character(patient_bill_complete$preop_medication_6)))

summary(patient_bill_complete)

patient_bill_complete$NoOfMed <- as.factor(patient_bill_complete$NoOfMed)
ggplot(patient_bill_complete,aes(NoOfMed,amount)) +geom_boxplot()

sqldf("select count(distinct patient_id), avg(amount), NoOfMed from patient_bill_complete group by NoOfMed")
sqldf("select  avg(NoOfMed) from patient_bill_complete")


#lab results

summary(patient_bill_complete)

patient_bill_complete$LabResultBracket1 <- ifelse(patient_bill_complete$lab_result_1 < 15, "<15",">=15")     
patient_bill_complete$LabResultBracket1 <- as.factor(patient_bill_complete$LabResultBracket1)
p1 <- ggplot(patient_bill_complete,aes(LabResultBracket1,amount)) + geom_boxplot()


patient_bill_complete$LabResultBracket2 <- ifelse(patient_bill_complete$lab_result_2 < 30, "<30",">=30")     
patient_bill_complete$LabResultBracket2 <- as.factor(patient_bill_complete$LabResultBracket2)
p2 <- ggplot(patient_bill_complete,aes(LabResultBracket2,amount)) + geom_boxplot()

patient_bill_complete$LabResultBracket3 <- ifelse(patient_bill_complete$lab_result_3 < 100, "<100",">=100")     
patient_bill_complete$LabResultBracket3 <- as.factor(patient_bill_complete$LabResultBracket3)
p3 <- ggplot(patient_bill_complete,aes(LabResultBracket3,amount)) + geom_boxplot()


multiplot(p1, p2, p3,cols=1)

#corelation test
library(Hmisc)
rcorr(as.matrix(patient_bill_complete[,c("amount","lab_result_1", "lab_result_2","lab_result_3")]))

#time of visit

patvisit <- patient_bill_complete[,c("patient_id",'date_of_admission')]
summary(patvisit$patient_id)

##### Modeling ####



### regression models - Linear regression

pbc.no.na <- na.omit(patient_bill_complete)
summary(pbc.no.na)

lm.1 <- lm(formula = amount ~ medical_history_1 + medical_history_2 + medical_history_3 + medical_history_4 +medical_history_5
            + medical_history_6 + medical_history_7 + preop_medication_1 +preop_medication_2 + preop_medication_3 +preop_medication_4
           + preop_medication_5 +preop_medication_6 + symptom_1 +symptom_2 +symptom_3 +symptom_4 +symptom_5 +weight +height
           + PeriodOfStay +gender +race +resident_status + Age, data = pbc.no.na)

lm.1 <- lm(formula = amount ~ NoOfSymp * resident_status * AgeBracket * bmiBracket * NoOfHist , data = pbc.no.na)
#+ lab_result_1 + lab_result_2 + lab_result_3
summary(lm.1)
abline(lm.1, col="red")

library("MASS")

# backward selection
step.b <- stepAIC(lm.1, direction="backward")
step.b$anova # display results

summary(step.b)
plot(step.b)


lm.0 <- lm(formula =  amount ~ Age + gender + race + PeriodOfStay + weight,  data = pbc.no.na)
summary(lm.0)



### The elastic net 


#chi squared test

summary(final_factors)
final_factors <- pbc.no.na[, c("NoOfSymp" , "NoOfHist")]
chisq.test(final_factors)


library(glmnet)

pbc.no.naTrain <- subset(pbc.no.na, date_of_admission < '2015-01-01')
summary(pbc.no.naTrain)

pbc.no.naTest <- subset(pbc.no.na, date_of_admission >= '2015-01-01')
summary(pbc.no.naTest)

# construct x and y matrix for glmnet()
x <- model.matrix(amount ~ NoOfSymp + resident_status + AgeBracket + bmiBracket + NoOfHist + symptom_1 + symptom_2
                  +symptom_3 + symptom_4 + symptom_5, pbc.no.naTrain)

#lab_result_1 + lab_result_2 + lab_result_3
head(x)
y <- pbc.no.naTrain$amount
head(y)

x.test <- model.matrix(amount ~ NoOfSymp + resident_status + AgeBracket + bmiBracket + NoOfHist + symptom_1 + symptom_2
                       +symptom_3 + symptom_4 + symptom_5 , pbc.no.naTest)
y.test <- pbc.no.naTest$amount
#y.test <- as.data.frame(y.test)


# glmnet with alpha=1 means LASSO, 0.5 is elastic net
lasso.mod <- glmnet(x, y, alpha=0.5)
plot(lasso.mod, xvar="lambda", label=TRUE)
summary(lasso.mod)

# CV for optimal lambda
set.seed(1)
lasso.cv <- cv.glmnet(x, y, alpha=1)
plot(lasso.cv)

# optimal lambda
lasso.lam <- lasso.cv$lambda.min
log(lasso.lam)
points(log(lasso.lam), min(lasso.cv$cvm), cex=3)

#alternatively, set optimal lambda to lambda.1se for a more parsimonious model
lasso.lam2 <- lasso.cv$lambda.1se
log(lasso.lam2)
min(lasso.cv$cvm) + lasso.cv$cvsd[which.min(lasso.cv$cvm)]
points(log(lasso.lam2), min(lasso.cv$cvm) + lasso.cv$cvsd[which.min(lasso.cv$cvm)], cex=3)

# plot optimal lambda
plot(lasso.mod, xvar="dev", label = TRUE)
abline(v=log(lasso.lam), lty=2)
abline(v=log(lasso.lam2), lty=2)


# final model
lasso2 <- predict(lasso.mod, type="coefficient", s=lasso.lam2, exact=TRUE)
summary(lasso2)
lasso2

# prediciton using optimal lambda
y.pred <- predict(lasso.mod, s=lasso.lam2, newx=x.test, exact=TRUE)
colnames(y.pred)[1] <- "predicted"
y.pred <- as.data.frame(y.pred)

y.pred$actual <- pbc.no.naTest$amount

tail(y.pred)
y.pred$percentdiff <- ((y.pred$actual - y.pred$predicted)/y.pred$actual)*100
mean(abs(y.pred$percentdiff))
#rmse
sqrt(mean((y.pred$actual - y.pred$predicted)^2))
#mape
mean(y.pred$actual - y.pred$predicted)

