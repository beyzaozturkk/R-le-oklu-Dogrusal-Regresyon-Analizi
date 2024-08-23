
#Coklu Dogrusal Regreyon Analizi

student<-read.csv('Student_Performance_Examine.csv', header = TRUE , sep = "," , dec = ".")
View(student)
nrow(student)

student1 <- student[c("Hours.Studied","Previous.Scores","Sleep.Hours",                     
                           "Sample.Question.Papers.Practiced","Performance.Index")]

cor(na.omit(student1))

pairs(na.omit(student1) , pch = 19)

##  Kayip Gozlemler

library(mice)

## Kayip Gozlem Oruntusu

md.pattern(student1)

set.seed(145)
sample_Index <- sample(1:nrow(student1) , size = 0.8*nrow(student1))

train_Set <-student1[sample_Index , ]
test_Set <- student1[-sample_Index , ]

View(train_Set)
names(train_Set)

step(lm(Performance.Index ~ Hours.Studied + Previous.Scores + Sleep.Hours + 
          Sample.Question.Papers.Practiced, data  = train_Set))

model_std <- step(lm(Performance.Index ~ Hours.Studied + Previous.Scores + Sleep.Hours + 
               Sample.Question.Papers.Practiced, data  = train_Set))
# model1 <- lm(Humidity9am ~ . , data  = trainSet)

model_std
summary(model_std)

AIC(model_std , k = 6)
BIC(model_std)

par(mfrow = c(2, 2))

plot(model_std)

predictions<-predict(model_std,test_Set)
predictions

library(caret)

R2(predictions , test_Set$Performance.Index)
RMSE(predictions , test_Set$Performance.Index)
MAE(predictions , test_Set$Performance.Index)


# Cook's Distance(Aykiri degerler)

dist <- cooks.distance(model_std)
olcut1 <- mean(dist)*3
olcut2 <- 4 / length(dist)

olcut1Index <- which(dist > olcut1)
olcut2Index <- which(dist > olcut2)

olcut1
length(olcut1Index)
length(olcut2Index)


plot(1:length(dist) , dist , type="p" , ylim = range(dist)*c(1,0.07))
abline( h = olcut1 , col = "red")
trainSetRemoved <- train_Set[-olcut1Index , ]

step(lm(Performance.Index ~ Hours.Studied + Previous.Scores + Sleep.Hours + 
          Sample.Question.Papers.Practiced, data  = trainSetRemoved))
model_std_removed<-step(lm(Performance.Index ~ Hours.Studied + Previous.Scores + Sleep.Hours + Sample.Question.Papers.Practiced, data  = trainSetRemoved))
summary(model_std_removed)
AIC(model_std_removed,k=6)
BIC(model_std_removed)

#Coklu Baginti Problemi
library(car)
vif(model_std)

#Kategorik Verilerle Model Olusturma
student_cat <- student[ student$Extracurricular.Activities == "Yes" | 
                          student$Extracurricular.Activities == "No", ]
nrow(student_cat)
student_cat<-student_cat[c("Hours.Studied","Extracurricular.Activities","Previous.Scores","Sleep.Hours",                     
                           "Sample.Question.Papers.Practiced","Performance.Index")]
student_cat$Extracurricular.Activities<-as.character(student_cat$Extracurricular.Activities )
student_cat$Extracurricular.Activities<-as.factor(student_cat$Extracurricular.Activities )
student_cat<-na.omit(student_cat)

set.seed(145)
sample_Index_cat <- sample(1:nrow(student_cat) , size = 0.8*nrow(student_cat))

train_Set_cat <- student_cat[sample_Index_cat , ]
test_Set_cat <- student_cat[-sample_Index_cat , ]


model_Cat <- lm(Performance.Index ~ Hours.Studied + Extracurricular.Activities + Previous.Scores + Sleep.Hours + 
                  Sample.Question.Papers.Practiced , data  = train_Set_cat)
summary(model_Cat)

predictions_Cat <- predict(model_Cat , test_Set_cat)

library(caret)

R2(predictions_Cat , test_Set_cat$Performance.Index)
RMSE(predictions_Cat , test_Set_cat$Performance.Index)
MAE(predictions_Cat , test_Set_cat$Performance.Index)
