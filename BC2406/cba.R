library("data.table")
library("ggplot2")
library("tidyverse")
library("ggpubr")
library("caTools")
library("rpart")
library("rpart.plot")
library("mice")
library("caret")
library("car")

setwd("/Users/geraldho/Library/CloudStorage/OneDrive-NanyangTechnologicalUniversity/NTU/BC2406/AY22 BC2406 CBA")

#Q1 (a)
home.dt <- fread("homeloan2.csv", na.strings = c("", "NA"))

str(home.dt)



#we proceed to factor the categorical variables, and change data 
#type of dependents to integer
home.dt$Gender=as.factor(home.dt$Gender)
home.dt$Married=as.factor(home.dt$Married)
home.dt$Education=as.factor(home.dt$Education)
home.dt$Self_Employed=as.factor(home.dt$Self_Employed)
home.dt$Property_Area=as.factor(home.dt$Property_Area)
home.dt$Loan_Status=as.factor(home.dt$Loan_Status)
home.dt[home.dt=="3+"]="3"
home.dt$Dependents=as.factor(home.dt$Dependents)
home.dt$Credit_Score = as.factor(home.dt$Credit_Score)
home.dt$Loan_Amount_Term = as.factor(home.dt$Loan_Amount_Term)

str(home.dt)
#we check the levels of the dataframe
sapply(home.dt, levels)

#we need to relevel $Education
home.dt$Education <- relevel(home.dt$Education, ref = "Not Graduate")

#Q1 (b)
#checking missing values
sapply(home.dt, function(x) sum(is.na(x)))

#checking for the mode for categorical variables
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

apply(home.dt, 2, Mode)

#replacing all missing values
home.dt$Gender[is.na(home.dt$Gender)]="Male"
home.dt$Married[is.na(home.dt$Married)]="Yes"
home.dt$Dependents[is.na(home.dt$Dependents)]="0"
home.dt$Self_Employed[is.na(home.dt$Self_Employed)]="No"
home.dt$Loan_Amount_Term[is.na(home.dt$Loan_Amount_Term)]="360"
home.dt = home.dt[,-1]

#mice to impute
home.dt <- complete(mice(home.dt, method = "pmm", seed = 8))

#Q2
#plot categorical variables against loan_status
categoricalvariables<-names(keep(home.dt,is.factor))
homeplots<-list()
for(x in 1:(length(categoricalvariables)-1)){
  homeplots[x]<-list(ggplot(data=home.dt,aes_string(categoricalvariables[x],fill='Loan_Status'))+
                    geom_bar(position = 'dodge')+
                    scale_fill_brewer(palette = 'Paired'))
}
ggarrange(plotlist=homeplots,ncol = 2, nrow = 4)

#plot categorical variables against loan_status for loan approval rate
homeplots2<-list()
for(x in 1:(length(categoricalvariables)-1)){
  homeplots2[x]<-list(ggplot(data=home.dt,aes_string(categoricalvariables[x],fill='Loan_Status'))+
                       geom_bar(position = 'fill')+
                       scale_fill_brewer(palette = 'Pastel1'))
}
ggarrange(plotlist=homeplots2,ncol = 2, nrow = 4)

#plot continuous variables against loan_status
numintvariables<-names(keep(home.dt,is.numeric))
homeplots3<-list()
for(x in 1:length(numintvariables)){
  homeplots3[x]<-list(ggplot(data=home.dt,aes_string(x='Loan_Status',y=numintvariables[x],color='Loan_Status'))+
                     geom_boxplot() )
}
ggarrange(plotlist=homeplots3,ncol = 3, nrow = 1)

#density plot
homeplots4<-list()
for(x in 1:length(numintvariables)){
  homeplots4[x]<-list(ggplot(data=home.dt,aes_string(x=numintvariables[x],fill='Loan_Status'))+
                        geom_density(alpha = 0.5) + scale_fill_brewer(palette = "Accent"))
}
ggarrange(plotlist=homeplots4,ncol = 1, nrow = 3)

#Q3 models
#we first split the data 7:3 for model training and testing
set.seed(8)
sample = sample.split(home.dt$Loan_Status, SplitRatio = 0.7)
train <- subset(home.dt, sample == T)
test <- subset(home.dt, sample == F)

#logistic regression
m1 <- glm(Loan_Status ~., family = binomial, data = train)
summary(m1)

m2 <- step(m1, direction = "backward")
summary(m2)

#check for multicolinearity
vif(m2)

#confusion matrix 
pred1 <- as.factor(predict(m2, newdata=test, type="response") >= 0.5) %>% 
  fct_recode("N" = "FALSE", "Y" = "TRUE")

confusionMatrix(pred1, test$Loan_Status, positive = "Y")

#CART models
set.seed(8)
m3 <- rpart(Loan_Status ~., data = train, method = "class", 
            control = rpart.control(minsplit = 2, cp = 0))
printcp(m3)
plotcp(m3)

#finding optimal cp value to prune
CVerror.cap <- m3$cptable[which.min(m3$cptable[,"xerror"]), "xerror"] + m3$cptable[which.min(m3$cptable[,"xerror"]), "xstd"]

i <- 1; j<- 4
while (m3$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

cp.opt = ifelse(i > 1, sqrt(m3$cptable[i,1] * m3$cptable[i-1,1]), 1)

#prune and plot optimal tree
m4 <- prune(m3, cp = cp.opt)
rpart.plot(m4, nn = T)

#confusionmatrix
pred2 <- as.factor(predict(m4, newdata=test)[, 2] >= 0.5) %>%
  fct_recode("N" = "FALSE", "Y" = "TRUE")

confusionMatrix(pred2, test$Loan_Status, positive = "Y")

#Q5
ggplot(data=home.dt,aes(x = Gender, fill = Credit_Score))+
  geom_bar(position = 'fill')+
  scale_fill_brewer(palette = 'Pastel1')

chisq.test(home.dt$Gender, home.dt$Loan_Status)
