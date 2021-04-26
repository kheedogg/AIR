# Dataset of 550 000 observations about the black Friday in a retail store, 
# it contains different kinds of variables either numerical or categorical.
# It contains missing values. 


#################### Variables Explanation
# User_ID : User
# Product_ID : Id Product
# Gender : Boolean 
# Age : Age customer
# Occupation : Id Occupation of each customer
# City_Category
# Stay_In_Current_City_Years
# Marital_Status : ��ȥ ���� (0 : X , 1 : O)
# Product_Category_1 (��з�)
# Product_Category_2 (�ߺз�)
# Product_Category_3 (�Һз�)

# Purchase : Purchase amount in dollars <=== "Dependent Variable" :)





########## Install Packages
install.packages('dplyr')
install.packages('ggplot2')
install.packages('MASS')
install.packages('caret')
install.packages('hydroGOF')
install.packages('randomForest')
install.packages('corrplot')


### The Purpose of 'BlackFriday' data Analysis : Prediction of Purchasing amount on BlackFriday, 29th, November
data <- read.csv('C:\\Users\\kheed\\Desktop\\���Ƹ� AIR\\2019_1\\Third Analysis\\BlackFriday\\BlackFriday.csv')
head(data)
str(data)
colnames(data)
dim(data)
library(dplyr)
glimpse(data)
summary(data)


######### Processing newdata
max(data$User_ID)
length(unique(data$User_ID))
head(data$User_ID)
firstindex<-rep(0,6040)
num<-rep(0,6040)
newdata<-matrix(NA,6040,8)
colnames(newdata)<-c('Gender','Age','Occupation','City_Category','Stay_In_Current_City_Years','Martial_Status','Product_num','Purchase')
head(newdata)
for( i in 1000001 : 1006040 ) {
  which<-which(data$User_ID==i)
  i<-substr(i,4,7)
  i<-as.numeric(i)
  firstindex[i]<-which[1]
  num[i]<-length(which)
  for( n in 1:6 ) {
  newdata[i,n]<-data[firstindex[i],n+2]
  }
  newdata[i,7]<-num[i]
  newdata[i,8]<-sum(data$Purchase[which])
  }
head(newdata)


# ��ǰ 5�� ������ ������ User_ID ��ȣ ã��
s<-NA
for( i in 1000001:1006040 ) {
  which<-which(data$User_ID==i)
  if(length(which)==5){s<-i}
}
s


length(which(is.na(newdata[,1])))
na.index<-which(is.na(newdata[,1]))
newdata <- newdata[-na.index,]
sum(!complete.cases(newdata))
dim(newdata)
summary(newdata)
newdata[,1] <- ifelse(newdata[,1]==1, 'F',ifelse(newdata[,1]==2,'M',NA))
newdata[,2] <- ifelse(newdata[,2]==1,'0-17',ifelse(newdata[,2]==7,'55+',ifelse(newdata[,2]==3,'26-35',
                                            ifelse(newdata[,2]==5,'46-50',ifelse(newdata[,2]==6,'51-55',
                                            ifelse(newdata[,2]==4,'36-45',ifelse(newdata[,2]==2,'18-25',NA)))))))
newdata[,4] <- ifelse(newdata[,4]==1,'A',ifelse(newdata[,4]==3,'C',ifelse(newdata[,4]==2,'B',NA)))

is.matrix(newdata)
newdata$Gender
newdata<-as.data.frame(newdata)

str(newdata)
# ���� ���� ����
newdata$Stay_In_Current_City_Years <- as.integer(as.character(newdata$Stay_In_Current_City_Years))
newdata$Product_num <- as.integer(as.character(newdata$Product_num))
newdata$Purchase <- as.integer(as.character(newdata$Purchase))
glimpse(newdata)






#################### EDA_data

# user_ID
sort( table(data$User_ID)[1:50] , decreasing=T )[1]
length( unique(data[,1]) )
data$User_ID <- as.integer(data$User_ID)
str(data)

# Product_ID

# Gender

# Age
table(data$Age)

# Occupation
table(data$Occupation)
data$Occupation <- as.factor(data$Occupation)
str(data)

# City_Category
table(data$City_Category)

# Stay_In_Current_City_Years
table(data$Stay_In_Current_City_Years)

# Marital_Status
table(data$Marital_Status)
data$Marital_Status <- as.factor(data$Marital_Status)
str(data)

# Product_Category_1&2&3
table(data$Product_Category_1)
table(data$Product_Category_2)
table(data$Product_Category_3)
for( i in 9:11 ) {
  data[,i] <- as.factor(data[,i])
}
str(data)



# is.NA
dim(data)
NAnum<-rep(0,12)
for( i in 1:12 ) { 
  NAnum[i]<-sum(is.na(data[,i]))
  }
NAnum  # 10�� 11���� ����ġ ���� Ȯ��
       # Product_Category_2&3 -> 10�� 11��

Missingnum <- rep(0,12)
for ( i in 1:12 ) {
  Missingnum[i]<-sum(!complete.cases((data[,i])))

}
Missingnum



library(ggplot2)
product_ID_20 <-
data %>%
  group_by(Product_ID) %>%
  count() %>%
  arrange(desc(n))
product_ID_20[1:20,] %>%
  ggplot(aes(x=Product_ID, y=n, fill=Product_ID)) +
  geom_col()+theme(axis.text.x=element_text(angle=45,vjust=.5))


str(data)
data %>%
  ggplot(aes(x=Purchase))+geom_histogram(fill='brown')




P1<- data %>%
  ggplot(aes(x=Gender, y=Purchase, fill=Gender))+geom_boxplot()
P2 <- data %>%
  ggplot(aes(x=City_Category, y=Purchase, fill=Gender))+geom_boxplot()
P3 <- data %>%
  ggplot(aes(x=Marital_Status, y=Purchase, fill=Gender))+geom_boxplot()
P4 <- data %>%
  ggplot(aes(x=Age, fill=Gender))+geom_bar(position='dodge')
P5 <- data %>%
  ggplot(aes(Occupation, fill=Age))+geom_bar()

P1# ������ ���� ���Ծ�
P2# ������ ���ֵ��ÿ� ���� ���Ծ�
P3# ������ ��ȥ���¿� ���� ���Ծ�
P4# ������ ���� �� ���̹��ֺ� ��� ��
P5# ������ �� ���ֿ� ���ϴ� ��� ��





#################### EDA_newdata

plot(newdata)   # Purchase�� Gender�� City_Category�� Product_num�� ������ ���� �ް� �ִ� ���� �ð������� Ȯ���� �� �ִ�.
glimpse(newdata)
summary(newdata)

########## is.na
for ( i in 1:8) {
  print( sum(!complete.cases(newdata[,i])))
}         # ����ġ ���� �� Ȯ��


# Gender
table(newdata$Gender)
boxplot(Purchase~Gender,data=newdata,ylim=c(0,4e+06))
#���ڰ� �ణ �� ���� �Һ�
t.test(Purchase~Gender,data=newdata)
#���ణ ���Ծ׿� ���̰� �ִٶ�� �ؼ�����.
#����, ���ں��� ���ڰ� �� ���� �Һ��Ѵٶ�� ���� �� �ִ�.

library(ggplot2)
newdata %>%
  ggplot(aes(x=Gender,y=Purchase,fill=Gender)) +
  geom_boxplot()
#������ ���Ծ� ���� ����==>���ڰ� �� ����


# Age
table(newdata$Age)

newdata %>%
  ggplot(aes(x=Age, y=Purchase,fill=Gender))+ylim(0,2.5e+06) + geom_boxplot()
#���� ���� �� ���ں��� ���ڰ� ��������� �� ���� �Һ��� ���� �� �� �ִ�.
newdata %>%
  ggplot(aes(x=Age, y=Purchase, fill=Age)) +ylim(0,2.5e+06)+ geom_boxplot()
#26-35�� ���ֿ��� ���� ���� �Һ��� ���� �� �� �ִ�.


# Occupation
table(newdata$Occupation)

occupation.purchase<-newdata %>%
  ggplot(aes(x=Occupation,y=Purchase,fill=Occupation))+ylim(0,2.5e+06)+geom_boxplot()
occupation.purchase
Occupation <- newdata %>%
              group_by(Occupation) %>%
              count() %>%
              arrange(desc(n))
Occupation[1:6,] %>%
  ggplot(aes(x=Occupation,y=n,fill=Occupation)) + geom_col()
#���� 4,0,7,1,17,12 ������ ������ ���� ���� �ִ� ���� �� �� �ִ�.
length(unique(newdata$Occupation))
mbyo<-rep(0,21)
for( i in 1:21 ) {
  which<-which(newdata$Occupation==i-1)
  mbyo[i]<-mean(newdata$Purchase[which])  
}
sortmbyo<-sort(mbyo,decreasing = T)
sortmbyo
which(mbyo==sortmbyo[1])  # 20�������� ���� ���� ���� ������
which(mbyo==sortmbyo[2])  # 19�������� ���� ���� ���� ������
which(mbyo==sortmbyo[3])  # 5�������� ���� ���� ���� ������


# City_Category
table(newdata$City_Category)

newdata %>%
  ggplot(aes(x=City_Category,y=Purchase,fill=City_Category))+geom_boxplot()
#B, A, C ������ ���ð� ������ ���� �ϴ� ���� �� �� �ִ�.
#������ ���� ���ð� ���Ծ׿� ���̰� ������ ������ �� �ִ�.
#�� ���ܰ� ��� ���� ������ ANOVA �͹�����:������ܰ� ����� �����ϴ�. �� ����
lm<-lm(Purchase~City_Category,data=newdata)
summary(lm)
anova<-anova(lm)
anova
summary(anova)
aov<-aov(Purchase~City_Category,data=newdata)
summary(aov)
res<-residuals(object=aov)
n<-length(res)
shapiro.test(res[sample(1:n,5000)])
#���� ���Լ� ������


# Stay_In_Current_City_Years
sort(table(newdata$Stay_In_Current_City_Years),decreasing=T)

newdata %>%
  ggplot(aes(x=Stay_In_Current_City_Years, y=Purchase, fill=Stay_In_Current_City_Years)) +
  geom_col()


# Martial_Status
table(newdata$Martial_Status)

newdata %>%
  ggplot(aes(x=Martial_Status,y=Purchase, fill=Martial_Status)) + ylim(0,2.5e+06) + geom_boxplot()
#��ȥ���ΰ� ����� ���̰� ���� ���δ�.
t.test(Purchase~Martial_Status, data=newdata)
#��ȥ���ΰ� ����� ���̰� ���ٰ� �� �� �ִ�.


newdata %>%
  ggplot(aes(x=Martial_Status, y=Purchase, fill=Gender)) + geom_boxplot() + ylim(0,2.5e+06)
#��ȥ���ΰ� ���Ծ� ���̴� ������ ��ȥ���ΰ������� ���ں��� ���ڰ� ������ �� ���� ���� ���������� �� �� �ִ�.



# Product_num
sort(table(newdata$Product_num), decreasing=T)[1:50]

newdata %>%
  ggplot(aes(x=Product_num, y=Purchase))+ geom_point()

newdata %>%
  ggplot(aes(x=Product_num, y=Purchase, fill=Gender, color=Gender)) + geom_point()
#�Ķ������� ���������� ��������� ���ʿ� ��ġ�Ǿ� ������,
#���������� �� �� �ֵ���, ��ǰ�� ������ ���Ծ��� ���� ���� ������谡 �ִ� ���� �� �� �ִ�.



# Purchase
sort(table(newdata$Purchase),decreasing=T)[1:50]
newdata %>%
  ggplot(aes(y=Purchase,fill=))+geom_boxplot()
summary(newdata$Purchase)


#################### Analysis


library(caret)
train_idx <- createDataPartition(newdata$Gender, times = 1, p=.7,list=FALSE)
train_newdata <- newdata[train_idx,]
test_newdata <- newdata[-train_idx,]

prop.table(table(newdata$Gender))
prop.table(table(train_newdata$Gender))
prop.table(table(test_newdata$Gender))

#################### Lm

lm1<-lm(Purchase~.,data=train_newdata)
summary(lm1)

library(MASS)
lm_step1<-stepAIC(lm1, direction='both')
lm_step2<-step(lm1, direction='both')
summary(lm_step1); summary(lm_step2)


pred_lm <- predict(lm_step1, test_newdata)
library(hydroGOF)
LM<-rmse(test_newdata$Purchase, pred_lm)
summary(newdata$Purchase)
sd(newdata$Purchase)


#################### RandomForest

library(randomForest)
dim(newdata)
rf_mod <- randomForest(Purchase ~., data = train_newdata, importance=T)
plot(rf_mod)

rf_mod2<-randomForest(Purchase~., data=train_newdata, ntree=100, importance=T)
rf_mod2
summary(rf_mod2)
plot(rf_mod2)

# ���� �߿䵵 Ȯ��
varImpPlot(rf_mod)
varImpPlot(rf_mod2)

## �̺����� �����Ͱ� �ƴϹǷ� confusion matri���� �� ����. 
## ���� ROC Curve�� �� �� ����.
pred_rf <-predict(rf_mod2, test_newdata)
RF<-rmse(test_newdata$Purchase, pred_rf)


#################### GBM

install.packages('gbm')
install.packages('xgboost')
install.packages('h2o')
install.packages('caret')
install.packages('pdp')
install.packages('lime')

library(gbm)
gbm_mod <- gbm(Purchase~., train_newdata, distribution = 'gaussian', n.trees=1000, shrinkage=0.01, interaction.depth=4 )
summary(gbm_mod)
#Product_num������ ���� �߿��� ������ ���´�.
plot(gbm_mod,i='Product_num') #�׷����� ���� �� ������ ���Ӻ��������� ���� ���� ������踦 ���Ѵ�.

#������ ���� ���� Ȯ���ϱ�
n.trees <- seq(from=100 ,to=1000, by=50) 
predmatrix <- predict(gbm_mod,test_newdata,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#MSE ����ϱ�
test.error<-with(test_newdata,apply( (predmatrix-Purchase)^2,2,mean))
head(sqrt(test.error)) #RMSE����ϱ�.

#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
abline(h = min(test.error),col="red") #������ �������� �κ� ����.
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)
min(test.error)
#������ ������ 350�϶� ���� ���� ������ �����ٰ� �� �� �ִ�.
y_hat_gbm <- predict(gbm_mod,test_newdata,n.trees=350)
GBM <- rmse(test_newdata$Purchase,y_hat_gbm)






##########�𵨺� RMSE ��

LM; RF; GBM









