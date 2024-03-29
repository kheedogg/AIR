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
# Marital_Status : 결혼 여부 (0 : X , 1 : O)
# Product_Category_1 (대분류)
# Product_Category_2 (중분류)
# Product_Category_3 (소분류)

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
data <- read.csv('C:\\Users\\kheed\\Desktop\\동아리 AIR\\2019_1\\Third Analysis\\BlackFriday\\BlackFriday.csv')
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


# 상품 5개 구입한 고객의 User_ID 번호 찾기
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
# 변수 형태 지정
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
NAnum  # 10과 11열에 결측치 존재 확인
       # Product_Category_2&3 -> 10열 11열

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

P1# 성별에 따른 구입액
P2# 성별과 거주도시에 따른 구입액
P3# 성별과 결혼상태에 따른 구입액
P4# 성별에 따라 각 나이범주별 사람 수
P5# 직업별 각 범주에 속하는 사람 수





#################### EDA_newdata

plot(newdata)   # Purchase가 Gender와 City_Category와 Product_num의 영향을 많이 받고 있는 것을 시각적으로 확인할 수 있다.
glimpse(newdata)
summary(newdata)

########## is.na
for ( i in 1:8) {
  print( sum(!complete.cases(newdata[,i])))
}         # 결측치 없는 것 확인


# Gender
table(newdata$Gender)
boxplot(Purchase~Gender,data=newdata,ylim=c(0,4e+06))
#남자가 약간 더 많이 소비
t.test(Purchase~Gender,data=newdata)
#남녀간 구입액에 차이가 있다라고 해석가능.
#따라서, 여자보다 남자가 더 많이 소비한다라고 말할 수 있다.

library(ggplot2)
newdata %>%
  ggplot(aes(x=Gender,y=Purchase,fill=Gender)) +
  geom_boxplot()
#성별간 구입액 차이 있음==>남자가 더 많음


# Age
table(newdata$Age)

newdata %>%
  ggplot(aes(x=Age, y=Purchase,fill=Gender))+ylim(0,2.5e+06) + geom_boxplot()
#성별 범주 간 여자보다 남자가 상대적으로 더 많이 소비한 것을 알 수 있다.
newdata %>%
  ggplot(aes(x=Age, y=Purchase, fill=Age)) +ylim(0,2.5e+06)+ geom_boxplot()
#26-35의 범주에서 가장 많이 소비한 것을 알 수 있다.


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
#직업 4,0,7,1,17,12 순으로 직업을 많이 갖고 있는 것을 알 수 있다.
length(unique(newdata$Occupation))
mbyo<-rep(0,21)
for( i in 1:21 ) {
  which<-which(newdata$Occupation==i-1)
  mbyo[i]<-mean(newdata$Purchase[which])  
}
sortmbyo<-sort(mbyo,decreasing = T)
sortmbyo
which(mbyo==sortmbyo[1])  # 20번직업이 구입 가장 많이 구입함
which(mbyo==sortmbyo[2])  # 19번직업이 구입 가장 많이 구입함
which(mbyo==sortmbyo[3])  # 5번직업이 구입 가장 많이 구입함


# City_Category
table(newdata$City_Category)

newdata %>%
  ggplot(aes(x=City_Category,y=Purchase,fill=City_Category))+geom_boxplot()
#B, A, C 순으로 도시간 구입을 많이 하는 것을 알 수 있다.
#눈으로 봐도 도시간 구입액에 차이가 있음을 짐작할 수 있다.
#세 집단간 평균 차이 겁정은 ANOVA 귀무가설:모든집단간 평균은 동일하다. 로 검정
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
#잔차 정규성 만족족


# Stay_In_Current_City_Years
sort(table(newdata$Stay_In_Current_City_Years),decreasing=T)

newdata %>%
  ggplot(aes(x=Stay_In_Current_City_Years, y=Purchase, fill=Stay_In_Current_City_Years)) +
  geom_col()


# Martial_Status
table(newdata$Martial_Status)

newdata %>%
  ggplot(aes(x=Martial_Status,y=Purchase, fill=Martial_Status)) + ylim(0,2.5e+06) + geom_boxplot()
#결혼여부간 지출액 차이가 없어 보인다.
t.test(Purchase~Martial_Status, data=newdata)
#결혼여부간 지출액 차이가 없다고 할 수 있다.


newdata %>%
  ggplot(aes(x=Martial_Status, y=Purchase, fill=Gender)) + geom_boxplot() + ylim(0,2.5e+06)
#결혼여부간 구입액 차이는 없으나 결혼여부간에서도 여자보다 남자가 지출이 더 많은 것을 마찬가지로 알 수 있다.



# Product_num
sort(table(newdata$Product_num), decreasing=T)[1:50]

newdata %>%
  ggplot(aes(x=Product_num, y=Purchase))+ geom_point()

newdata %>%
  ggplot(aes(x=Product_num, y=Purchase, fill=Gender, color=Gender)) + geom_point()
#파란색점이 빨간색보다 상대적으로 위쪽에 배치되어 있으며,
#직관적으로 알 수 있듯이, 상품의 개수와 구입액이 강한 양의 상관관계가 있는 것을 알 수 있다.



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

# 변수 중요도 확인
varImpPlot(rf_mod)
varImpPlot(rf_mod2)

## 이분할적 데이터가 아니므로 confusion matri만들 수 없다. 
## 또한 ROC Curve도 볼 수 없다.
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
#Product_num변수가 가장 중요한 변수로 나온다.
plot(gbm_mod,i='Product_num') #그래프로 보아 이 변수는 종속변수에대해 강한 양의 상관관계를 지닌다.

#최적의 나무 갯수 확인하기
n.trees <- seq(from=100 ,to=1000, by=50) 
predmatrix <- predict(gbm_mod,test_newdata,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#MSE 계산하기
test.error<-with(test_newdata,apply( (predmatrix-Purchase)^2,2,mean))
head(sqrt(test.error)) #RMSE계산하기.

#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
abline(h = min(test.error),col="red") #에러가 가장작은 부분 보기.
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)
min(test.error)
#나무의 갯수가 350일때 가장 적은 에러를 가진다고 볼 수 있다.
y_hat_gbm <- predict(gbm_mod,test_newdata,n.trees=350)
GBM <- rmse(test_newdata$Purchase,y_hat_gbm)






##########모델별 RMSE 비교

LM; RF; GBM










