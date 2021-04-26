library(ggplot2)
library(dplyr)
library(data.table)
library(knitr)
setwd('C:\\Users\\kheed\\Desktop\\동아리 AIR\\2019_1\\Second Analysis')
data <- read.csv('ramen-ratings.csv',header=T)
################변수설명###################
#Review : 상품에 달린 리뷰의 갯수
#Brand : 상품의 브랜드회사
#Variety : 상품의 특징
#Style : 상품의 형태(팩,컵,접시,그릇)
#Country : 상품생산된 나라
#Stars : 상품의 평점
#Top.Ten : 년도별 탑텐 선정.

View(data)

colnames(data)
colnames(data)[1] <- c('Review')
colnames(data)

###################EDA#####################
summary(data)
glimpse(data)
str(data)


# 변수 형태 지정
data$Stars <- as.numeric(as.character(data$Stars))


#NA
sum(is.na(data[,1]))
sum(is.na(data[,2]))
sum(is.na(data[,3]))
sum(is.na(data[,4]))
sum(is.na(data[,5]))
sum(is.na(data[,6]))  #3개의 NA데이터 발견.
sum(is.na(data[,7]))

length(which(is.na(data$Review)))
length(which(is.na(data$Brand)))
length(which(is.na(data$Style)))
length(which(is.na(data$Country)))
length(which(is.na(data$Stars))) #3개의 NA데이터 발견.
length(which(is.na(data$Top.Ten))) 
#Warning message:
#In is.na(data$Top.ten) :
#is.na() applied to non-(list or vector) of type 'NULL'
#Top.ten 데이터들은 NA가 아닌 NULL형태. 즉, 변환 필요

##Stars 변수 NA 처리
data <- data[-which(is.na(data$Stars)==TRUE),]
length(which(is.na(data$Stars)==TRUE)) #NA데이터 처리확인.



##Top.Ten data
table(data$Top.Ten)
#12년도 8등, 13년도 5등,7등,8등, 
#14년도 2등,3등, 15년도 2등,3등,5등, 
#16년도 2,3,4,6등 NA
#년도 별 수상한 등수가 겹치거나 다르게 수상하여 등수에 NA가 생긴거라 예상
#즉 이 변수를 수상을 했고 안했고로 나눠서 처리.
#'\n'이라는 데이터를 해석 할 수 없으므로 제거하기로 결정.
data<- data[-which(data$Top.Ten=='\n'),]
table(data$Top.Ten)#제거 확인.
#변수를 수상여부로 새로운 변수 만들기
data$award <- ifelse(data$Top.Ten=='',0,1)
data$award <- as.factor(data$award) #Binary data로 지정.
table(data$award)

##Review & Stars (수치형 데이터)
data %>%
  ggplot(aes(x=Review,y=Stars))+geom_point()+geom_jitter()
#조금의 선형성이 보이므로 리뷰의 갯수가 많으면 점수도 높을 가능성이 있다고 볼 수 있다.


##Brand
unique(data$Brand)[1:30]
length(unique(data$Brand))
#355개의 브랜드로 나뉘어져 있음.
sort(table(data$Brand),decreasing=T)[1:20]
#특정 브랜드가 수상을 많이 했는가?
data_award <- data[which(data$award==1),]
sort(table(data_award$Brand),decreasing = T)[1:30]
#제품을 많이 내는 브랜드라고 해서 수상을 한것이 아님을 알 수 있다.

#수상한 브랜드의 시각화.
A<-data %>%
  group_by(Brand,award) %>%
  filter(award==1) %>%
  tally() %>%
  mutate(win=n) %>%
  tally()

A<-as.data.frame(A)
glimpse(A)
A$n <- as.numeric(A$n)
colnames(A)


A %>%
  ggplot(aes(x=Brand,y=n,fill=n))+geom_bar(stat='identity') +coord_flip()  
#상을 가장 많이 수상한 회사는 prima taste가 가장 많이 수상하였다.



##Style
table(data$Style)
#''로 된 데이터는 삭제, Can은 데이터 수가 1 이므로 의미없다고 생각.
#bar로 된 라멘은 없으므로 오기로 판단하여 삭제.
#box또한 컵과 팩의 중의적 의미를 담고 있으므로 삭제하기로 결정.
data <- data[-which(data$Style=='' | data$Style=='Box' | data$Style=='Bar' | data$Style=='Can'),]
table(data$Style)

#Style & Stars
data %>%
  ggplot(aes(x=Style,y=Stars,fill=Style))+geom_boxplot()
#형태에 따른 평균차가 없어 보임.


##country
table(data$Country)
length(table(data$Country))
#나라 변수는 38개의 나라로 된 레벨로 범주화 되어있음.
#나라별 데이터가 겹치기도 하고 ex) U.S.A & U.S 데이터의 수도 각기 달라서
#대륙별로 데이터를 합치기로 결정.

data$Continent[data$Country=="Australia"]<-"Australia/Oceania"
data$Continent[data$Country=="Bangladesh"]<-"Asia"
data$Continent[data$Country=="Brazil"]<-"South America"
data$Continent[data$Country=="Cambodia"]<-"Asia"
data$Continent[data$Country=="Canada"]<-"North America"
data$Continent[data$Country=="China"]<-"Asia"
data$Continent[data$Country=="Colombia"]<-"South America"
data$Continent[data$Country=="Dubai"]<-"Asia"
data$Continent[data$Country=="Estonia"]<-"Europe"
data$Continent[data$Country=="Fiji"]<-"Australia/Oceania"
data$Continent[data$Country=="Finland"]<-"Europe"
data$Continent[data$Country=="Germany"]<-"Europe"
data$Continent[data$Country=="Ghana"]<-"Africa"
data$Continent[data$Country=="Holland"]<-"Europe"
data$Continent[data$Country=="Hong Kong"]<-"Asia"
data$Continent[data$Country=="Hungary"]<-"Europe"
data$Continent[data$Country=="India"]<-"Asia"
data$Continent[data$Country=="Indonesia"]<-"Asia"
data$Continent[data$Country=="Japan"]<-"Asia"
data$Continent[data$Country=="Malaysia"]<-"Asia"
data$Continent[data$Country=="Mexico"]<-"North America"
data$Continent[data$Country=="Myanmar"]<-"Asia"
data$Continent[data$Country=="Nepal"]<-"Asia"
data$Continent[data$Country=="Netherlands"]<-"Europe"
data$Continent[data$Country=="Nigeria"]<-"Africa"
data$Continent[data$Country=="Pakistan"]<-"Asia"
data$Continent[data$Country=="Philippines"]<-"Asia"
data$Continent[data$Country=="Poland"]<-"Europe"
data$Continent[data$Country=="Sarawak"]<-"Asia"
data$Continent[data$Country=="Singapore"]<-"Asia"
data$Continent[data$Country=="South Korea"]<-"Asia"
data$Continent[data$Country=="Sweden"]<-"Europe"
data$Continent[data$Country=="Taiwan"]<-"Asia"
data$Continent[data$Country=="Thailand"]<-"Asia"
data$Continent[data$Country=="UK"]<-"Europe"
data$Continent[data$Country=="United States"]<-"North America"
data$Continent[data$Country=="USA"]<-"North America"
data$Continent[data$Country=="Vietnam"]<-"Asia"

data$Continent <- as.factor(data$Continent)
table(data$Continent)
#Africa level 3개의 데이터를 가짐.
#South America도 11개 정도 밖에 데이터가 얼마 없음.

data[which(data$Continent=='Africa'|data$Continent=='South America'),]
#데이터의 갯수도 더 작고 수상한 희귀한 데이터도 아니므로 제거하기로 결정.
data <- data[-which(data$Continent=='Africa'|data$Continent=='South America'),]
table(data$Continent)
#제거 확인.

#대륙별 평점의 차이를 보기위한 상자그림
data %>%
  ggplot(aes(x=Continent,y=Stars,fill=Continent))+geom_boxplot()
#아시아가 가장 평점이 높다고 볼 수 있다.







######Text Analytics
library(e1071)
library(caret)
library(quanteda)
library(irlba)
library(randomForest)
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
#install.packages('stringr')
#install.packages('plyr')
#install.packages('KoNLP') # 한국어 텍스트 마이닝
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(stringr)
library(plyr)
library(KoNLP)


View(data)
attach(data)
#remove(Variety)
#wordcloud(Country)
str(Variety)
glimpse(Variety)

# Convert our class label in to a factor.
data$Variety <- as.character(data$Variety)
data$Variety[1:20]

# Check data to see if there are missing values.
length(which(!complete.cases((Variety))))



# Check data to see if there are NA.
length(which(is.na(Variety)))


# Tokenize SMS text messages.
train.Variety <- tokens(data$Variety, what = 'word',
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
# 숫자, 구두점, 심볼, 하이픈(-) 제거 후 토큰화


# Lower case the tokens.
train.Variety <- tokens_tolower(train.Variety)  # 전부 소문자화


# 필요없는 단어 제거
library(stopwords)
library(quanteda)
train.Variety <- tokens_select(train.Variety,stopwords('en'),selection = 'remove',padding = F)

train.Variety <- tokens_select(train.Variety,stopwords('po'),selection = 'remove',padding = F)
train.Variety <- tokens_select(train.Variety,stopwords('SMART'),selection = 'remove',padding = F)

train.Variety <-tokens_select(train.Variety,c('flavor','flavour','noodles','noodle','instant','ramen','bowl','style','udon',
                                      'yum','with','cup','mi','demae','penang','soup','souce')
                              ,selection = 'remove',padding = F)  # padding = F : 띄어쓰기 무시, 해석 X



# Performs stemming on the tokens.
# train.Variety <- tokens_wordstem(train.Variety, language = 'english')

wordcount <- table(unlist(train.Variety))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
glimpse(df_word)
?rename

#View(df_word)


top_20 <- df_word %>%
  arrange(desc(Freq)) %>%
  head(20)
top_20

# Vermicelli : 이탈리아 파스타면의 일종으로 흔히 아는 스파게티면보다 얇은 형태를 띈다.

library(RColorBrewer)
pal <- brewer.pal(8,"Dark2")  # Dark2 색상 목록에서 8개 색상 추출

library(wordcloud)
wordcloud(words = df_word$Var1,  # 단어
          freq = df_word$Freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal)          # 색깔 목록









######Text Analytics2
View(data_award)

# Check data to see if there are missing values.
length(which(!complete.cases((data_award))))


# Check data to see if there are NA.
length(which(is.na(data_award)))


# Convert our class label in to a factor.
data_award$Variety <- as.character(data_award$Variety)
data_award$Variety[1:20]

# Tokenize SMS text messages.
train.Variety2 <- tokens(data_award$Variety, what = 'word',
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_hyphens = TRUE)
# 숫자, 구두점, 심볼, 하이픈(-) 제거 후 토큰화


# Lower case the tokens.
train.Variety2 <- tokens_tolower(train.Variety2)  # 전부 소문자화



# 필요없는 단어 제거
train.Variety2 <- tokens_select(train.Variety2,stopwords('en'),selection = 'remove',padding = F)
train.Variety2 <- tokens_select(train.Variety2,stopwords('po'),selection = 'remove',padding = F)
train.Variety2 <- tokens_select(train.Variety2,stopwords('SMART'),selection = 'remove',padding = F)

train.Variety2 <-tokens_select(train.Variety2,c('flavor','flavour','noodles','noodle','instant','ramen','bowl','style','udon',
                                              'yum','with','cup','mi','demae','penang','soup','souce')
                              ,selection = 'remove',padding = F)

# Performs stemming on the tokens.
# train.Variety2 <- tokens_wordstem(train.Variety2, language = 'english')

wordcount2 <- table(unlist(train.Variety2))
df_word2 <- as.data.frame(wordcount2, stringsAsFactors = F)
library(dplyr)

?rename
#View(df_word)


top_20 <- df_word2 %>%
  arrange(desc(freq)) %>%
  head(20)
top_20
# mian : 밀로 만든 중국면
# laksa : 동남 아시아의 페라 나칸(Peranakan) 요리로 유명한 매운 누들수프


library(RColorBrewer)
pal2 <- brewer.pal(8,'Dark2')[5:9]  # Dark2 색상 목록에서 8개 색상 추출



library(wordcloud)
wordcloud(words = df_word2$Var1,  # 단어
          freq = df_word2$Freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .01,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal2)          # 색깔 목록


# 전체로 만든 워드 쿨라우드와 수상한 데이터의 워드클라우드에서의 차이가 별로 없다고 본다.



######################Analysis######################
summary(data)
glimpse(data)

library(caret)
train_idx <- createDataPartition(data$award, times = 1, p=.7,list=FALSE)
train_data <- data[train_idx,]
test_data <- data[-train_idx,]

prop.table(table(data$award))
prop.table(table(train_data$award))
prop.table(table(test_data$award))
# 수상비율 동일하게 train과 test set이 잘 뽑혔는지 확인


##LM model
#Variety는 독립변수로 넣지 않고, Top.Ten데이터는 award로 대체.
#Country데이터와 Contient데이터는 둘 중 하나만 넣어보기로.
lm_model <- lm(Stars~.-Variety-Top.Ten-Continent,data=train_data)
summary(lm_model)
#모델은 적합하다고 나옴,
#Brand 데이터 대부분이 적합하지 않다고 판단. 
#Adj-R^2값이 0.3837로 매우 좋지 않은 모델임을 알 수 있다.


lm_mod_2 <- lm(Stars~.-Variety-Top.Ten-Country,data=train_data)
summary(lm_mod_2)
#country 대신 contient변수를 넣으니 변화 미미.(더안좋아짐.)

lm_mod_3 <- lm(Stars ~.-Variety-Top.Ten-Country-Brand, data = train_data)
summary(lm_mod_3)



## 회귀 모델의 적합도가 매우 떨어져서 도시변수의 빈도수가 많은 도시들만 뽑아서
## 다시 모델 추정

wordcloud(data$Country, random.order = F)
# Japan, Korea, Taiwan 의 빈도수가 매우 많은 것 확인.


data2 <- data[which(data$Country=='South Korea'|data$Country=='Japan'|data$Country=='USA'|data$Country=='Taiwan'),]
train_idx2 <- createDataPartition(data2$award, list = FALSE, p=0.7, times = 1)
train_data2 <- data2[train_idx2,]
test_data2 <- data2[-train_idx2,]

View(data$Country)
View(data2)

prop.table(table(data2$award))
prop.table(table(train_data2$award))
prop.table(table(test_data2$award))
# 수상 비율별로 데이터 잘 추출되었는지 확인.


lm2_mod_1 <- lm(Stars ~.-Variety-Top.Ten-Continent,data=train_data2)
summary(lm2_mod_1)


lm2_mod_2 <- lm(Stars ~. -Variety-Top.Ten-Brand-Continent,data=train_data2)
summary(lm2_mod_2)

# 회귀모델의 적합도 높이기 위해 stepwise 써보기
#install.packages('MASS')
library(MASS)
lm2_mod_3 <- stepAIC(lm2_mod_2,direction = 'both')
summary(lm2_mod_3)

predictions2_3 <- predict(lm2_mod_3, test_data2)
rmse(test_data2$Stars, predictions2_3)

#install.packages('randomForest')
library(randomForest)
dim(data2)
rf_mod <- randomForest(Stars ~. - Variety-Brand-Continent-Top.Ten, data = train_data2)
plot(rf_mod)

# 변수 중요도 확인
varImpPlot(rf_mod)

## 이분할적 데이터가 아니므로 confusion matri만들 수 없다. 
## 또한 ROC Curve도 볼 수 없다.
predictions_rf <-predict(rf_mod, test_data2)
#install.packages('hydroGOF')
library(hydroGOF)
rmse(test_data2$Stars, predictions_rf)



############어떤 모델을 사용하더라도 모델의 적합도가 0.4이하정도의 수준으로 머무른다.
############따라서, 이상치 제거 후 모델 돌려보기로 결정

data %>%
  ggplot(aes(y=Stars)) + geom_boxplot()

boxplot(data$Stars)$stats

data3 <- data %>%
  filter(Stars>=1.75)

train_idx3 <- createDataPartition(data3$award, times = 1, p = 0.7, list = FALSE)


train_data3 <- data3[train_idx3,]
test_data3 <- data3[-train_idx3,]

prop.table(table(data3$award))
prop.table(table(train_data3$award))
prop.table(table(test_data3$award))

lm3_mod_1 <- lm(Stars ~.-Variety-Top.Ten-Continent, data=train_data3)
summary(lm3_mod_1)

step_mod <- stepAIC(lm3_mod_1, direction = 'both')
summary(step_mod)

predictions3_1 <-predict(step_mod, test_data3)
rmse(test_data3$Stars, predictions3_1)


rf_mod2 <- randomForest(Stars ~. - Variety-Brand-Continent-Top.Ten, data = train_data3)
plot(rf_mod2)

# 변수 중요도 확인
varImpPlot(rf_mod2)

## 이분할적 데이터가 아니므로 confusion matri만들 수 없다. 
## 또한 ROC Curve도 볼 수 없다.
predictions_rf2 <-predict(rf_mod2, test_data3)
rmse(test_data3$Stars, predictions_rf2)



# 위와 동일한 방법에 따라, 중요 나라 4개에서의 결과 확인
outlier_idx <- boxplot(data2$Stars)$stats
rm_out_data <- data[-outlier_idx,]
train_idx4 <- createDataPartition(rm_out_data$award, times = 1, p = .7, list = F)
train_data4 <- rm_out_data[train_idx4,]
test_data4 <- rm_out_data[-train_idx4,]

prop.table(table(rm_out_data$award))
prop.table(table(train_data4$award))
prop.table(table(test_data4$award))


lm3_mod_2 <- lm(Stars ~. -Variety-Continent-Top.Ten, data = train_data4)
summary(lm3_mod_2)
step_mod2 <- stepAIC(lm3_mod_2, direction = 'both')
summary(step_mod2)

predictions3_2 <-predict(step_mod2, test_data4)
rmse(test_data4$Stars, predictions3_2)












