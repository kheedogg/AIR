getwd()
.libPaths()


setwd('C:\\Users\\kheed\\Desktop\\동아리 AIR\\2019-1st semester activity\\The first Analysis')

all<-read.csv('C:\\Users\\kheed\\Desktop\\동아리 AIR\\2019-1st semester activity\\The first Analysis\\heart.csv')

?fread
??fread
####################install.packages('data.table')
library(data.table)
all<-fread('heart.csv')

summary(all)
#age : 나이
#sex : (1 = male; 0 = female)
#cp : 가슴통증 정도
#trestbps : 혈압
#chol : 혈청 콜레스테롤 -> 콜레스테롤이 많으면 혈관이 좁아짐
#fbs : 공복혈당이 120이상인가? 보통 100~109가 정상영역, 120이상 당뇨병영역 (1 = true; 0 = false)
#restecg : 심전도 결과 (0~2)
#thalach : 최대 심박수 (신생아-1분 약 130회, 5~13세-80~90회, 20세 이상-70~75회)
#			   (일반적으로 신체가 작을 수록 많음)
#exang : 움직임으로 인한 협심증 유무, 삼장근처 조이는 것 같은 통증을 협심증이라함 (1 = yes; 0 = no)
#oldpeak : 심전도로 알수 있는 심전도 신호 하강 정도 -> 하강하면 가슴통증 유발
#slope : 심전도 신호그래프에서 갑자기 꺾이는 정도(심전도 직선 기울기)
#ca : 투시 진단으로 채색한 주요 혈관 수
#thal : 운동중 휴식중 혈액이 얼마나 잘 흐르는지 검사하는 테스트 결과 
#       1 = 정상; 2 = 고정 결함; 3 = 되돌릴 수 없는 결함
#target : 심장병의 유무 1 or 0


colnames(all)

all$ca
all$thal
all$target
all$oldpeak

########all$thal '0'-해석불가로 관측치 2행 제거########
which(all$thal=='0')
all<-all[-which(all$thal=='0'),]
summary(all)













