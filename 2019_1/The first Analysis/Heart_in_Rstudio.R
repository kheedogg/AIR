getwd()
.libPaths()


setwd('C:\\Users\\kheed\\Desktop\\���Ƹ� AIR\\2019-1st semester activity\\The first Analysis')

all<-read.csv('C:\\Users\\kheed\\Desktop\\���Ƹ� AIR\\2019-1st semester activity\\The first Analysis\\heart.csv')

?fread
??fread
####################install.packages('data.table')
library(data.table)
all<-fread('heart.csv')

summary(all)
#age : ����
#sex : (1 = male; 0 = female)
#cp : �������� ����
#trestbps : ����
#chol : ��û �ݷ����׷� -> �ݷ����׷��� ������ ������ ������
#fbs : ���������� 120�̻��ΰ�? ���� 100~109�� ���󿵿�, 120�̻� �索������ (1 = true; 0 = false)
#restecg : ������ ��� (0~2)
#thalach : �ִ� �ɹڼ� (�Ż���-1�� �� 130ȸ, 5~13��-80~90ȸ, 20�� �̻�-70~75ȸ)
#			   (�Ϲ������� ��ü�� ���� ���� ����)
#exang : ���������� ���� ������ ����, �����ó ���̴� �� ���� ������ �������̶��� (1 = yes; 0 = no)
#oldpeak : �������� �˼� �ִ� ������ ��ȣ �ϰ� ���� -> �ϰ��ϸ� �������� ����
#slope : ������ ��ȣ�׷������� ���ڱ� ���̴� ����(������ ���� ����)
#ca : ���� �������� ä���� �ֿ� ���� ��
#thal : ��� �޽��� ������ �󸶳� �� �帣���� �˻��ϴ� �׽�Ʈ ��� 
#       1 = ����; 2 = ���� ����; 3 = �ǵ��� �� ���� ����
#target : ���庴�� ���� 1 or 0


colnames(all)

all$ca
all$thal
all$target
all$oldpeak

########all$thal '0'-�ؼ��Ұ��� ����ġ 2�� ����########
which(all$thal=='0')
all<-all[-which(all$thal=='0'),]
summary(all)












