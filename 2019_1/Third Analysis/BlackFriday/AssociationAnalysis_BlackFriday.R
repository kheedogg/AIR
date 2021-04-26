install.packages('arules')
install.packages('arulesViz')
install.packages('tidyverse')
library(arules)
library(arulesViz)
library(tidyverse)
data<- read.csv('C:\\Users\\kheed\\Desktop\\���Ƹ� AIR\\2019_1\\Third Analysis\\BlackFriday\\BlackFriday.csv')
attach(data)                       # ������ ���� 
glimpse(data)
select <- dplyr::select   # MASS ��Ű���� select �Լ��� �浹�ϱ� ������ �����ؾ� ��  


products<-data %>%
  select(User_ID,Product_ID) %>%
  group_by(User_ID)%>%
  arrange(User_ID)%>%
  mutate(id=row_number())%>%
  spread(User_ID,Product_ID)%>%
  t()                              # �� ���� id��ȣ


rownames(products)
products[1,]
products=products[-1,]  #id �� �����

write.csv(products, file = 'products.csv')  #������ �������� csv�������� 

Products = read.transactions('products.csv', sep = ',',rm.duplicates = TRUE)
#transaction �����͸� ������Ģ�м��� ���� sparse format�� itemMatrix�� ���ε��ϱ� ���ؼ�
# �������м��� sparse matrix ���  

#glimpse(Products)  #�� ��
#str(Products)  # �� ��
summary(Products)
#inspect(Products)
itemFrequencyPlot(Products, topN = 25,type='absolute')

Products@data



rules = apriori(data = Products,control=list(verbos=F),
                parameter = list(support = 0.008, confidence = 0.80, maxtime = 0))

rules1 = apriori(data = Products,control=list(verbos=F),
                 parameter = list(support = 0.012, confidence = 0.65, maxtime = 0),appearance=list(rhs="P00265242",default='lhs'))



plot(rules, method = 'graph')

x11()
plot(rules1, method = 'graph')
