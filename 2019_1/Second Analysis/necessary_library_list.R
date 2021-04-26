############### install.packages

install.packages('data.table')
#library(data.table)
#Data<-fread('customer.txt',header=T,sep='\t')

install.packages('ggplot2')
library(ggplot2)

install.packages('read.xlsx')     # for R version 3.5.0
library(read.xlsx)
D<-read.xlsx('Suicide Rates Overview 1985 to 2016(����ġä��).xlsx',header=T,sep='\t')

install.packages('readxl')
library('readxl')

install.packages('dplyr') # %>% ������ # glimpse function implementaion
library(dplyr)

install.packages('knitr') # kable
library(knitr)

#predictions <-predict(rf_mod, test_data2)
install.packages('hydroGOF')
library(hydroGOF) # rmse ���� ���յ� ���� �Լ�
#rmse(test_data2$Stars, predictions)

install.packages('MASS')  # stepAIC �Լ�
library(MASS)

####Text Analytics

install.packages('e1071')
library(e1071)
  
install.packages('caret') # for a random stratified split
                          # createDataPartition �Լ� �̿�
library(caret)

install.packages('quanteda')
library(quanteda)

install.packages('irlba')
library(irlba)

install.packages('randomForest')
library(randomForest)


###### Text mining

text <- readLines(file.choose())
#example# Read the text file from internet
#filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
#text <- readLines(filePath)
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages('stringr')
install.packages('plyr')
install.packages('KoNLP') # �ѱ��� �ؽ�Ʈ ���̴�
install.packages('stopwords')

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(stringr)
library(plyr)
library(KoNLP)
library(stopwords)

install.packages('text mining')



