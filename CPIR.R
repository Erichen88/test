rm(list = ls())       #�M���k������ܼ�
cat("\014")           #�M��console�������e
##################################################
library(xts)

CPI <- read.csv("C:/Users/user/Desktop/RQuant_tw/part 4/024/CPI.csv") 
CPI <-xts(CPI[,-1],order.by = as.Date(CPI$time))

head(CPI,3)
tail(CPI,3)

length(CPI) 
#�簣�̫�3�����
CPItrain <- CPI [1:(length(CPI) -3) ,]
tail(CPItrain ,3)
#ø�s�ɧǹ�
plot(CPI , main = "CPI 2005 -2014") 

library(urca)
#�i��ADF����˩w�A���˵����G
#�ǦC���I�Z����0
#�����utype�v����ܱa���Ʋ���������'drift'

CPItrain<-na.omit(CPItrain) 
summary(ur.df(CPItrain,type="drift"))
#�έp�q��-8.093,�p��1%��۩ʤ����U���{�ɭ�-3.46
#�ڵ���L���]�A������߰��]�A�ǦC���w�A�C
Box.test(CPItrain,type="Ljung-Box",fitdf=0,lag=12)
#�P�_CPI�ǦC�O�_�������T�ǦC�A��LB�˩w(Ljung-Box)
#Ljung-Box �ĥ�Q�έp�q
#p�Ȭ�0.00006413<0.05
#�ڵ���L���]�A������߰��]�A�{��CPI�ǦC���O�����T�ǦC�C
par(mfrow=c(2,1)) 

acf(CPItrain) #�b�Ĥ@�ӵe�����e�X�ǦC���۬����Y�ƹ� 
pacf(CPItrain) #�b�ĤG�ӵe�����e�X�ǦC�����۬����Y�ƹ�

mod1<-arima(CPItrain,order=c(1,0,1),method="ML") 
mod1 #ARMA(1,1)
mod2<-arima(CPItrain, order = c(1,0,2),method = "ML") 
mod2
mod3<-arima(CPItrain, order = c(1,0,3),method = "ML")  
mod4<-arima(CPItrain, order = c(2,0,1),method = "ML")  
mod5<-arima(CPItrain, order = c(2,0,2),method = "ML")  
mod6<-arima(CPItrain, order = c(2,0,3),method = "ML")  
mod7<-arima(CPItrain, order = c(3,0,1),method = "ML")  
mod8<-arima(CPItrain, order = c(3,0,2),method = "ML")  
mod9<-arima(CPItrain, order = c(3,0,3),method = "ML")
###ARMA(1,1)��AIC�̤p###
confint(mod1) 
#�B��confint() �p��ҫ����Y�ƪ��H��϶�
#AR1��MA1���H��϶����]�t0
#�]���ڭ̥i�H���b5%���H�ߤ��ǤU�A �Ҧ��Y�Ƴ��O����۪��C

tsdiag(mod1)
#ø�s�ɶ��ǦC�������E�_��

tsdiag(mod1,gof.lag=20)
#�W�[Ljung-Box�˩w�����ᶥ��

acf(CPItrain,40)
#ø�s�̤j���ᶥ�Ƭ�40���۬����Y�ƹ�

predict(mod1, n.ahead = 3) 
#�B��predict()��ư��H�W���p���ҫ��良�Ӫ��ǦC�ȶi��w��
#�z�L�{���X�i�o���A
#2014�~3��4��5�몺���CPI�w���Ȥ��O�� 
#100.2715, 100.2404, 100.2376�C 
#���F����w�����G�A�ڭ̥i�H�˵����ƶ���
#2014�~3��4��5�몺�������CPI����ڭȡC

tail(CPI,3) 