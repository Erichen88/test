rm(list = ls())       #清除右邊視窗變數
cat("\014")           #清除console視窗內容
##################################################
library(xts)

CPI <- read.csv("C:/Users/user/Desktop/RQuant_tw/part 4/024/CPI.csv") 
CPI <-xts(CPI[,-1],order.by = as.Date(CPI$time))

head(CPI,3)
tail(CPI,3)

length(CPI) 
#剔除最後3期資料
CPItrain <- CPI [1:(length(CPI) -3) ,]
tail(CPItrain ,3)
#繪製時序圖
plot(CPI , main = "CPI 2005 -2014") 

library(urca)
#進行ADF單跟檢定，並檢視結果
#序列的截距不為0
#類型「type」中選擇帶有飄移項的類型'drift'

CPItrain<-na.omit(CPItrain) 
summary(ur.df(CPItrain,type="drift"))
#統計量為-8.093,小於1%顯著性水平下的臨界值-3.46
#拒絕虛無假設，接受對立假設，序列為定態。
Box.test(CPItrain,type="Ljung-Box",fitdf=0,lag=12)
#判斷CPI序列是否為白雜訊序列，採LB檢定(Ljung-Box)
#Ljung-Box 採用Q統計量
#p值為0.00006413<0.05
#拒絕虛無假設，接受對立假設，認為CPI序列不是白雜訊序列。
par(mfrow=c(2,1)) 

acf(CPItrain) #在第一個畫面中畫出序列的自相關係數圖 
pacf(CPItrain) #在第二個畫面中畫出序列的偏自相關係數圖

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
###ARMA(1,1)的AIC最小###
confint(mod1) 
#運用confint() 計算模型中係數的信賴區間
#AR1跟MA1的信賴區間都包含0
#因此我們可以說在5%的信心水準下， 所有係數都是不顯著的。

tsdiag(mod1)
#繪製時間序列模擬的診斷圖

tsdiag(mod1,gof.lag=20)
#增加Ljung-Box檢定的落後階數

acf(CPItrain,40)
#繪製最大落後階數為40的自相關係數圖

predict(mod1, n.ahead = 3) 
#運用predict()函數基於以上估計的模型對未來的序列值進行預測
#透過程式碼可得知，
#2014年3月4月5月的月度CPI預測值分別為 
#100.2715, 100.2404, 100.2376。 
#為了比較預測結果，我們可以檢視原資料集中
#2014年3月4月5月的月度環比CPI的實際值。

tail(CPI,3) 
