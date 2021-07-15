# 비경제활동인구수 ARIMA 시계열 분석 및 예측

library(astsa)
library(lmtest)
library(forecast)
library(lubridate) #ymd function
library(portes) #ljungbox
library(fUnitRoots) #adfTest function 사전에 패키지
library(ggplot2)

z<- scan("C:/Users/Playdata/Documents/r1.txt") # 데이터 호출
pop <- ts(z, start = c(2010, 1), frequency = 12) # 시계열 데이터로 변환
pop

lpop<-log(pop) # 로그변환
d1lpop<-diff(lpop, lag=1) # d=1 일반 차분

acf2(d1lpop, main = "그림 1-1 로그, 일반차분의 SACF와 SPACF")
d12lpop <- diff(lpop, lag = 12) #계절차분

acf2(d12lpop, main="그림 1-2 로그, 계절차분의 SACF 와 SPACF")
d1_12lpop <- diff(d12lpop) # 일반 1차차분과 계절차분 모두시행

acf2(d1_12lpop, main = "그림 1-3 로그, 일반, 계절차분의 SACF와 SPACF")
arima(lpop, order = c(0,1,1)) # 비계절성 MA(1)과 1차 차분- 26 -

sarima(lpop, 0,1,1, 1,1,0, 12) #fit111와 동일
acf2(pop, main ="그림 1-4 비경제활동인구 자료의 ACF & PACF")

#계절성은 AR(1)모형, 비계절성은 MA(1)모형을 따르고 각각 1차 차분 실행
fit111 <- arima(lpop, order = c(0,1,1), seasonal = list(order = c(1,1,0), period =12), include.mean = F); fit111

#계절성은 AR(1)모형, 비계절성은 AR(1)모형을 따르고 각각 1차 차분 실행
fit222 = arima(lpop, order = c(1,1,0), 
               seasonal = list(order = c(1,1,0), period =12), include.mean = F); fit222

#계절성은 MA(1)모형, 비계절성은 MA(1)모형을 따르고 각각 1차 차분 실행
fit333 = arima(lpop, order = c(0,1,1), 
               seasonal = list(order = c(0,1,1), period =12), include.mean = F); fit333

#계절성은 MA(1)모형, 비계절성은 AR(1)모형을 따르고 각각 1차 차분 실행
fit444 = arima(lpop, order = c(1,1,0), 
               seasonal = list(order = c(0,1,1), period =12), include.mean = F); fit444

sarima(lpop, 0,1,1, 1,1,0, 12) #fit111와 동일
sarima(lpop, 1,1,0, 1,1,0, 12) #fit222와 동일
sarima(lpop, 0,1,1, 0,1,1, 12) #fit333와 동일
sarima(lpop, 1,1,0, 0,1,1, 12) #fit444와 동일

coeftest(fit111) # 각 계수의 유의성 파악
coeftest(fit222)
coeftest(fit333)
coeftest(fit444)

summary(fit111) # 계수값, 분산, 우도, aic, 오차값 파악
summary(fit222)
summary(fit333)
summary(fit444)

fit111$aic # aic 값을 파악
fit222$aic
fit333$aic
fit444$aic

fit111$sigma2 # 분산값을 파악
fit222$sigma2
fit333$sigma2
fit444$sigma2

fit1111 = arima(lpop, order = c(1,2,0), 
                seasonal = list(order = c(0,2,1), period =12), include.mean = F); fit1111 #과대차분을 시도

fit11 <- arima(lpop, order = c(0,1,2), seasonal = list(order = c(1,1,0), period =12), include.mean = F); fit11 #과대적합을 시도

Box.test(resid(fit111), lag=6, type="L", fitdf = 2) #ljung-box test
Box.test(resid(fit111), lag=12, type="L", fitdf = 2) #ljung-box test
Box.test(resid(fit111), lag=24, type="L", fitdf = 2) #ljung-box test

ts.plot(resid(fit111), ylab = "잔차", main = "그림 2-1 비경제활동인구의 잔차 시
계열 그림"); abline(h=0) 

par(new=T) #잔차의 분산을 비교하고자 두 그래프를 겹침

ts.plot(resid(fit11), ylab= "", col='red')
ts.plot(resid(fit111), main = "그림 2-2 비경제활동인구의 잔차 시계열 그림"); 
abline(h=0)

acf2(resid(fit111), main="그림 2-3 비경제활동인구의 잔차 SACF와 SPACF")

qqnorm(resid(fit111), main = "그림 2-4 비경제활동인구의 잔차의 정규성 검정")
qqline(resid(fit111), col="red")
sarima.for(pop, 20, 0,1,1, 1,1,0, 12) # 미래 20시차 예측
