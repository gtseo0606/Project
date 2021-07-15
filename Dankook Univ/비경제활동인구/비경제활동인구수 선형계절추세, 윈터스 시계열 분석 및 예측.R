# 비경제활동인구수 선형계절추세, 윈터스 시계열 분석 및 예측

library(astsa) # acf2
library(lmtest) # dwtest
library(forecast)

# 2.2.1 선형계절추세모형
z<-read.table("r3.txt", header = F)
pop <- ts(z, frequency = 12, start = c(2010, 1))
lnpop<-log(pop)

trend<- time(lnpop)-2010
trend

y = factor(cycle(lnpop)) #factor를 이용한 월별 factor 생성 / 1~12가 반복
y

reg <- lm(lnpop ~ 0+trend+y) #선형계절추세모형
dwtest(reg)
summary(reg)

ts.plot(pop, fitted(reg), xlab="day", ylab= "sold pop", lty = 1:2, main = "비경제활동인구와 2차 추세모형 예측값")
legend("topleft", legend = c("pop", "fitted"), lty = 1:2)
model.matrix(reg) # 모형 계획행렬resid=ts(resid(reg), start = c(2010, 1), frequency = 12)

ts.plot(resid(reg), ylab="residual", main = "2.2.1 잔차"); abline(h=0)
acf2(resid(reg), main="잔차의 ACF & PACF")

# 2.3.1 윈터스 가법모형
pop <- ts(z, start = c(2010, 1), frequency = 12)

# Holt Winters additive model 가법형 모델
fit6 <- hw(pop, seasonal="additive", h = 12)
fit6$model

plot(fit6, xlab = "year", ylab = "pop", lty=1, col = "blue",
     main = "2.3.1 가법모형") #예측오차로 시계열 그림
lines(fit6$fitted, col = "red", lty =2)
legend("topleft", lty = 1:2, col = c("blue", "red"), c("Pop", "Additive"))

ts.plot(resid(fit6), ylab="residual",
        main = "2.3.1 가법모형의 예측오차") ; abline(h= 0)
acf(resid(fit6), main = "Residual ACF")

t.test(resid(fit6), mu = 0)
