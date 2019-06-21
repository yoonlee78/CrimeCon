csvcrimepop <- read.csv("data/csvcrimepop.csv")
csvcrimepop
str(csvcrimepop)

install.packages("corrplot")
library(corrplot)

plot(csvcrimepop[, 11:17])
overallcor <- cor(csvcrimepop[, 11:17])
overallcor
corrplot(overallcor)

attach(csvcrimepop)
cor.test(인구수당편의점수, 인구수당범죄합계)
cor.test(인구수당편의점수, 인구수당살인)
cor.test(인구수당편의점수, 인구수당강도)
cor.test(인구수당편의점수, 인구수당성폭력)
cor.test(인구수당편의점수, 인구수당절도)
cor.test(인구수당편의점수, 인구수당폭력)
cor.test(월500이상, 인구수당범죄합계)
cor.test(야간보행만족도, 인구수당범죄합계)


install.packages('ggplot2')
library(ggplot2)
install.packages('jtools')
library(jtools)


ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당범죄합계)) +   geom_point(shape=16, size=3, colour="blue")  +   ggtitle("Scatter Plot: 편의점수 vs. 범죄합계") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당강도)) +   geom_point(shape=16, size=3, colour="red")  +   ggtitle("Scatter Plot: 편의점수 vs. 강도") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당성폭력)) +   geom_point(shape=16, size=3, colour="blue")  +   ggtitle("Scatter Plot: 편의점수 vs. 성폭력") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당절도)) +   geom_point(shape=16, size=3, colour="blue")  +   ggtitle("Scatter Plot: 편의점수 vs. 절도") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당폭력)) +   geom_point(shape=16, size=3, colour="blue")  +   ggtitle("Scatter Plot: 편의점수 vs. 폭력") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

finalvs <- read.csv("data/finalvariableset.csv")

m0 <- lm(인구수당범죄합계 ~ 평균생활인구, data=finalvs)
summary(m0)


m1 <- lm(인구수당범죄합계 ~ 인구수당편의점수, data=finalvs)
summary(m1)

m2 <- lm(인구수당범죄합계 ~ 인구수당편의점수 + 평균생활인구, data=finalvs)
summary(m2)

m3 <- lm(인구수당범죄합계 ~ 인구수당편의점수*평균생활인구, data=finalvs)
summary(m3)

m4 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+일인가구비율, data=finalvs)
summary(m4)


m5 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+월500이상, data=finalvs)
summary(m5)

m6 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+일인월세, data=finalvs)
summary(m6)

m7 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+ 주관적위험 + 주관적ses + 야간보행만족도, data=finalvs)
summary(m7)

m8 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+ 합계16, data=finalvs)
summary(m8)

m9 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+ 합계16 + 인구수당편의점수:평균생활인구, data=finalvs)
summary(m9)

m10 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+ 합계16 + 주관적위험 + 주관적ses + 야간보행만족도 + 인구수당편의점수:평균생활인구, data=finalvs)
summary(m10)


m11 <- lm(인구수당범죄합계 ~ 합계16, data=finalvs)
summary(m11)

m12 <- lm(인구수당범죄합계 ~ 인구수당편의점수+합계16, data=finalvs)
summary(m12)


attach(finalvs)
cor.test(인구수당범죄합계,합계16)

sm0 <- lm(인구수당성폭력 ~ 평균생활인구, data=finalvs)
summary(sm0)


sm1 <- lm(인구수당성폭력 ~ 인구수당편의점수, data=finalvs)
summary(sm1)

sm2 <- lm(인구수당성폭력 ~ 인구수당편의점수 + 평균생활인구, data=finalvs)
summary(sm2)

sm3 <- lm(인구수당성폭력 ~ 인구수당편의점수*평균생활인구, data=finalvs)
summary(sm3)

sm4 <- lm(인구수당성폭력 ~ 인구수당편의점수+평균생활인구+일인가구비율, data=finalvs)
summary(sm4)


sm5 <- lm(인구수당성폭력 ~ 인구수당편의점수+평균생활인구+월500이상, data=finalvs)
summary(sm5)

sm6 <- lm(인구수당성폭력 ~ 인구수당편의점수+평균생활인구+일인월세, data=finalvs)
summary(sm6)

sm7 <- lm(인구수당성폭력 ~ 인구수당편의점수+평균생활인구+ 주관적위험 + 주관적ses + 야간보행만족도, data=finalvs)
summary(sm7)

sm8 <- lm(인구수당성폭력 ~ 인구수당편의점수+평균생활인구+ 합계16, data=finalvs)
summary(sm8)

m9 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+ 합계16 + 인구수당편의점수:평균생활인구, data=finalvs)
summary(m9)

m10 <- lm(인구수당범죄합계 ~ 인구수당편의점수+평균생활인구+ 합계16 + 주관적위험 + 주관적ses + 야간보행만족도 + 인구수당편의점수:평균생활인구, data=finalvs)
summary(m10)




vm0 <- lm(인구수당폭력 ~ 평균생활인구, data=finalvs)
summary(vm0)


vm1 <- lm(인구수당폭력 ~ 인구수당편의점수, data=finalvs)
summary(vm1)

vm2 <- lm(인구수당폭력 ~ 인구수당편의점수 + 평균생활인구, data=finalvs)
summary(vm2)

vm3 <- lm(인구수당폭력 ~ 인구수당편의점수*평균생활인구, data=finalvs)
summary(vm3)

sm4 <- lm(인구수당성폭력 ~ 인구수당편의점수+평균생활인구+일인가구비율, data=finalvs)
summary(sm4)


vm5 <- lm(인구수당폭력 ~ 인구수당편의점수+평균생활인구+월500이상, data=finalvs)
summary(vm5)

vm6 <- lm(인구수당폭력 ~ 인구수당편의점수+평균생활인구+일인월세, data=finalvs)
summary(vm6)

vm7 <- lm(인구수당폭력 ~ 인구수당편의점수+평균생활인구+ 주관적위험 + 주관적ses + 야간보행만족도, data=finalvs)
summary(vm7)

vm8 <- lm(인구수당폭력 ~ 인구수당편의점수+평균생활인구+ 합계16, data=finalvs)
summary(vm8)

vm9 <- lm(인구수당폭력 ~ 인구수당편의점수*평균생활인구+ 합계16, data=finalvs)
summary(vm9)


tm0 <- lm(인구수당절도 ~ 평균생활인구, data=finalvs)
summary(tm0)


tm1 <- lm(인구수당절도 ~ 인구수당편의점수, data=finalvs)
summary(tm1)

tm2 <- lm(인구수당절도 ~ 인구수당편의점수 + 평균생활인구, data=finalvs)
summary(tm2)

tm3 <- lm(인구수당절도 ~ 인구수당편의점수*평균생활인구, data=finalvs)
summary(tm3)

tm4 <- lm(인구수당절도 ~ 인구수당편의점수+평균생활인구+일인가구비율, data=finalvs)
summary(tm4)


tm5 <- lm(인구수당절도 ~ 인구수당편의점수+평균생활인구+월500이상, data=finalvs)
summary(tm5)

tm6 <- lm(인구수당절도 ~ 인구수당편의점수+평균생활인구+일인월세, data=finalvs)
summary(tm6)

tm7 <- lm(인구수당절도 ~ 인구수당편의점수+평균생활인구+ 주관적위험 + 주관적ses + 야간보행만족도, data=finalvs)
summary(tm7)

tm8 <- lm(인구수당절도 ~ 인구수당편의점수+평균생활인구+ 합계16, data=finalvs)
summary(tm8)

tm9 <- lm(인구수당절도 ~ 인구수당편의점수*평균생활인구+ 합계16, data=finalvs)
summary(tm9)


tm10 <- lm(인구수당절도 ~ 인구수당편의점수*평균생활인구+ 일인월세+합계16, data=finalvs)
summary(tm10)


vm0 <- lm(인구수당폭력 ~ 평균생활인구, data=finalvs)
summary(vm0)


vm1 <- lm(인구수당폭력 ~ 인구수당편의점수, data=finalvs)
summary(vm1)

vm2 <- lm(인구수당폭력 ~ 인구수당편의점수 + 평균생활인구, data=finalvs)
summary(vm2)

vm3 <- lm(인구수당폭력 ~ 인구수당편의점수*평균생활인구, data=finalvs)
summary(vm3)

sm4 <- lm(인구수당성폭력 ~ 인구수당편의점수+평균생활인구+일인가구비율, data=finalvs)
summary(sm4)


vm5 <- lm(인구수당폭력 ~ 인구수당편의점수+평균생활인구+월500이상, data=finalvs)
summary(vm5)

vm6 <- lm(인구수당폭력 ~ 인구수당편의점수+평균생활인구+일인월세, data=finalvs)
summary(vm6)

vm7 <- lm(인구수당폭력 ~ 인구수당편의점수+평균생활인구+ 주관적위험 + 주관적ses + 야간보행만족도, data=finalvs)
summary(vm7)

vm8 <- lm(인구수당폭력 ~ 인구수당편의점수+평균생활인구+ 합계16, data=finalvs)
summary(vm8)

vm9 <- lm(인구수당폭력 ~ 인구수당편의점수*평균생활인구+ 합계16, data=finalvs)
summary(vm9)


rm0 <- lm(인구수당강도 ~ 평균생활인구, data=finalvs)
summary(rm0)


rm1 <- lm(인구수당강도 ~ 인구수당편의점수, data=finalvs)
summary(rm1)

rm2 <- lm(인구수당강도 ~ 인구수당편의점수 + 평균생활인구, data=finalvs)
summary(rm2)

rm3 <- lm(인구수당강도 ~ 인구수당편의점수*평균생활인구, data=finalvs)
summary(rm3)

rm4 <- lm(인구수당강도 ~ 인구수당편의점수+평균생활인구+일인가구비율, data=finalvs)
summary(rm4)


rm5 <- lm(인구수당강도 ~ 인구수당편의점수+평균생활인구+월500이상, data=finalvs)
summary(rm5)

rm6 <- lm(인구수당강도 ~ 인구수당편의점수+평균생활인구+일인월세, data=finalvs)
summary(rm6)

rm7 <- lm(인구수당강도 ~ 인구수당편의점수+평균생활인구+ 주관적위험 + 주관적ses + 야간보행만족도, data=finalvs)
summary(rm7)

rm8 <- lm(인구수당강도 ~ 인구수당편의점수+평균생활인구+ 합계16, data=finalvs)
summary(rm8)

rm9 <- lm(인구수당강도 ~ 인구수당편의점수*평균생활인구+ 합계16, data=finalvs)
summary(rm9)


