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

library(ggplot2)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당범죄합계)) +   geom_point(shape=16, size=3, colour="blue")  +   ggtitle("Scatter Plot: 편의점수 vs. 범죄합계") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당강도)) +   geom_point(shape=16, size=3, colour="red")  +   ggtitle("Scatter Plot: 편의점수 vs. 강도") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당성폭력)) +   geom_point(shape=16, size=3, colour="blue")  +   ggtitle("Scatter Plot: 편의점수 vs. 성폭력") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당절도)) +   geom_point(shape=16, size=3, colour="blue")  +   ggtitle("Scatter Plot: 편의점수 vs. 절도") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)

ggplot(data=csvcrimepop, aes(x=인구수당편의점수, y=인구수당폭력)) +   geom_point(shape=16, size=3, colour="blue")  +   ggtitle("Scatter Plot: 편의점수 vs. 폭력") + geom_text(aes(label=자치구, size =2, vjust = -1, hjust =0)) + stat_smooth(method = lm, level = 0.95)


