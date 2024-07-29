
#DATASET 조작
highschool = read.csv("Highschool.csv",header=TRUE, fileEncoding = "CP949", encoding = "UTF-8")
str(highschool)

highschool$highschool = ordered(highschool $highschool, levels = c("강서고","신목고","영일고"))
highschool$highschool = as.factor(highschool$highschool)
highschool$avg.weekend = round(highschool$avg.weekend, digits = 3)
highschool$ gender= as.factor(highschool$gender)
str(highschool)
highschool

#기술통계 
Descript= highschool %>% group_by(highschool) %>% summarise(count = n(), mean = mean(avg.weekend), median = median(avg.weekend),max = max(avg.weekend),min = min(avg.weekend),sd = sd(avg.weekend))

par(mfrow = c(1, 1))
par(family = "AppleGothic")

#boxplot
boxplot(highschool$avg.weekend~ highschool$highschool, main ="Description of  Seoul Highschool students average study time at Weekend",col = rainbow(3),horizontal = TRUE,xlab = "average time",ylab = "highschool name")

#정규성 검사
shapiro.test(highschool$avg.weekend[highschool == "영일고"])
shapiro.test(highschool$avg.weekend[highschool == "신목고"])
shapiro.test(highschool$avg.weekend[highschool == "강서고"])

#aov test
result= aov(avg.weekend~highschool, data = highschool)
summary(result)

#Detail
par(mfrow =c(2,2))
plot(result)


TukeyHSD(result)

