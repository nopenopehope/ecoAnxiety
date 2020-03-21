install.packages('psych')

library("ggplot2")
library(readxl)
library(psych)

dat <- read_excel("C:/Users/User/Downloads/dat.xlsx", 
                  sheet = "Лист1")
View(dat)

ggplot() + 
  geom_histogram(data = dat, aes(x = sum), binwidth = 4,  fill = '#123456') + 
  theme_bw() + 
  labs(x = "Общий балл", y = 'Количество респондентов')

shapiro.test(dat$sum) #нормальное распределение

alpha(dat$sum)

rawdat = dat[, 2:28]

splitHalf(rawdat, raw = TRUE, check.keys = FALSE)


set.seed (736)
sample1 <- sample (1:ncol (rawdat), round(ncol (rawdat)/2, 0))
sample2 <- (1:ncol (rawdat)) [-sample1]
df1<-rawdat[,sample1]
df2<-rawdat[,sample2]

df1$sum = rowSums(df1)
df2$sum = rowSums(df2)

cor(df1$sum, df2$sum, method = 'p', )

dat[, 2:28]


average = replicate(27, 0)
average = c(colSums(rawdat)/272)

hard = average / 3

hard

cat(hard,sep="\n")

rawdat$sum = rowSums(rawdat)

maxres = rawdat[order(rawdat$sum, decreasing = TRUE), ][1:91, ]
minres = rawdat[order(rawdat$sum, decreasing = FALSE), ][1:94,]

hardmax = c(colSums(maxres[1:27])/91) / 3
hardmin = c(colSums(minres[1:27])/94) / 3

discr = hardmax - hardmin
cat(discr,sep="\n")

alpha(rawdat[1:27])
summary(alpha(rawdat[1:27]))$raw_alpha


without = replicate(27, 0)
for (i in 1:27) {
  without[i] = summary(alpha(rawdat[1:27]))$raw_alpha - 
    summary(alpha(rawdat[1:27][-i]))$raw_alpha
}
summary(alpha(rawdat[1:27][-1]))
cat(without,sep="\n")


