# Chapter 16 Design of experiment
getwd()
setwd("./Documents/RUMINO2023/theRbook3e")
dataPath = "./dataSet/"

# factorial design analysis 
dat = read.table(paste0(dataPath, "growth.txt"), header = T, 
                 colClasses = list(diet = "factor", supplement = "factor"))
head(dat)
str(dat)
attach(dat)
gm <- tapply(gain, list(diet, supplement), mean)
gm
detach(dat)

## 각 요인들의 효과와 요인간의 상호작용 탐색
mod <- lm(gain ~ diet * supplement, data = dat)
summary(aov(mod))

## 각 요인 구성원들의 효과
summary(mod)

## Coefficients estimates의 계산
### intercept: 알파벳 순서로 barley와 agrimore가 intercept로 선택됨
interceptValue = mean(dat[dat$diet == 'barley' & dat$supplement == 'agrimore', 'gain']) 
interceptValue
### dietoats
dietoatsValue = mean(dat[dat$diet == 'oats' & dat$supplement == 'agrimore', 'gain'])
dietoatsValue
dietoatsValue - interceptValue # intercept 값이 뒤에 온다는 사실
### dietwheat
dietwheatValue = mean(dat[dat$diet == 'wheat' & dat$supplement == 'agrimore', 'gain'])
dietwheatValue
dietwheatValue - interceptValue
### supplementcontrol
supplementcontrolValue = mean(dat[dat$diet == 'barley' & dat$supplement == 'control', 'gain'])
supplementcontrolValue
supplementcontrolValue - interceptValue


# Pseudo-replication
dat <- read.table(paste0(dataPath, "splityield.txt"), header = T)
head(dat)

dat %>%
  group_by(block, irrigation, density) %>%
  summarise(n = n())

# DOE structure
4 * 2 * 3 * 3 #block (4) * irrigation (2) * density(3) * fertilizer (3)
dim(dat)

# target factor는 irrigation, density, fertilizer 이고 replication은 block인 상태

mod <- aov(yield ~ fertilizer * density * irrigation, data = dat)
summary(mod)
# degree of freedom for residuals

# replication: 4
# formula for residual df: N - k
72 - (2*3*3)

dim(mtcars)

summary(aov(mpg ~ as.factor(cyl)*as.factor(am), data = mtcars))
31 - 26

(2 + 2 + 1 + 4 + 2 + 2 + 4) + 54 

(2 + 2 + 4 + 4) + 36

72 - 24

72 - (4 + 5 + 15)

EUCAL

