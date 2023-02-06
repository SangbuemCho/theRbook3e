# Chapter 16 Design of experiment
getwd()
setwd("./Documents/RUMINO2023/theRbook3e")
dataPath = "./dataSet/"

# 1. factorial design analysis ----
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


# 2. Pseudo-replication ----
dat <- read.table("./dataSet/splityield.txt", header = T)
head(dat)
# DOE structure
dim(dat)
#block (4) * irrigation (2) * density(3) * fertilizer (3)
# 각 fertilizer에 대한 반복이 없고, block으로 반복이 이루어진 상태
4 * 2 * 3 * 3 


# target factor는 irrigation, density, fertilizer 이고 replication은 block인 상태
# 일반 ANOVA
mod <- aov(yield ~ fertilizer * density * irrigation, data = dat)
summary(mod)
# 자유도 계산
2 + 2 + 1 + 4 + 2 + 2+ 4 + 54
# pseudo-replication 적용
mod1 <- aov(yield ~ fertilizer * density * irrigation +
              Error(block/irrigation/density), data = dat)

summary(mod1)

# 자유도 계산에 관한 내용은 ch16.txt 파일 참조

# pseudo-replication의 두 번째 예제


dat2 <- data.frame(block = rep(c("b1", "b2"), each = 18),
                   group1 = rep(c("A", "B"), each = 9, times = 2),
                   group2 = rep(c("a", "b", "c"), each = 3, times = 4),
                   value = rnorm(36, 100, 10))
head(dat2)

# group2가 value에 미치는 효과와 group2의 효과가 group1에 조건에 따라서 다르게 나타나는지를 알아봄
# pseudo-replication 요소는 block, group1이 된다.

# 우선 그냥 anova
mod2 <- aov(value ~ group2*group1, data = dat2)
summary(mod2)
# residual의 자유도가 30이고 total의 자유도가 35가 된다

# pseudo-replication 적용
mod3 <- aov(value ~ group2*group1+Error(block/group1), data = dat2)
summary(mod3)

# 자유도 확인 
(1 + 1 + 1) + 2 + 2 + 28
# 제거된 pseudo-replication 자유도
1 + 1 + 1
# block (2) * group1 (2) = 4




