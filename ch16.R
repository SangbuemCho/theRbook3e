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


# 3. Contrast analysis ----
dat <- read.table("./DataSet/competition.txt", header = T)
head(dat)
str(dat)
dat$clipping <- as.factor(dat$clipping)
str(dat)
unique(dat$clipping)


# 대비 조합의 경우의 수
# 실험설계
# 총 5개의 시험구, 대조구 1 개와 처리구 4개
# 처리구는 처리 방법에 따라서 2 개의 집단으로 구분(n 집단과 r 집단)
# 각 집단은 수준에 따라서 n25, n50 그리고 r5와 r10으로 구분됨

# Contrast 분석

# 분석 준비: 정규성, 반복수, 등분산
# 정규성 검정
library(rstatix)
dat %>%
  group_by(clipping) %>%
  shapiro_test(biomass)

# balance test
dat %>%
  group_by(clipping) %>%
  summarise(n = n())

# equal variance test

levene_test(biomass ~ clipping , data = dat)

# General ANOVA

mod <- aov(biomass ~ clipping, data = dat)
summary(mod) # Mean Sq of residuals (4961)을 기억해두자, SEM 계산에 사용됨

# contrast 구성
contrasts(dat$clipping) <- cbind(c(4,-1, -1, -1, -1),
                                c(0, 1, 1, -1, -1),
                                c(0, 0, 0, 1, -1),
                                c(0, 1, -1, 0, 0))

dat$clipping

mod2 <- lm(biomass ~ clipping, data = dat)
summary(mod2)

# 각 constrast 구성 요소 확인 
overAll = mean(dat$biomass)
# contrast 1: overall mean vs. treatment mean [4, -1, -1, -1, -1]
avgNR = mean(dat[dat$clipping != 'control', 'biomass']) 
overAll - avgNR 

# contrast 2: [0, 1, 1, -1, -1]
# half of the difference between the root- (r5, r10) and shoot- (n25, n50) treatment
avgN = mean(dat[dat$clipping %in% c('n25', 'n50'), 'biomass'])
avgR = mean(dat[dat$clipping %in% c('r10', 'r5'), 'biomass'])
(avgN - avgR) / 2

# contrast 3: [0,0,0,1, -1]
# half of the difference between r5 and r10
avgR5 = mean(dat[dat$clipping == 'r5', 'biomass'])
avgR10 = mean(dat[dat$clipping == 'r10', 'biomass'])
(avgR10 - avgR5) / 2 
# r10이 먼저 온 이유는 알파뱃과 숫자의 순서에 따라 r10이 r5보다 먼저이기 때문
# contrast matrix에서 r10이 1로 할당되고 r5가 -1로 할당되어서

# contrast 4: [0,1, -1, 0, 0]
avgN25 = mean(dat[dat$clipping == 'n25', 'biomass'])
avgN50 = mean(dat[dat$clipping == 'n50', 'biomass'])

# Standard error of mean 계산
summary(mod) # Mean Sq residuals 4961을 전체 관측치 30으로 나누고 제급근을 취하면 SEM
sqrt(4961/30)

# report 작성
tapply(dat$biomass, dat$clipping, mean) -> a

matrix(a, ncol = length(a)) %>% 
  as_tibble() %>%
  set_names(names(a)) -> report

report$SEM = sqrt(4961 / 30)
report$Treatment = 0.000921
report$`Shoot vs Root` = 0.000921
report$`Root dept` = 0.9967
report$`Shoot levels` = 0.697313

report

# Polynomial contrasts
poly <- read.table("./dataSet/poly.txt", header = T, 
                   colClasses = list(treatment = "factor"))
str(poly)
poly_means = tapply(poly$response, poly$treatment, mean)
poly_means
barplot(poly_means, names = levels(poly$treatment))

# linear regression analysis
summary(lm(response ~ treatment, data = poly))
# intercept 는 treatment - high의 평균
# treatment - high가 알파벳 순으로 가장 빠르기 때문
# 변수의 순서형이 적용되지 않음

# 순서형 변수 지정
poly$treatment <- ordered(poly$treatment, levels = c('verylow', 'low', 'medium', 'high'))
levels(poly$treatment)
barplot(tapply(poly$response, poly$treatment, mean), names = levels(poly$treatment))
str(poly)
mod1 <- lm(response ~ treatment, data = poly)
summary(mod1)

# where L, linear; Q, quadratic; C, cubic effects
contrasts(poly$treatment) # polynomial contrast structure

# multiple coefficients and contrasts
pm_coefs <- coef(mod1)
pm_coefs[1] # intercept
contrasts(poly$treatment) %*% pm_coefs[2:4] + pm_coefs[1]
tapply(poly$response, poly$treatment, mean)










