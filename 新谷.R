suppressMessages(library(tidyverse))
suppressMessages(library(broom))


# 購入系
t <- read.csv("total.csv")
t %>% select(1, 2, 3,   アイテム,  購入率) %>% tbl_df() %>% select(-X) %>%
  mutate(購入率 * 1000)

cor(t$金額シェア, t$購入率)
cor(t$累積シェア, t$購入率)
cor(t$購入者あたり購入金額, t$購入率)


# t test 
t.test(t$購入率 ~ t$sex)
t %>% select(購入率, sex) %>% group_by(sex) %>% summarise(mean(購入率))


# regression
lm(購入率 ~ sex + age + sex * age, data = t) %>% tidy()
t %>% colnames()


# # アンケートデータ
#
# #d2分析
# d2
# t.test(d2$朝食の頻度~d2$SEX)
# t.test(d2$朝食の頻度~d2$JOB)
#
# cor(d2$朝食の頻度, d2$日食の頻度)
#
# #回帰分析
# kaiki<-d2%>%select(-id,-PREFECTURE)
# lm(朝食の頻度~., data=kaiki)
# anova(lm(朝食の頻度~居住形態, data=d2))
#
#



#主成分分析
syuinbunseki <- read.csv("total.csv")
syuinbunseki %>% select()
hindo <- syuinbunseki %>% select(購入率, カテゴリー)
hindo2 <- hindo[!is.na(hindo)]
hindo_syuin <- prcomp(hindo2, scale = TRUE)
#?
