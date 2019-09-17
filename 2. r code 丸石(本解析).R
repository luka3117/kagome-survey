suppressMessages(library(tidyverse))

# setwd("C:/Users/uasami reo/Downloads/マクロミル/07_カゴメ部門(FIX)/分析用データ/アンケートデータ/01_ローデータ/01_スクリーニングデータ")

d <- read.csv("data.for.analysis.csv", fileEncoding = "cp932") %>%
  tbl_df()
d1 <- d %>% select(id,   一人暮らし:家族)
d1
d2 <-
  gather(data = d1,
         key =   居住形態,
         value =  x,
         一人暮らし ,
         配偶者 ,
         家族)  %>% filter(x != 0) %>% arrange(id) %>% left_join(., d, by = "id") %>%
  select(-x)

# d2$SEX %>% factor(labels = c("男性","女性"))
# d2$JOB %>% factor(labels =c("学生","その他"))
# d2$料理人 %>% factor(labels =)
# d2$購入人 %>% factor(labels =c("ご自身","配偶者","家族"))


d2 <- d2 %>%
  select(id:JOB,   朝食の頻度:購入人) %>%
  mutate(
    SEX = factor(SEX, labels = c("男性", "女性")),
    JOB = factor(JOB, labels = c("その他", "学生")),
    料理人  = factor(料理人, labels = c("ご自身", "配偶者", "家族")) ,
    購入人  = factor(購入人, labels = c("ご自身", "配偶者", "家族"))
  ) %>%
  select(id:JOB,   料理人,   購入人, everything())

d2

