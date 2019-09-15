suppressMessages(library(dplyr))
library(tidyr)

# setwd("C:/Users/uasami reo/Downloads/マクロミル/07_カゴメ部門(FIX)/分析用データ/アンケートデータ/01_ローデータ/01_スクリーニングデータ")

d <- read.csv("data.for.analysis.csv", fileEncoding = "cp932") %>%
  tbl_df()
d1<-d %>% select(id, 一人暮らし:家族)

d1
gather(data = d1, key = x,  value =  居住形態, 一人暮らし ,配偶者 , 家族)










