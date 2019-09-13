install.packages("dplyr")
suppressMessages(library(dplyr))

setwd("C:/Users/uasami reo/Downloads/マクロミル/07_カゴメ部門(FIX)/分析用データ/アンケートデータ/01_ローデータ/01_スクリーニングデータ")



kagome <- read.csv("978145_rawdata.csv", fileEncoding = "cp932")

kagome.survey<-kagome %>% select(-ANSWERDATE, -CELL, -SAMPLEID, -CELLNAME, -Q6_6FA, -Q7_6FA) %>% mutate(id=1:nrow(.)) %>% select(id, everything())

kagome.survey <- kagome.survey %>% select(-AGEID)
kagome.survey <- kagome.survey %>% select(-AREA)
kagome.survey <- kagome.survey %>% select(-Q1_14FA)
Q2_t <- (kagome.survey$Q2S1 + kagome.survey$Q2S4) /2
Q2_n <- (kagome.survey$Q2S2 + kagome.survey$Q2S3 + kagome.survey$Q2S5 + kagome.survey$Q2S6) / 4
Q2_n

Q3_t <- (kagome.survey$Q3S1 + kagome.survey$Q3S4) /2
Q3_n <- (kagome.survey$Q3S2 + kagome.survey$Q3S3 + kagome.survey$Q3S5 + kagome.survey$Q3S6) / 4

Q4_t <- (kagome.survey$Q4S1 + kagome.survey$Q4S4) /2
Q4_n <- (kagome.survey$Q4S2 + kagome.survey$Q4S3 + kagome.survey$Q4S5 + kagome.survey$Q4S6)/4

Q5_t <- (kagome.survey$Q5S1 + kagome.survey$Q5S4) / 2
Q5_n <- (kagome.survey$Q5S2 + kagome.survey$Q5S3 + kagome.survey$Q5S5 + kagome.survey$Q5S6)/4

kagome.survey <- kagome.survey%>% select(-Q2S1,-Q2S2,-Q2S3,-Q2S4,-Q2S5,-Q2S6,-Q3S1,-Q3S2,-Q3S3,-Q3S4,-Q3S5,-Q3S6,-Q4S1,-Q4S2,-Q4S3,-Q4S4,-Q4S5,-Q4S6,-Q5S1,-Q5S2,-Q5S3,-Q5S4,-Q5S5,-Q5S6)
kagome.survey
kagome.survey <- kagome.survey %>% mutate(Q2_t,Q2_n,Q3_t,Q3_n,Q4_t,Q4_n,Q5_t,Q5_n)
kagome.survey <- kagome.survey %>% select(id,SEX,AGE,PREFECTURE,JOB,Q1_1,Q1_2,Q1_3,Q1_4,Q1_5,Q1_6,Q1_7,Q1_8,Q1_9,Q1_10,Q1_11,Q1_12,Q1_13,Q1_14,Q2_t,Q2_n,Q3_t,Q3_n,Q4_t,Q4_n,Q5_t,Q5_n,everything())

#後は、NAの処理をして、
#記述統計とかグラフを出す。

#NAの数を調べる（Q7のNAの数を調べた）
length(which(is.na(kagome.survey$Q7)) == NA)
length(kagome.survey$Q7)
#Q７が30000個、Q7のNAが4111個じゃけ、NAを置き換える
#Q7のNA以外の平均を求める
mean(kagome.survey$Q7,na.rm = TRUE)
#Q7のNAを1.56で置き換える
kagome.survey$Q7 <- replace(kagome.survey$Q7,which(is.na(kagome.survey$Q7)),1.56)
head(kagome.survey$Q7)

#上の要領でNAを置き換えていく
length(which(is.na(kagome.survey$Q2_t)) == NA)
length(which(is.na(kagome.survey$Q2_n)) == NA)
kagome.survey$Q2_t
length(which(is.na(kagome.survey$Q3_t)) == NA)
kagome.survey$Q3_t <- replace(kagome.survey$Q3_t,which(is.na(kagome.survey$Q3_t)),mean(kagome.survey$Q3_t,na.rm = TRUE))
kagome.survey$Q3_t
length(which(is.na(kagome.survey$Q3_n)) == NA)
kagome.survey$Q3_n <- replace(kagome.survey$Q3_n,which(is.na(kagome.survey$Q3_n)),mean(kagome.survey$Q3_n,na.rm = TRUE))
kagome.survey$Q3_n
length(which(is.na(kagome.survey$Q4_t)) == NA)
kagome.survey$Q4_t <- replace(kagome.survey$Q4_t,which(is.na(kagome.survey$Q4_t)),mean(kagome.survey$Q4_t,na.rm = TRUE))
length(which(is.na(kagome.survey$Q4_n)) == NA)
kagome.survey$Q4_n <- replace(kagome.survey$Q4_n,which(is.na(kagome.survey$Q4_n)),mean(kagome.survey$Q4_n,na.rm = TRUE))
length(which(is.na(kagome.survey$Q5_t)) == NA)
kagome.survey$Q5_t <- replace(kagome.survey$Q5_t,which(is.na(kagome.survey$Q5_t)),mean(kagome.survey$Q5_t,na.rm = TRUE))
length(which(is.na(kagome.survey$Q5_n)) == NA)
kagome.survey$Q5_n <- replace(kagome.survey$Q5_n,which(is.na(kagome.survey$Q5_n)),mean(kagome.survey$Q5_n,na.rm = TRUE))
length(which(is.na(kagome.survey$Q6)) == NA)
kagome.survey$Q6 <-replace(kagome.survey$Q6,which(is.na(kagome.survey$Q6)),mean(kagome.survey$Q6,na.rm = TRUE))
length(which(is.na(kagome.survey$Q7)) == NA)

head(kagome.survey)
sum(kagome.survey$Q1_1)
sum(kagome.survey$Q1_2)
sum(kagome.survey$Q1_3)
sum(kagome.survey$Q1_4)
sum(kagome.survey$Q1_5)
sum(kagome.survey$Q1_6)
sum(kagome.survey$Q1_7)


Q1 <- cbind(kagome.survey$Q1_1,kagome.survey$Q1_2,kagome.survey$Q1_3,kagome.survey$Q1_4,kagome.survey$Q1_5,kagome.survey$Q1_6,kagome.survey$Q1_7,kagome.survey$Q1_8,kagome.survey$Q1_9,kagome.survey$Q1_10,kagome.survey$Q1_11,kagome.survey$Q1_12,kagome.survey$Q1_13,kagome.survey$Q1_14)
barplot(Q1,xlab = "同居している家族",ylab="sum",names.arg=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14"),main="Q1のBarplot")


mean(kagome.survey$Q2_t)
var(kagome.survey$Q2_t)
hinndo <- cbind(kagome.survey$Q2_t,kagome.survey$Q2_n,kagome.survey$Q3_t,kagome.survey$Q3_n,kagome.survey$Q4_t,kagome.survey$Q4_n,kagome.survey$Q5_t,kagome.survey$Q5_n)

boxplot(hinndo,main ="Q2-Q5のBoxplot",ylab="Value",xlab="question",names=c("2t","2n","3t","3n","4t","4n","5t","5n"))

library(psych)
psych::pairs.panels(hinndo)