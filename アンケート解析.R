suppressMessages(library(dplyr))
# setwd("環境に応じてファイルのパスを指定")

kagome <- read.csv("978145_rawdata.csv", fileEncoding = "cp932")
kagome.survey<-kagome %>% select(-ANSWERDATE, -CELL, -SAMPLEID, -CELLNAME, -Q6_6FA, -Q7_6FA) %>% mutate(id=1:nrow(.)) %>% select(id, everything())
