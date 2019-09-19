suppressMessages(library(tidyverse))

kagome.survey<-read.csv(file = "data.for.analysis.csv", fileEncoding = "cp932")
頻度 <- kagome.survey %>%
  select(SEX, 朝食の頻度,日食の頻度,外朝食の頻度,外日食の頻度,内朝食の頻度,内日食の頻度,作朝食の頻度,作日食の頻度) %>% 
  tbl_df()


頻度 %>% group_by(SEX) %>% 
  summarise_each(funs(mean(., na.rm=T)))


頻度[complete.cases(頻度), ] %>% prcomp() %>% biplot()
x11()
頻度[complete.cases(頻度), ] %>% prcomp() %>% biplot

dev.off()

%>% prcomp() %>% biplot()


dev.off()

頻度 <- kagome.survey %>% select(朝食の頻度,日食の頻度,外朝食の頻度,外日食の頻度,内朝食の頻度,内日食の頻度,作朝食の頻度,作日食の頻度)

頻度 <- scale(頻度) %>% matrix()
  
prcomp(頻度)  





dev.off()
par(family = "HiraKakuProN-W3")
prcomp(as.matrix(相関行列頻度)) %>% biplot()


# 
# 頻度
# 相関行列頻度 <- cor(頻度, use = "complete.obs")
# 相関行列頻度
# 固有値頻度 <- eigen(相関行列頻度)
# 固有値頻度
# 対角化固有値頻度 <- diag(固有値頻度$values)
# 頻度の固有ベクトル <- 固有値頻度$vectors
# 対角化固有値頻度
# 頻度の固有ベクトル
# library(knitr)
# 種目 <- c("朝食の頻度","日食の頻度","外朝食の頻度","外日食の頻度","内朝食の頻度","内日食の頻度","作朝食の頻度","作日食の頻度")
# 頻度の主成分 <- data.frame("頻度"=頻度,"第1主成分"=頻度の固有ベクトル[,1],"第2主成分"=頻度の固有ベクトル[,2])
# kable(頻度の主成分,format="markdown")
