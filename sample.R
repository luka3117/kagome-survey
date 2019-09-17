t.test(d2$朝食の頻度~d2$SEX)
cor(d2$朝食の頻度, d2$日食の頻度)
lm(朝食の頻度~., data=d2)
anova(lm(朝食の頻度~居住形態, data=d2))
