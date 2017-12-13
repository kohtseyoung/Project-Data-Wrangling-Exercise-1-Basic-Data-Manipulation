library(dplyr)
library(tidyr)
library(tibble)
nuggets <-read.csv(file ="C:/Users/Tse Young/Desktop/New folder (3)/Assignment/titanic_original.csv",header=TRUE)
exercise2 <- data.frame(nuggets)
exercise2$embarked <- gsub("^$","S",exercise2$embarked)

mean_age <- mean(exercise2$age,na.rm=TRUE)
exercise2$age[is.na(exercise2$age)] <-mean_age

exercise2$boat <- gsub("^$","None",exercise2$boat)
exercise2 <- exercise2 %>% mutate(has_cabin_number=if_else(cabin!="",1,0))

exercise2
write.csv(exercise2,file="C:/Users/Tse Young/Desktop/New folder (3)/Assignment/titanic_clean.csv")
