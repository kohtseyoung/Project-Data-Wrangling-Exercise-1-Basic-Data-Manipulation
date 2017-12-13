library(dplyr)
library(tidyr)
library(tibble)
chicken <-read.csv(file ="C:/Users/Tse Young/Desktop/New folder (3)/Assignment/refine_original.csv",header=TRUE)
exercise1 <- data.frame(chicken)
# To change company names to lowercase
exercise1$company <- tolower(exercise1$company)
# To change company names based on the required filters
exercise1$company[grepl("^p",exercise1$company)] <- "philips"
exercise1$company[grepl("^a",exercise1$company)] <- "akzo"
exercise1$company[grepl("^v",exercise1$company)] <- "van houten"
exercise1$company[grepl("^u",exercise1$company)] <- "unilever"
exercise1$company[grepl(".l{2}",exercise1$company)] <- "philips"
#To split column into two
exercise1<- exercise1 %>% separate(Product.code...number,c("product_code","product_number"),"-")
#To create a new column specifying the names of the product based on the product code
exercise1<-exercise1 %>% mutate(product_category=if_else(product_code=="p","Smartphone",
                                      if_else(product_code=="x","Laptop",
                                      if_else(product_code=="v","TV",
                                      "Tablet"))))
#To readjust the columns positions for clarity 
exercise1<- exercise1 %>% select(company:product_code,product_category,product_number:name)
#To combine seperate address into one full address
exercise1 <- exercise1 %>% unite(full_address,c(address:country),sep=",",remove=FALSE)
# To create binary using select/mutate/distict function
exercise1.1 <- exercise1 %>% mutate(yesno=1) %>% distinct %>% spread(company,yesno,fill=0)
exercise1.1 <- exercise1.1 %>% mutate(yesno=1) %>% distinct %>% spread(product_category,yesno,fill=0)
# To combine binary data with original table as spread eliminates the applied columns
exercise1.1binary <- exercise1.1 %>% select(akzo:TV)
exercise1 <-cbind(exercise1,exercise1.1binary)
# To rename binary columns

names(exercise1)[10:13]<- gsub("^","country_",names(exercise1)[10:13])
names(exercise1)[14:17]<- gsub("^","product_",names(exercise1)[14:17])

exercise1
write.csv(exercise1,file="C:/Users/Tse Young/Desktop/New folder (3)/Assignment/refine_clean.csv")
