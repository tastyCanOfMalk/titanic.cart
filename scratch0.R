if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(tibble)) install.packages("tibble")
library(tibble)
if (!require(tree)) install.packages("tree")
library(tree)

setwd("/home/e/R/titanic.cart")

x.train <- read_csv("data/train.csv")
x.test <- read_csv("data/test.csv")
x.test$Survived <- NA
x.full <-rbind(x.train,x.test)

glimpse(x.full)
summary(x.full)

# Variables unique levels
x.levels <- cbind(colnames(x.full),
                  (as.data.frame(sapply(x.full,function(x) length(unique(x))))))
colnames(x.levels) <- c("var","levels")
row.names(x.levels) <- NULL
x.levels[order(-x.levels[,2]),]

# NA values
missing_values <- x.full %>% 
  summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, 
                         key="feature", 
                         value="missing_pct")

missing_values %>% 
  ggplot(aes(x=reorder(feature,missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+
  theme_bw()

# Filling in missing data
## Fill in Age with mean(Age,na.rm=T)
### Categorizing further categorizable variables
x.full2 <- x.full %>%
  mutate(Age = ifelse(is.na(Age), mean(x.full$Age, na.rm=TRUE), Age),
    `Age Group` = case_when(Age < 13 ~ "Age.0012", 
                            Age >= 13 & Age < 18 ~ "Age.1317",
                            Age >= 18 & Age < 60 ~ "Age.1859",
                            Age >= 60 ~ "Age.60Ov"))

## Fill in Embarked with most popular
# xx <- na.omit(x.full)
# barplot(table(xx$Embarked))
barplot(na.omit(table(x.full$Embarked)))
x.full2$Embarked <- 
  replace(x.full2$Embarked, which(is.na(x.full2$Embarked)), 'S')
#


# https://www.kaggle.com/hiteshp/head-start-for-data-scientist







tree.titanic <- tree(Survived~.,data=x)
