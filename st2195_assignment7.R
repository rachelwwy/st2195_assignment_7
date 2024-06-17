library(ggplot2)
library(tidyverse)
titanic_df <- read.csv("Titanic.csv")

#check class of every column, change to make sense
sapply(titanic_df, class)
titanic_df$Survived <- as.factor(titanic_df$Survived)
titanic_df$Sex <- as.factor(titanic_df$Sex)

sapply(titanic_df, class)
summary(titanic_df)

#check for NA 
sum(is.na(titanic_df))

#drop rows with NA
titanic_df_dropna <- titanic_df[rowSums(is.na(titanic_df)) <=0,]

# a series of bar chart to show gender, ticket class and survival of the passengers onboard
gender_bar <- ggplot(titanic_df_dropna) +     
              geom_bar(aes(x= Sex), position = "stack") +
              labs(title = 'Gender', x = 'Gender type', y = 'Number of people')
gender_bar 

class_bar <- ggplot(titanic_df_dropna) +   
            geom_bar(aes(x= Pclass), position = "stack") +
            labs( title = 'Class of travel', x= 'Class of travel', y ='Number of people')
class_bar

Survival_bar <- ggplot(titanic_df_dropna)+
                geom_bar(aes(x= Survived))+
                labs(title = 'Survival rate', x= 'Survived')
Survival_bar

# Generate a histogram for the passengers’ age
ggplot(titanic_df_dropna, aes(x= Age))+
  geom_histogram()
# describe the passengers’ age using the following two boxplots: age per ticket class and age based on survival
age_class <- boxplot(Age~Pclass, data = titanic_df_dropna, main = "Age per ticket class", xlab = "Class of travel", ylab = "Age")

age_survival <- boxplot(Age~Survived, data=titanic_df_dropna, main="Age and survival", xlab="Survived", ylab="Age")

#Generate a histogram for the travel fare
fare <- ggplot(titanic_df_dropna, aes(x=Fare))+
  geom_histogram(col='blue', fill='white')
fare

# table showing the number of people who did not pay
No_pay <- titanic_df_dropna%>%
          select(Fare) %>%
          filter(Fare == 0)
table(No_pay)

#A chart of your choice to describe the family size per ticket class

df1 <- mutate(titanic_df_dropna, family_size = titanic_df_dropna$SibSp + titanic_df_dropna$Parch +1)

boxplot(family_size~Pclass, data = df1, main = "Family size per ticket class", xlab = "Ticket class", ylab = "Family size")

#A series of stacked bar charts to show the how survival differs for different gender and ticket class

survival_sex <- ggplot(titanic_df_dropna, aes(x = Survived, y = Sex, fill = Sex))+
                      geom_bar(position = "stack", stat = "identity") 

survival_class <- ggplot(titanic_df_dropna, aes(x = Survived, y = Pclass, fill = Pclass))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Class of travel and Survival", x = "Survived", y = "Class of travel")
survival_class

ggplot(titanic_df_dropna, aes(x = Sex, y = Survived, fill = Survived)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap( ~ Pclass)

#A violin chart describing how survival related to age and gender
titanic_Survived <- titanic_df_dropna[titanic_df_dropna$Survived ==1,]
titanic_nonSurvived <- titanic_df_dropna[titanic_df_dropna$Survived ==0,]
  
ggplot(titanic_Survived, aes(Age,Sex))+
       geom_violin()
       

# A violin chart describing the survival rate related to age and ticket class

ggplot(titanic_Survived, aes(Age, Pclass))+
  geom_violin()
ggplot(titanic_nonSurvived, aes(Age, Pclass))+
  geom_violin()
 
# From the graphs above, it is observed that survival is the highest amongst the female age group between 20 to 30 years, and male aged 25 years, and highest survival rate are first class passengers. The family size is highest in the third class however it does not increase the survival rate as the highest fatalities are also from third class.
