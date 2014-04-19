
## I created two forms of data, AllStates.csv and AllStates.txt. Use whichever you
## prefer

##  Variable code:
# SCHL: educational attainment, bb, 1-24  
# MAR: marital Status  1-5
# WAGP: Wage or salary income
# RAC1P: race,  1-9. 

setwd("/home/zhuob/BigComp/SimpleData")
big <- read.table("AllStates.txt", header=T)
dim(big)  # 9286739       6

# loading dplyr
library(dplyr)

#Chaning the Data into a tabled dataframe
All_df <- tbl_df(big)


new <- All_df %.%  # Specifying the dataframe
  group_by(ST) %.% #Grouping by State
  summarise(HighEd = mean(SCHL > 20, na.rm=TRUE)) %.% #Higher education Proportion
  arrange(ST) # Arranging in ascending order

State <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", 
  "HI", "ID", "IL", "IN", "IA", "KS","KY", "LA", "ME", "MD", "MA", "MI", "MN", 
  "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR"
  , "PA", "RI", "SC", "SD", "TN",  "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")




#Finding the proportions within Each State of races
#with higher educations.
new2 <- All_df %.%
  filter(SCHL > 19) %.% # Associates degree or above
  group_by(ST) %.%
  summarise(HighEd.White.percent=mean(RAC1P == 1, na.rm=TRUE))


new3 <- All_df %.%
filter(SCHL > 19) %.%
  group_by(ST) %.%
  summarise(HighEd.NotWhite.percent=mean(RAC1P > 1, na.rm=TRUE))%.%
  arrange(ST)

summary.dat <- cbind(new, new2, new3, State)


# want to see among white people, what percentage of them receive higher education
White.HighEd <- All_df%.%
  filter(RAC1P == 1)%.% 
  group_by(ST)%.%
  summarise(HighEd.White.percent= mean(SCHL > 19, na.rm= TRUE))

# Among non-white people,  what percentage of them receive higher education
Nonwhite.HighEd <- All_df%.%
filter(RAC1P == 6)%.% 
  group_by(ST)%.%
  summarise(HighEd.White.percent= mean(SCHL > 19, na.rm= TRUE))

## define a function for summarizing
#  All you need is to specify the race category (from 1 -9)

race <- function(x)  # x defines the race group 
{ table <- All_df%.%
    filter(RAC1P == x)%.%
    group_by(ST)%.%
    # round to the third decimal place
    summarise(percent=round(mean(SCHL > 19, na.rm=TRUE), 3))%.%
    arrange(ST)
  RAC1P <- rep(x, dim(table)[1])
  # create a table containg the race code
  return(cbind(table, RAC1P, State)) 
}

# example: I want to see what portion of white people with higher education
cbind(race(1), race(6))

# draw some figures
library(ggplot2)

white.hi <- race(1)

names(white.hi)
plot(white.hi$perce)


###q3: What proportion of people in each state are non-white? ######
getwd()
setwd("C:/Users/Acer/Documents/OSU/ST 599/BigComp")
print(All_df)
table(All_df$RAC1P)

group<-group_by(All_df, ST)
pro<-summarise(group,prop_nowhite=mean(RAC1P>1,na.rm=TRUE))
head(pro)

###q4:What is the mean salary for each level of education in each state? [I did not write this yet!]##
#state_AL<-subset(All_df, ST=1)
#schl19<-filter(state_AL,SCHL==19)
#summarise(schl19,avg.salary_19=mean(WAGP,na.rm=TRUE))


##Avg salary for educ level 19 by state
filt<-filter(All_df,SCHL==19)
group<-group_by(filt, ST)
summarise(group,avg.salary_19=mean(WAGP,na.rm=TRUE))

##function to compute avg salary for educ level
wage_educ<-function(x)
{ 
    schl_x<-filter(All_df,SCHL == x)  ##filter tirst by edu level
    group_x<-group_by(schl_x,ST) # group by state
    avg.salary_19<-summarise(group_x,avg.salary_19=mean(WAGP,na.rm=TRUE))
  return(avg.salary_19)
  
     }
###Average salary for each education level by state
##the function create double ST var for each educ level and the same var name
result<-data.frame(cbind(State,wage_educ(19) ,wage_educ(20),wage_educ(20),wage_educ(22),wage_educ(23),wage_educ(24)))
head(result)

##here is code to delete the doublon of ST
result1<-result[,-c(4,6,8,10,12)]

##renaming the columns
colnames(result1)[3]<-'avg.salary_19'
colnames(result1)[4]<-'avg.salary_20'
colnames(result1)[5]<-'avg.salary_21'
colnames(result1)[6]<-'avg.salary_22'
colnames(result1)[7]<-'avg.salary_23'
colnames(result1)[8]<-'avg.salary_24'
head(result1)
result0<-cbind(result1,pro$prop_nowhite)
colnames(result0)[9]<-'prop_nonwhite'
head(result0)

###plotting
library(ggplot2)
qplot(as.factor(State),prop_nonwhite,data=result0,geom = "jitter",size = I(2))+
  ggtitle("Proportion of non white by state") 

qplot(State,avg.salary_24,data=result0,size=I(2))+
  ggtitle("Avg salary for education level 24 by state") 
 #point(State,avg.salary_23,data=result0)
#ggplot(result0,aes(x=factor(""),fill=State))+geom_bar()
