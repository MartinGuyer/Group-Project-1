library(dplyr)
library(ggplot2)
library(scales)


## I created two forms of data, AllStates.csv and AllStates.txt. Use whichever you
## prefer

##  Variable code:
# SCHL: educational attainment, bb, 1-24  
# MAR: marital Status  1-5
# WAGP: Wage or salary income
# RAC1P: race,  1-9. 

setwd("/home/zhuob/Group-Project-1/Project1/Results")

##  big <- read.table("AllStates.txt", header=T)# read data
##  saveRDS(big, "/home/zhuob/BigComp/SimpleData/allstates.rds")

# it's more convinient to save it as .rds 
big <- readRDS("allstates.rds")
usa_df <- readRDS("usa-state-map_all.rds") # read the map

#Chaning the Data into a tabled dataframe
All_df <- tbl_df(big)

edu <- All_df %.%  # Specifying the dataframe
  filter(ST != 11) %.%
  group_by(ST) %.% #Grouping by State
  summarise(HighEd = mean(SCHL > 20, na.rm=TRUE)) %.% #Higher education Proportion
  arrange(ST) # Arranging in ascending order


state <- tolower(state.abb) # get states name from the map
edu.1 <- cbind(edu, state) # when combinded, state was changed to numeric
edu.1$state <- as.character(edu.1$state) # change state to character
edu_map <- inner_join(edu.1, usa_df, by ="state") 

qplot(x, y, data = edu_map, 
      geom = "polygon", group = group, fill = HighEd) +
  coord_equal()

qplot(HighEd, reorder(state, HighEd), data = edu_map, 
      main= "Higher education percentage by state")

#  Finding the percentage within Each State of races
#  with higher educations.


## define a function for summarizing
  
# 1 White alone 
# 2 .Black or African American alone 
# 3 .American Indian alone 
# 4 .Alaska Native alone  
#5 .American Indian and Alaska Native tribes specified; or American 
# .Indian or Alaska native, not specified and no other races 
# 6 .Asian alone 
# 7 .Native Hawaiian and Other Pacific Islander alone 
# 8 .Some other race alone 
# 9 .Two or more major race groups 


decode<- function(x) 
{
  
  if (x == 1) {return( "White")}
  else if (x == 2) {return("African")}
  else if( x == 3) {return("American Indian")}
  else if( x == 4) {return("Alaska")}
  else if( x == 5 ) {return("Tribes")}
  else if( x == 6 ) {return( "Asian")}
  else if( x == 7 ) {return( "Hawaiian")}
  else if( x == 8 ) {return("Others races alone")}
  else if( x== 9) {return("Major races group")}  

}

decode(9)

edu2 <- All_df%.%
  filter(ST != 11)%.%
group_by(RAC1P)%.%
  summarise(HighEd = mean(SCHL > 19, na.rm=TRUE)) %.% #Higher education Proportion
  arrange(HighEd) # Arranging in ascending order
edu2

edu3 <- All_df%.%
  filter(ST != 11)%.%
  group_by(RAC1P)%.%
  summarise(HighEd = var(SCHL > 19, na.rm=TRUE)) %.% #Higher education Proportion
  arrange(HighEd) # Arranging in ascending order
edu3

edu_race <- function(x)
{   
  
  table <- All_df%.%
       filter(RAC1P == x & ST != 11)%.%
       group_by(ST)%.%
       # round to the third decimal place
       summarise(percent=round(mean(SCHL >19 , na.rm=TRUE), 3))%.%
       arrange(ST)
  
  t1 <- cbind(table, state)
  t1$state <- as.character(t1$state)
  t2 <- inner_join(t1, usa_df, by = "state")

  qplot(x, y, data = t2, main= decode(x),
        geom = "polygon", group = group, fill = percent )+  
    coord_equal()
}

edu_race(1)
edu_race(2)
edu_race(3)
edu_race(5)
edu_race(6)
edu_race(7)
edu_race(8)
edu_race(9)



# Note : for Alaska group, no sample from VT and RI

table <- All_df%.%
  filter(RAC1P == 4 & ST != 11)%.%
  group_by(ST)%.%
  # round to the third decimal place
  summarise(percent=round(mean(SCHL >19 , na.rm=TRUE), 3))%.%
  arrange(ST)

### no obs from ST 50 VT and ST 44 (RI)
state2 <- state[c(-39, -45)]
t1 <- cbind(table, state2)
t1$state <- as.character(t1$state2)
t2 <- inner_join(t1, usa_df, by = "state")

qplot(x, y, data = t2, main= decode(4),
      geom = "polygon", group = group, fill = percent )+  
  coord_equal()






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
filt<-filter(All_df,SCHL==19& ST != 11)
group<-group_by(filt, ST)
summarise(group,avg.salary_19=mean(WAGP,na.rm=TRUE))



#--------------------------I modified the following code-----------------------

##function to compute avg salary for educ level
wage_educ<-function(x)
{ 
  schl_x<-filter(All_df,SCHL == x & ST!= 11)  
  ##filter tirst by edu level, and delete DC
  group_x<-group_by(schl_x,ST) # group by state
 # colname <- paste("ave.salary", x)
  table<-summarise(group_x, colname=mean(WAGP,na.rm=TRUE))
  colnames(table)[2] <- paste("ave.salary", x)
 return(table)
}


wage_educ(22)
###Average salary for each education level by state
##the function create double ST var for each educ level and the same var name
result<-cbind(state,wage_educ(19), wage_educ(20),
                         wage_educ(21), wage_educ(22),
                         wage_educ(23),wage_educ(24))
head(result)
result$state <- as.character(result$state)

##here is code to delete the doublon of ST
result1<-result[,-c(4,6,8,10,12)]
## this table contains ave.salary at all educational levels
head(result1)
result1

### to answer the question 
## Which of these educational degrees has the highest average salary for each state?

salary <- index <- rep(0, dim(result1)[1])


for ( i in 1:length(index))
{
  result1$salary[i] <- max(result1[i, 3:8]) # find the maximum income
  # locate the education level corresponding to max wage
  result1$index[i] <-  max.col(result[i, 3:8]) + 2
 }

#-------------------------------------------------------------------------------------------
#                                                                                         ##      
## it looks wield, since that for every state, education level 23 has the highest         ##  
# average income, someone help me to figure out possible errors in my code??              ##
#                                                                                         ##    
#-----------------------------------------------------------------------------------------##


salary_map <- inner_join(result1, usa_df, by ="state") 
qplot(reorder(state, salary), salary, data = salary_map, xlab="State",
      main= "Highest income by state/educational level")


##-------------------------- My code ends here --------------- Bin------
head(result1)
head(result1)
result0<-cbind(result1,pro$prop_nowhite)
colnames(result0)[9]<-'prop_nonwhite'
head(result0)

###plotting
library(ggplot2)
qplot(reorder(State,prop_nonwhite),prop_nonwhite,data=result0,
      xlab='State', ylab='Proportion' ,geom = "jitter",size = I(2))+
  ggtitle("Proportion of non white by state") 

qplot(reorder(State,avg.salary_19),avg.salary_19,xlab='State',ylab='Average Salary',data=result0,size=I(2))+
  ggtitle("Individual with one or more years of college credit, no degree ") 
#
qplot(reorder(State,avg.salary_20),avg.salary_20,xlab='State',ylab='Average Salary',data=result0,size=I(2))+
  ggtitle("Individual with Associate's degree")
#
qplot(reorder(State,avg.salary_21),avg.salary_21,xlab='State',ylab='Average Salary',data=result0,size=I(2))+
  ggtitle("Individual with Bachelor's degree")
#
qplot(reorder(State,avg.salary_22),avg.salary_22,xlab='State',ylab='Average Salary',data=result0,size=I(2))+
  ggtitle("Individual with Master's degree ")
#
qplot(reorder(State,avg.salary_23),avg.salary_23,xlab='State',ylab='Average Salary',data=result0,size=I(2))+
  ggtitle("Individual with Professional degree beyond a bachelor's degree")
#
qplot(reorder(State,avg.salary_24),avg.salary_24,xlab='State',ylab='Average Salary', data=result0,size=I(2))+
  ggtitle("Individual with Doctorate degree")


#exporting output-result0 into csv
write.csv(result0, file="C:/Users/Acer/Documents/OSU/ST 599/BigComp/result01.csv",
          col.names = T)
#3educ level
#19: 





