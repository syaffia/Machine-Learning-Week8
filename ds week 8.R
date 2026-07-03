#week 8
#upload data
student_data<- read.csv("C:/Users/atiqa/Downloads/Telegram Desktop/student_data.csv")
View(student_data)

#student_fail
Student_fail<- Student_fail<- student_data[student_data$final_exam_mark< 40,] 
View(Student_fail)

install.packages("dplyr")
library(dplyr)
library(tidyverse)
mydata1<- student_data %>% filter(final_exam_mark< 40) %>% arrange(desc(final_exam_mark))
View(mydata1)

mydata2<- student_data %>% filter(final_exam_mark< 40) %>% arrange(final_exam_mark)
View(mydata2)

#select function
mydata2 <- student_data%>% select(student_id,coursework_mark, final_exam_mark)
View(mydata2)

#glimpse
glimpse(student_data)


#mutate : to create new variables
mydata4 <- student_data%>% mutate(Total_Mark=(coursework_mark + final_exam_mark/200*100))
View(mydata4)
glimpse(mydata4)

#descriptive statistics
iris
head(iris)
tail(iris)
head(iris,10)
tail(iris,10)

#string: show structure, same as glimpse, but no data type
str(iris) 

#min and max
min(iris$Sepal.Length) #this produces 4.3
max(iris$Sepal.Length) #this produces 7.9

#range
range(iris$Sepal.Length)

#sd
sd(iris$Sepal.Length) #this produces 0.8280661
#variance
var(iris$Sepal.Length) #this produces 0.6856935

#summary 
summary(iris)

#quartile
A<-c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)

sort(A) #descending
quantile(A)

#counting column and row
nrow(iris)
ncol(iris)

#counting the number of NA
sum(is.na(iris$Sepal.Length))

#histogram
hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length",
xlab = "Sepal Length (cm)",
ylab = "Frequency", 
col = "lightblue", 
border = "black")

#identify outlier using boxplot
data<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data, main = "Boxplot")

first_q<-quantile(data,0.25) #this is 26
print(first_q)
third_q<-quantile(data,0.75) #this is 31.75
print(third_q)
inter_q <- quantile(data,0.5)
print(inter_q)
iqr<-IQR(data) #this produces 5.75
print(iqr)

#lower extreme
lower_extreme <-first_q- 1.5 * iqr 
print(lower_extreme)

#upper extreme
upper_extreme<-third_q + 1.5 * iqr #this produces 40.375
print(upper_extreme)

#how to deal with outlier
#remove the outlier
data_new<-data
data_new <- data_new[!data_new<lower_extreme]
data_new <- data_new[!data_new>upper_extreme]
data_new

#replace outlier with mean
data_new <- data
avg <- round(mean(data_new)) #for the purpose of example we round up value
data_new[data_new<lower_extreme] <- avg
data_new[data_new>upper_extreme] <- avg
data_new

#replace with upper and lower value
data_new <- data
data_new[data_new<lower_extreme] <- lower_extreme
data_new[data_new>upper_extreme] <- upper_extreme
data_new

#new boxplot
boxplot(data_new, main = "Boxplot")
