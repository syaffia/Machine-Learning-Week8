# Import dataset
student_data <- read.csv("C:/Users/atiqa/Downloads/Telegram Desktop/student_data.csv")

# View dataset
View(student_data)

# Quest 1
student_above80 <- student_data %>% filter(final_exam_mark > 80)
View(student_above80)

# Quest 2
student_sorted <- student_data %>% arrange(coursework_mark)
View(student_sorted)

#Quest 3
student_result <- student_data %>% select(student_name, final_exam_mark)
View(student_result)

#Quest 4
nrow(student_data)
ncol(student_data)

# Quest 5
summary(student_data)
glimpse(student_data)

# Quest 6
# Identify outliers using boxplot
boxplot(student_data$final_exam_mark, main = "Boxplot of Final Exam Marks")

# First Quartile,Q1
first_q <- quantile(student_data$final_exam_mark, 0.25)
print(first_q)

# Third Quartile,Q3
third_q <- quantile(student_data$final_exam_mark, 0.75)
print(third_q)

# Median,Q2
inter_q <- quantile(student_data$final_exam_mark, 0.50)
print(inter_q)

# IQR
iqr <- IQR(student_data$final_exam_mark)
print(iqr)

# Lower Extreme
lower_extreme <- first_q - 1.5 * iqr
print(lower_extreme)

# Upper Extreme
upper_extreme <- third_q + 1.5 * iqr
print(upper_extreme)

# Display the outlier values
student_data$final_exam_mark[student_data$final_exam_mark < lower_extreme | student_data$final_exam_mark > upper_extreme]
