install.packages("ggplot2")
library(ggplot2)

# Загрузка датасета
student_data <- read.csv("study_performance.csv")
student_data <- student_data[sample(nrow(student_data), 200), ]
student_data$gender <- as.factor(student_data$gender)

summary(student_data)

# Просмотр датасета
head(student_data)

# Гистограмма оценок по математике + гендер
ggplot(student_data, aes(x = math_score, fill = gender)) + 
  geom_histogram(bins = 15, position="dodge", color = "black") +
  labs(title = "Оценки по математике + гендер", x = "Баллы", y = "Частота") +
  scale_fill_manual(values = c("male" = "darkblue", "female" = "pink"))

# Полигон частот оценок по математике + гендер
ggplot(student_data, aes(x = math_score, color = gender)) + 
  geom_freqpoly(bins = 15) +
  labs(title = "Полигоны частот оценок по математике + гендер", x = "Баллы", y = "Частота") +
  scale_fill_manual(values = c("male" = "darkblue", "female" = "pink"))
