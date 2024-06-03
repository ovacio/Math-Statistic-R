install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)

# Загрузка датасета
student_data <- read.csv("study_performance.csv")
student_data <- student_data[sample(nrow(student_data), 200), ]
student_data$gender <- as.factor(student_data$gender)

summary(student_data)

# Просмотр датасета
head(student_data)

characteristics_calculation = function(meaning){
  # Среднее
  average <- sum(meaning) / length(meaning)
  average_function <- mean(meaning)
  
  #Медиана
  sorted_mean <- sort(meaning)
  length_sorted <- length(sorted_mean)
  if (length_sorted %% 2 == 0) {
    median_mean <- (sorted_mean[length_sorted / 2] + sorted_mean[length_sorted / 2 + 1]) / 2
  } else {
    median_mean <- sorted_mean[(length_sorted + 1) / 2]
  }
  median_mean_function <- median(meaning)
  
  #Мода
  mode_mean <- as.numeric(names(sort(table(meaning), decreasing = TRUE)[1]))
  mode_mean_function <-  as.numeric(names(table(meaning))[which.max(table(meaning))])
  
  #Дисперсия
  variance_mean <- sum((meaning - average)^2) / (length(meaning) - 1)
  variance_mean_function <- var(meaning)
  
  #Среднее квадратичное отклонение
  sigma_math <- sqrt(variance_mean)
  sigma_math_function <- sd(meaning)
  
  print(paste("Среднее:", average, "Среднее через встр.:", average_function))
  print(paste("Медиана:", median_mean, "Медиана через встр.:", median_mean_function))
  print(paste("Мода:", mode_mean, "Мода через встр.:", mode_mean_function))
  print(paste("Дисперсия:", variance_mean, "Дисперсия через встр.:", variance_mean_function))
  print(paste("СКО:", sigma_math, "СКО через встр.:", sigma_math_function))
}

# Гистограмма оценок по математике + гендер
ggplot(student_data, aes(x = math_score, fill = gender)) + 
  geom_histogram(bins = 15, position="dodge", color = "black") +
  labs(title = "Оценки по математике + гендер", x = "Баллы", y = "Частота") +
  scale_fill_manual(values = c("male" = "darkblue", "female" = "pink"))


# Гистограмма ланча по количеству(категориальная переменная)
ggplot(student_data, aes(x = lunch)) + 
  geom_bar(fill = "brown", color = "black") +
  labs(title = "Ланч", x = "Тип ланча", y = "Частота")

# Полигон частот оценок по математике + гендер
ggplot(student_data, aes(x = math_score, color = gender)) + 
  geom_freqpoly(bins = 15) +
  labs(title = "Полигоны частот оценок по математике + гендер", x = "Баллы", y = "Частота") +
  scale_fill_manual(values = c("male" = "darkblue", "female" = "pink"))

# Очистка данных от выбросов
Q1 <- quantile(student_data$math_score, 0.25)
Q3 <- quantile(student_data$math_score, 0.75)
IQR <- IQR(student_data$math_score)

emissions_student_data <- subset(student_data, student_data$math_score > (Q1 - 1.5 * IQR) & student_data$math_score < (Q3 + 1.5 * IQR))

# Гистограмма оценок по математике + гендер без выбросов
ggplot(emissions_student_data, aes(x = math_score, fill = gender)) + 
  geom_histogram(bins = 15, position="dodge", color = "black") +
  labs(title = "Оценки по математике + гендер без выбросов", x = "Баллы", y = "Частота") +
  scale_fill_manual(values = c("male" = "darkblue", "female" = "pink"))

characteristics_calculation(student_data$math_score)

  
