install.packages("ggplot2")
library(ggplot2)

# Загрузка датасета
student_data <- read.csv("study_performance.csv")
student_data <- student_data[sample(nrow(student_data), 200), ]
student_data$gender <- as.factor(student_data$gender)

summary(student_data)

# Просмотр датасета
head(student_data)

# Гипотеза: Среднее значение math_score равняется 68
hypothetical_mean <- 68

#Уровень значимости
alpha <- 0.05

# Стандартное отклонение
std <- sd(student_data$math_score, na.rm = TRUE)
# Размер выборки
n <- length(student_data$math_score)
# Среднее значение выборки
sample_mean <- mean(student_data$math_score, na.rm = TRUE)

# Критическое значение t-распределения
# Используем критическое значение, чтобы узнать область, в которой нулевая гипотеза будет отвергнута
t_critical <- qt(1 - alpha/2, df = n - 1)

# Границы доверительного интервала для t-распределения Стьюдента
lower_bound_t <- sample_mean - t_critical * std / sqrt(n)
upper_bound_t <- sample_mean + t_critical * std / sqrt(n)

# Вывод для t-распределения Стьюдента
cat("Границы доверительного интервала для t-распределения Стьюдента: [", lower_bound_t, ":", upper_bound_t, "]\n", sep = "")
cat(if (lower_bound_t <= hypothetical_mean && hypothetical_mean <= upper_bound_t) 
  "С 95% вероятностью среднее значение генеральной совокупности находится в интервале [lower_bound_t:upper_bound_t], гипотеза верна\n" 
  else 
    "Гипотеза неверна\n"
)

# Нахождение p-value для t-распределения Стьюдента
# pt - кумулятивное распределение t-распределения CDF
# lower.tail = false берем, потому что нам нужна сумма правых и левых хвостов для двустороннего теста
t_stat <- (sample_mean - hypothetical_mean) / (std / sqrt(n))
p_value_t <- 2 * pt(abs(t_stat), df = n - 1, lower.tail = FALSE)
cat("P-value для t-распределения Стьюдента:", p_value_t, "\n")
cat(if (p_value_t >= alpha) "Гипотеза не может быть отклонена\n" else "Можно отклонить гипотезу\n")

# Дополнительный анализ для нормального распределения
# Используем критическое значение, чтобы узнать область, в которой нулевая гипотеза будет отвергнута
z_critical <- qnorm(1 - alpha/2)

# Границы доверительного интервала для нормального распределения
lower_bound_norm <- sample_mean - z_critical * std / sqrt(n)
upper_bound_norm <- sample_mean + z_critical * std / sqrt(n)

# Вывод для нормального распределения
cat("Границы доверительного интервала для нормального распределения: [", lower_bound_norm, ":", upper_bound_norm, "]\n", sep = "")
cat(if (lower_bound_norm <= hypothetical_mean && hypothetical_mean <= upper_bound_norm) 
  "С 95% вероятностью среднее значение генеральной совокупности находится в интервале [lower_bound_norm:upper_bound_norm], гипотеза верна\n" 
  else 
    "Гипотеза неверна\n"
)

# Нахождение p-value для нормального распределения
# lower.tail = false берем, потому что нам нужна сумма правых и левых хвостов для двустороннего теста
z_stat <- (sample_mean - hypothetical_mean) / (std / sqrt(n))
p_value_norm <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
cat("P-value для нормального распределения:", p_value_norm, "\n")
cat(if (p_value_norm >= alpha) "Гипотеза не может быть отклонена\n" else "Можно отклонить гипотезу\n")

# Дополнительный анализ для распределения хи-квадрат

# Критическое значение для хи-квадрат распределения
# Используем критическое значение, чтобы узнать область, в которой нулевая гипотеза будет отвергнута
chi_critical <- qchisq(1 - alpha, df = n - 1)

# Доверительный интервал для хи-квадрат
lower_bound_chi <- sample_mean - chi_critical * std / sqrt(n)
upper_bound_chi <- sample_mean + chi_critical * std / sqrt(n)

# Вывод для хи-квадрат
cat("Границы доверительного интервала для хи-квадрат распределения: [", lower_bound_chi, ":", upper_bound_chi, "]\n", sep = "")
cat(if (lower_bound_chi <= hypothetical_mean && hypothetical_mean <= upper_bound_chi) 
  "С 95% вероятностью среднее значение генеральной совокупности находится в интервале [lower_bound_chi:upper_bound_chi], гипотеза верна\n" 
  else 
    "Гипотеза неверна\n"
)

# Нахождение p-value для хи-квадрат распределения
# lower.tail = false берем, потому что нам нужна сумма правых и левых хвостов для двустороннего теста
chi_stat <- (sample_mean - hypothetical_mean) / (std / sqrt(n))
p_value_chi <- 2 * pchisq(abs(chi_stat), df = n - 1, lower.tail = FALSE)
cat("P-value для хи-квадрат распределения:", p_value_chi, "\n")
cat(if (p_value_chi >= alpha) "Гипотеза не может быть отклонена\n" else "Можно отклонить гипотезу\n")

