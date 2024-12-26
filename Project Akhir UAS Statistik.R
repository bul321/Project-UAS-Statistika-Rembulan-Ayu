set.seed(123)
n <- 100
BelanjaOnline <- rnorm(n, mean = 200, sd = 50)  # Variabel independen (dalam ribuan rupiah)
BelanjaOffline <- 100 + 0.7 * BelanjaOnline + rnorm(n, mean = 0, sd = 30)  # Variabel dependen

# Membuat dataframe
data <- data.frame(BelanjaOnline, BelanjaOffline)

# Melihat data awal
head(data)

# Model regresi linear
model <- lm(BelanjaOffline ~ BelanjaOnline, data = data)

# 2.1 Uji Normalitas Residual
shapiro_test <- shapiro.test(residuals(model))

# 2.2 Uji Homoskedastisitas
library(lmtest)
bptest_test <- bptest(model)  # Uji Breusch-Pagan

# 2.3 Uji Linearitas
library(car)
crPlots(model)  # Plot untuk mengecek linearitas

# Menampilkan ringkasan model
summary_model <- summary(model)
summary_model

library(ggplot2)

# Scatter plot dengan garis regresi
scatter_plot <- ggplot(data, aes(x = BelanjaOnline, y = BelanjaOffline)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Hubungan Belanja Online dan Offline",
       x = "Belanja Online (Ribu Rupiah)",
       y = "Belanja Offline (Ribu Rupiah)") +
  theme_minimal()

# Residual plot
residual_plot <- ggplot(data, aes(x = model$fitted.values, y = model$residuals)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Plot Residual",
       x = "Nilai Prediksi",
       y = "Residual") +
  theme_minimal()

# Menampilkan visualisasi
print(scatter_plot)
print(residual_plot)

interpretasi <- list(
  "Koefisien Regresi" = summary_model$coefficients,
  "R-Squared" = summary_model$r.squared,
  "Asumsi Normalitas" = shapiro_test,
  "Asumsi Homoskedastisitas" = bptest_test,
  "Asumsi Linearitas" = "Lihat Partial Residual Plots"
)
interpretasi

