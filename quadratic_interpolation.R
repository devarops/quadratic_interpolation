library(ggplot2)
library(testthat)

interpCuad <- function(x, y, xq) {
  yq <- numeric(0)
  n <- length(x)

  # Formula de tres puntos adelantada
  i <- 1
  L1 <- function(t) (t - x[i + 1]) / (x[i] - x[i + 1])
  L2 <- function(t) (t - x[i]) / (x[i + 1] - x[i])
  h <- (x[i + 1] - x[i])
  m123 <- (-3 * y[i] + 4 * y[i + 1] - y[i + 2]) / (2 * h)
  m13 <- (y[i + 2] - y[i]) / (2 * h)
  y1 <- function(t) m123 * (t - x[i]) + y[i]
  y2 <- function(t) m13 * (t - x[i + 1]) + y[i + 1]
  p <- function(x) L1(x) * y1(x) + L2(x) * y2(x)
  ind <- (x[i] <= xq) & (xq < x[i + 1])
  t <- xq[ind]
  yq <- c(yq, p(t))

  # Ciclo que corre la interpolacion para todos los valores de x(i) cuando i = 1 hasta n - 3
  for (i in 1:(n - 3)) {
    L1 <- function(t) (t - x[i + 2]) / (x[i + 1] - x[i + 2])
    L2 <- function(t) (t - x[i + 1]) / (x[i + 2] - x[i + 1])
    h <- (x[i + 1] - x[i])
    m13 <- (y[i + 2] - y[i]) / (2 * h)
    m24 <- (y[i + 3] - y[i + 1]) / (2 * h)
    y1 <- function(t) m13 * (t - x[i + 1]) + y[i + 1]
    y2 <- function(t) m24 * (t - x[i + 2]) + y[i + 2]
    p <- function(x) L1(x) * y1(x) + L2(x) * y2(x)
    ind <- (x[i + 1] <= xq) & (xq < x[i + 2])
    t <- xq[ind]
    yq <- c(yq, p(t))
  }

  # Formula de tres puntos atrasada
  i <- n - 2
  L1 <- function(t) (t - x[i + 2]) / (x[i + 1] - x[i + 2])
  L2 <- function(t) (t - x[i + 1]) / (x[i + 2] - x[i + 1])
  h <- (x[i + 2] - x[i + 1])
  m123 <- (y[i] - 4 * y[i + 1] + 3 * y[i + 2]) / (2 * h)
  m13 <- (y[i + 2] - y[i]) / (2 * h)
  y1 <- function(t) m13 * (t - x[i + 1]) + y[i + 1]
  y2 <- function(t) m123 * (t - x[i + 2]) + y[i + 2]
  p <- function(x) L1(x) * y1(x) + L2(x) * y2(x)
  ind <- (x[i + 1] <= xq) & (xq <= x[i + 2])
  t <- xq[ind]
  yq <- c(yq, p(t))

  return(yq)
}

# Set up plot labels and parameters
xlabel <- 'Eje X'
ylabel <- 'Eje Y'
min_x <- 0
max_x <- 2 * pi

# Define points
x <- seq(min_x, max_x, length.out = 4)
xq <- seq(min_x, max_x, length.out = 16)

y <- sin(x) # Function is sine of x

# Graph domain (illustrative only)
x_grafica <- seq(min_x, max_x, length.out = 1000)

# Create a data frame for ggplot
plot_data <- data.frame(
  x_grafica = x_grafica,
  sin_x_grafica = sin(x_grafica)
)

# Perform quadratic interpolation
y_interpolados <- interpCuad(x, y, xq)

# Perform linear interpolation
y_lineal <- approx(x, y, xq)$y

# Calculate RMS errors
y_verdadera <- sin(xq)
errorRMSlineal <- sqrt(mean((y_lineal - y_verdadera)^2))
errorRMSinterpolado <- sqrt(mean((y_interpolados - y_verdadera)^2))

# Print RMS errors
cat("RMS Error for Linear Interpolation: ", errorRMSlineal, "\n")
cat("RMS Error for Quadratic Interpolation: ", errorRMSinterpolado, "\n")

# Verify results are consistent with the previous MATLAB version
expect_equal(errorRMSlineal, 0.24873492, tolerance = 1e-8) # Linear interpolation error is consistent with the previous MATLAB version
expect_equal(errorRMSinterpolado, 0.13246188, tolerance = 1e-8) # Quadratic interpolation error is consistent with the previous MATLAB version

# Plot continuous graph (illustrative)
graphics.off()
plt <- ggplot(plot_data, aes(x = x_grafica, y = sin_x_grafica)) +
  geom_line(color = "black") +
  labs(x = xlabel, y = ylabel) +
  theme(text = element_text(size = 13))

# Create a data frame for plotting interpolated values
interp_data <- data.frame(
  xq = xq,
  y_interpolados = y_interpolados,
  y_lineal = y_lineal
)

# Plot data points
plt <- plt + geom_point(data = data.frame(x = x, y = y), aes(x = x, y = y), color = "blue", size = 3)

# Plot quadratic interpolation in blue
plt <- plt + geom_line(data = interp_data, aes(x = xq, y = y_interpolados), color = "blue", linetype = "dashed", size = 1) +
  geom_point(data = interp_data, aes(x = xq, y = y_interpolados), color = "blue", shape = 8, size = 3)

# Plot linear interpolation in red
plt <- plt + geom_line(data = interp_data, aes(x = xq, y = y_lineal), color = "red", linetype = "dotted", size = 1) +
  geom_point(data = interp_data, aes(x = xq, y = y_lineal), color = "red", shape = 4, size = 3)

# Show the plot
print(plt)

