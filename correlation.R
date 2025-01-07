# Set working directory
cwd <- getwd()

setwd(cwd)

# Read data
data <- read.csv("Amazon.csv")

data$rating_count <- as.numeric(gsub(",", "", data$rating_count))
data$rating <- as.numeric(data$rating)

# Filter data to remove NA values
filtered_data <- na.omit(data[, c("rating_count", "rating")])

x = filtered_data$rating_count
y = filtered_data$rating

# Visualization: scatter plot
png("Scatter_plot.png", width = 480, height = 480)
plot(x, y,
     col = "lightblue", 
     xlab = "Rating count of product", 
     ylab = "Rating of prodcut", 
     main = "Impact of Rating count on Amazon product rating")

abline(lm(y ~ x), col = "red", lwd = 3)

#adding legend 
legend("topleft", legend = c("Linear fit line"),
       lwd = 3, lty = c(1, 2, 1), col = c("red"))


dev.off()

# Visualization: Histogram with KDE
png("Histogram.png", width = 480, height = 480)
hist(filtered_data$rating, 
     breaks = 20, 
     col = "pink", 
     xlab = "Rating", 
     ylab = "Frequency", 
     main = "Distribution of Amazon product ratings",
     freq = FALSE) # Plot density instead of frequency


xfit <- seq(min(y), max(y), length = 120)
yfit <- dnorm(xfit, mean = mean(y), sd = sd(y))
lines(xfit, yfit, col = "blue")

legend("topright", legend = c('Product Ratings', "Normal distribution overlay"),
       lwd = 3, lty = c(0, 1), col = c('pink', "blue"), 
       pch = c(22, NA), cex = 1, pt.cex = 1.3, pt.bg = "white")
dev.off()

# Statistical Analysis: Normality Test
#correlation Test
print(cor.test(filtered_data$rating_count, 
               filtered_data$rating, 
               method="pearson"))
