library(ggplot2)

factors <- read.delim("data/headers.txt")
full_design <- FrF2(nfactors = 7, replicate = 1, randomize = TRUE, factor.names = factors)

full_design$Y <- rnorm(nrow(full_design), mean = 50, sd = 10)

summary(full_design$Y)

# Histogram
histogram_plot <- ggplot(full_design, aes(x = Y)) + 
  geom_histogram(binwidth = 2, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  ggtitle("Response (Y)")
print(histogram_plot)

# Boxplots 
for (factor in factors) {
  boxplot_plot <- ggplot(full_design, aes_string(x = factor, y = "Y")) + 
    geom_boxplot(aes(fill = factor)) + 
    theme_minimal() + 
    ggtitle(paste("Box of", factor, "vs. Response (Y)"))
  print(boxplot_plot)
}

# Linear model to analyze main effects and interactions
model <- lm(Y ~ .^7, data = full_design)  
summary(model)

# Check residuals
par(mfrow=c(2,2))
plot(model)

plot(model)
