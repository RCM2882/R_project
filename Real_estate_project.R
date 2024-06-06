real_estate <-  read.csv("RealEstateUnitedStates.csv")
View(real_estate)
summary(real_estate)
hist(real_estate$Average.Sales.Price, main = "Distribution of Prices", xlab = "Price")
ggplot(data = real_estate, mapping = aes(x = Home.Size, y = Average.Sales.Price)) +
  geom_boxplot() +
  labs(x = "Home Size", y = "Average Sales Price") +
  ggtitle("Boxplot of Average Sales Price vs Home Size")
ggplot(data = real_estate, mapping = aes(x = Home.Size, y = Average.Sales.Price)) +
  geom_point() +
  labs(x = "Home Size", y = "Average Sales Price") +
  ggtitle("Scatter Plot of Home Size vs. Average Sales Price")
ggplot(data = real_estate, mapping = aes(x = Home.Size, y = Median.Income...Current.Dollars)) +
  geom_point() +
  labs(x = "Home Size", y = "Median Income") +
  ggtitle("Scatter Plot of Home Size vs. Median Income")
ggplot(data = real_estate, mapping = aes(x = Home.Size, y = Mean.Income...Current.Dollars)) +
  geom_point() +
  labs(x = "Home Size", y = "Mean Income") +
  ggtitle("Scatter Plot of Home Size vs. Mean Income")
ggplot(data = real_estate, mapping = aes(x = Home.Size)) +
  geom_bar(bins = 20, fill = "yellow", color = "red") +
  labs(x = "Size of Home", y = "Frequency") +
  ggtitle("Histogram of Home Size")
class(real_estate$Home.Size)
real_estate$Home.Size <- as.numeric(real_estate$Home.Size)
unique(real_estate$Home.Size)
ggplot(data = real_estate, mapping = aes(x = Average.Sales.Price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "yellow") +
  labs(x = "Average Sales Price", y = "Frequency") +
  ggtitle("Histogram of Average Sales Price")
ggplot(data = real_estate, mapping = aes(x = Median.Income...Current.Dollars)) +
  geom_histogram(bins = 25, fill = "green", color = "black") +
  labs(x = "Median Income", y = "Frequency") +
  ggtitle("Histogram of Median Income")
ggplot(data = real_estate, mapping = aes(x = Mean.Income...Current.Dollars)) +
  geom_histogram(bins = 25, fill = "yellow", color = "purple") +
  labs(x = "Mean Income", y = "Frequency") +
  ggtitle("Histogram of Mean Income")
ggplot(data = real_estate, mapping = aes(y = Home.Size)) +
  geom_boxplot(fill = "orange", color = "blue") +
  labs(y = "Home Size") +
  ggtitle("Boxplot of Home Size")
sum(!is.finite(real_estate$Home.Size))
median_home_size <- median(real_estate$Home.Size, na.rm = TRUE)
real_estate$Home.Size[!is.finite(real_estate$Home.Size)] <- median_home_size
ggplot(data = real_estate, mapping = aes(y = Home.Size)) +
  geom_boxplot(fill = "orange", color = "blue") +
  labs(y = "Home Size") +
  ggtitle("Boxplot of Home Size")
ggplot(data = real_estate, mapping = aes(y = Average.Sales.Price)) +
  geom_boxplot(fill = "blue", color = "orange") +
  labs(y = "Average Sales Price") +
  ggtitle("Boxplot of Average Sales Price")
ggplot(data = real_estate, mapping = aes(y = Median.Income...Current.Dollars)) +
  geom_boxplot(fill = "purple", color = "darkgreen") +
  labs(y = "Median Income") +
  ggtitle("Boxplot of Median Income")
ggplot(data = real_estate, mapping = aes(y = Mean.Income...Current.Dollars)) +
  geom_boxplot(fill = "darkblue", color = "yellow") +
  labs(y = "Mean Income") +
  ggtitle("Boxplot of Mean Income")
real_estate <-  read.csv("RealEstateUnitedStates.csv")
ggplot(data = real_estate, mapping = aes(x = Home.Size, y = Median.Income...Current.Dollars)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "orange")
  labs(x = "Home Size", y = "Median Income") +
  ggtitle("Scatter Plot of Home Size vs. Median Income")
  income_quartiles <- quantile(real_estate$Median.Income...Current.Dollars, probs = c(0.75))
  highest_income <- real_estate[real_estate$Median.Income...Current.Dollars >= income_quartiles, ]
  ggplot(data = highest_income, mapping = aes(x = Home.Size, y = Median.Income...Current.Dollars, color = Region)) +
    geom_point() +
    labs(x = "Home Size", y = "Median Income", color = "Region") +
    ggtitle("Typical Home Size Purchase for People with Highest Incomes (Top Quartile)")
  