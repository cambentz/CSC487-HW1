# Question 1
# (1a) Read file into a dataframe variable
su <- read.delim("Su_raw_matrix.txt")

# (1b) Calculate mean and standard deviation of Liver_2 column
mean_liver_2 <- mean(su$Liver_2.CEL, na.rm = TRUE)
sd_liver_2 <- sd(su$Liver_2.CEL, na.rm = TRUE)

# (1c) Calculate column averages and totals
col_averages <- colMeans(su, na.rm = TRUE)
col_totals <- colSums(su, na.rm = TRUE)

# Question one test functions
#print(head(su))
#cat("Mean:", mean_liver_2, 
#      "\nStd Dev:", sd_liver_2, 
#      "\nCol Avgs:", col_averages,
#      "\nCol Totals:", col_totals)

# Question 2
# (2a) Generate and plot mean = 0, sigma = 0.2
data_q2a <- rnorm(10000, mean = 0, sd = 0.2)
hist(data_q2a, main = "Histogram Q2a (mean = 0, sigma = 0.2)", xlim = c(-5,5))

# (2b) Generate and plot mean = 0, sigma = 0.5
data_q2b <- rnorm(10000, mean = 0, sd = 0.5)
hist(data_q2b, main = "Histogram Q2b (mean = 0, sigma = 0.5)", xlim = c(-5,5))

# Question 3
# (3a) Create test dataframe
dat <- data.frame(cond = factor(rep(c("A", "B"), each = 200)),
                  rating = c(rnorm(200), rnorm(200, mean = 0.8)))

# (3b) Overlaid histogram
library(ggplot2)
ggplot(dat, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = 0.5, alpha = 0.5, position = "identity")

# (3c) Interleaved histogram
ggplot(dat, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = 0.5, position = "dodge")

# (3d) Density plot
ggplot(dat, aes(x = rating, colour = cond)) +
  geom_density()

# (3e) Density plot w/ semitransparent fill
ggplot(dat, aes(x = rating, fill = cond)) +
  geom_density(alpha = 0.3)

# (f) Create diabetes dataframe and use ggplot
diabetes <- read.csv("diabetes_train.csv")

# Test function
print(head(diabetes))

# Overlaid diabetes histogram
ggplot(diabetes, aes(x = mass, fill = class)) + 
  geom_histogram(binwidth = 0.5, alpha = 0.5, position = "identity")

# Interleaved diabetes histogram
ggplot(diabetes, aes(x = mass, fill = class)) +
  geom_histogram(binwidth = 0.5, position = "dodge")

# Diabetes density plot
ggplot(diabetes, aes(x = mass, colour = class)) +
  geom_density()

# Diabetes density plot w/ semitransparent fill
ggplot(diabetes, aes(x = mass, fill = class)) +
  geom_density(alpha = 0.3)

# Question 4
# Create dataframe
passengers <- read.csv("titanic.csv")

# Test functions
#print(head(passengers))
#colnames(passengers)

# (4a) Drop na values and summarize
library(tidyr)
library(dplyr)
passengers %>% drop_na() %>% summary()

# (4b) Filter male passengers
passengers %>% filter(Sex == "male")

# (4c) Sort by fare
passengers %>% arrange(desc(Fare))

# (4d) Create family size column
passengers <- passengers %>% mutate(FamSize = Parch + SibSp)

# (4e) Group and summarize
passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))


# Question 5
# (5) Calculate 10,30,50,60 percentiles of skinn attributes for diabetes data
quantile(diabetes$skin, probs = c(0.1, 0.3, 0.5, 0.6), na.rm = TRUE)
