# I import ggplot2
library(ggplot2)

# I chose iris dataset to explore
str(iris)
# There are 150 observations of 5 variables
# 4 of the variables are continuous, interval, numerical/quantitative
# 1 of the variables is nominal, categorical/qualitative

# How many individuals are there in the dataset?
ggplot(data = iris) +
  geom_bar(mapping = aes(x = Species, fill = Species), alpha = 0.7) +
  labs(title = "Sample Count by Iris Species", 
       subtitle = "The amount of each three flower species in the dataset",
       caption = "Sample including 150 individuals",
       fill = "Iris Species") +
  ylab(label = "Sample Count") +
  xlab(label = "Iris species") +
  theme_minimal() +
  theme(legend.position = "left",
        axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))
# There are 150 individuals grouped by three different species.

# Checking the distribution of the flower sepal length for the whole dataset (use histograms, is it normal distribution?)
ggplot(data = iris) +
  geom_histogram(mapping = aes(x = Sepal.Length, fill = Species), alpha = 0.7) +
  labs(title = "Distribution of Iris Sepal Length", 
       subtitle = "Distributuion of sepal length of the whole dataset across three species",
       caption = "Sample including 150 individuals") +
  ylab(label = "Sample Count") +
  xlab(label = "Sepal Length") +
  theme_minimal() +
  theme(legend.position = "left",
        axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))
# What kind of distribution is this?

# Checking the distribution of the flower sepal width for the whole dataset (use histograms, is it normal distribution?)
ggplot(data = iris) +
  geom_histogram(mapping = aes(x = Sepal.Width, fill = Species), alpha = 0.7) +
  labs(title = "Distribution of Iris Sepal Width", 
       subtitle = "Distributuion of sepal width of the whole dataset across three species",
       caption = "Sample including 150 individuals") +
  ylab(label = "Sample Count") +
  xlab(label = "Sepal Width") +
  theme_minimal() +
  theme(legend.position = "left",
        axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))
# What kind of distribution is this?

# Checking the distribution of the flower petal length for the whole dataset (use histograms, is it normal distribution?)
ggplot(data = iris) +
  geom_histogram(mapping = aes(x = Petal.Length, , fill = Species), alpha = 0.7) +
  labs(title = "Distribution of Iris Petal Length", 
       subtitle = "Distributuion of petal length of the whole dataset across three species",
       caption = "Sample including 150 individuals") +
  ylab(label = "Sample Count") +
  xlab(label = "Petal Length") +
  theme_minimal() +
  theme(legend.position = "left",
        axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))
# What kind of distribution is this?

# Checking the distribution of the flower petal width for the whole dataset (use histograms, is it normal distribution?)
ggplot(data = iris) +
  geom_histogram(mapping = aes(x = Petal.Width, fill = Species), alpha = 0.7) +
  labs(title = "Distribution of Iris Petal Width", 
       subtitle = "Distributuion of petal width of the whole dataset across three species",
       caption = "Sample including 150 individuals") +
  ylab(label = "Sample Count") +
  xlab(label = "Petal Width") +
  theme_minimal() +
  theme(legend.position = "left",
        axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))
# What kind of distribution is this?

# correlation between petal length and petal width across all three species
ggplot(data = iris) +
  geom_jitter(mapping = aes(x = Petal.Width, y = Petal.Length, col = Species), alpha = 0.7) +
  geom_smooth(mapping = aes(x = Petal.Width, y = Petal.Length), method = lm) + # this adds the trend line
  labs(title = "Correlation between Petal Width and Petal Length", 
       subtitle = "Correlation between the length and width of the petal of the whole dataset across three species",
       caption = "Sample including 150 individuals") +
  ylab(label = "Petal Length") +
  xlab(label = "Petal Width") +
  theme_minimal() +
  theme(legend.position = "left",
        axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# modelling with lm
# 1. we create a model 
iris_model <- lm(iris$Petal.Length ~ iris$Petal.Width)

# 2. we plot it
plot(y = iris$Petal.Length, x = iris$Petal.Width)

# 3. we get the intercept and slope by coef() function
e <- coef(iris_model)[1]
f <- coef(iris_model)[2]
e
f

# 4. we draw the line with help of abline() function
abline(e, f, lty = 4)

summary(iris_model)



# I want to separate the dataset by the specie to explore them separately 
# setosa dataset
my_setosa <- subset(iris, Species == "setosa")
str(my_setosa)
# versicolor dataset
my_versicolor <- subset(iris, Species == "versicolor")
str(my_versicolor)
# virginica dataset
my_virginica <- subset(iris, Species == "virginica")
str(my_virginica)

# setosa dataset: what is the distribution of sepal length? NORMAL DISTRIBUTION
ggplot(data = my_setosa) +
  geom_histogram(aes (x = Sepal.Length), fill = "red", alpha = 0.7) +
  labs(title = "Setosa: Distribution of Sepal Length", 
       subtitle = "Distributuion of sepal length of Iris Setosa",
       caption = "Setosa sample including 50 individuals") +
  ylab(label = "Setosa Count") +
  xlab(label = "Setosa Sepal Length") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# setosa dataset: what is the distribution of sepal width? RIGHT SKEWED
ggplot(data = my_setosa) +
  geom_histogram(aes (x = Sepal.Width), fill = "red", alpha = 0.7) +
  labs(title = "Setosa: Distribution of Sepal Width", 
       subtitle = "Distributuion of sepal width of Iris Setosa",
       caption = "Setosa sample including 50 individuals") +
  ylab(label = "Setosa Count") +
  xlab(label = "Setosa Sepal Width") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))


# setosa dataset: what is the distribution of petal length? NORMAL
# mean and median added
ggplot(data = my_setosa) +
  geom_histogram(aes (x = Petal.Length), fill = "red", alpha = 0.7) +
  geom_vline(xintercept = mean(my_setosa$Petal.Length), col = "blue", lwd = 1) +
  geom_vline(xintercept = median(my_setosa$Petal.Length), col = "green", lwd = 1) +
  annotate("text", x = 1.35,
           y = 9,
           label = paste("Mean =", mean(my_setosa$Petal.Length)),
           col = "blue",
           size = 5) +
  annotate("text", x = 1.6,
           y = 9,
           label = paste("Median =", median(my_setosa$Petal.Length)),
           col = "green",
           size = 5) +
  labs(title = "Mean and Median on Distribution of Iris Satosa Petal Length", 
       subtitle = "Distributuion of petal length of Iris Setosa with mean and median values",
       caption = "Setosa sample including 50 individuals") +
  ylab(label = "Setosa Count") +
  xlab(label = "Setosa Petal Length (cm)") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# setosa dataset: what is the distribution of petal width? RIGHT SKEWED
# mean and median added
ggplot(data = my_setosa) +
  geom_histogram(aes (x = Petal.Width), fill = "red", alpha = 0.7) +
  geom_vline(xintercept = mean(my_setosa$Petal.Width), col = "blue", lwd = 1) +
  geom_vline(xintercept = median(my_setosa$Petal.Width), col = "green", lwd = 1) +
  annotate("text", x = 0.3,
           y = 11,
           label = paste("Mean =", mean(my_setosa$Petal.Width)),
           col = "blue",
           size = 5) +
  annotate("text", x = 0.15,
           y = 11,
           label = paste("Median =", median(my_setosa$Petal.Width)),
           col = "green",
           size = 5) +
  labs(title = "Mean and Median on Distribution of Iris Satosa Petal Width", 
       subtitle = "Distributuion of petal width of Iris Setosa with mean and median values",
       caption = "Setosa sample including 50 individuals") +
  ylab(label = "Setosa Count") +
  xlab(label = "Setosa Petal Width (cm)") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))


# versicolor dataset: what is the distribution of sepal length? RIGHT SKEWED
ggplot(data = my_versicolor) +
  geom_histogram(aes (x = Sepal.Length), fill = "green3", alpha = 0.7) +
  labs(title = "Versicolor: Distribution of Sepal Length", 
       subtitle = "Distributuion of sepal length of Iris Versicolor",
       caption = "Versicolor sample including 50 individuals") +
  ylab(label = "Versicolor Count") +
  xlab(label = "Versicolor Sepal Length") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# versicolor dataset: what is the distribution of sepal width? LEFT SKEWED
ggplot(data = my_versicolor) +
  geom_histogram(aes (x = Sepal.Width), fill = "green3", alpha = 0.7) +
  labs(title = "Versicolor: Distribution of Sepal Width", 
       subtitle = "Distributuion of sepal width of Iris Versicolor",
       caption = "Versicolor sample including 50 individuals") +
  ylab(label = "Versicolor Count") +
  xlab(label = "Versicolor Sepal Width") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# versicolor dataset: what is the distribution of petal length? LEFT SKEWED
ggplot(data = my_versicolor) +
  geom_histogram(aes (x = Petal.Length), fill = "green3", alpha = 0.7) +
  labs(title = "Versicolor: Distribution of Petal Length", 
       subtitle = "Distributuion of petal length of Iris Versicolor",
       caption = "Versicolor sample including 50 individuals") +
  ylab(label = "Versicolor Count") +
  xlab(label = "Versicolor Petal Length") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# versicolor dataset: what is the distribution of petal width?
ggplot(data = my_versicolor) +
  geom_histogram(aes (x = Petal.Width), fill = "green3", alpha = 0.7) +
  labs(title = "Versicolor: Distribution of Petal Width", 
       subtitle = "Distributuion of petal width of Iris Versicolor",
       caption = "Versicolor sample including 50 individuals") +
  ylab(label = "Versicolor Count") +
  xlab(label = "Versicolor Petal Width") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# virginica dataset: what is the distribution of sepal length?
ggplot(data = my_virginica) +
  geom_histogram(aes (x = Sepal.Length), fill = "lightblue", alpha = 0.7) +
  labs(title = "Virginica: Distribution of Sepal Length", 
       subtitle = "Distributuion of sepal length of Iris Virginica",
       caption = "Virginica sample including 50 individuals") +
  ylab(label = "Virginica Count") +
  xlab(label = "Virginica Sepal Length") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# virginica dataset: what is the distribution of sepal width?
ggplot(data = my_virginica) +
  geom_histogram(aes (x = Sepal.Width), fill = "lightblue", alpha = 0.7) +
  labs(title = "Virginica: Distribution of Sepal Width", 
       subtitle = "Distributuion of sepal width of Iris Virginica",
       caption = "Virginica sample including 50 individuals") +
  ylab(label = "Virginica Count") +
  xlab(label = "Virginica Sepal Width") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# virginica dataset: what is the distribution of petal length?
ggplot(data = my_virginica) +
  geom_histogram(aes (x = Petal.Length), fill = "lightblue", alpha = 0.7) +
  labs(title = "Virginica: Distribution of Petal Length", 
       subtitle = "Distributuion of petal length of Iris Virginica",
       caption = "Virginica sample including 50 individuals") +
  ylab(label = "Virginica Count") +
  xlab(label = "Virginica Petal Length") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# virginica dataset: what is the distribution of petal width?
ggplot(data = my_virginica) +
  geom_histogram(aes (x = Petal.Width), fill = "lightblue", alpha = 0.7) +
  labs(title = "Virginica: Distribution of Petal Width", 
       subtitle = "Distributuion of petal width of Iris Virginica",
       caption = "Virginica sample including 50 individuals") +
  ylab(label = "Virginica Count") +
  xlab(label = "Virginica Petal Width") +
  theme_minimal() +
  theme(axis.line = element_line(color = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        plot.title = element_text(face = "bold"))

# Checking the median and mean of the variables
# mean
# my_setosa
mean(my_setosa$Sepal.Length)
mean(my_setosa$Sepal.Width)
mean(my_setosa$Petal.Length)
mean(my_setosa$Petal.Width)
# my_versicolor
mean(my_versicolor$Sepal.Length)
mean(my_versicolor$Sepal.Width)
mean(my_versicolor$Petal.Length)
mean(my_versicolor$Petal.Width)
# my_viginica
mean(my_virginica$Sepal.Length)
mean(my_virginica$Sepal.Width)
mean(my_virginica$Petal.Length)
mean(my_virginica$Petal.Width)

# Adding mean to histogram with base R
hist(my_setosa$Petal.Length)                                   # Draw histogram
abline(v = mean(my_setosa$Petal.Length),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = 1.7,
     y = 8,
     paste("Mean =", mean(my_setosa$Petal.Length)),
     col = "red",
     cex = 2)

# Calculating the mean of petal width for my_setosa
mean(my_setosa$Petal.Width)

# Adding mean value to histogram with base R
hist(my_setosa$Petal.Width)                                   # Draw histogram
abline(v = mean(my_setosa$Petal.Width),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = 0.34,
     y = 10,
     paste("Mean =", mean(my_setosa$Petal.Width)),
     col = "red",
     cex = 2)

# Checking the mode 
# There is no built in function to find mode in R, so we write one
mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# my_setosa
mode(my_setosa$Sepal.Length)
mode(my_setosa$Sepal.Width)
mode(my_setosa$Petal.Length)
mode(my_setosa$Petal.Width)
# my_versicolor
mode(my_versicolor$Sepal.Length)
mode(my_versicolor$Sepal.Width)
mode(my_versicolor$Petal.Length)
mode(my_versicolor$Petal.Width)
# my_viginica
mode(my_virginica$Sepal.Length)
mode(my_virginica$Sepal.Width)
mode(my_virginica$Petal.Length)
mode(my_virginica$Petal.Width)



# Calculations variance and sd for setosa 
variance <- (my_setosa$Petal.Length - mean(my_setosa$Petal.Length))^2
variance
variance <- sum(variance) / (length(my_setosa$Petal.Length) - 1)
variance

sd <- sqrt(variance)
sd

# Calculation coefficient of variation
cv <- (sd / mean(my_setosa$Petal.Length)) 
cv

setosa_model_petal <- lm(my_setosa$Petal.Length ~ my_setosa$Petal.Width)

a <- coef(setosa_model_sepal)[1]
b <- coef(setosa_model_sepal)[2]
a
b

# z-score of setosa petal length when x=1.4
z <- (1.4 - mean(my_setosa$Petal.Length))/sd(my_setosa$Petal.Length)
# a calculation with the number values
control_z <- (1.4 - 1.462)/0.173664
control_z

