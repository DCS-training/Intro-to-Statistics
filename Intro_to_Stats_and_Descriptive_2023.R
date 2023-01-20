# Intro to Stats and Descriptive Stats
# Part 1- Intro to Stats
# This section will be covered in a power Point presentation

# Part 2- Stats in R
# R is a very powerful tool for statistical analysis and data analysis more broadly.
# We will primarily be working with the data set 'painters', which is included in the MASS package by default in R/RStudio. See also this tutorial browseURL("http://www.r-tutor.com/elementary-statistics")

library(tidyverse)
library(MASS)
head(painters)

# This dataset contains some data on 18th century painters.
# https://r-data.pmagunia.com/dataset/r-dataset-package-mass-painters

# #### Qualitative Data ####

painters = as.data.frame(painters)
painters$School # The school variable denotes the School to which the painter belonged, represented by letters A-H. These are factor type data, and can also be considered categorical or qualitative.

# ==== Frequency ====

# Instead of just looking at the painters and which school they belonged to, we can use descriptive statistics to better understand and visualise this. The frequency distribution of qualitative data such as this, can provide a count of how many variable are within a certain category, in this case, how many painters are in each school.

table(painters$School) # The table function in base R allows for a quick representation of the frequency distribution. The input is the categorical variable we want to determine the frequency distribution of. 
# We can also visualise this in column format, using the cbind function. 
cbind(table(painters$School))

# We can also visualise this using plots.
plot(table(painters$School))
barplot(table(painters$School))

ggplot(painters, aes(x=School, fill = School)) + geom_bar() # Consult the ggplot2() package for more information on this. ggplot() is a standard for data visualisation.

# We can also check the relative frequency distribution. This is essentially distributing the frequency as a proportion, or a percentage. It is achieved simply by dividing the frequency by the total sample size (times this output by 100 for a percentage).
freq.school <- table(painters$School)

cbind(freq.school/nrow(painters))*100 # nrow() retruns the number of rows in a given dataset, in this case, the total number of painters. This is the exact same as:

cbind(freq.school/54)*100
nrow(painters) # This returns 54.

# We can also control the number of decimal points displayed in R using the round function.

round((freq.school/54)*100) # Round to no decimal places.
round((freq.school/54)*100,2) # Round to 2 decimal places. The second argument in round dictates the number of decimal places.

barplot(round((freq.school/54)*100))

# ==== Central Tendency ====
# We can use the central tendency to further understand the painters data.
# Say we want to compare the painters composition scores from schools A and D. We can use the central tendency, and the mean() function to get an idea of which school produced better composition painters on average.
mean(painters[painters$School=="A",]$Composition) # mean() takes numerical vectors as an input and calculates the mean from them.painters[painters$School=="A",] subsets painters so only rows where School is A are used. 
mean(painters[painters$School=="D",]$Composition)

# With this, we can see that painters from School A, on average, have a higher composition score than those from School D.

# tapply() can be used to apply a function to multiple objects and variables.
tapply(painters$Composition, painters$School, mean) # tapply() usually takes three arguments, first the variable you want to apply the function to (painters$Composition), and then the variable to group the first by (painters$School) and finally the function you want to apply to the data (mean). See browseURL("https://r-coder.com/tapply-r/")

# Try visualising this with plot functions.

# We can also extract the highest value from this using the max() function.
max(tapply(painters$Composition, painters$School, mean))

# The median can be calculated using the functions median().
median(painters[painters$School=="D",]$Drawing)

# Try to compare the means of all schools for a chosen variable.

# The mode is slightly more complicated, as R does not have a built in function to calculate the mode. Packages like "DescTools" have mode functions, but it is easy to create a user defined function to calculate the mode.
my_mode <- function(x) {                     # Create mode function 
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

# The mode is just the most commonly occurring value, so this function just outputs the unique value with the most occurrences in a data frame.

my_mode(painters$Expression)
# This isn't a particularly meaningful result, but the mode can also be applied to categorical values, not only numerical. 

my_mode(painters$School) # Here we can see that A and D are the most common schools, which we visualised above with the plots. 

# ==== Variability ====
#We can also use R to calculate the variability of data. Standard Deviation and Variance are straightforward, just the functions sd() and var() respectively.

sd(painters$Expression)

painters %>%
  group_by(School) %>%
  summarise(Variance = var(Colour)) # We can also use pipes (%>%) and the group_by() and summarise() functions in the dplyr package, part of the tidyverse. Try applying this to other variables or checking different statistics, this can be applied to any of the descriptive stats we've looked at so far, whether you use group_by(), or tapply(), or some other method, is up to you!

painters %>%
  group_by(School) %>%
  summarise(Range = range(Colour)) # Note that with range, two outputs are given for each variable, the minimum and maximum. This can be easier than using max() and min(). But this will depend what statistics you actually want to see or visualise. 

# #### Generate Random Data ####
# Often in statistics it is helpful to have random data to compare any real data you are testing. These are also the methods I used to generate the 'height' data and dice rolls/coin flips for the powerpoint.

# ==== Normal Distribution ====
# rnorm() generates data that is normally distributed, ie., has most data close to the mean, with long tails that have far fewer occurrences.
# rnorm() takes three main inputs, the number of occurrences, the mean and the standard deviation of the data. For example:
rnorm(500,171,10) # 500 values where the mean value is 171 and the sd 10.
# We can convert this into a dataframe to plot it using ggplot and visualise the data to see if it is indeed normally distributed.

height <- data.frame(rnorm(500,171,10))
names(height) <- "Height" # Change the name to something sensible. 
ggplot(height, aes(x=Height)) + geom_histogram()
ggplot(height, aes(x=Height)) + geom_density()

# Note that these are not perfectly normally distributed, and the exact values each of you get will be different. This is due to the randomness, or stochasticity built into R.

# Try with your own inputs.

# ==== Uniform Data ====
# unif() is the main method for generating uniform distributions. It has two main functions, runif() and rdunif().
# rdunif() generates a random uniform distribution with discrete values. runif a radonm uniform distribution with continuous values.

rdunif(1000,25,50) # This takes three inputs, the number of occurrences, the minimum and the maximum. Try and replicate 50 coin flips, or 100 rolls of a 6 sided dice.

# runif() is particularly useful for generating random numbers within a range. This takes the same exact inputs, but as continuous.
runif(1000,25,50) # You can use rdunif() for discrete, or round runif() depending on the situation.
round(runif(1000,25,50))

# There is much more to statistics in R, but hopefully this introduction covers some of the fundamental building blocks! There are three follow up workshops offered by CDCS looking into more complex (and interesting!) applications of these theories in R.

# See also:
browseURL("https://r-coder.com/uniform-distribution-r/")
browseURL("https://r-lang.com/rnorm-function-in-r/")

# #### END ####



