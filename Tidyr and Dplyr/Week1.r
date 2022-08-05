
# Week 1 Practice Code

# Type your name here: Michelle Lin 

# Complete these coding tasks, then submit your R file for another student to compare against the example output.

# Problem 1 

# Create a data frame that includes two columns, one named "Animals" and the other named "Foods". The first column should be this vector (note the intentional repeated values): Dog, Cat, Fish, Fish, Lizard


#The second column should be this vector: Bread, Orange, Chocolate, Carrots, Milk


#### Write your code below:

Animals <- c("Dog", "Cat", "Fish", "Fish", "Lizard")
Food <- c("Bread", "Orange", "Chocolate", "Carrots", "Milk")
animals_food <- data.frame(Animals, Food)

# Problem 2

# Using the data frame created in Problem 2, use the table() command to create a frequency table for the column called "Animals".

#### Write your code below:

table(animals_food$Animals)

# Problem 3

# Use read.csv() to import the survey data included in this assignment. Using that data, make a histogram of the column called "pid7".


#### Write your code below:
sampledata <- read.csv("/cloud/project/SampleData.csv")
hist(sampledata$pid7)


