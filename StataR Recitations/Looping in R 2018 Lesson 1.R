# Loops in R - 2018 Lesson 1 #

########### Loops ############
# There are 3 types of loops
  # 1. For
  # 2. While 
  # 3. Repeat
# You can get further insight into these functions by typing ?Control

# For Loops #

# A loop allows you to iterate items over a vector or matrix. 

# General Structure: 
#   for (variable in vector) {
#     commands
#   }
# We often use "i" for variable

for (i in 1:5) {
  print (i^2) 
}
# In the above example, the index variable is i, the vector includes the numbers 1-5, and then we want i^2 for the output. 

# There are three parts common to all for loops: 
# 1. The Sequence: Describes the name we give to the object that indexes the iteration (i) and the values that the index shouuld iterate over (1:5)
# 2. The body: This comes between the braces and describes the operations to iterate. 
# 3. The definition of where to store the results 


# The vector specified in the loop does not have to be sequential, so we can use the concatenate function to specify other numbers:

for (i in c(-3,6,2,5,9)) {
  print(i^2)
}

# We can also define the vector beforehand: 

x <- c(-3,6,2,5,9) 
for (i in x) {
  print(i^2)
}

y <- rep(NA, 10)
y
for (i in 1:10){
  y[i] <- i
}
y

# In the above, we create a vector of all NAs. We then loop through i from 1:10 and for each i, take the ith element of the result and assign that to i. Now we've gone from all NAs to the numbers 1-10 in our y vector. 

# We can also do more than just i^2

x <- c(-3,6,2,5,9) 
for (i in x) {
  print(c(i^2, i^3))
}

# We can't really do anything with the output though. We can't take the mean or add the numbers to an existing data frame for example. To get past this, we can start by creating a vector to store the output. 

Storage <- numeric(5)
for (i in 1:5) {
  Storage[i] <- i^2
}
Storage

# The third line states that the ith element of our empty storage vector will be filled with i^2. Now we can use them. So for example, we can take the mean:
mean(Storage)

# If we're not doing something like 1:5, we can use the sequential values as index values: 
x
Storage2 <- numeric(5)
Storage2

for (i in 1:5) {
  Storage2[i] <- (x[i])^2
}

Storage2

# When i is equal to 1, the square of the first element of the vector x is going to be stored as the first element of our Storage2 vector.  

# We don't have to do the whole thing and can only loop through some of the sequence using an if statement. 

for(i in 1:7) { 
  if(i==3)
    next
  print (i^2) 
}

# We use an if statement to skip 3 in the above. 

# How about doing something more complicated like converting from celcius to farenheit? 

for(DegC in c(-3,6,2,5,9)){
  DegF<-DegC*(9/5)+32
  print(c(DegC, DegF))
}

# Can we use for loops with multiple columns/variables? 

# First we generate 4 variables comprised of random numbers using rnorm, which generates random deviates from a normal distribution (negative and positive)

df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

for (i in 1:ncol(df)) {
  print(median(df[[i]]))
}

# Can we loop over the entire matrix instead of over each column individually? 

myMatrix <- matrix(1:50, nrow = 5)
myMatrix

for (i in myMatrix) {
  print(i^2)
}

# We use ncol in our sequence to tell R that we want the median for each column/variable. 
# We use double square brackets here to pull a column from a dataframe. A dataframe is built on top of a list where each element is a column. Double brackets pulls out elements inside a list inside a dataframe. 


# Nested For Loops 
# General Structure 
# for (var1 in vect1) { 
#   for (var2 in vect2) {
#     for (varn in vectn) {
#       commands
#     } #n
#   } #2
# } #1

for(i in 1:3) {
  for(j in 1:2) {
    print(i+j)
  } #2
} #1

# The list of numbers in the output comes from a code that says that we have 3 i values to loop through and 2 j values to loop through and we have to find every combination of i+j. 

# The above is just to get you used to loops. R runs vectorized calculations much more efficiently than loops, so if you can do it just be creating vectors and doing calculations with them, definitely do that. 

# Using a nested loop with a matrix 
for(row in 1:nrow(myMatrix)){
  for(col in 1:ncol(myMatrix)){
    print(paste('Row is = ', row, 'and column = ', col, 'and the value is:', myMatrix[row, col]))
  }
}

# It spells out for each one the location of the observation. Text description of what each one is. 

# Creating a matrix and setting its elements to specific values 
# First we'll create a 30 * 30 matrix 
mymat <- matrix(nrow = 30, ncol = 30)
mymat

# For each row and column, assign values based on position: product of two indexes. In the below, we use "dim" to retrieve or set the dimension of an object. [1] is for the row and [2] is for the column. 
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Now we can go back and look at a part of the matrix. 
mymat[1:10, 1:10]
# As you see in the above, the value of the element is just the row * the column. 

# While Loops #

# General condition 
# while(cond) expr

# While loops will end up being better when the number of iterations is not necessarily fixed and known in advance. If there are conditions that may not be predictable beforehand and you don't know the number of iterations.

# In contrast to for loops, while loops usually include a condition that is expressed by a comparison between a control variable and a value by using greater than, less than or equal to, or any other thing that evaluates a logical value that could be True or False. 

# For example, what if we want to return a statement for everything that is NOT the value we care about: 

readinteger <- function(){
  n <- readline(prompt="Please, enter your Answer: ")
}
  
  response <- as.integer(readinteger())
  
  while(response!=42) {
    print("Sorry, the answer to whatever the question MUST be 42");
    response <- as.integer(readinteger());
  }

# We won't go too far into it, but in the first line above, we use "function" as the basic mechanism to define new functions in the R language

#### Repeat Loop ###
  
  # The repeat loop is similar to the while loop except that it is made so that the blocks of each instruction of are executed at least once, no matter what the result of the condition is. 
  
  # Adhering to other languages, one could call this loop "repeat until" to emphasize the fact that the instructions i1 and i2 are executed until the condition remains False (F) or, equivalently, becomes True (T), thus exiting; but in any case, at least once.
  
  # So if we look at what we did above, but slightly adjust it: 
  
readinteger <- function() {
  n <- readline(prompt="Please, enter your ANSWER: ")
}

repeat {
  response <- as.integer(readinteger());
  if(response==42) {
    print("Well done!");
    break
  } else print("Sorry, the answer to whatever the question MUST be 42");
  }

# Note that you had to set a condition within the loop upon which to exit with the clause break. This clause introduces us to the notion of exiting or interrupting cycles within loops.

# When the R interpreter encounters a break, it will pass control to the instruction immediately after the end of the loop (if any). In the case of nested loops, the break will permit to exit only from the innermost loop.


############ With all that being said, if you can avoid loops, do. Other options include: 

# 1. Vectorization: Convert repeated operations on simple numbers ("scalars") into single operations on vectors or matrices. A vector is just a single entity consisting of a collection of things. If you combine vectors (vertically or horizontally), you get a matrix. 

# Why would you do:
for (i in 1:n) {
  v3[i] <- v1[i] + v2[i]
}
v3
# When you can just do: 
v2 <- v1 + v2
v3

# 2. The apply family (apply, lapply, and sapply)
  # apply() is for when you want to apply a given function to the rows (index "1") or columns (index "2") of a matrix
    # For example, fiven a matrix M, the call apply(M,1,fun) or apply(M, 2,fun) will apply the specified function fun to the rows of M, if 1 is specified.
      # Let's create a matrix to show this: 
      mymat2<-matrix(rep(seq(5), 4), ncol = 5)
      mymat2
      #Let's then use apply to get the sum of rows and columns
      apply(mymat2, 1, sum) #Rows
      apply(mymat2, 2, sum) #Columns
      
  # lapply() is for when you want to apply a given function to every element of a list and obtain a list as a result (which explains the "l" in the function name)
  # sapply() is for when you want to apply a function to every element of a list but you wish to obtain a vector rather than a list
