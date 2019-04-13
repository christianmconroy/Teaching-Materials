# R Recitation Week 5 - GGPLOT2 Intro to Data Viz #

# Bars with percent and count, stacked and faceted bars, histograms, stacked histograms, and boxplots, density plot, jitter, scatter by color faceted and with line

# Data for today can be accessed via: 
# 1. https://www.kaggle.com/c/titanic/data [train.csv -> This is the titanic data]
# 2. All others are internal

# GGPLOT2 is the standard vizualization package of R - GG stands for grammar of graphics 

# What do we need for GGPlot? 
    # 1. Data. Duh. 
    # 2. Layers - Your points, your lines, your bars, etc.
    # 3. Scales - Rendering the layers correctly
    # 4. Coordinates - Default system you use is the 2 dimensional XY plane
    # 5. Faceting - Provides vizual drill down into the data (Like pivot tables and pivot charts in Excel)
    # 6. Themes - Fonts, color schemes, and other customization

# More simply, we need data, an aesthetic (mapping data on a vizualwith color, shapes, and sizes), and layer (Dots, lines, bars, etc.)

# From a syntax perspective, the layer is key as "geom" notes the type of layer you'll be using 

# Review of the plots we've done so far!!!
install.packages('webuse')
library(webuse)
webuse("nlsw88")
attach(nlsw88)
hist(wage, xlab="Wages", ylab = "", main = "Wages Distribution")
plot(wage[industry == 2], pch=19, col="blue")

head(industry)

?hist

#### Titanic example #####
# Not only will you see this dataset in problem sets at McCourt at some point, but titanic Dataset might be similar to what customer profile data might look like for example (i.e. whether a customer might leave and whether a dude might die.#

# Titanic data set consists of survival, ticket class, sex, age in years, # siblings, # parents, ticket number, fare, cabin number, port of embarkment, 

# This is good for people who aren't at McCourt and who aren't statisticians # 

#### Set the WD ####
getwd()

setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/Week5RRecitation")

##### Load in the data ####
titanic <- read.csv("titanic.csv", stringsAsFactors = F)
head(titanic)
attach(titanic)

install.packages('meme')
library(meme)
u <- "https://i.ytimg.com/vi/xtj-a_GG0e8/maxresdefault.jpg"
meme(u, vjust = 0.2, size = '2', "", "Kevin... We're Home.")

##### Setting up the data - Factorize things ####

# Can either import as strings and convert to factors or import as factors and convert what needs to be converted to strings. Not sure of anyway to import a la carte in any way. 
class(Pclass)
class(Survived)
class(Sex)
class(Embarked)
# Two interger classes and a character. We cannot make good vizuals from this. We need all three to be factors so that we can look at levels. (i.e. Pclass levels of 1,2,3 and survived of 0 or 1 for example). It doesn't really make sense to say that a Pclass of 3 is three times a Pclass of 1 for example. 
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# We have to have them as factors to do any color coding anyways. 

# Now we can analyze. 

##### Vizualization 1: Who survives and who doesn't? #####
# Reference for everything
?ggplot
?aes
?labs
geom_
theme_
# The above 2 pull up drop down menus

install.packages('ggplot2')
library(ggplot2)

# Bar plot of frequency counts for who survived and who did not.
ggplot(titanic, aes(x = Survived)) + geom_bar()


# Above: ggplot tells R to start building a new vizualization. aes stands for aesthetic; so we're telling ggplot2 to map the x axis to the survived column/variable of the titanic data set. geom-bar is our layer. So we have our data, aesthetic, and layer. 

# Bar plot with percentages 
  # To just get the percentages, I would do: 
prop.table(table(titanic$Survived))
  
  # To show quickly how to vizualize the percentages, I would do: 

ggplot(titanic, aes(x = Survived)) + geom_bar() + stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100), "%\n", ..count..), y=0.5*..count..),geom="text", colour="white", size=4)

# OK. Let's break it down. 
# First, I added counts using stat_count. R uses this .. notation to hold the values - Look at the help menu for stat_count. 
# To create the label text, I used paste0.
# To combine the calculated values and get a percentage, I used the "..count../sum(..count..)*100" notation. 
# The \n allows us to put frequency and percentage on different lines
# sprintf produces values rounded to one decimal place. You can use round instead, but I prefer sprintf because it keeps a zero in the decimal place even when the decimal part is zero, while round returns just the integer part when the decimal part is zero.
# y=0.5* -> This is where we calculate the y-position for each percent label.We do 0.5 times our counts o that the y-position of each label will be reduced by half the height of the bar section containing that label.
                                                             
# Customize the bar plot a little bit - Add a theme and a main title and y axis title
ggplot(titanic, aes(x = Survived)) + geom_bar() + theme_bw() + labs(y = "Passenger Count", title="Titanic Survival Rate")
# Doesn't really matter what order you put it in, so long as ggplot is first.

# What is we want to know about the survival rate by gender? As you recall from the movie, it was who first? Women and children first. 
ggplot(titanic, aes(x = Sex, fill  = Survived)) + geom_bar() + theme_bw() + labs(y = "Passenger Count", title="Titanic Survival Rate")
# So, I map the X axis now to Sex
# I use something called "fill" and assign a categorical variable. This gives a nice color codiong. 

# What else might explain this? How about in addition to gender the class of your ticket also playing a role? First class decks were higher up and closer to the deck with life boats, so maybe you had a better chance. 

ggplot(titanic, aes(x = Pclass, fill  = Survived)) + geom_bar() + theme_bw() + labs(y = "Passenger Count", title="Titanic Survival Rate by Pclass")

# What if we want to know the survival rate by gender and ticket class? 
# We use facets (pivot table like drill down into the data) here

ggplot(titanic, aes(x = Sex, fill  = Survived)) + theme_bw() + facet_wrap(~ Pclass) + geom_bar()  + labs(y = "Passenger Count", title="Titanic Survival Rate by Pclass and Sex")

# What we do above is facet. We could technically use facet_grid or facet_wrap. Faceting is the mechanism by which we could do this vizual drill down. We're still mapping the x axis, this time to Sex. I'm filling (the colors) to survived or not survived. And I am faceting to Pclass. 
# The tilda is like by(). Please facet the Titanic data by Pclass. 

# What we see here is that females in first class overwhelmingly survived. Females in second class also overwhelmingly survived and in third class had about a 50-50 split. Males were fucked though. 

# Looking at continuous variables - What if we want to look at how age impacts survivability? 
# Let's start with just a basic histogram showing the distribution of the age. 
ggplot(titanic, aes(x = Age)) + theme_bw() +geom_histogram(binwidth=5) + labs(y = "Passenger Count", x = "Age (binwidth=5)", title="Titanic Age Distribution")

# In the above, binwdith says please bin my data into blocks of 5 years. Block everyone from 0-5, 6-10, 11-15, 16-20, etc. GGPlot can pick a default for you, but it's not usually optimal. You'll usually have to know your data and/or do some trial and error. We can run it without binwidth specified as shown below.  

ggplot(titanic, aes(x = Age)) + theme_bw() +geom_histogram() + labs(y = "Passenger Count", x = "Age (binwidth=5)", title="Titanic Age Distribution")

# We know that we have missing data in the age column though. Out output above showed 177 missing values!!! GGPlot removed the 177 missing values automatically, 

# The output shows a long tail out to the right because older passengers weren't that prevalent. Life expectancy was also lower back in the day, so the fact that there were folks that were 80 is important. 


# What are the survival rates by age? 
ggplot(titanic, aes(x = Age, fill = Survived)) + theme_bw() + geom_histogram(binwidth = 5) + labs(y = "Passenger Count", x = "Age (binwidth=5)", title="Titanic Survival Rates by Age")
# Above, we can color code the bars to illustrate  survival by age. 
# We can see that children, especially at the younger end, disproportionately survived. At the high end of the spectrum, survivability is quite low. At 52-53, there is much more orange. That 80 year old though! Damn! 
# We're still missing 177 passengers though.

ggplot(titanic, aes(x = Age, fill = Survived)) + theme_bw() + geom_histogram(binwidth = 5) + facet_wrap(~Sex) + labs(y = "Passenger Count", x = "Age (binwidth=5)", title="Titanic Survival Rates by Age")

# Can also use a box and whisker plot!
ggplot(titanic, aes(x = Survived, y = Age)) + theme_bw() + geom_boxplot() + labs(y = "Age", x = "Age (binwidth=5)", title="Titanic Survival Rates by Age")

# In the above, we have the survived along the x axis still and on the y axis, we have the age. Now we have the age distribution all up of 0 (perished) and 1 (survived). So it looks like in general those who survived tended to be younger than those who perished by a bit.

# Could obviously have added more box and whisker plots via something like the passenger class grouping. 

# Let's look at survivability and class of ticket but this time with age!
ggplot(titanic, aes(x = Age, fill = Survived)) + theme_bw() + facet_wrap(Sex ~ Pclass) + geom_density(alpha = 0.5) + labs(y = "Age", x = "Survived", title="Titanic Survival Rates by Age, PClass and Sex")

# The code above gives us age mapped to the x axis. 
# We added sex and pclass to the facet wrap to give us a four dimensional view: Age as segmented by both sex and pclass and also the relative survival rate for ages based on the combination of sex and p class.

# We do a density plot above. Density plots are analogous to histograms but they are smoothed out. Histograms are jagged because they are squared off bars. The information that is portrayed is essentially the same. 

# The alpha value of 0.5 sets the transparency (i.e. we can see through). If we didn't set alpha, one would be opaque, so we wouldn't see what's behind. 

# The density plot expands on our idea of women and children first. Depending on the p class, children disproportionately survive. 

# Below we can just compare the density of the age distribution for both groups. 

ggplot(titanic, aes(x = Age, fill = Survived)) + geom_density(col = NA, alpha = 0.35)

# We can of course go back to doing just histogram: 
ggplot(titanic, aes(x = Age, fill = Survived)) + theme_bw() + facet_wrap(Sex ~ Pclass) + geom_histogram(binwidth = 5) + labs(y = "Age", x = "Survived", title="Titanic Survival Rates by Age, PClass and Sex")

# All we change is the density part. Only three females (girls?) in first class perish! That top left tells a lot. Same with second class. All of the girls in second class survive actually. 


# What about working with continuous variables? Is there any relationship between sepal length and petal length?
install.packages('webuse')
library(webuse)
webuse("iris")
head(iris)
attach(iris)
install.packages('ggplot2')
library(ggplot2)

# Let's map the sequal length onto the x aesthetic and the sequal width onto the y aesthetic. 
ggplot(iris, aes(x=seplen, y=sepwid)) + geom_point()

ggplot(iris, aes(x=seplen, y=sepwid)) + geom_jitter(alpha = 0.6)

# What does jitter do again? Jittering is the act of adding random noise to data in order to prevent overplotting in statistical graphs. For example, age is measured in years and body weight is measured in pounds or kilograms. If you construct a scatter plot of weight versus age for a sufficiently large sample of people, there might be many people recorded as, say, 29 years and 70 kg, and therefore many markers plotted at the point (29, 70).

# Again, alpha is for transparency. 

# What if we want to look at this relationship for each flower species? 
colnames(iris)[1] <- "species"
head(species)
# Just really annoying to have a variable have the same name as the dataset. 

ggplot(iris, aes(x=seplen, y=sepwid)) + geom_jitter(alpha = 0.6) + facet_grid(.~ species)

# We just have numbers for each graph though. Not good. Let's add text to show the levels represented by the numbers. 
class(species)
iris$species <- as.factor(iris$species)
levels(iris$species) <- c("Setosa", "Versicolor", "Virginica")

# Now we get the names. 
ggplot(iris, aes(x=seplen, y=sepwid)) + geom_jitter(alpha = 0.6) + facet_grid(.~ species)

# We still have a great deal of empty space on our plots, so let's change up the scale and compress. 

ggplot(iris, aes(x=seplen, y=sepwid)) + geom_jitter(alpha = 0.6) + facet_grid(.~ species) + stat_smooth(method = "lm", se = F, col = "red") + scale_y_continuous("Sepal width (cm)", limits = c(2,5), expand = c(0,0)) + scale_x_continuous("Sepal Length(cm)", limits = c(4,8), expand = c(0,0)) + coord_equal()

# When you start writing scale_y, you see in the dropdown that R differentiates based on what kind of variable this is. Same thing with scale_x. You can also use this instead of lab for your axis labelling because your naming the axis, which R takes by default to mean the legend of the axis and positions the text accordingly.

# Limits is a numeric vector that gives you the min and the max, so here we do 2,5 for Y and 4,8 for x. 

# Expans is a numeric vector that gives multiplicative and additive expansion constants and makes sure there is a bit of space between each axis and the data. The default is c(0.05,0) for continuous variables, so we're allowing it to be 0,0 to make the graph tighter and more compact.
# What your coord_equal(ratio = 1) does is make sure that the an equal length on both axis represents the same change in units. The default, ratio = 1, ensures that one unit on the x-axis is the same length as one unit on the y-axis. Ratios higher than one make units on the y axis longer than units on the x-axis, and vice versa. So 1cm = 5units for both axes, for example. 


# Or if we want it all in one plot, we can just color code the points. 

# Scatter
ggplot(iris, aes(x=seplen, y=sepwid)) +geom_point(aes(col=species))

# Jigger
ggplot(iris, aes(x=seplen, y=sepwid, col=species)) + geom_point() + geom_jitter() 

# To add lines to these. 

ggplot(iris, aes(x=seplen, y=sepwid)) + geom_jitter(alpha = 0.6) + facet_grid(.~ species) + stat_smooth(method = "lm", se = F, col = "red") + scale_y_continuous("Sepal width (cm)", limits = c(2,5), expand = c(0,0)) + scale_x_continuous("Sepal Length(cm)", limits = c(4,8), expand = c(0,0)) + coord_equal()


# Further resources for learning more about GGPLOT

# 1. ggplot2 by Hadley Wickham