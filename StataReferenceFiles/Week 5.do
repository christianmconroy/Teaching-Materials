/* Recitation Week 5 */ 
/* Important to note that I've changed the settings to allow wrap text */ 
cd "C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations"
cd
use auto-week5.dta, clear

/* Use current data to make a box-and-whisker plot (Bonus to start) */ 

graph box weight, by(domestic)



























/*STATA Recitation Week 4*/ 

cd "C:\Users\Amanda\Documents\Georgetown\STATA" /*set your own working directory
as this one will not work on your computer*/ 

/*Activity 1*/ 

/*1. Load the dataset*/ 

use auto-week5, clear 

/*2. Generate new variables and summarize the results*/

gen x1 = 1
gen x2 = 2
gen y = x1 + x2

sum y x1 x2

/*3. Change value of x1 and rerun the do-file.*/ 

replace x1 = 2

replace y = x1 + x2 /*updates y given changes to x1*/ 

/*save modified dataset*/

save auto-modified, replace

/*Activity 2*/ 

sysuse census, clear

/*1. Create variable giving the total population age 17 and younger in each 
state. What is the average state population of that group*/ 

gen pop17 = poplt5 + pop5_17

sum pop17

/*2. Create new variable giving proportion of residents age 17 and younger in 
each state. Which state has the lowest proportion and what is the proportion?*/

gen poppro = pop17/pop 

sort poppro
list poppro state in f

/*3. Create new variable giving proportion of marriages. Which state has the 
highes proportion and what is the proportion?*/

gen marpro = marriage/pop

gsort -marpro
list marpro state in f

/*4. Create new variable giving proportion of . Which state has the 
highes proportion and what is the proportion?*/

gen divpro = divorce/pop

gsort -divpro
list divpro state in f

/*5. What is the average rural population for all states?*/

gen rural = pop - popurban

sum rural

/*6. Create a variable with the log of population for each state. Create 
histograms for population and log of population. Export graphs*/

gen logpop = log(pop)

histogram logpop, title(Log of Population)
graph export logpop.png, replace

histogram pop, title(Population)
graph export pop.png, replace 

/*7. Save the modified dataset under a new name*/ 

save census-modified, replace 

