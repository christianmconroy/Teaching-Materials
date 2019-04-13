/* In Class Activity 1 */ 
clear
sysuse auto
browse
list
/* Can use the below to get rid of the need to constantly press space to get 
to the end */ 
set more off
sort price
list
count
describe
describe mpg make
describe Weight 
/* Does not work because Stata is case sensitive */ 
d we
d m
/* Does not work because m is ambiguous */
d mp
d m*
d t*
d f*
sum mpg
sum mpg make
/* Make yields no results because it is a string variable */ 

/* In class activity 2 */ 
clear
sysuse auto
describe
sum price mpg weight length
disp 41-12
/* Difference is 29 */ 
sum price mpg weight length if price < 4000
sum price mpg weight length if price < 4000 & foreign == 1
sum price
list if price == 15906
/* The make is Cad. Seville and the price is $15906 */ 

/* After class review exercise */ 
sysuse lifeexp, clear
describe
/* Variables: 6; Strings: 2; Numeric: 4; Observations: 68 */ 
codebook
/* gnppc: 5 missing; safewater: 28 missing; */ 
sort lexp
list lexp (country)
disp 79-54
/* Greatest minus least life expectancy is 25 */ 



