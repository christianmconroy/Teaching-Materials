* Georgetown University, McCourt School of Public Policy Stata Recitation Spring 2018
* Week 7 Project 4: local and loop
* March 18 2018

* not require in Stata 15 above
set more off

clear

*Q1

* save do file

* make sure you use the right quotation marks for a local ` ' 
local mynumber 4
disp `mynumber'

* the following doesn't work because locals are deleted immediately after the code
* that defines a local has run through (No error; just doesn't show 4)
disp `mynumber'

* further examples
local nextnumber 5
gen x_`nextnumber' = `nextnumber'

* note that stata actually didn't generate an observation with x_5 = 5
* because there are no observations in the data set at this point. 
* It only generated x_5 with no observations. But the gen command
* would not have worked without a value assignment.
br

local mylocalexample hello, world
disp "The text of the local macro, mylocalexample, is: || `mylocalexample' ||"

* Q2

* need to drop previously generated variables
drop x_*

local mynumber 4
disp "The local macro named mynumber is equal to: `mynumber'"
disp "The command I am trying to run is: generate x_4 = `mynumber'"
gen x_4 = `mynumber'

/* Don't need the double quotation at the end like we did in the x_4 example
even though it theoretically closes the entire sentence string. */
 
local nextnumber 5
disp "The local macro named nextnumber is equal to: `nextnumber'
disp "The command I am trying to run is: generate x_`nextnumber' = `nextnumber'
gen x_`nextnumber' = `nextnumber'

local nextnumber2 6
disp "The local macro named nextnumber is equal to: `nextnumber2'"
disp "The command I am trying to run is: generate x_`nextnumber2' = `nextnumber2'"
gen x_`nextnumber2' = `nextnumber2'

local nextnumber3 7
disp "The local macro named nextnumber is equal to: `nextnumber3'"
disp "The command I am trying to run is: generate x_`nextnumber3' = `nextnumber3'"
gen x_`nextnumber3' = `nextnumber3'

* again, no obs in data set, thus no values were actually assigned
* but the variables were created
br

* Q3
webuse nhanes2, clear
local ctrlvars age bpsystol bpdiast tcresult
disp "controlvars: `ctrlvars'"
sum `ctrlvars'
reg weight `ctrlvars'


* Q4
local num 1
disp "generate x_`num' = `num'
gen x_`num' = `num'

local num 2
disp "generate x_`num' = `num'
gen x_`num' = `num'

local num 3
disp "generate x_`num' = `num'
gen x_`num' = `num'

local num 4
disp "generate x_`num' = `num'
gen x_`num' = `num'

local num 5
disp "generate x_`num' = `num'
gen x_`num' = `num'

* since this time we do have obs, values were succesfully assigned.
br x_*

// start of loop, watch out for the { }

foreach mynumber in 1 2 3 4 5 6 7 8 9 10 {

	disp "My favorite number is `mynumber'"
	
	}
	
// end of loop (To note is that this is another comment format)

// start of loop

foreach food in carrots pasta soup salad {
	
	disp "Today, I want to eat `food'"
	
	}
	
// end of loop


* we need to clear the variables created above at the beginning of Q4
* in case you haven't done so  since the loop generated variables will 
* have the same name

drop x_*

// start of loop

foreach num in 1 2 3 4 5 6 7 8 9 10 {

	gen x_`num' = `num'
	
	}
	
// end of loop

br x_*

* Q5
help foreach

// start of loop

foreach mylname in dog cat chicken mouse {

	disp "The next animal is: -- `mylname'--"
	
	}
	
// end of loop

* Q6

sum hgb-iron

// start of loop

 foreach yvar in hgb hct tibc iron {
 
	disp "The next dependent variables (yvar) is `yvar'"
	
	reg `yvar' weight height
	
	}
	
// end of loop


* Q7

// start of loop

foreach var in hgb hct tibc iron {

	disp "Command to run: gen ln_`var' = ln(`var')"
	gen ln_`var' = ln(`var')
	label variable ln_`var' "Log of `var'"
	
	}
	
// end of loop

de ln_*
sum ln_*

foreach var in hgb hct tibc iron {

	disp "Command to run: egen z_`var' = std(`var')"
	egen z_`var' = std(`var')
	label variable z_`var' "Standardized version of `var'"
	reg z_`var' weight height
	}
	
// end of loop

de z_*
sum z_*

local blackWoman race==1 & sex==1
local blackMan race==1 & sex==0
local controlVars height bpsystol tcresult
reg hgb weight `controlVars' if `blackWoman'
logit highbp weight `controlVars' if `blackMan'




