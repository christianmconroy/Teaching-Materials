* Working with Dates (EXTRA)
import excel "Stata Class File_long.xlsx", 	/// Imports the excel file
			sheet("Sheet1") /// Tells stata which sheet to import
			firstrow 		/// Tells stata first row is variable names
			case(lower) 	/// Makes variable names lowercase (Stata is case sensitive)
			allstring 		/// Forces all to be string, some numeric won't import well
			clear
			
	list arrdate in 1/10
	gen arr_d=date(arrdate, "MDY") 			/*Need to input format of the date of birth, 
											"MDY" (Month Day Year) or 
											"DMY" (Day Month Year)[So it's days since 1/1/1960 up here] */
		label var arr_d "Arrest Date" 		/*Adds a label to the variable*/
		list arr_d arrdate in 1/10
		format arr_d %td  					/*dates in stata are numeric values that are 
											the number of days (or other denomination) 
											since 1/1/1960, they need to be formatted.
											% td takes it from days since to an actual date format*/
		list arr_d arrdate in 1/10
		
	

/*Once it's in stata date format it's easy to transform*/
	gen arr_m=month(arr_d)				/*generates month of arrest variable*/
	gen arr_y=year(arr_d)				/*generates year of arrest variable*/
	gen arr_my=mofd(arr_d) 				/*generates variable equal to the month/year of 
										arrest, note numerically this is the 
										number of months since 1/1/1960, need to 
										format*/
list arr_m arr_y arr_my arr_d in 1/10
	format arr_my %tm 					/*formats it as year month*/
	list arr_m arr_y arr_my arr_d in 1/10
	gen arr_jan25=(arr_d==td(25jan2014)) 	/*Shortcut for creating dummies is to put 
											the if statement after the equal sign in 
											parenthesis, downside is only for zero/one 
											variables, problem for missings*/
											/*Note td() calls the label otherwise 
											you would have to calculate days from 
											1/1/1960*/		
