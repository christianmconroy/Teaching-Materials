/* In-class activity 1 */ 
use https://stats.idre.ucla.edu/stat/stata/notes/hsb2, clear

*1
reg write female

*2
reg write female schtyp prog id, robust

*3
reg write read math science if schtyp == 1, robust

*4
* Manually eliminate data with data editor and run a regression. 

*5
twoway (scatter write math) (lfit write math)

*6
twoway (scatter write female) (lfit write female)
