* Electricity Data RD Analysis *

* 1. Import and Clean the Data 
cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations
import delimited "Electricity Exercise File.csv", varnames(1) 

replace demandlossmw = "." if demandlossmw == "NA" & "Various" & "unknown" & "N/A" ///
& "--" & "-" & "UNK" & 

