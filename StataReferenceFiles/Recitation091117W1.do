* Ways to Open Files *
/* Ways to Open Files */ 
/* 1. Sysuse */
sysuse lifeexp.dta, clear
/* 2. Just click on the dta file in your folders */
/* 3. Import DTA file with full file name */
use /Users/christianmconroy/Desktop/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/EnvSurvey.dta, clear
/* 4. Set working directory and just call name in */
cd /Users/christianmconroy/Desktop/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations
use EnvSurvey.dta
/* 5. Import from other non-dta file */
import excel "/Users/christianmconroy/Downloads/Data_All_160526 (2)/Excel/Sheet_1.xls", sheet("Sheet1")


