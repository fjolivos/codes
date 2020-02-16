********************************************************************************
*Asymmetric Social Comparison and Life Satisfaction in Social Networks
*Journal of Happiness Studies
*Olivos, Olivos-Jara & Browne
*Febraury 16th
********************************************************************************

clear all
version 14
set more off

global root = "C:\Users\fjoli\Dropbox\Data\Barometro de la Felicidad"
global output = "C:\Users\fjoli\Dropbox\180301 Social Comparison and Life Satisfaction\Submition JHS\Results"

********************************************************************************
******************************Data Management***********************************
********************************************************************************

use "${root}\basefinal - felicidad - Wide.dta" 

***************NAME GENERATOR VARIABLES

*Size of the network

foreach var of varlist P20_A_1-P20_A_5{
	encode `var', generate(`var'e)
}
foreach var of varlist P20_A_1e-P20_A_5e{
	recode `var' (1=0) (2/593=1)
}
lab def P20_A 1 "1"
foreach var of varlist P20_A_1e-P20_A_5e{
	lab val `var' P20_A
}
egen size = rowtotal( P20_A_1e-P20_A_5e)
tab size
sum size 
rename size Size

dis 2204-140 // at least 1 alter
dis 100-6.35 // at least 1 alter
dis 1*750 + 2*568 + 3*325 + 4*177 + 5*244 // total alters

*Ratio Happiness

foreach var of varlist P24_A-P24_E{
	recode `var' (1=4) (2=3) (3=2) (4=1)(9=.)
}
egen hapAlt = rowmean( P24_A-P24_E)

tab P1
recode P1 (9=.) (1=4) (2=3) (3=2) (4=1)
lab def P1_n 1 "Nada Feliz" 2 "No muy feliz" 3 "Bastante Feliz" 4 "Muy feliz"
lab val P1 P1_n
tab P1

gen ratio = hapAlt/P1
sum ratio
tab ratio

*Ratio meadian

egen hapAlt_median = rowmedian(P24_A-P24_E)
gen ratio_median = hapAlt_median/P1

*Standard operationalization
*Downward (I1)

gen Down = abs(P1-hapAlt) if P1>=hapAlt
replace Down = 0 if P1<hapAlt

*Upward (I2)

gen Up = abs(P1-hapAlt) if P1<=hapAlt
replace Up = 0 if P1>hapAlt


*Alternatives measurements: Median

gen Down_median = abs(P1-hapAlt_median) if P1>=hapAlt_median
replace Down_median = 0 if P1<hapAlt_median
gen Up_median = abs(P1-hapAlt_median) if P1<=hapAlt_median
replace Up_median = 0 if P1>hapAlt_median

*Simlarity age

egen ageAlt = rowmean( P20_C_1-P20_C_5)
gen ageSim = abs(EDAD_ENCUESTADO-ageAlt)

*Similarity Gender

foreach var of varlist P20_B_1-P20_B_5 SEXO_ENCUESTADO {
	recode `var' (1=1) (2=0) (9=.)
}
egen genAlt = rowmean(P20_B_1-P20_B_5)

gen genSim = abs(SEXO_ENCUESTADO-genAlt)

*Similarity Education

recode educ_enc2 (1=1) (2/3=0), gen(educ_enc3)
foreach var of varlist P20_E_1-P20_E_5{
	recode `var' (1/5=0) (6/10=1) (99=.)
}
egen eduAlt = rowmean(P20_E_1-P20_E_5)
gen eduSim = abs(educ_enc3-eduAlt)

*Proportion family

foreach var of varlist P20_D_1-P20_D_5{
	recode `var' (1/2=1) (3/9=0)
}
egen famAlt = rowmean(P20_D_1-P20_D_5)

*Overall similarity 

foreach var of varlist eduSim genSim ageSim {
	egen `var's = std(`var')
}

egen Similarity = rowmean(eduSims genSims ageSims)

*Reciprocal support

foreach var of varlist P22_A-P22_E{
	recode `var' (9=.) (2/3=2) (1=1)
}

foreach var of varlist P23_A-P23_E{
	recode `var' (9=.) (2/4=2) (1=1)
}


gen recA = .
replace recA = 3 if P22_A==1 & P23_A==1
replace recA = 2 if P22_A==1 & P23_A==2
replace recA = 2 if P22_A==2 & P23_A==1
replace recA = 1 if P22_A==2 & P23_A==2

gen recB = .
replace recB = 3 if P22_B==1 & P23_B==1
replace recB = 2 if P22_B==1 & P23_B==2
replace recB = 2 if P22_B==2 & P23_B==1
replace recB = 1 if P22_B==2 & P23_B==2

gen recC = .
replace recC = 3 if P22_C==1 & P23_C==1
replace recC = 2 if P22_C==1 & P23_C==2
replace recC = 2 if P22_C==2 & P23_C==1
replace recC = 1 if P22_C==2 & P23_C==2

gen recD = .
replace recD = 3 if P22_D==1 & P23_D==1
replace recD = 2 if P22_D==1 & P23_D==2
replace recD = 2 if P22_D==2 & P23_D==1
replace recD = 1 if P22_D==2 & P23_D==2

gen recE = .
replace recE = 3 if P22_E==1 & P23_E==1
replace recE = 2 if P22_E==1 & P23_E==2
replace recE = 2 if P22_E==2 & P23_E==1
replace recE = 1 if P22_E==2 & P23_E==2

egen recNetwork = rowmean (recA-recE)
recode recNetwork (1/2.8=0) (3=1), gen(recNetwork2)

*Time 
foreach var of varlist P21_A-P21_E{
recode `var' (9=.) (1/3=0) (4=1)
}

egen time = rowmean(P21_A-P21_E)

******INDIVIDUAL VARIABLES

*Education
rename  educ_enc3 Education
lab def Education 1 "Superior Education"
lab val Education Education

*Gender
rename  SEXO_ENCUESTADO Gender
lab def Gender 1 "Male" 0 "Female"
lab val Gender Gender

*Age
rename EDAD_ENCUESTADO Age
gen    Age2 = (Age*Age)/100

*Marital Status
recode  P77 (1=1) (2/4=0) (9=.), gen(Single)
lab def Single 1 "Single"
lab val Single Single

recode  P77 (1=0) (2=1) (3/4=0) (9=.), gen(Married)
lab def Married 1 "Married"
lab val Married Married

recode  P77 (1=0) (2=0) (3/4=1) (9=.), gen(Other)
lab def Other 1 "Other"
lab val Other Other

recode  P77 (1=1) (2=2) (3/4=3) (9=.), gen(Marital)
lab def Marital 1 "Single" 2 "Married" 3 "Other"
lab val Marital Marital

*Labor Market Status
recode  P54_COD (0/8=1) (9=0) (10/14=0) (15/16=0) (99=.), gen(Employed)
recode  P54_COD (0/8=0) (9=1) (10/14=0) (15/16=1) (99=.), gen(Unemployed)
recode  P54_COD (0/8=0) (9=0) (10/14=1) (15/16=0) (99=.), gen(Inactive)
lab def Employed 1 "Employed"
lab val Employed Employed
lab def Unemployed 1 "Unemployed"
lab def Inactive 1 "Inactive"
lab val Unemployed Unemployed
lab val Inactive Inactive 

recode  P54_COD (0/8=1) (9=2) (10/14=3) (15/16=2) (99=.), gen(Labor)
lab def Labor 1 "Employed" 2 "Unemployed" 3 "Inactive"
lab val Labor Labor

*Self-perception of health

lab var P45 "Self-perception of health"
recode  P45 (9=.)

*Date of Survey

gen   day   = day(fecha)
gen   month = month(fecha)
gen   year  = year(fecha)
order fecha day month year

gen     fecha2  = (year*10000) + (month*100) + day
recode  fecha2 (20141102/20141216=1) (20141217/20150101=2) (20150102/20150130=3)
lab var fecha2 "Fecha"
lab def fecha2 1 "2 Nov - 15 Dic" 2 "16 Dic - 1 Jan" 3 "2 - 30 Jan"
lab val fecha2 fecha2
tab     fecha2

********************************************************************************
***********************************Analyses*************************************
********************************************************************************

*Intraclass correlation

encode NOMBRE_ENCUESTADOR, gen(interviewer)
mixed  Size || interviewer:
estat  icc

*Alpha SWL

alpha P5_A-P5_E
sum   Diener_escala

*Listwise deletion 

recode  ageAlt (99=.) 
keep if Size!=0 

*****TABLE S1

local varlist1 Education Gender Employed Unemployed Inactive Single Married Other ///
      recNetwork2 P45 fecha2 Diener_escala Down Up Age Size ageAlt genAlt eduAlt famAlt P45
tabstat `varlist1', stat(mean sd median p10 p25 p75 p90 N min max) c(s)

drop if Down==. | Up==. | Age==. |  Education==. | Gender==. | Marital==. | Labor==. | Size==. | recNetwork2==. | Diener_escala==. | P45==. | fecha2==. | ageAlt==. | genAlt==. | eduAlt==. | famAlt==. 

*****TABLE 1

local varlist1 Education Gender Employed Unemployed Inactive Single Married Other ///
      recNetwork2 P45 fecha2 Diener_escala Down Up Age Size ageAlt genAlt eduAlt famAlt P45
tabstat `varlist1', stat(mean sd median p10 p25 p75 p90 N min max) c(s)

*Diener scale items for appendix

*****TABLE S2

sum P5_A P5_B P5_C P5_D P5_E
foreach var of varlist P5_A P5_B P5_C P5_D P5_E{
	tab `var'
}

*****TABLE S3

polychoric Down Up recNetwork2 Size Age Age2 Education Gender Marital Labor P45 ///
           fecha2 ageAlt genAlt eduAlt famAlt

*****TABLE 2		   

*Model A
reg Diener_escala Down Up, beta vce(robust)
ereturn list r2_a
outreg2 using "${output}\table2", ///
        dec(3) alpha(.01, .05, .1) symbol(***, **, *) excel label ctitle(Model A) replace

*Model B
reg Diener_escala Down Up ///
    Age Age2 i.Education ///
	i.Gender i.Marital i.Labor P45, beta vce(robust)
ereturn list r2_a
outreg2 using "${output}\table2", ///
        dec(3) alpha(.01, .05, .1) symbol(***, **, *) excel append label ctitle(Model B)

*Model C
reg Diener_escala Down Up ///
    Age Age2 i.Education ///
	i.Gender i.Marital i.Labor P45 ///
	Size ageAlt genAlt eduAlt famAlt i.fecha2 , beta vce(robust)
ereturn list r2_a
outreg2 using "${output}\table2", ///
        dec(3) alpha(.01, .05, .1) symbol(***, **, *) excel append label ctitle(Model C)

*Ttest for down upward

egen Down_sd =std(Down)
egen Up_sd = std(Up)
egen Diener_escala_sd =std(Diener_escala)

reg Diener_escala_sd Down_sd Up_sd ///
    Size Age Age2 i.Education ///
	i.Gender i.Marital i.Labor P45 ///
	ageAlt genAlt eduAlt famAlt i.fecha2, coeflegend vce(robust)
ereturn list r2_a
test _b[Down_sd]=_b[Up_sd]


*Formas alternativas de medición

reg Diener_escala Down_median Up_median ///
    Size Age Age2 i.Education ///
	i.Gender i.Marital i.Labor P45 i.fecha2 ///
	ageAlt genAlt eduAlt famAlt, beta vce(robust)


*****TABLE 2	

*Model D
reg Diener_escala Down Up ///
    Size Age Age2 i.Education i.Gender i.Marital i.Labor P45 ///
	i.fecha2 i.recNetwork2 ageAlt genAlt eduAlt famAlt, vce(robust)
outreg2 using "${output}\table3", ///
        dec(3) alpha(.01, .05, .1) symbol(***, **, *) ctitle(Model D) ///
        drop(Size Age Age2 i.Education i.Gender i.Marital i.Labor P45 i.fecha2 excel ageAlt genAlt eduAlt famAlt) replace

*Interactions

*Model E
reg Diener_escala Down Up ///
    Size Age Age2 i.Education i.Gender i.Marital i.Labor P45 ///
	i.fecha2 i.recNetwork2 c.Down#i.recNetwork2 ageAlt genAlt eduAlt famAlt, vce(robust)
ereturn list r2_a
outreg2 using "${output}\table3", ///
        dec(3) alpha(.01, .05, .1) symbol(***, **, *) ctitle(Model E) ///
        drop(Size Age Age2 i.Education i.Gender i.Marital i.Labor P45 i.fecha2 excel ageAlt genAlt eduAlt famAlt) append 

*Model F
reg Diener_escala Down Up ///
    Size Age Age2 i.Education i.Gender i.Marital i.Labor P45 ///
	i.fecha2 i.recNetwork2 c.Up#i.recNetwork2 ageAlt genAlt eduAlt famAlt, vce(robust)
ereturn list r2_a
outreg2 using "${output}\table3", ///
       dec(3) alpha(.01, .05, .1) symbol(***, **, *) ctitle(Model F) ///
       drop(Size Age Age2 i.Education i.Gender i.Marital i.Labor P45 i.fecha2 excel ageAlt genAlt eduAlt famAlt) append  

*Model G
reg Diener_escala Down Up ///
    Size Age Age2 i.Education i.Gender i.Marital i.Labor P45 ///
	i.fecha2 i.recNetwork2 c.Down#i.recNetwork2 c.Up#i.recNetwork2 ageAlt genAlt eduAlt famAlt, vce(robust)
ereturn list r2_a
outreg2 using "${output}\table3", ///
        dec(3) alpha(.01, .05, .1) symbol(***, **, *) ctitle(Model G) ///
        drop(Size Age Age2 i.Education i.Gender i.Marital i.Labor P45 i.fecha2 excel ageAlt genAlt eduAlt famAlt) append  

test _b[Down]=_b[Up]

****Propensity Score Matching

*Supplementary material III (Before listwise deletion)
recode Down (0=0) (0.2/5=1), gen(Down2)
teffects psmatch (Diener_escala) ///
        (Down2 Size Age Education Gender Marital Labor P45 fecha2 recNetwork2 ageAlt genAlt eduAlt famAlt), ///
		cal(0.2) atet
tebalance density 
tebalance summarize

recode Up (0=0) (0.2/5=1), gen(Up2)
teffects psmatch (Diener_escala) ///
        (Up2 Size Age Education Gender Marital Labor P45 fecha2 recNetwork2 ageAlt genAlt eduAlt famAlt), ///
		cal(0.2) atet 
tebalance density 
tebalance summarize


