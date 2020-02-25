********************************************************************************
*Francisco Olivos
*Poetics
*Untangling causal beliefs
*Stata 14
********************************************************************************

version 14
clear all
set more off

global root   = "C:\Users\fjoli\Dropbox\MaSocSR\Sem III\SaSR10\assignment data"
global output = "C:\Users\fjoli\Dropbox\190216 Viñetas Social Indicator Research\Results"

use "${root}\basefinal - felicidad - Wide.dta", clear

********************************************************************************
*                                Data management                               *   
********************************************************************************

d // Orignal sample  2,267 

*Deletion of vignette non-response

foreach var of varlist ///
    P7_V1 P7_V2 P7_V3 P7_V4 P7_V5 P7_V6 P7_V7 P7_V8 P7_V9 P7_V10 P7_V11 P7_V12{
    recode `var' (0/10=1), gen(`var'c)
}

egen count = ///
	 rowtotal(P7_V1c P7_V2c P7_V3c P7_V4c P7_V5c P7_V6c P7_V7c P7_V8c P7_V9c P7_V10c P7_V11c P7_V12c)
tab  count
recode count (0/9=.), gen(idmissvig)

drop if count<12 // respondents with less than 12 vignettes rated were removed (4)

d //


*Delation of respondent without variance of rating

egen sdvig = ///
	rowsd(P7_V1 P7_V2 P7_V3 P7_V4 P7_V5 P7_V6 P7_V7 P7_V8 P7_V9 P7_V10 P7_V11 P7_V12)
recode sdvig (0=.) (0.1/5=1), gen(variation)  
tab variation

drop if sdvig==0

d // sample after vignette cleaning 2,132    

****VARIABLES

*Sexo

clonevar gender = SEXO_ENCUESTADO
recode   gender (1=1) (2=0)
lab def  gender 1 "Male" 0 "Female"
lab val  gender sex

*Dummies of decks

tab numero_deck, gen(deck)

*Self-perception of health

clonevar health = P45
lab var  health "Self-perception of health"
recode   health (9=.)

*Life satisfaction

clonevar life = P2
lab var  life "Life satisfaction"
recode   life (99=.)

*Age

clonevar age = EDAD_ENCUESTADO
lab var  age "Age"

*Age range 

recode  age (1/35=1) (36/52=2) (53/95=3), gen(edad_3)
lab def edad3 1 "18-35" ///
              2 "36-52" /// 
			  3 "53-95"
lab val edad_3 edad3

tab edad_3, gen(agerange)

*Education

clonevar education = educ_enc2
lab var  education "Educational level"
tab      education, gen(education)

lab var education1  "Higher education" 
lab var education2  "High school" 
lab var education3  "Primary school" 

*SES

clonevar ses = TNSE
lab var  ses "Socioeconomic status"
tab      ses, gen(ses)

lab var ses1 "High"
lab var ses2 "Medium"
lab var ses3 "Low"

*Family Index

foreach var of varlist P9_A-P9_I{
recode `var' (7/99=.)
}
alpha  P9_A-P9_I
egen fam = rowmean( P9_A-P9_I)

*Stresful life style

clonevar stress = P49
recode stress (9=.)
lab var stress "Stressful" 

*Income sufficiency

clonevar ecosit = P31
recode   ecosit (9=.)
tab      ecosit, gen(ecosit)

lab def ecosit1 1 "Yes" ///
                0 "No"
lab var ecosit1 "Good economic situation"				
				
lab def ecosit2 1 "Yes" ///
                0 "No"	
lab var ecosit2 "Neither good nor bad economic situation"					

lab def ecosit3 1 "Yes" ///
                0 "No"				

lab def ecosit4 1 "Yes" ///
                0 "No"
				
*Indebteness

clonevar debt = P35
recode   debt (9=.)
lab var  debt "Level of indebtedness" 

*Marital status

clonevar marital = P77
recode   marital (9=.) (3/4=3)
tab      marital, gen(marital)

lab def marital1 1 "Yes" ///
                 0 "No"

lab def marital2 1 "Yes" ///
                 0 "No"

lab def marital3 1 "Yes" ///
                 0 "No"				 

lab var marital1 "Single"
lab var marital2 "Married"
lab var marital3 "Divorced or widowed"				 
				 			 
*Mobility

clonevar parents = P37_D 
recode   parents (88/99=.)
lab var  parents "Social mobility"

gen     expo = P46_B_A if P46_A==1 | P46_A==1
replace expo = 0 if P46_A==2 & P46_B==2

gen     mob = .
replace mob = 1 if P48_A==1 | P48_B==1 | P48_C==1 | P48_D==1 | P48_E==1 | P48_F==1
replace mob = 0 if P48_A==2 & P48_B==2 & P48_C==2 & P48_D==2 & P48_E==2 & P48_F==2

keep FOLIO-FOLIO_UC gender age edad_3 agerange1 agerange2 agerange3 education ///
     education1 education2 education3 ses ses1 ses2 ses3 life health sdvig /// 
	 variation idmissvig deck1-deck10 P45 count ///
	 fam stress ecosit ecosit1 ecosit2 ecosit3 marital1 marital2 marital3 parents P51_J P51_Q marital P77

********************************************************************************
*                                    Analysis                                  *   
********************************************************************************

*Descriptives Respondent Level

local varlist1 gender agerange1 agerange2 agerange3 education1 education2 education3 life deck1-deck10 ///
      P8 fam stress ecosit ecosit1 ecosit2 marital1 marital3 parents health

tabstat `varlist1' [aw=POND_B], stat(mean sd median p10 p25 p75 p90 N min max) c(s)

drop if gender==. | age==. | ses==. | life==. | health==. | education==. | P8==. ///
        | fam==. | stress==. | ecosit==. | ecosit1==. | ecosit2==. |  ///
		marital1==. | marital3==. | parents==. 

d // Analytic sample 1,989 

local varlist1 gender agerange1 agerange2 agerange3 education1 education2 education3 life deck1-deck10 ///
      P8 fam stress ecosit ecosit1 ecosit2 ecosit3 marital1 marital2 marital3 parents health
tabstat `varlist1' [aw=POND_B], stat(mean sd median N min max) c(s)

*****Level of difficulty

*Distribution of difficulty

recode P8 (9=.)
tab    P8

*Age and Education (Tables A1 and A2)

tab P8 edad_3 [aw=POND_B], col nofreq
tab P8 edad_3, col chi2 V nofreq
tab P8 education [aw=POND_B], col nofreq
tab P8 education, col chi2 V nofreq

*Weighted Cramer's V for RR2

svyset [pweight=POND_B]
svy: tab P8 edad_3, col 
di "Cramer's V=" e(cun_Pear)/e(N)

svy: tab P8 education, col
di "Cramer's V=" e(cun_Pear)/e(N)

*****Model vignette dimensions  

*General vignette model

reshape long P7_V id_vig sexo edad relacion calidad ingresos salud estilo avance, ///
        i(FOLIO) j(vi)
		
lab def  sexo 1 "Male" ///
              2 "Female"
lab val  sexo sexo

lab def edad 1 "20" ///
             2 "30" ///
			 3 "40" ///
			 4 "50" ///
			 5 "60" ///
			 6 "70"
lab val edad edad

lab def relacion 1 "Without couple for 5 years" ///
                 2 "Without couple for 1 year" ///
				 3 "Recent break up" ///
				 4 "With couple for 1 year and" ///
				 5 "With couple for 5 years"
lab val relacion relacion

lab def calidad 1 "Very bad" ///
                2 "Bad" ///
				3 "Good" ///
				4 "Very good"
lab val calidad calidad

lab def ingresos 1 "210,000" ///
                 2 "400,000" ///
				 3 "600,000" ///
				 4 "800,000" ///
				 5 "1,000,000" ///
				 6 "2,000,000" ///
				 7 "3,000,000" ///
				 8 "5,000,000"
lab val ingresos ingresos

lab def salud 1 "Very bad" ///
              2 "Bad" ///
			  3 "Good" ///
			  4 "Very good"
lab val salud salud

lab def estilo 1 "Exciting" ///
               2 "Quiet" ///
			   3 "Boring" ///
			   4 "Stressful"
lab val estilo estilo

lab def avance 1 "Much worse" ///
               2 "Worse" 3 "Same" ///
			   4 "Better" ///
			   5 "Much better"
			   
lab val avance avance

recode salud (1=4) (2=3) (3=2) (4=1)
lab def salud2 1 "Very good" ///
               2 "Good" ///
			   3 "Bad" ///
			   4 "Very bad"
lab val salud salud2

tab relacion, gen(rela)
tab estilo, gen(estilo)

lab def  dic 0 "No" ///
             1 "Yes"
lab val  rela1 dic
lab val  rela2 dic
lab val  rela3 dic
lab val  rela4 dic
lab val  rela5 dic

lab val estilo1 dic
lab val estilo2 dic
lab val estilo3 dic
lab val estilo4 dic

*Model with standardised dimensions (Table 4)

tab relacion, gen(relacion2)
tab estilo, gen(estilo2)

foreach var of varlist sexo edad relacion21 relacion22 relacion23 relacion24 calidad ingresos salud estilo22 estilo23 estilo24 avance numero_deck{
	egen s`var' = std(`var')
}

xtmixed P7_V ///
        ssexo sedad srelacion21 srelacion22 srelacion23 srelacion24 scalidad singresos ssalud sestilo22 sestilo23 sestilo24 savance ///
		i.numero_deck ///
		|| FOLIO:, pweight(POND_B) pwscale(size) variance  covariance(unstructured)


*Andernach & Schunck : squared individual level residual (Table A3) 

order FOLIO vi numero_deck P7_V res

gen   res2 = res*res

order FOLIO vi numero_deck sexo edad relacion calidad ingresos salud estilo avance P7_V res res2

gen V2 = vi*vi
gen V3 = vi*vi*vi

xtmixed res2 ///
        vi V2 V3 ///
		i.education i.edad_3 ///
|| FOLIO:, pweight(POND_B) pwscale(size) variance  covariance(unstructured)
outreg2 using "${output}\Results\squared", dec(3) drop(i.numero_deck) label excel ct(Residuals) alpha(0.001, 0.01, 0.05) replace

mltrsq

*Orthogonality (Table 3)

polychoric  sexo edad relacion calidad ingresos salud estilo avance [aw=POND_B]

*******************************Multilevel model 

*Unstandardised (Table 4)

set more off
xtmixed P7_V ///
        i.sexo c.edad rela1 rela2 rela3 rela4 calidad ingresos salud estilo2 estilo3 estilo4 avance ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO:, pweight(POND_B) pwscale(size) variance  covariance(unstructured)
mltrsq
sleep 1000
outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) replace



*Fixed effect

*Matsize should be bigger for this:
*reg P7_V ///
*        i.sexo c.edad rela1 rela2 rela3 rela4 calidad ingresos salud estilo2 estilo3 estilo4 avance ///
*		i.numero_deck i.FOLIO [pweight=POND_B], vce(robust)
	
*Standardised

foreach var of varlist sexo edad rela1 rela2 rela3 rela4 calidad ingresos salud estilo2 estilo3 estilo4 avance numero_deck{
egen s`var' = std(`var')
}

lab var ssexo Sex
lab var sedad Age
lab var srela1 "Without couple for 5 years"
lab var srela2 "Without couple for 1 year"
lab var srela3 "Recent break up"
lab var srela4 "With couple for 1 year and"
lab var scalidad "Family relationships" 
lab var singresos Income
lab var ssalud Health
lab var sestilo2 Quiet
lab var sestilo3 Boring
lab var sestilo4 Stressful 
lab var savance "Intergenerational mobility"


set more off
xtmixed P7_V ///
        ssexo sedad srela1 srela2 srela3 srela4 scalidad singresos ssalud sestilo2 sestilo3 sestilo4 savance ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO:, pweight(POND_B) pwscale(size) variance  covariance(unstructured)
estimates store stand
sleep 1000
outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 2) alpha(0.001, 0.01, 0.05) append

*Adding marks for a footnote referring to ref. category

lab var srela1 "Without couple for 5 years*"
lab var srela2 "Without couple for 1 year*"
lab var srela3 "Recent break up*"
lab var srela4 "With couple for 1 year and*"

lab var sestilo2 "Quiet^"
lab var sestilo3 "Boring^"
lab var sestilo4 "Stressful^" 


set more off
xtmixed P7_V ///
        ssexo sedad srela1 srela2 srela3 srela4 scalidad singresos ssalud sestilo2 sestilo3 sestilo4 savance ///
		|| FOLIO:, pweight(POND_B) pwscale(size) variance  covariance(unstructured)
estimates store stand
		
coefplot stand, drop(_cons) xline(0, lwidth(thin) lpattern(dash)) xtitle(Happiness rating) graphregion(fcolor(white)) scheme(s2mono)  ///
               xscale(range(-.6 .6)) xlabel(-.6 "-.6" -.4 "-.4" -.2 "-.2" 0 "0" .2 ".2" .4 ".4" .6 ".6") ///
			   groups(sestilo2 sestilo3 sestilo4="{bf:Lifestyle}" srela1 srela2 srela3 srela4="{bf:Partner Relationship}")  
graph save "${output}\stand", replace
graph export "${output}\stand.png", replace
graph export "${output}\stand.pdf", replace

*Linear predictions

set more off
xtmixed P7_V ///
        i.sexo i.edad i.relacion i.calidad i.ingresos i.salud i.estilo i.avance ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO:, pweight(POND_B) pwscale(size) variance  covariance(unstructured)

margins sexo edad relacion calidad ingresos salud estilo avance, atmeans


sleep 1000
outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) replace
		
		
*******************************Vignette-level interactions
		

set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.sexo#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.sexo, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) replace

margins, dydx(ingresos) over(sexo) atmeans 
marginsplot, xtitle("Sex") title("") ytitle("Belief about income") scheme(s2mono) name("sex", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8))
*graph save "${output}\sex.gph", replace
*graph export "${output}\sex.png", replace
*graph export "${output}\sex.pdf", replace		
										
set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.edad#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.edad, pweight(POND_B) pwscale(size) variance  	// not convergence with cov(unst)	
		
*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append

margins , dydx(ingresos) over(edad) atmeans   
marginsplot, xtitle("Age") title("") ytitle("Belief about income") scheme(s2mono) name("age", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8))
*graph save "${output}\age", replace
*graph export "${output}\age.png", replace
*graph export "${output}\age.pdf", replace

set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.rela1#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.rela1, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append

margins, dydx(ingresos) over(rela1) atmeans   
marginsplot, xtitle("Without partner for 5 years") title("") ytitle("Belief about income") scheme(s2mono) name("rela1", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8))
*graph save "${output}\rela1", replace
*graph export "${output}\rela1.png", replace
*graph export "${output}\rela1.pdf", replace


set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.rela2#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append

margins, dydx(ingresos) over(rela2) atmeans   
marginsplot, xtitle("Without partner for 1 years") title("") ytitle("Belief about income") scheme(s2mono) name("rela2", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8))
*graph save "${output}\rela2", replace
*graph export "${output}\rela2.png", replace
*graph export "${output}\rela2.pdf", replace

set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.rela3#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.rela3, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append

margins, dydx(ingresos) over(rela3) atmeans   
marginsplot, xtitle("Recent break up") title("") ytitle("Belief about income") scheme(s2mono) name("rela3", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8))
*graph save "${output}\rela3", replace
*graph export "${output}\rela3.png", replace
*graph export "${output}\rela3.pdf", replace

set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.rela4#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.rela4, pweight(POND_B) pwscale(size) variance  
*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append
		
margins, dydx(ingresos) over(rela4) atmeans
marginsplot, xtitle("With partner for 1 year") title("") ytitle("Belief about income") scheme(s2mono) name("rela4", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8)) //  the difference is not significnat. atmenas distorts it. 
*graph save "${output}\rela4", replace
*graph export "${output}\rela4.png", replace
*graph export "${output}\rela4.pdf", replace
		
		
set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.calidad#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.calidad, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append

margins, dydx(ingresos) over(calidad) atmeans
marginsplot, xtitle("Family relationships") title("") ytitle("Belief about income") scheme(s2mono) name("calidad", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8)) //  the difference is not significnat. atmenas distorts it. 
*graph save "${output}\rela4", replace
*graph export "${output}\rela4.png", replace
*graph export "${output}\rela4.pdf", replace
		
*set more off
*xtmixed P7_V ///
*        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance c.salud#c.ingresos ///
*		i.numero_deck ///
*		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
*		|| FOLIO:, pweight(POND_B) pwscale(size) variance  covariance(unstructured)

lab def heal 1 "Very good" 2 "" 3 "" 4 "Very bad"
lab val salud heal 

set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.salud#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.salud, pweight(POND_B) pwscale(size) variance  

*margins salud, at(ingresos = (1(1)8)) atmeans
*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append

margins, dydx(ingresos) over(salud) atmeans 
marginsplot, xtitle("Health") title("") ytitle("Belief about income") scheme(s2mono) name("salud", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8)) ///
			xlabel(1 `""Very" "good""' 2 "2" 3 "3" 4 `""Very" "bad""')
			
lab val salud salud
			
*graph save "${output}\health", replace
*graph export "${output}\health.png", replace
*graph export "${output}\health.pdf", replace		
						
		
set more off
xtmixed P7_V ///
       i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.estilo2#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.estilo2, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append

margins, dydx(ingresos) over(estilo2) atmeans 
marginsplot, xtitle("Quiet lifestyle") title("") ytitle("Belief about income") scheme(s2mono) name("estilo2", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8))
*graph save "${output}\estilo2", replace
*graph export "${output}\estilo2.png", replace
*graph export "${output}\estilo2.pdf", replace

set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.estilo3#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.estilo3, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append
				
margins, dydx(ingresos) over(estilo3) atmeans   
marginsplot, xtitle("Boring lifestyle") title("") ytitle("Belief about income") scheme(s2mono) name("estilo3", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8))
*graph save "${output}\estilo3", replace
*graph export "${output}\estilo3.png", replace
*graph export "${output}\estilo3.pdf", replace		
						
				
			
set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.estilo4#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.estilo4, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append		

margins, dydx(ingresos) over(estilo4) atmeans   
marginsplot, xtitle("Stressful lifestyle") title("") ytitle("Belief about income") scheme(s2mono) name("estilo4", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8))
*graph save "${output}\estilo4", replace
*graph export "${output}\estilo4.png", replace
*graph export "${output}\estilo4.pdf", replace

lab def graph 1 "Much worse" 2 "" 3 "Same" 4 "" 5 "Much better" 
lab val avance graph 

set more off
xtmixed P7_V ///
       i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance i.avance#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: R.avance, pweight(POND_B) pwscale(size) variance  

*sleep 1000
*outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append		

margins, dydx(ingresos) over(avance) atmeans   
marginsplot, xtitle("Intergenerational mobility") title("") ytitle("Belief about income") scheme(s2mono) name("avance", replace) ///
            graphregion(fcolor(white)) ///
            yscale(range(.05 .25)) ylabel(.05 ".05" .1 ".10" .15 ".15" .2 ".20" .25 ".25") graphregion(margin(6 8 8 8)) ///
			xlabel(1 `""Much" "worse""' 2 "2" 3 "Same" 4 "4" 5 `""Much" "better""')
*graph save "${output}\avance", replace
*graph export "${output}\avance.png", replace
*graph export "${output}\avance.pdf", replace

lab val avance avance

gr combine  sex age rela1 rela2 rela3 rela4 calidad salud estilo2 estilo3 estilo4 avance, graphregion(fcolor(white)) scheme(s2mono)
		
		
*Significant interactions in a single model


set more off
xtmixed P7_V ///
        i.sexo c.edad i.rela1 i.rela2 i.rela3 i.rela4 calidad ingresos salud i.estilo2 i.estilo3 i.estilo4 avance ///
		c.edad#c.ingresos 1.rela1#c.ingresos 1.rela3#c.ingresos 1.rela4#c.ingresos c.salud#c.ingresos 1.estilo3#c.ingresos 1.estilo4#c.ingresos c.avance#c.ingresos ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO:, pweight(POND_B) pwscale(size) variance  covariance(unstructured)
		
sleep 1000
outreg2 using "${output}\Results\multivel_R&R", dec(3) drop(i.numero_deck) label excel ct(Model 1) alpha(0.001, 0.01, 0.05) append		


*******************************Cross-level interactions interactions

global root = "C:\Users\fjoli\Dropbox\MaSocSR\Sem III\SaSR10\assignment data"
global output = "C:\Users\fjoli\Dropbox\190216 Viñetas Social Indicator Research\Results"


set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.rela1#i.marital1  ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  // unstructured covariance structure not allowed for factor variables

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) replace

*Reported
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.rela1#i.marital3 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append



*Reported
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.rela2#i.marital1  ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  // unstructured covariance structure not allowed for factor variables

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append

*Reported
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.rela2#i.marital3 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append


xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.rela3#i.marital1  ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  // unstructured covariance structure not allowed for factor variables

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append

xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.rela3#i.marital3 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append

xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.rela4#i.marital1  ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  // unstructured covariance structure not allowed for factor variables

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append

xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.rela4#i.marital3 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append


*Family 

set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.calidad#c.fam ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append

set more off

xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.salud#c.health ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append

xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		i.avance#c.parents ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.rela2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append

*Income
set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		c.ingresos#i.ecosit1 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: ingresos, pweight(POND_B) pwscale(size) variance  covariance(unstructured)


sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 4) alpha(0.001, 0.01, 0.05) append

*Income
set more off
xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		c.ingresos#i.ecosit2 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: ingresos, pweight(POND_B) pwscale(size) variance  covariance(unstructured)

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 6) alpha(0.001, 0.01, 0.05) append	


*Lifestyle

set more off

xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		c.stress#i.estilo2 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.estilo2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 6) alpha(0.001, 0.01, 0.05) append	

xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		c.stress#i.estilo3 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.estilo2, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 6) alpha(0.001, 0.01, 0.05) append	


xtmixed P7_V ///
        i.sexo i.edad i.rela1 i.rela2 i.rela3 i.rela4 i.calidad ingresos i.salud i.estilo2 i.estilo3 i.estilo4 i.avance ///
		c.stress#i.estilo4 ///
		i.marital1 i.marital3 gender age fam i.ecosit1 i.ecosit2 health stress parents i.numero_deck ///
		|| FOLIO: R.estilo4, pweight(POND_B) pwscale(size) variance  

sleep 1000
outreg2 using "multivel_R&R.xlm", dec(3) drop(i.numero_deck) label excel ct(Model 6) alpha(0.001, 0.01, 0.05) append	

*--------------------TABLE A4

xtmixed P7_V ///
         i.sexo i.edad i.relacion i.calidad i.ingresos i.salud i.estilo i.avance ///
		i.numero_deck ///
		gender i.marital1 i.marital3 fam ecosit1 ecosit2 health stress parents ///
		|| FOLIO: , pweight(POND_B) pwscale(size) variance covariance(unstructured)

		
margin sexo     , atmeans
margin edad     , atmeans
margin relacion , atmeans
margin calidad  , atmeans
margin ingresos , atmeans
margin salud    , atmeans		
		
*70 vs  20

test 1.edad = 6.edad

*70 vs 40		
	
test 3.edad = 6.edad

*Recent breakup (B=-0.130, p<0.001) vs had a partner for 5 years 

test 3.relacion = 5.relacion

*1 year rel = 5 years rel

test 4.relacion = 5.relacion

*1 year single vs 5 years rel

test 2.relacion = 5.relacion

*Having good vs very good family relationships

test 2.calidad = 4.calidad

test 1.calidad = 4.calidad

test 2.calidad = 3.calidad

test 1.calidad = 3.calidad

test 4.calidad = 3.calidad

*Income

test 1.ingresos = 2.ingresos

test 2.ingresos = 3.ingresos

test 3.ingresos = 4.ingresos

test 5.ingresos = 6.ingresos

test 6.ingresos = 7.ingresos

test 7.ingresos = 8.ingresos

*Exciting vs boring 

test 3.estilo = 1.estilo

*Exciting vs stressful

test 4.estilo = 1.estilo

*Nevertheless, there is no significant difference between having an exciting or quiet life. 	

test 2.estilo = 1.estilo

	


