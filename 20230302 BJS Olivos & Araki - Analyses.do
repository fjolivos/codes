********************************************************************************
*                      Cultural Capital and Teacher Bias                       *
*                               May 25 2022                                    *
*                             Olivos & Araki                                   *
********************************************************************************       

clear
set more off

run "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Analyses\ck_teachers_DM.do"

global root   = "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Analyses"
global output = "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Analyses"



***Next


*Check teachers data
*Specify model sof imputation
*Run analysis with analytical sample (create "sample" with complete variables)
*Modify interactions

*****************Analyses before reshaping

drop if ids ==. 
drop if grade9==1 // grade9 did not have follow-up


sum tchiask01 nfactorw1 nfactorw2 legcul01w1 legcul03w1 legcul04w1 read02w1 extra01w1 extra03w1 ///
	legcul01w2 legcul03w2 legcul04w2 read02w2 extra01w2 extra03w2 ///
	tmatpra01 tengpra01 gm1  gc1 ge1 cog1  selfcon1 matsu1 engsu1 clschnsexw1 clsmatsexw1 clsengsexw1 clshrsexw1 ///
	tmatask02 tchiask02  tengask02  tmatpra02 tchipra02 tengpra02 hrtpra02 hrtcri02 ///
	gm2 gc2 ge2 cog2 matsu2 engsu2 stprhedu stmigrant sthktype stsex if w2status==1

keep if w2status==1


*Teachers variables

gen hragew2  = 2015-hrc02w2
gen matagew2 = 2015-matb02w2
gen chnagew2 = 2015-chnb02w2
gen engagew2 = 2015-engb02w2

gen hragew1  = 2015-hrc02w1
gen matagew1 = 2015-matb02w1
gen chnagew1 = 2015-chnb02w1
gen engagew1 = 2015-engb02w1

sum hragew2 hrc04w2 hrc06w2 hrc07w2 clshrsexw2  matagew2 matb04w2 matb06w2 matb07w2 clsmatsexw2 ///
     chnagew2 chnb04w2 chnb06w2 chnb07w2 clschnsexw2 engagew2 engb04w2 engb06w2 engb07w2 clsengsexw1 hra01w1

sum hragew1 hrc04w1 hrc06w1 hrc07w1 clshrsexw1  matagew1 matb04w1 matb06w1 matb07w1 clsmatsexw1 ///
     chnagew1 chnb04w1 chnb06w1 chnb07w1 clschnsexw1 engagew1 engb04w1 engb06w1 engb07w1 clsengsexw1 hra01w1	 

*Parents about teachers for robustness checks

gen schoolworkw1 = bb0501 
gen conductw1    = bb0502 

gen schoolworkw2 = w2bb0501 
gen conductw2    = w2bb0502 
	 
***Plot change

gen changeck = nfactorw1 - nfactorw2
 histogram  changeck, percent normal xtitle("Change in Cultural Capital Score")	scheme(s2mono)
graph export "${output}\CKchange.png", replace 
 

**************Reshape

reshape long nfactorw tmatask0 tchiask0 tengask0 tmatpra0 tchipra0 tengpra0 hrtpra0 hrtcri0 ///
legcul01w legcul03w legcul04w read02w extra01w extra03w ///
gm gc ge cog  numbestfrie selfcon matsu chisu engsu teacon ///
hragew matagew chnagew engagew hrc04w hrc07w clshrsexw  matb02w matb04w matb07w clsmatsexw /// teachers' variables
chnb02w chnb04w chnb07w clschnsexw engb02w engb04w engb07w clsengsexw hra01w /// 07=experience, 04=degree, 02=birth
schoolworkw conductw ///
, i(ids) j(wave)

xtset ids wave 

lab var legcul01w "Visiting museums"

lab var legcul03w "Musical instruments"

lab var legcul04w "Calligraphy"

lab var read02w "Books at home"

lab var extra01w "Extracurricular activities"

lab var extra03w "Summer/winter camps"


gen hpocc = .
replace hpocc = focc1 if  focc1>=mocc1
replace hpocc = mocc1 if  focc1<mocc1 
replace hpocc = focc1 if  mocc1==.
replace hpocc = mocc1 if  focc1==. 

*Teachers' characteristics

tab hrc04w, gen(hrc04w) 
tab matb04w, gen(matb04w) 

replace matb04w1 = 1 if matb04w==2 | matb04w==4
replace matb04w2 = 1 if matb04w==5
replace matb04w3 = 1 if matb04w==6
replace matb04w4 = 1 if matb04w==7

replace matb04w1 = 0 if matb04w>4
replace matb04w2 = 0 if matb04w!=5
replace matb04w3 = 0 if matb04w!=6
replace matb04w4 = 0 if matb04w!=7

tab chnb04w, gen(chnb04w) 
tab engb04w, gen(engb04w)

tab hra01w, gen(hra01w)

recode clshrsexw (1=0) (2=1), gen(hrfemale)
recode clsmatsexw (1=0) (2=1), gen(matfemale)
recode clschnsexw (1=0) (2=1), gen(chnfemale)
recode clsengsexw (1=0) (2=1), gen(engfemale)

****Descriptives

*Analytical sample

xtreg hrtpra0 tmatpra0 tchipra0 tengpra0 tmatask0 tchiask0 tengask0 hrtcri0 legcul01w legcul03w legcul04w read02w extra01w extra03w nfactorw ///
      gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave ///
	  hragew matagew chnagew engagew hrc04w hrc07w clshrsexw  matb02w matb04w matb07w clsmatsexw /// teachers' variables
      chnb02w chnb04w chnb07w clschnsexw engb02w engb04w engb07w clsengsexw hra01w ///
	  c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc
gen asample = e(sample) // 8,300   

graph box nfactorw if asample==1 & wave==1, by(stsex, note("")) ytitle(Cultural Capital Index) ///
             		scheme(s2mono)
graph export "${output}\BoxCulK_genw1.png", replace

graph box nfactorw if asample==1 & wave==1, by(stsex, note("")) ytitle(Cultural Capital Index) ///
             		scheme(s2mono)
graph export "${output}\BoxCulK_genw2.png", replace

*Students' characteristics

tabstat hrtpra0 tmatpra0 tchipra0 tengpra0 tmatask0 tchiask0 tengask0 hrtcri0 ///
        nfactorw  legcul01w legcul03w legcul04w read02w extra01w extra03w ///
		gm gc ge cog numbestfrie selfcon matsu chisu engsu ///
		if asample==1 & wave==1, stat(mean sd min max) c(s)

tabstat hrtpra0 tmatpra0 tchipra0 tengpra0 tmatask0 tchiask0 tengask0 hrtcri0 ///
        nfactorw  legcul01w legcul03w legcul04w read02w extra01w extra03w ///
		gm gc ge cog numbestfrie selfcon matsu chisu engsu ///
	    if asample==1 & wave==2, stat(mean sd min max) c(s)	
recode stprhedu (1/7=0) (8/9=1), gen(pcoll)
recode stmigrant (1=1) (2/3=0), gen(migrant)
tabstat pcoll migrant sthktype stsex if asample==1 & wave==1, stat(mean sd min max) c(s)		
		
*Teachers' characteristics

tabstat hragew hrc07w hrfemale hrc04w1 hrc04w2 hrc04w3 hrc04w4 hra01w1-hra01w4 ///
		matagew matb07w matfemale matb04w1-matb04w4 ///
		chnagew chnb07w chnfemale chnb04w1-chnb04w4 ///
		engagew engb07w engfemale engb04w1-engb04w4 ///
		if asample==1 & wave==1, stat(mean sd min max) c(s)

tabstat hragew hrc07w hrfemale hrc04w1 hrc04w2 hrc04w3 hrc04w4 hra01w1-hra01w4 ///
		matagew matb07w matfemale matb04w1-matb04w4 ///
		chnagew chnb07w chnfemale chnb04w1-chnb04w4 ///
		engagew engb07w engfemale engb04w1-engb04w4 ///
		if asample==1 & wave==2, stat(mean sd min max) c(s)

		
*Cronbach's alpha

alpha legcul01w legcul03w legcul04w read02w extra01w extra03w if wave==1 & asample==1, item //  0.5449
alpha legcul01w legcul03w legcul04w read02w extra01w extra03w if wave==2 & asample==1, item	// 0.4845	
				
		
*************FE models (Table 3-4)

*Hausman can not be used without vce cluster and they support fe

*Praised by homeroom tacher

xtreg hrtpra0 nfactorw i.wave if asample==1, vce(cluster clsids)
outreg2 using "${output}\index", dec(3) label excel ct(hrtpra) alpha(0.001, 0.01, 0.05) replace
xtreg hrtpra0 nfactorw i.wave if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\index", dec(3) label excel ct(hrtpra) alpha(0.001, 0.01, 0.05) append
xtreg hrtpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave if asample==1, vce(cluster clsids)
outreg2 using "${output}\index", dec(3) label excel ct(hrtpra) alpha(0.001, 0.01, 0.05) append
xtreg hrtpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1 // Reported Table 3
estimate store a2
outreg2 using "${output}\index", dec(3) label excel ct(hrtpra) alpha(0.001, 0.01, 0.05) append

xtreg hrtpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1, fe vce(cluster clsids) 
estimate store a1
outreg2 using "${output}\index", dec(3) label excel ct(hrtpra) alpha(0.001, 0.01, 0.05) append

*hausman a1 a2, sigmamore

*Praised by subject teachers


xtreg tmatpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1, vce(cluster clsids) // Reported Table 3
estimate store b2
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
xtreg tmatpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1, fe vce(cluster clsids) // Reported Table 4
estimate store b1
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*hausman b1 b2, sigmamore

xtreg tchipra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1, vce(cluster clsids) // Reported Table 3
estimate store c2 
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
xtreg tchipra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1, fe vce(cluster clsids)// Reported Table 4
estimate store c1
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*hausman c1 c2, sigmamore

xtreg tengpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 if asample==1, vce(cluster clsids) // Reported Table 3
estimate store d2
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
xtreg tengpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 if asample==1, fe vce(cluster clsids)// Reported Table 4
estimate store d1
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*hausman d1 d2, sigmamore

*Ask question to student 

xtreg tmatask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1, vce(cluster clsids) // Reported Table 3
estimate store e2
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
xtreg tmatask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1, fe vce(cluster clsids)// Reported Table 4
estimate store e1
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*hausman e1 e2, sigmamore

xtreg tchiask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1, vce(cluster clsids) // Reported Table 3
estimate store f2
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
xtreg tchiask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1, fe vce(cluster clsids)// Reported Table 4
estimate store f1
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*hausman f1 f2, sigmamore

xtreg tengask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 if asample==1, vce(cluster clsids) // Reported Table 3
estimate store g2
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
xtreg tengask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 if asample==1, fe vce(cluster clsids) // Reported Table 4
estimate store g1
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*hausman g1 g2, sigmamore

*Home room teacher criticizes

xtreg hrtcri0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1, vce(cluster clsids) // Reported Table 3
estimate store h2
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
xtreg hrtcri0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1, fe vce(cluster clsids) // Reported Table 4
estimate store h1
outreg2 using "${output}\index", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*hausman h1 h2, sigmamore

*************FE models per item (Table 5)

xtreg hrtcri0 legcul01w gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1, fe vce(cluster clsids) // delete, just the base table
outreg2 using "${output}\items", dec(3) label excel ct(FE) alpha(0.001, 0.01, 0.05) replace

*Praised by homeroom tacher

xtreg hrtpra0 legcul01w legcul03w legcul04w read02w extra01w extra03w gm gc ge cog numbestfrie selfcon matsu chisu engsu hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 i.wave if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\items", dec(3) label excel ct(hrtpra0) alpha(0.001, 0.01, 0.05) append

*Praised by subject teachers

xtreg tmatpra0 legcul01w legcul03w legcul04w read02w extra01w extra03w gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\items", dec(3) label excel ct(tmatpra0) alpha(0.001, 0.01, 0.05) append

xtreg tchipra0 legcul01w legcul03w legcul04w read02w extra01w extra03w gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\items", dec(3) label excel ct(tchipra0) alpha(0.001, 0.01, 0.05) append

xtreg tengpra0 legcul01w legcul03w legcul04w read02w extra01w extra03w gm gc ge cog numbestfrie selfcon matsu chisu engsu engagew engb07w engfemale engb04w2-engb04w4 i.wave if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\items", dec(3) label excel ct(tengpra0) alpha(0.001, 0.01, 0.05) append


*Ask question to student 

xtreg tmatask0 legcul01w legcul03w legcul04w read02w extra01w extra03w gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\items", dec(3) label excel ct(tmatask0) alpha(0.001, 0.01, 0.05) append

xtreg tchiask0 legcul01w legcul03w legcul04w read02w extra01w extra03w gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\items", dec(3) label excel ct(tchiask0) alpha(0.001, 0.01, 0.05) append

xtreg tengask0 legcul01w legcul03w legcul04w read02w extra01w extra03w gm gc ge cog numbestfrie selfcon matsu chisu engsu engagew engb07w engfemale engb04w2-engb04w4 i.wave if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\items", dec(3) label excel ct(tengask0) alpha(0.001, 0.01, 0.05) append

*Homre room teacher criticizes

xtreg hrtcri0 legcul01w legcul03w legcul04w read02w extra01w extra03w gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\items", dec(3) label excel ct(hrtcri0) alpha(0.001, 0.01, 0.05) append


***Heterogeneity by gender

xtreg hrtpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave ///
			  if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(hrtpra0) alpha(0.001, 0.01, 0.05) replace

foreach var of varlist hrtpra0 hrtcri0{
xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 ///
			  if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
			  
xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            c.nfactorw#i.stsex if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            c.nfactorw#c.stprhedu c.nfactorw#i.stmigrant c.nfactorw#i.sthktype c.nfactorw#i.stsex c.nfactorw#i.hpocc if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
}


foreach var of varlist tmatpra0 tmatask0 {
xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave ///
			if asample==1, vce(cluster clsids)
			
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
			  
xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave  matagew matb07w matfemale matb04w2-matb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            c.nfactorw#i.stsex if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave  matagew matb07w matfemale matb04w2-matb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            c.nfactorw#c.stprhedu c.nfactorw#i.stmigrant c.nfactorw#i.sthktype c.nfactorw#i.stsex c.nfactorw#i.hpocc if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
}


foreach var of varlist tchipra0 tchiask0 {
xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 ///
			  if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
			  
xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            c.nfactorw#i.stsex if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            c.nfactorw#c.stprhedu c.nfactorw#i.stmigrant c.nfactorw#i.sthktype c.nfactorw#i.stsex c.nfactorw#i.hpocc if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
}



foreach var of varlist tengpra0 tengask0  {
xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 ///
			  if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
			  
xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            c.nfactorw#i.stsex if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg `var' nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 ///
            c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
            c.nfactorw#c.stprhedu c.nfactorw#i.stmigrant c.nfactorw#i.sthktype c.nfactorw#i.stsex c.nfactorw#i.hpocc if asample==1, vce(cluster clsids)
outreg2 using "${output}\inte", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append
}


*Visualization of heterogeneities

xtreg hrtcri0  nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 ///
               c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
               c.nfactorw#c.stprhedu c.nfactorw#i.stmigrant c.nfactorw#i.sthktype c.nfactorw#i.stsex c.nfactorw#i.hpocc if asample==1, vce(cluster clsids)
			   
margins, at(nfactorw = (-1 (1) 3) stsex = (0 1)) atmeans 
marginsplot, xtitle(Cultural Capital Index) ytitle(Homeeroom teacher criticizes me) ///
             yscale(range(1.5(.5) 3.5)) ylabel(1.5 "1.5 " 2 "2.0" 2.5 "2.5" 3 "3.0" 3.5 "3.5") ///
			 scheme(s2mono) noci title("")
graph export "${output}\cri_hrt.png", as(png) replace

xtreg tmatask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 ///
               c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
               c.nfactorw#c.stprhedu c.nfactorw#i.stmigrant c.nfactorw#i.sthktype c.nfactorw#i.stsex c.nfactorw#i.hpocc if asample==1, vce(cluster clsids)
			   
margins, at(nfactorw = (-1 (1) 3) stsex = (0 1)) atmeans 

marginsplot, xtitle(Cultural Capital Index) ytitle(Mathematics teacher asks me questions) ///
             yscale(range(1.5(.5) 3.5)) ylabel(1.5 "1.5 " 2 "2.0" 2.5 "2.5" 3 "3.0" 3.5 "3.5") ///
			 scheme(s2mono) noci title("")
graph export "${output}\tmatask0.png", as(png) replace


xtreg tchiask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave  chnagew chnb07w chnfemale chnb04w2-chnb04w4 ///
               c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
               c.nfactorw#c.stprhedu c.nfactorw#i.stmigrant c.nfactorw#i.sthktype c.nfactorw#i.stsex c.nfactorw#i.hpocc if asample==1, vce(cluster clsids)

margins, at(nfactorw = (-1 (1) 3) stsex = (0 1)) atmeans 

marginsplot, xtitle(Cultural Capital Index) ytitle(Chinese teacher asks me questions) ///
             yscale(range(1.5(.5) 3.5)) ylabel(1.5 "1.5 " 2 "2.0" 2.5 "2.5" 3 "3.0" 3.5 "3.5") ///
			 scheme(s2mono) noci title("")
graph export "${output}\tchiask0.png", as(png) replace
			   
xtreg tengask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 ///
               c.stprhedu i.stmigrant i.sthktype i.stsex i.hpocc ///
               c.nfactorw#c.stprhedu c.nfactorw#i.stmigrant c.nfactorw#i.sthktype c.nfactorw#i.stsex c.nfactorw#i.hpocc if asample==1, vce(cluster clsids)

margins, at(nfactorw = (-1 (1) 3) stsex = (0 1)) atmeans 

marginsplot, xtitle(Cultural Capital Index) ytitle(English teacher asks me questions) ///
             yscale(range(1.5(.5) 3.5)) ylabel(1.5 "1.5 " 2 "2.0" 2.5 "2.5" 3 "3.0" 3.5 "3.5") ///
			 scheme(s2mono) noci title("")
graph export "${output}\tengask0.png", as(png) replace



*********Robustness checks

***Fixed effects ordinal logit models

*Praised by homeroom tacher

feologit hrtpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1, group(ids) cluster(clsids)
outreg2 using "${output}\feologit", dec(3) label excel ct(hrtpra) alpha(0.001, 0.01, 0.05) replace

*Praised by subject teachers

feologit tmatpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1, group(ids) cluster(clsids)
outreg2 using "${output}\feologit", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

feologit tchipra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1, group(ids) cluster(clsids)
outreg2 using "${output}\feologit", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

feologit tengpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 if asample==1, group(ids) cluster(clsids)
outreg2 using "${output}\feologit", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*Ask question to student 

feologit tmatask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1, group(ids) cluster(clsids)
outreg2 using "${output}\feologit", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

feologit tchiask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1, group(ids) cluster(clsids)
outreg2 using "${output}\feologit", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

feologit tengask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 if asample==1, group(ids) cluster(clsids)
outreg2 using "${output}\feologit", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

*Home room teacher criticizes

feologit hrtcri0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1, group(ids)  cluster(clsids)
outreg2 using "${output}\feologit", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append


***Including parents variables

xtreg hrtpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 ///
schoolworkw conductw if asample==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_pa", dec(3) label excel ct(hrtpra) alpha(0.001, 0.01, 0.05) replace

xtreg tmatpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 ///
schoolworkw conductw if asample==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_pa", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tchipra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 ///
schoolworkw conductw if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\index_pa", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tengpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 ///
schoolworkw conductw if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\index_pa", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tmatask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 ///
schoolworkw conductw if asample==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_pa", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tchiask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 ///
schoolworkw conductw if asample==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_pa", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tengask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 ///
schoolworkw conductw if asample==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_pa", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg hrtcri0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 ///
schoolworkw conductw if asample==1, fe vce(cluster clsids)
outreg2 using "${output}\index_pa", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append



***Subsumple of followed-up teachers

xtreg hrtpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1 & fuhrcw2==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_fut", dec(3) label excel ct(hrtpra) alpha(0.001, 0.01, 0.05) replace

xtreg tmatpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1 & fumatcw2==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_fut", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tchipra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1 & fuchncw2==1, fe vce(cluster clsids)
outreg2 using "${output}\index_fut", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tengpra0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 if asample==1 & fuengcw2==1, fe vce(cluster clsids)
outreg2 using "${output}\index_fut", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tmatask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave matagew matb07w matfemale matb04w2-matb04w4 if asample==1 & fumatcw2==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_fut", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tchiask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave chnagew chnb07w chnfemale chnb04w2-chnb04w4 if asample==1 & fuchncw2==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_fut", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg tengask0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave engagew engb07w engfemale engb04w2-engb04w4 if asample==1 & fuengcw2==1, fe vce(cluster clsids) 
outreg2 using "${output}\index_fut", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append

xtreg hrtcri0 nfactorw gm gc ge cog numbestfrie selfcon matsu chisu engsu i.wave hragew hrc07w hrfemale hrc04w2-hrc04w4 hra01w2-hra01w4 if asample==1 & fuhrcw2==1, fe vce(cluster clsids)
outreg2 using "${output}\index_fut", dec(3) label excel ct(`var') alpha(0.001, 0.01, 0.05) append


***SEM model

clear
set more off

run "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Analyses\ck_teachers_DM.do"

global root   = "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Analyses"
global output = "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Analyses"

drop if ids ==. 
drop if grade9==1 // grade9 did not have follow-up


sum tchiask01 nfactorw1 nfactorw2 legcul01w1 legcul03w1 legcul04w1 read02w1 extra01w1 extra03w1 ///
	legcul01w2 legcul03w2 legcul04w2 read02w2 extra01w2 extra03w2 ///
	tmatpra01 tengpra01 gm1  gc1 ge1 cog1  selfcon1 matsu1 engsu1 clschnsexw1 clsmatsexw1 clsengsexw1 clshrsexw1 ///
	tmatask02 tchiask02  tengask02  tmatpra02 tchipra02 tengpra02 hrtpra02 hrtcri02 ///
	gm2 gc2 ge2 cog2 matsu2 engsu2 stprhedu stmigrant sthktype stsex if w2status==1

keep if w2status==1

gen hragew2  = 2015-hrc02w2
gen matagew2 = 2015-matb02w2
gen chnagew2 = 2015-chnb02w2
gen engagew2 = 2015-engb02w2

gen hragew1  = 2015-hrc02w1
gen matagew1 = 2015-matb02w1
gen chnagew1 = 2015-chnb02w1
gen engagew1 = 2015-engb02w1

sum hragew1 matagew1 chnagew1 engagew1 hrc04w1 hrc07w1 clshrsexw1  matb02w1 matb04w1 matb07w1 clsmatsexw1 chnb02w1 chnb04w1 chnb07w1 clschnsexw1 engb02w1 engb04w1 engb07w1 clsengsexw1 hra01w1 
	 
sum hragew2 matagew2 chnagew2 engagew2 hrc04w2 hrc07w2 clshrsexw2  matb02w2 matb04w2 matb07w2 clsmatsexw2 chnb02w2 chnb04w2 chnb07w2 clschnsexw2 engb02w2 engb04w2 engb07w2 clsengsexw2 hra01w2 

*Structural models with controls

sem (tmatask02 tchiask02 tengask02 tmatpra02 tchipra02 tengpra02 hrtpra02 hrtcri02 nfactorw2 <- tmatask01 tchiask01 tengask01 tmatpra01 tchipra01 tengpra01 hrtpra01 hrtcri01 nfactorw1)
sem (tmatask02 tchiask02 tengask02 tmatpra02 tchipra02 tengpra02 hrtpra02 hrtcri02 nfactorw2 <- tmatask01 tchiask01 tengask01 tmatpra01 tchipra01 tengpra01 hrtpra01 hrtcri01 nfactorw1 stprhedu stmigrant sthktype stsex) ///
    (tmatask01 tchiask01 tengask01 tmatpra01 tchipra01 tengpra01 hrtpra01 hrtcri01 nfactorw1 <- stprhedu stmigrant sthktype stsex)


*Structural models with invariant controls	
	
sem (hrtpra02 hrtcri02 nfactorw2 <- hrtpra01 hrtcri01 nfactorw1 stprhedu stmigrant sthktype stsex) ///
    (hrtpra01 hrtcri01 nfactorw1 <- stprhedu stmigrant sthktype stsex), stand ///
	var(e.hrtpra01*e.hrtcri01 e.hrtpra01*e.nfactorw1 e.hrtcri01*e.nfactorw1 ///
	    e.hrtpra02*e.hrtcri02 e.hrtpra02*e.nfactorw2 e.hrtcri02*e.nfactorw2)	
	
sem (tmatask02 tmatpra02 nfactorw2 <- tmatask01 tmatpra01 nfactorw1 stprhedu stmigrant sthktype stsex) ///
    (tmatask01 tmatpra01 nfactorw1 <- stprhedu stmigrant sthktype stsex), stand ///
	var(e.tmatask01*e.tmatpra01 e.tmatask01*e.nfactorw1 e.tmatpra01*e.nfactorw1 ///
	    e.tmatask02*e.tmatpra02 e.tmatask02*e.nfactorw2 e.tmatpra02*e.nfactorw2)

sem (tchiask02 tchipra02 nfactorw2 <- tchiask01 tchipra01 nfactorw1 stprhedu stmigrant sthktype stsex) ///
    (tchiask01 tchipra01 nfactorw1 <- stprhedu stmigrant sthktype stsex), stand ///
	var(e.tchiask01*e.tchipra01 e.tchiask01*e.nfactorw1 e.tchipra01*e.nfactorw1 ///
	    e.tchiask02*e.tchipra02 e.tchiask02*e.nfactorw2 e.tchipra02*e.nfactorw2)
	
sem (tengask02 tengpra02 nfactorw2 <- tengask01 tengpra01 nfactorw1 stprhedu stmigrant sthktype stsex) ///
    (tengask01 tengpra01 nfactorw1 <- stprhedu stmigrant sthktype stsex), stand ///
	var(e.tengask01*e.tengpra01 e.tengask01*e.nfactorw1 e.tengpra01*e.nfactorw1 ///
	    e.tengask02*e.tengpra02 e.tengask02*e.nfactorw2 e.tengpra02*e.nfactorw2)


*Structural models with time variante reciprocal controls		

sem (hrtpra02 hrtcri02 nfactorw2 gm2 gc2 ge2 cog2 numbestfrie2 selfcon2 matsu2 chisu2 engsu2 <- hrtpra01 hrtcri01 nfactorw1 gm1 gc1 ge1 cog1 numbestfrie1 selfcon1 matsu1 chisu1 engsu1 stprhedu stmigrant sthktype stsex) ///
    (hrtpra01 hrtcri01 nfactorw1 gm1 gc1 ge1 cog1 numbestfrie1 selfcon1 matsu1 chisu1 engsu1 <- stprhedu stmigrant sthktype stsex), stand ///
	var(e.hrtpra01*e.hrtcri01 e.hrtpra01*e.nfactorw1 e.hrtcri01*e.nfactorw1 ///
	    e.hrtpra02*e.hrtcri02 e.hrtpra02*e.nfactorw2 e.hrtcri02*e.nfactorw2 ///
		e.gm2*e.gc2 e.gm2*e.ge2 e.gm2*e.cog2 e.gm2*e.numbestfrie2 e.gm2*e.selfcon2 e.gm2*e.matsu2 e.gm2*e.chisu2 e.gm2*e.engsu2 ///
		e.gc2*e.ge2 e.gc2*e.cog2 e.gc2*e.numbestfrie2 e.gc2*e.selfcon2 e.gc2*e.matsu2 e.gc2*e.chisu2 e.gc2*e.engsu2 ///
		e.ge2*e.cog2 e.ge2*e.numbestfrie2 e.ge2*e.selfcon2 e.ge2*e.matsu2 e.ge2*e.chisu2 e.ge2*e.engsu2 ///
		e.numbestfrie2*e.selfcon2 e.numbestfrie2*e.matsu2 e.numbestfrie2*e.chisu2 e.numbestfrie2*e.engsu2 ///
		e.selfcon2*e.matsu2 e.selfcon2*e.chisu2 e.selfcon2*e.engsu2 ///
		e.matsu2*e.chisu2 e.matsu2*e.engsu2 ///
		e.chisu2*e.engsu2 ///
		e.gm1*e.gc1 e.gm1*e.ge1 e.gm1*e.cog1 e.gm1*e.numbestfrie1 e.gm1*e.selfcon1 e.gm1*e.matsu1 e.gm1*e.chisu1 e.gm1*e.engsu1 ///
		e.gc1*e.ge1 e.gc1*e.cog1 e.gc1*e.numbestfrie1 e.gc1*e.selfcon1 e.gc1*e.matsu1 e.gc1*e.chisu1 e.gc1*e.engsu1 ///
		e.ge1*e.cog1 e.ge1*e.numbestfrie1 e.ge1*e.selfcon1 e.ge1*e.matsu1 e.ge1*e.chisu1 e.ge1*e.engsu1 ///
		e.numbestfrie1*e.selfcon1 e.numbestfrie1*e.matsu1 e.numbestfrie1*e.chisu1 e.numbestfrie1*e.engsu1 ///
		e.selfcon1*e.matsu1 e.selfcon1*e.chisu1 e.selfcon1*e.engsu1 ///
		e.matsu1*e.chisu1 e.matsu1*e.engsu1 ///
		e.chisu1*e.engsu1 ///
		e.hrtpra01*e.gm1 e.hrtcri01*e.gm1 e.nfactorw1*e.gm1 ///
		e.hrtpra01*e.gc1 e.hrtcri01*e.gc1 e.nfactorw1*e.gc1 ///
		e.hrtpra01*e.ge1 e.hrtcri01*e.ge1 e.nfactorw1*e.ge1 ///
		e.hrtpra01*e.cog1 e.hrtcri01*e.cog1 e.nfactorw1*e.cog1 ///
		e.hrtpra01*e.numbestfrie1 e.hrtcri01*e.numbestfrie1 e.nfactorw1*e.numbestfrie1 ///
		e.hrtpra01*e.selfcon1 e.hrtcri01*e.selfcon1 e.nfactorw1*e.selfcon1 ///
		e.hrtpra01*e.matsu1 e.hrtcri01*e.matsu1 e.nfactorw1*e.matsu1 ///
		e.hrtpra01*e.chisu1 e.hrtcri01*e.chisu1 e.nfactorw1*e.chisu1 ///
		e.hrtpra01*e.engsu1 e.hrtcri01*e.engsu1 e.nfactorw1*e.engsu1)			
		

sem (tmatask02 tmatpra02 nfactorw2 gm2 gc2 ge2 cog2 numbestfrie2 selfcon2 matsu2 chisu2 engsu2 <- tmatask01 tmatpra01 nfactorw1 gm1 gc1 ge1 cog1 numbestfrie1 selfcon1 matsu1 chisu1 engsu1 stprhedu stmigrant sthktype stsex) ///
    (tmatask01 tmatpra01 nfactorw1 gm1 gc1 ge1 cog1 numbestfrie1 selfcon1 matsu1 chisu1 engsu1 <- stprhedu stmigrant sthktype stsex), stand ///
	var(e.tmatask01*e.tmatpra01 e.tmatask01*e.nfactorw1 e.tmatpra01*e.nfactorw1 ///
	    e.tmatask02*e.tmatpra02 e.tmatask02*e.nfactorw2 e.tmatpra02*e.nfactorw2 ///
		e.gm2*e.gc2 e.gm2*e.ge2 e.gm2*e.cog2 e.gm2*e.numbestfrie2 e.gm2*e.selfcon2 e.gm2*e.matsu2 e.gm2*e.chisu2 e.gm2*e.engsu2 ///
		e.gc2*e.ge2 e.gc2*e.cog2 e.gc2*e.numbestfrie2 e.gc2*e.selfcon2 e.gc2*e.matsu2 e.gc2*e.chisu2 e.gc2*e.engsu2 ///
		e.ge2*e.cog2 e.ge2*e.numbestfrie2 e.ge2*e.selfcon2 e.ge2*e.matsu2 e.ge2*e.chisu2 e.ge2*e.engsu2 ///
		e.numbestfrie2*e.selfcon2 e.numbestfrie2*e.matsu2 e.numbestfrie2*e.chisu2 e.numbestfrie2*e.engsu2 ///
		e.selfcon2*e.matsu2 e.selfcon2*e.chisu2 e.selfcon2*e.engsu2 ///
		e.matsu2*e.chisu2 e.matsu2*e.engsu2 ///
		e.chisu2*e.engsu2 ///
		e.gm1*e.gc1 e.gm1*e.ge1 e.gm1*e.cog1 e.gm1*e.numbestfrie1 e.gm1*e.selfcon1 e.gm1*e.matsu1 e.gm1*e.chisu1 e.gm1*e.engsu1 ///
		e.gc1*e.ge1 e.gc1*e.cog1 e.gc1*e.numbestfrie1 e.gc1*e.selfcon1 e.gc1*e.matsu1 e.gc1*e.chisu1 e.gc1*e.engsu1 ///
		e.ge1*e.cog1 e.ge1*e.numbestfrie1 e.ge1*e.selfcon1 e.ge1*e.matsu1 e.ge1*e.chisu1 e.ge1*e.engsu1 ///
		e.numbestfrie1*e.selfcon1 e.numbestfrie1*e.matsu1 e.numbestfrie1*e.chisu1 e.numbestfrie1*e.engsu1 ///
		e.selfcon1*e.matsu1 e.selfcon1*e.chisu1 e.selfcon1*e.engsu1 ///
		e.matsu1*e.chisu1 e.matsu1*e.engsu1 ///
		e.chisu1*e.engsu1 ///
		e.tmatask01*e.gm1 e.tmatpra01*e.gm1 e.nfactorw1*e.gm1 ///
		e.tmatask01*e.gc1 e.tmatpra01*e.gc1 e.nfactorw1*e.gc1 ///
		e.tmatask01*e.ge1 e.tmatpra01*e.ge1 e.nfactorw1*e.ge1 ///
		e.tmatask01*e.cog1 e.tmatpra01*e.cog1 e.nfactorw1*e.cog1 ///
		e.tmatask01*e.numbestfrie1 e.tmatpra01*e.numbestfrie1 e.nfactorw1*e.numbestfrie1 ///
		e.tmatask01*e.selfcon1 e.tmatpra01*e.selfcon1 e.nfactorw1*e.selfcon1 ///
		e.tmatask01*e.matsu1 e.tmatpra01*e.matsu1 e.nfactorw1*e.matsu1 ///
		e.tmatask01*e.chisu1 e.tmatpra01*e.chisu1 e.nfactorw1*e.chisu1 ///
		e.tmatask01*e.engsu1 e.tmatpra01*e.engsu1 e.nfactorw1*e.engsu1)

sem (tchiask02 tchipra02 nfactorw2 gm2 gc2 ge2 cog2 numbestfrie2 selfcon2 matsu2 chisu2 engsu2 <- tchiask01 tchipra01 nfactorw1 gm1 gc1 ge1 cog1 numbestfrie1 selfcon1 matsu1 chisu1 engsu1 stprhedu stmigrant sthktype stsex) ///
    (tchiask01 tchipra01 nfactorw1 gm1 gc1 ge1 cog1 numbestfrie1 selfcon1 matsu1 chisu1 engsu1 <- stprhedu stmigrant sthktype stsex), stand ///
	var(e.tchiask01*e.tchipra01 e.tchiask01*e.nfactorw1 e.tchipra01*e.nfactorw1 ///
	    e.tchiask02*e.tchipra02 e.tchiask02*e.nfactorw2 e.tchipra02*e.nfactorw2 ///
		e.gm2*e.gc2 e.gm2*e.ge2 e.gm2*e.cog2 e.gm2*e.numbestfrie2 e.gm2*e.selfcon2 e.gm2*e.matsu2 e.gm2*e.chisu2 e.gm2*e.engsu2 ///
		e.gc2*e.ge2 e.gc2*e.cog2 e.gc2*e.numbestfrie2 e.gc2*e.selfcon2 e.gc2*e.matsu2 e.gc2*e.chisu2 e.gc2*e.engsu2 ///
		e.ge2*e.cog2 e.ge2*e.numbestfrie2 e.ge2*e.selfcon2 e.ge2*e.matsu2 e.ge2*e.chisu2 e.ge2*e.engsu2 ///
		e.numbestfrie2*e.selfcon2 e.numbestfrie2*e.matsu2 e.numbestfrie2*e.chisu2 e.numbestfrie2*e.engsu2 ///
		e.selfcon2*e.matsu2 e.selfcon2*e.chisu2 e.selfcon2*e.engsu2 ///
		e.matsu2*e.chisu2 e.matsu2*e.engsu2 ///
		e.chisu2*e.engsu2 ///
		e.gm1*e.gc1 e.gm1*e.ge1 e.gm1*e.cog1 e.gm1*e.numbestfrie1 e.gm1*e.selfcon1 e.gm1*e.matsu1 e.gm1*e.chisu1 e.gm1*e.engsu1 ///
		e.gc1*e.ge1 e.gc1*e.cog1 e.gc1*e.numbestfrie1 e.gc1*e.selfcon1 e.gc1*e.matsu1 e.gc1*e.chisu1 e.gc1*e.engsu1 ///
		e.ge1*e.cog1 e.ge1*e.numbestfrie1 e.ge1*e.selfcon1 e.ge1*e.matsu1 e.ge1*e.chisu1 e.ge1*e.engsu1 ///
		e.numbestfrie1*e.selfcon1 e.numbestfrie1*e.matsu1 e.numbestfrie1*e.chisu1 e.numbestfrie1*e.engsu1 ///
		e.selfcon1*e.matsu1 e.selfcon1*e.chisu1 e.selfcon1*e.engsu1 ///
		e.matsu1*e.chisu1 e.matsu1*e.engsu1 ///
		e.chisu1*e.engsu1 ///
		e.tchiask01*e.gm1 e.tchipra01*e.gm1 e.nfactorw1*e.gm1 ///
		e.tchiask01*e.gc1 e.tchipra01*e.gc1 e.nfactorw1*e.gc1 ///
		e.tchiask01*e.ge1 e.tchipra01*e.ge1 e.nfactorw1*e.ge1 ///
		e.tchiask01*e.cog1 e.tchipra01*e.cog1 e.nfactorw1*e.cog1 ///
		e.tchiask01*e.numbestfrie1 e.tchipra01*e.numbestfrie1 e.nfactorw1*e.numbestfrie1 ///
		e.tchiask01*e.selfcon1 e.tchipra01*e.selfcon1 e.nfactorw1*e.selfcon1 ///
		e.tchiask01*e.matsu1 e.tchipra01*e.matsu1 e.nfactorw1*e.matsu1 ///
		e.tchiask01*e.chisu1 e.tchipra01*e.chisu1 e.nfactorw1*e.chisu1 ///
		e.tchiask01*e.engsu1 e.tchipra01*e.engsu1 e.nfactorw1*e.engsu1)

sem (tengask02 tengpra02 nfactorw2 gm2 gc2 ge2 cog2 numbestfrie2 selfcon2 matsu2 chisu2 engsu2 <- tengask01 tengpra01 nfactorw1 gm1 gc1 ge1 cog1 numbestfrie1 selfcon1 matsu1 chisu1 engsu1 stprhedu stmigrant sthktype stsex) ///
    (tengask01 tengpra01 nfactorw1 gm1 gc1 ge1 cog1 numbestfrie1 selfcon1 matsu1 chisu1 engsu1 <- stprhedu stmigrant sthktype stsex), stand ///
	var(e.tengask01*e.tengpra01 e.tengask01*e.nfactorw1 e.tengpra01*e.nfactorw1 ///
	    e.tengask02*e.tengpra02 e.tengask02*e.nfactorw2 e.tengpra02*e.nfactorw2 ///
		e.gm2*e.gc2 e.gm2*e.ge2 e.gm2*e.cog2 e.gm2*e.numbestfrie2 e.gm2*e.selfcon2 e.gm2*e.matsu2 e.gm2*e.chisu2 e.gm2*e.engsu2 ///
		e.gc2*e.ge2 e.gc2*e.cog2 e.gc2*e.numbestfrie2 e.gc2*e.selfcon2 e.gc2*e.matsu2 e.gc2*e.chisu2 e.gc2*e.engsu2 ///
		e.ge2*e.cog2 e.ge2*e.numbestfrie2 e.ge2*e.selfcon2 e.ge2*e.matsu2 e.ge2*e.chisu2 e.ge2*e.engsu2 ///
		e.numbestfrie2*e.selfcon2 e.numbestfrie2*e.matsu2 e.numbestfrie2*e.chisu2 e.numbestfrie2*e.engsu2 ///
		e.selfcon2*e.matsu2 e.selfcon2*e.chisu2 e.selfcon2*e.engsu2 ///
		e.matsu2*e.chisu2 e.matsu2*e.engsu2 ///
		e.chisu2*e.engsu2 ///
		e.gm1*e.gc1 e.gm1*e.ge1 e.gm1*e.cog1 e.gm1*e.numbestfrie1 e.gm1*e.selfcon1 e.gm1*e.matsu1 e.gm1*e.chisu1 e.gm1*e.engsu1 ///
		e.gc1*e.ge1 e.gc1*e.cog1 e.gc1*e.numbestfrie1 e.gc1*e.selfcon1 e.gc1*e.matsu1 e.gc1*e.chisu1 e.gc1*e.engsu1 ///
		e.ge1*e.cog1 e.ge1*e.numbestfrie1 e.ge1*e.selfcon1 e.ge1*e.matsu1 e.ge1*e.chisu1 e.ge1*e.engsu1 ///
		e.numbestfrie1*e.selfcon1 e.numbestfrie1*e.matsu1 e.numbestfrie1*e.chisu1 e.numbestfrie1*e.engsu1 ///
		e.selfcon1*e.matsu1 e.selfcon1*e.chisu1 e.selfcon1*e.engsu1 ///
		e.matsu1*e.chisu1 e.matsu1*e.engsu1 ///
		e.chisu1*e.engsu1 ///
		e.tengask01*e.gm1 e.tengpra01*e.gm1 e.nfactorw1*e.gm1 ///
		e.tengask01*e.gc1 e.tengpra01*e.gc1 e.nfactorw1*e.gc1 ///
		e.tengask01*e.ge1 e.tengpra01*e.ge1 e.nfactorw1*e.ge1 ///
		e.tengask01*e.cog1 e.tengpra01*e.cog1 e.nfactorw1*e.cog1 ///
		e.tengask01*e.numbestfrie1 e.tengpra01*e.numbestfrie1 e.nfactorw1*e.numbestfrie1 ///
		e.tengask01*e.selfcon1 e.tengpra01*e.selfcon1 e.nfactorw1*e.selfcon1 ///
		e.tengask01*e.matsu1 e.tengpra01*e.matsu1 e.nfactorw1*e.matsu1 ///
		e.tengask01*e.chisu1 e.tengpra01*e.chisu1 e.nfactorw1*e.chisu1 ///
		e.tengask01*e.engsu1 e.tengpra01*e.engsu1 e.nfactorw1*e.engsu1)
		