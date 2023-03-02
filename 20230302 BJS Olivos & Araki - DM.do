********************************************************************************
*                      Cultural Capital and Teacher Bias                       *
*                               May 25 2022                                    *
*                             Olivos & Araki                                   *
******************************************************************************** 

*Data Management

clear all

use "C:\Users\Francisco\Dropbox\Data\CEPS\cepsw1studentEN.dta", clear
merge 1:1 ids using "C:\Users\Francisco\Dropbox\Data\CEPS\cepsw2studentEN.dta"

drop _merge
merge m:1 schids using "C:\Users\Francisco\Dropbox\Data\CEPS\cepsw1principalEN.dta"

drop _merge
merge 1:1 ids using "C:\Users\Francisco\Dropbox\Data\CEPS\cepsw1parentEN.dta"

drop _merge
merge 1:1 ids using "C:\Users\Francisco\Dropbox\Data\CEPS\cepsw2parentEN.dta"

*************************Cultural capital W1

*::::::::::::familiarity with legitimate culture
 
*b1701. How often do you do the following either alone or with your schoolmates?
*Visiting museums, zoos, science museums, etc.
tab b1701
clonevar legcul01w1 = b1701
lab var legcul01w1 "Visiting museums"

*B2805. How often do you do the following with your parents?
*Visiting museums, zoos, science museums, etc.
tab b2805
clonevar legcul02w1 = b2805

*B2001 Playing musical instruments 
tab b2001
clonevar legcul03w1 = b2001
lab var legcul03w1 "Musical instruments"

*B2003 Calligraphy
tab b2003
clonevar legcul04w1 = b2003
lab var legcul04w1 "Calligraphy"

*::::::::::The second aspect is reading: 

*B2802. How often do you do the following with your parents?
*Reading
tab b2802
clonevar read01 = b2802
lab var read01 "Reading with parents"

*B12. How many books do your family own? (not including textbooks or magazines)
tab b12
clonevar read02w1 = b12
lab var read02w1 "Books at home"


*::::::The third aspect is extracurricular activities

*b19. What kind of extra-curricular courses do you take?

gen extra01w1 = .
replace extra01w1 = 1 if b1900==0
replace extra01w1 = 0 if b1900==1
tab extra01w1 
lab var extra01w1 "Extracurricular activities"


*B21. What do you usually do in winter and summer vacations?
*summer/winter camps
tab b2102
clonevar extra03w1 = b2102
lab var extra03w1 "Summer/winter camps"

*cram
tab b2101
clonevar extra04w1 = b2101
lab var extra04w1 "Cram school"

*************************Cultural capital W2

*::::::::::::familiarity with legitimate culture
 
*b1701. How often do you do the following either alone or with your schoolmates?
*Visiting museums, zoos, science museums, etc. (PASTE YEAR)
tab w2b09
clonevar legcul01w2 = w2b09

*B2805. How often do you do the following with your parents?
*Visiting museums, zoos, science museums, etc.
tab w2a25
clonevar legcul02w2 = w2a25

recode legcul01w2 (5/6=5) // diff in num of categories

*B2001 Playing musical instruments 
tab w2b1201
clonevar legcul03w2 = w2b1201

*B2003 Calligraphy
tab w2b1203
clonevar legcul04w2 = w2b1203

*::::::::::The second aspect is reading: 

*B2802. How often do you do the following with your parents?
*Reading (Not asked)


*B12. How many books do your family own? (not including textbooks or magazines)
tab w2a11
clonevar read02w2 = w2a11

*::::::The third aspect is extracurricular activities

*b19. What kind of extra-curricular courses do you take?

gen extra01w2 = .
replace extra01w2 = 1 if w2b1100==0
replace extra01w2 = 0 if w2b1100==1
tab extra01w2 


*B21. What do you usually do in winter and summer vacations?
*summer/winter camps
tab w2b1402
clonevar extra03w2 = w2b1402
*cram
tab w2b1401
clonevar extra04w2 = w2b1401


*****Cultural capital Indexes
alpha legcul01w1 legcul02w1 legcul03w1 legcul04w1 read02w1 extra01w1 extra03w1 extra04w1, item
alpha legcul01w2 legcul02w2 legcul03w2 legcul04w2 read02w2 extra01w2 extra03w2 extra04w2, item

factor legcul01w1 legcul02w1 legcul03w1 legcul04w1 read02w1 extra01w1 extra03w1 extra04w1
predict factorw1 if e(sample)

factor legcul01w2 legcul02w2 legcul03w2 legcul04w2 read02w2 extra01w2 extra03w2 extra04w2
predict factorw2 if e(sample)

*****Cultural capital Index (deleting items suggested by Satoshi: museum schoolmates and cram)

alpha legcul02w1 legcul03w1 legcul04w1 read02w1 extra01w1 extra03w1 
alpha legcul02w2 legcul03w2 legcul04w2 read02w2 extra01w2 extra03w2

factor legcul02w1 legcul03w1 legcul04w1 read02w1 extra01w1 extra03w1 
predict nfactorw1 if e(sample)

factor legcul02w2 legcul03w2 legcul04w2 read02w2 extra01w2 extra03w2
predict nfactorw2 if e(sample)


polychoricpca legcul01w1 legcul02w1 legcul03w1 legcul04w1 read02w1 extra01w1 extra03w1 extra04w1, score(factorcheckw1) nscore(1)

****Teachers' attitudes

clonevar tmatask01 = c1304 
clonevar tchiask01 = c1305 
clonevar tengask01 = c1306 
clonevar tmatpra01 = c1307 
clonevar tchipra01 = c1308 
clonevar tengpra01 = c1309 
clonevar hrtpra01 = c1704
clonevar hrtcri01 = c1705


clonevar tmatask02 = w2b0504 
clonevar tchiask02 = w2b0505 
clonevar tengask02 = w2b0506 
clonevar tmatpra02 = w2b0507 
clonevar tchipra02 = w2b0508 
clonevar tengpra02 = w2b0509 
clonevar hrtpra02 =  w2b0603
clonevar hrtcri02 =  w2b0604

*Student achievement

sort schids stdmat
by schids: gen gm1 = (_n - 1)/_N

sort schids stdchn
by schids: gen gc1 = (_n - 1)/_N

sort schids  stdeng
by schids: gen ge1 = (_n - 1)/_N

sort schids w2mat
by schids: gen gm2 = (_n - 1)/_N

sort schids w2eng
by schids: gen ge2 = (_n - 1)/_N

sort schids w2chn
by schids: gen gc2 = (_n - 1)/_N

*Cognitive skills

clonevar cog1 = cog3pl
clonevar cog2 = w2cog3pl 

*Self-confidence

clonevar selfcon1 = c25 
clonevar selfcon2 = w2b21

*Subjective performance

foreach var of varlist c1101 w2b02 c1102 w2b03 c1103 w2b04{
revrs `var', replace
}

clonevar matsu1 = c1101 
clonevar matsu2 = w2b02

clonevar chisu1 = c1102 
clonevar chisu2 = w2b03 

clonevar engsu1 = c1103 
clonevar engsu2 = w2b04

*Social capital

clonevar numbestfrie1 = c19 
clonevar numbestfrie2 = w2d09

*Parents occ

drop _merge
merge 1:1 ids using "C:\Users\Francisco\Dropbox\Data\CEPS\cepsw1parentEN.dta"

recode b08a (1/3=1) (4/10=0),gen(mocc1)
recode b08b (1/3=1) (4/10=0),gen(focc1)

gen pocc1 = .
replace pocc = focc1 if focc1>mocc1
replace pocc = mocc1 if focc1<mocc1
replace pocc = focc1 if focc1==mocc1


*School characteristics

clonevar urbansch = pla23
recode urbansch (1/2=1) (3/5=0)

clonevar edupasch = plb08

clonevar incpasch = plb09

clonevar occpasch =  plb1001


*Frequency in which parents are contacted by techers because of conduct

clonevar teacon1 = bb0502

clonevar teacon2 = w2bb0502 

*Homeroom teachers' characteristics

gen schclassid = clsids + (schids*1000)

drop _merge 

merge m:1 schclassid using "C:\Users\Francisco\Dropbox\Data\CEPS\cepsw1teacherEN.dta"

replace chnb01 = clshrsex if hra01==2
replace matb01 = clshrsex if hra01==1
replace engb01 = clshrsex if hra01==3

drop _merge 

merge m:1 schclassid using "C:\Users\Francisco\Dropbox\Data\CEPS\ceps2_w2tsex.dta"

rename chnb01 clschnsex 
rename matb01 clsmatsex
rename engb01 clsengsex

replace engb02 = hrc02 if hra01==3

replace engb04 = hrc04 if hra01==3  
replace engb06 = hrc06 if hra01==3 
replace engb07 = hrc07 if hra01==3

replace chnb02 = hrc02 if hra01==2

replace chnb04 = hrc04 if hra01==2  
replace chnb06 = hrc06 if hra01==2 
replace chnb07 = hrc07 if hra01==2


replace matb02 = hrc02 if hra01==1

replace matb04 = hrc04 if hra01==1  
replace matb06 = hrc06 if hra01==1 
replace matb07 = hrc07 if hra01==1


preserve
clear all
use "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Data\cepsw2teacherEN.dta", clear

*Data was entered in a different format than w1

gen hra01w2 = w2tchsubject

keep clsids w2tchhr w2tchc02 hra01w2 w2tchsubject w2tchc04 w2tchc06 w2tchc08 w2tchc01 w2tchstatus

reshape wide w2tchhr w2tchc02 hra01w2 w2tchc04 w2tchc06 w2tchc08 w2tchc01 w2tchstatus, i(clsids) j(w2tchsubject)


*Follow-up status

rename w2tchstatus0 fuhrcw2
rename w2tchstatus1 fumatcw2
rename w2tchstatus2 fuchncw2
rename w2tchstatus3 fuengcw2

replace fuhrcw2 = fumatcw2 if w2tchhr1==1
replace fuhrcw2 = fuchncw2 if w2tchhr2==1
replace fuhrcw2 = fuengcw2 if w2tchhr3==1


*Year of birth

rename w2tchc020 hrc02w2
rename w2tchc021 matb02w2
rename w2tchc022 chnb02w2 
rename w2tchc023 engb02w2

replace hrc02w2 = matb02w2 if w2tchhr1==1
replace hrc02w2 = chnb02w2 if w2tchhr2==1
replace hrc02w2 = engb02w2 if w2tchhr3==1

*Highest diploma

rename w2tchc040 hrc04w2
rename w2tchc041 matb04w2
rename w2tchc042 chnb04w2
rename w2tchc043 engb04w2

replace hrc04w2 = matb04w2 if w2tchhr1==1
replace hrc04w2 = chnb04w2 if w2tchhr2==1
replace hrc04w2 = engb04w2 if w2tchhr3==1

*Certification
 
rename w2tchc060 hrc06w2
rename w2tchc061 matb06w2
rename w2tchc062 chnb06w2
rename w2tchc063 engb06w2

replace hrc06w2 = matb06w2 if w2tchhr1==1
replace hrc06w2 = chnb06w2 if w2tchhr2==1
replace hrc06w2 = engb06w2 if w2tchhr3==1

*Experience teaching

rename w2tchc080 hrc07w2
rename w2tchc081 matb07w2
rename w2tchc082 chnb07w2
rename w2tchc083 engb07w2

replace hrc07w2 = matb07w2 if w2tchhr1==1
replace hrc07w2 = chnb07w2 if w2tchhr2==1
replace hrc07w2 = engb07w2 if w2tchhr3==1

*sex
rename w2tchc010 clshrsexw2 
rename w2tchc011 clsmatsexw2 
rename w2tchc012 clschnsexw2 
rename w2tchc013 clsengsexw2 

replace clshrsexw2 = clsmatsexw2 if w2tchhr1==1
replace clshrsexw2 = clschnsexw2 if w2tchhr2==1
replace clshrsexw2 = clsengsexw2 if w2tchhr3==1

*Subject of the homeroom teacher

gen hra01w2 = .

replace hra01w2 = hra01w20 if w2tchhr0==1
replace hra01w2 = hra01w21 if w2tchhr1==1
replace hra01w2 = hra01w22 if w2tchhr2==1
replace hra01w2 = hra01w23 if w2tchhr3==1

keep clsids hrc02w2 hrc04w2 hrc06w2 hrc07w2 clshrsexw2  matb02w2 matb04w2 matb06w2 matb07w2 clsmatsexw2 ///
     chnb02w2 chnb04w2 chnb06w2 chnb07w2 clschnsexw2 engb02w2 engb04w2 engb06w2 engb07w2 clsengsexw2 hra01w2 fuhrcw2 fumatcw2 fuchncw2 fuengcw2
	 
sum clsids hrc02w2 hrc04w2 hrc06w2 hrc07w2 clshrsexw2  matb02w2 matb04w2 matb06w2 matb07w2 clsmatsexw2 ///
     chnb02w2 chnb04w2 chnb06w2 chnb07w2 clschnsexw2 engb02w2 engb04w2 engb06w2 engb07w2 clsengsexw2 hra01w2

save "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Data\cepsw2teacherEN_v2.dta", replace	 
restore

drop _merge 


rename hrc02 hrc02w1 
rename hrc04 hrc04w1 
rename hrc06 hrc06w1 
rename hrc07 hrc07w1 
rename clshrsex clshrsexw1  
rename matb02 matb02w1 
rename matb04 matb04w1 
rename matb06 matb06w1 
rename matb07 matb07w1 
rename clsmatsex clsmatsexw1 
rename chnb02 chnb02w1 
rename chnb04 chnb04w1 
rename chnb06 chnb06w1 
rename chnb07 chnb07w1 
rename clschnsex clschnsexw1 
rename engb02 engb02w1 
rename engb04 engb04w1 
rename engb06 engb06w1 
rename engb07 engb07w1 
rename clsengsex clsengsexw1 
rename hra01 hra01w1

merge m:1 clsids using "C:\Users\Francisco\Dropbox\210913 Cultural K and Teacher bias\Data\cepsw2teacherEN_v2.dta"
