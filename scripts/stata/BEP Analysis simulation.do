********************************************************************************
*Program: Simulate data and model that will be used in QPP
*Author: Andrew Avitabile
********************************************************************************

********************************************************************************
*General options****************************************************************
********************************************************************************

* Clear everything
clear
********************************************************************************
*Create sample******************************************************************
********************************************************************************
*Set data to have 130,000 observations
*These represent pre-serivce teachers (PSTs) at traditional teacher education programs
*ASSUMPTION: There are approximately 10,000 PSTs in traditional TEPs in Texas in each graduating class. Assume that there will be this many each year in the data.
*The simulated data will span 10 years, so there will be 100,000 graduates during this time
*We also need to simulate data for people who would have been Freshman, Sophomores, and Juniors during the last few years in the data (i.e., we never observe them graduate). This makes an additional 30,000 PSTs.
set obs 130000

*Generate IDs for each PSTs
gen pst_id = _n

*Generate school IDs. Make it so there are 75 schools. This is approximately how many traditional TEPs there are in Texas.
gen school_id = int((pst_id - 1) / 1733.3333) + 1

*Draw random variables at the pst-level
drawnorm alpha e rv_program

*Draw correlated random variables to determine financial aid amount received via scholarships
drawnorm rv1 rv2, corr(1 .5 \ .5 1)
*Normalize between 0 and 1.
foreach v of varlist rv1 rv2 {
    qui summ `v'
    replace `v' = (`v' - r(min)) / (r(max) - r(min))
}

gen rand_uniform_pst_level = runiform()

*Generate a variable to randomly select one year as the graduation year for each PST
gen matriculate_year = 2009 + int(runiform() * 13)
replace matriculate_year = 2021 if matriculate_year > 2021

*Generate graduation year as four years after matriculation year - assuming everyone graduates on time
gen graduation_year = matriculate_year+3

*Generate years (2009 to 2021). Again, this is so that we have sample of 40,000 PSTs each year for 10 years.
expand 13
bysort pst_id: gen year = 2009 + _n - 1

*Keep just data between matriculation and graduation for PSTs
keep if year <= graduation_year & year >= matriculate_year 

*Keep data just 2012 and later
*We generated data from 2009 forward so that we have PSTs who were seniors in 2012 appear in the data.
keep if year >= 2012

*Sort the data
sort pst_id year

*Verify that there are 10,000 PSTs per graduating class and that they appear in four consecutive years
tab year graduation_year

********************************************************************************
*Assign treatment***************************************************************
********************************************************************************

*Make seven of the 50 schools eligible for the BEP
*ASSUMPTION: 14% OF SCHOOLS ARE ELIGIBLE FOR THE BEP
gen bep_eligible_school = (school_id <= 7)

*Generate a variable that indicates if a PST is enrolled in a BEP-targeted certification areas
*ASSUMPTION: 15% of PSTs pursue a certification in Spanish, ESL, or Bilingual Education
gen bep_eligible_cert_area = (rand_uniform_pst_level <= 0.15)

*Replace this variable in years after the implementation of BEP. 
*ASSUMPTION: the BEP causes the proportion of PSTs receving certificates to grow by 5% year-over-year
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.15*(1.05^1)) if year == 2017 & bep_eligible_cert_area == 1
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.15*(1.05^2)) if year == 2018 & bep_eligible_cert_area == 1
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.15*(1.05^3)) if year == 2019 & bep_eligible_cert_area == 1
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.15*(1.05^4)) if year == 2020 & bep_eligible_cert_area == 1
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.15*(1.05^5)) if year == 2021 & bep_eligible_cert_area == 1

* Generate a variable that flags BEP-eligible PSTs (i.e., those in a BEP-eligible school and and a BEP-targeted certification areas)
gen bep_eligible_pst = (bep_eligible_cert_area == 1 & bep_eligible_school == 1)

*See how many PSTs are eligible for the program
tab bep_eligible_pst

********************************************************************************
*DGM for outcomes***************************************************************
********************************************************************************

*Generate the BEP scholarship amount for eligible PSTs
gen bep_scholarship = 0
*For eligible PSTs, this is a function of 3000 times some random variable between 0 and 1 for PSTs.
*PSTs only recieve this amount after 2016, when the BEP program is implemented.
replace bep_scholarship = rv1 * 3000 if bep_eligible_pst == 1 & year > 2016

*Generate another variable for additional scholarships 
*This is a function of 5000 times some other random variable, which is correlated with rv1 (i.e., PSTs with more need get more money).
gen other_scholarship = rv2 * 5000

*Calculate total scholarship amount
gen total_scholarship = bep_scholarship + other_scholarship

*Generate a variable that identifies years after BEP implementation
gen post = (year >= 2017)

*True DGM for SBEC exam scores 
*Make it so that PSTs at BEP eligible institutions perform worse than non-BEP-eligible institutions on levels
*Also make it so that growth occurs post BEP for eligible PSTs
gen sbec_score = alpha - 0.6*bep_eligible_cert_area + 0.1*bep_eligible_pst*post + 0.1*e 

********************************************************************************
*Estimation*********************************************************************
********************************************************************************

*Pre-post, traditional DiD
reg total_scholarship i.bep_eligible_school##i.post, r 
reg sbec_score i.bep_eligible_school##i.post i.bep_eligible_cert_area if year == graduation_year, r 
reg bep_eligible_cert_area i.bep_eligible_school##i.post if year == matriculate_year, r 

*Event study framework
reg total_scholarship i.bep_eligible_school##ib2016.year, r 
reg sbec_score i.bep_eligible_school##ib2016.year i.bep_eligible_cert_area if year == graduation_year , r
reg bep_eligible_cert_area i.bep_eligible_school##ib2016.year if year == matriculate_year, r 