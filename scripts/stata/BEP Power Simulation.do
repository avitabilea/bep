********************************************************************************
*Program: Simulate data that will be used in QPP. Use this to estimate potnetial power
*Author: Andrew Avitabile
********************************************************************************
 
capture program drop one_simulation
program define one_simulation, rclass
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

*Make seven of the 75 TEPs are eligible for the BEP
*ASSUMPTION: 14% OF SCHOOLS ARE ELIGIBLE FOR THE BEP
gen bep_eligible_school = (school_id <= 7)

*Generate a variable that indicates if a PST is enrolled in a BEP-targeted certification areas
*ASSUMPTION: 15% of PSTs pursue a certification in Spanish, ESL, or Bilingual Education at non-BEP-eligible TEPs
gen bep_eligible_cert_area = (rand_uniform_pst_level <= 0.15) if bep_eligible_school == 0

*ASSUMPTION: 25% of PSTs pursue a certification in Spanish, ESL, or Bilingual Education at BEP-eligible TEPs
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.25) if bep_eligible_school == 1

*Replace this variable in years after the implementation of BEP. 
*ASSUMPTION: the BEP causes the proportion of PSTs receving certificates to grow by 5% year-over-year
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.25*(1.05^1)) if year == 2017 & bep_eligible_cert_area == 1
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.25*(1.05^2)) if year == 2018 & bep_eligible_cert_area == 1
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.25*(1.05^3)) if year == 2019 & bep_eligible_cert_area == 1
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.25*(1.05^4)) if year == 2020 & bep_eligible_cert_area == 1
replace bep_eligible_cert_area = (rand_uniform_pst_level <= 0.25*(1.05^5)) if year == 2021 & bep_eligible_cert_area == 1

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

* Pre-post, traditional DiD for total_scholarship
reg total_scholarship i.bep_eligible_pst##i.post, r

* Get standard error
lincom 1.bep_eligible_pst#1.post

* Save the results
return scalar p_total_scholarship  = `r(p)'

* Pre-post, traditional DiD for sbec_score
reg sbec_score i.bep_eligible_pst##i.post if year == graduation_year, r

* Get standard error
lincom 1.bep_eligible_pst#1.post

* Save the results
return scalar p_sbec_score  = `r(p)'

* Pre-post, traditional DiD for majoring in BEP eligible certification
reg bep_eligible_cert_area i.bep_eligible_school##i.post if year == matriculate_year, r 

* Get standard error
lincom 1.bep_eligible_school#1.post

* Save the results
return scalar p_cert_area  = `r(p)'

*Event study framework for total_scholarship
reg total_scholarship i.bep_eligible_pst##ib2016.year, r 
foreach n of numlist 2012/2021{
	lincom 1.bep_eligible_pst#`n'.year
	return scalar p_total_scholarship_`n'  = `r(p)'
}

*Event study framework for sbec_score
reg sbec_score i.bep_eligible_pst##ib2016.year if year == graduation_year , r
foreach n of numlist 2012/2021{
	lincom 1.bep_eligible_pst#`n'.year
	return scalar p_sbec_score_`n'  = `r(p)'
}

*Event study framework for majoring in BEP eligible certification
reg bep_eligible_cert_area i.bep_eligible_school##ib2016.year if year == matriculate_year, r 
foreach n of numlist 2012/2021{
	lincom 1.bep_eligible_school#`n'.year
	return scalar p_cert_area_`n'  = `r(p)'
}
end

********************************************************************************
*Run simulations****************************************************************
********************************************************************************
simulate p_total_scholarship=r(p_total_scholarship) p_sbec_score=r(p_sbec_score) p_cert_area=r(p_cert_area) p_total_scholarship_2012=r(p_total_scholarship_2012) p_total_scholarship_2013=r(p_total_scholarship_2013) p_total_scholarship_2014=r(p_total_scholarship_2014) p_total_scholarship_2015=r(p_total_scholarship_2015) p_total_scholarship_2017=r(p_total_scholarship_2017) p_total_scholarship_2018=r(p_total_scholarship_2018) p_total_scholarship_2019=r(p_total_scholarship_2019) p_total_scholarship_2020=r(p_total_scholarship_2020) p_total_scholarship_2021=r(p_total_scholarship_2021) p_sbec_score_2012=r(p_sbec_score_2012) p_sbec_score_2013=r(p_sbec_score_2013) p_sbec_score_2014=r(p_sbec_score_2014) p_sbec_score_2015=r(p_sbec_score_2015) p_sbec_score_2017=r(p_sbec_score_2017) p_sbec_score_2018=r(p_sbec_score_2018) p_sbec_score_2019=r(p_sbec_score_2019) p_sbec_score_2020=r(p_sbec_score_2020) p_sbec_score_2021=r(p_sbec_score_2021) p_cert_area_2012=r(p_cert_area_2012) p_cert_area_2013=r(p_cert_area_2013) p_cert_area_2014=r(p_cert_area_2014) p_cert_area_2015=r(p_cert_area_2015) p_cert_area_2017=r(p_cert_area_2017) p_cert_area_2018=r(p_cert_area_2018) p_cert_area_2019=r(p_cert_area_2019) p_cert_area_2020=r(p_cert_area_2020) p_cert_area_2021=r(p_cert_area_2021), reps(1000): one_simulation

*Create binary variables, indicating significance at the 95% level
gen sig_total_scholarship = (p_total_scholarship < 0.05)
gen sig_sbec_score = (p_sbec_score < 0.05)
gen sig_cert_area = (p_cert_area < 0.05)

foreach n of numlist 2012 2013 2014 2015 2017 2018 2019 2020 2021{
	gen sig_total_scholarship_`n' = (p_total_scholarship_`n' < 0.05)
	gen sig_sbec_score_`n' = (p_sbec_score_`n' < 0.05)
	gen sig_cert_area_`n' = (p_cert_area_`n' < 0.05)
}

*Save simulation results
save simulation_results, replace

********************************************************************************
*Create output******************************************************************
********************************************************************************

*Summarize the significance variables and store the results
est clear
estpost sum sig_total_scholarship sig_sbec_score sig_cert_area sig_total_scholarship_2012 sig_total_scholarship_2013 sig_total_scholarship_2014 sig_total_scholarship_2015 sig_total_scholarship_2017 sig_total_scholarship_2018 sig_total_scholarship_2019 sig_total_scholarship_2020 sig_total_scholarship_2021 sig_sbec_score_2012 sig_sbec_score_2013 sig_sbec_score_2014 sig_sbec_score_2015 sig_sbec_score_2017 sig_sbec_score_2018 sig_sbec_score_2019 sig_sbec_score_2020 sig_sbec_score_2021 sig_cert_area_2012 sig_cert_area_2013 sig_cert_area_2014 sig_cert_area_2015 sig_cert_area_2017 sig_cert_area_2018 sig_cert_area_2019 sig_cert_area_2020 sig_cert_area_2021

*Generate the LaTeX table with esttab
#delimit ;
esttab
using "${dropbox}Apps/Overleaf/Bilingual Education Program/power_analysis.tex",
cells((mean(fmt(3))))
label
fragment
noobs
booktabs
nolines
nodepvars
nonumbers
nomtitles
unstack
collabels(none)
replace
prehead("") 
varlabels(
sig_total_scholarship "\hspace{3mm} Total Scholarship (Post $\times$ Eligible PST) [RQ1]"
sig_sbec_score "\hspace{3mm} SBEC Exam Score (Post $\times$ Eligible PST) [RQ2]"
sig_cert_area "\hspace{3mm} BEP Certification Area (Post $\times$ Eligible TEP) [RQ3]"
sig_total_scholarship_2012 "\hspace{3mm} 2012 $\times$ Eligible PST"
sig_total_scholarship_2013 "\hspace{3mm} 2013 $\times$ Eligible PST"
sig_total_scholarship_2014 "\hspace{3mm} 2014 $\times$ Eligible PST"
sig_total_scholarship_2015 "\hspace{3mm} 2015 $\times$ Eligible PST"
sig_total_scholarship_2017 "\hspace{3mm} 2017 $\times$ Eligible PST"
sig_total_scholarship_2018 "\hspace{3mm} 2018 $\times$ Eligible PST"
sig_total_scholarship_2019 "\hspace{3mm} 2019 $\times$ Eligible PST"
sig_total_scholarship_2020 "\hspace{3mm} 2020 $\times$ Eligible PST"
sig_total_scholarship_2021 "\hspace{3mm} 2021 $\times$ Eligible PST"
sig_sbec_score_2012 "\hspace{3mm} 2012 $\times$ Eligible PST"
sig_sbec_score_2013 "\hspace{3mm} 2013 $\times$ Eligible PST"
sig_sbec_score_2014 "\hspace{3mm} 2014 $\times$ Eligible PST"
sig_sbec_score_2015 "\hspace{3mm} 2015 $\times$ Eligible PST"
sig_sbec_score_2017 "\hspace{3mm} 2017 $\times$ Eligible PST"
sig_sbec_score_2018 "\hspace{3mm} 2018 $\times$ Eligible PST"
sig_sbec_score_2019 "\hspace{3mm} 2019 $\times$ Eligible PST"
sig_sbec_score_2020 "\hspace{3mm} 2020 $\times$ Eligible PST"
sig_sbec_score_2021 "\hspace{3mm} 2021 $\times$ Eligible PST"
sig_cert_area_2012 "\hspace{3mm} 2012 $\times$ Eligible TEP"
sig_cert_area_2013 "\hspace{3mm} 2013 $\times$ Eligible TEP"
sig_cert_area_2014 "\hspace{3mm} 2014 $\times$ Eligible TEP"
sig_cert_area_2015 "\hspace{3mm} 2015 $\times$ Eligible TEP"
sig_cert_area_2017 "\hspace{3mm} 2017 $\times$ Eligible TEP"
sig_cert_area_2018 "\hspace{3mm} 2018 $\times$ Eligible TEP"
sig_cert_area_2019 "\hspace{3mm} 2019 $\times$ Eligible TEP"
sig_cert_area_2020 "\hspace{3mm} 2020 $\times$ Eligible TEP"
sig_cert_area_2021 "\hspace{3mm} 2021 $\times$ Eligible TEP"
,
blist(
sig_total_scholarship "\textbf{Diff-in-Diff} \vspace{.25mm} \\"
sig_total_scholarship_2012 "\\ \textbf{Event Study} \vspace{.25mm} \\ \hspace{1mm} \textit{Total Scholarship [RQ1]} \vspace{.25mm} \\ "
sig_sbec_score_2012 "\hspace{1mm} \textit{SBEC Exam Score [RQ2]} \vspace{.25mm} \\ "
sig_cert_area_2012 "\hspace{1mm} \textit{BEP Certification Area [RQ3]} \vspace{.25mm} \\ "
)
)
;
#delimit cr