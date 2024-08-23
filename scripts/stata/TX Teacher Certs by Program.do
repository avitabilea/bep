********************************************************************************
*Program: Analyze TPEIR data with number of teacher certifications by program
*Author: Andrew Avitabile
********************************************************************************

********************************************************************************
*General options****************************************************************
********************************************************************************

* Clear everything
clear

********************************************************************************
*Import data********************************************************************
********************************************************************************
use "https://github.com/avitabilea/bep/blob/master/data/cleaned/tx_teacher_certs_by_program.dta", clear 

********************************************************************************
*Label variables****************************************************************
********************************************************************************

la var epp "Educator preperation program (EPP)"
la var school_year "School year (e.g., 2012-13 = 2012)"
la var BEP "EPP participates in Bilingual Education Program (BEP)"
la var post "BEP implemented at EPP during school year"
la var alt "# certs. alternative route"
la var post_bac "# certs. post bac. route"
la var trad "# certs. trad. route"
la var bl "# certs. bilingual education (all routes)"
la var trad_bl_prop "Prop. trad. certs. that are bilingual education"

local routes alt post_bac trad
local route_labels "alt post-bac trad"
local subjects bl ela g m sci ss tot cte fa hel sped loe cs
local subject_labels "bilingual english generalist math science social-studies total career-tech fine-art health special-ed lang-other-than-english comp-sci"

forvalues i = 1/`:word count `routes'' {
    local route : word `i' of `routes'
    local route_label : word `i' of `route_labels'
    
    forvalues j = 1/`:word count `subjects'' {
        local subject : word `j' of `subjects'
        local subject_label : word `j' of `subject_labels'
        
        label var `route'_`subject' "# certs. `route_label' route `subject_label'"
    }
}

********************************************************************************
*Data prep**********************************************************************
********************************************************************************

*Create binary indicator for whether EPP participated in BEP
gen bep = (BEP == "BEP")

*Create a year variable centered around 2016
gen year_centered = school_year-2016

********************************************************************************
*Graphs*************************************************************************
********************************************************************************




********************************************************************************
*Event study********************************************************************
********************************************************************************

*Effect of BEP on number of bilingual education certificates
est clear
eststo event_study: areg trad_bl bep##ib2016.school_year, a(epp) cluster(epp)

// Generate a coefficient plot for interaction terms only
coefplot event_study, ///
    keep(*1.bep#*.school_year) ///
    vertical ///
    yline(0) ///
    xline(4.5, lpattern(dash)) ///
    coeflabels(1.bep#2012.school_year = "2012" ///
               1.bep#2013.school_year = "2013" ///
               1.bep#2014.school_year = "2014" ///
               1.bep#2015.school_year = "2015" ///
               1.bep#2016.school_year = "2016" ///
               1.bep#2017.school_year = "2017" ///
               1.bep#2018.school_year = "2018" ///
               1.bep#2019.school_year = "2019" ///
               1.bep#2020.school_year = "2020" ///
               1.bep#2021.school_year = "2021" ///
               1.bep#2022.school_year = "2022", ///
               angle(45) labsize(small)) ///
    title("Event study effect of BEP on Bilingual Education Certificates") ///
    xtitle("School Year") ///
    ytitle("Coefficient") ///
    note("Reference year: 2016") ///
    recast(connected) ciopts(recast(rcap)) ///
    omitted baselevels
   