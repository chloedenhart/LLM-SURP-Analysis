*Results reported in Supplementary Information of Gaechter, Koelle and Quercia 'Reciprocity and the Tragedies of Maintaining and Providing the Commons'
*Documentation of analysis reported in the main text is in separate file
*Stata/SE 14.1 for Windows
*requires Stata package grc1leg (from http://www.stata.com/users/vwiggins)
*July 27, 2017



************************************************************************************
*** START DATA ANALYSIS: SUPPLEMENTARY INFORMATION
************************************************************************************
set more off

******************************************
*** Table S1 Panel A: PUBLIC GOOD SIZE
******************************************
* Test differences in Public Goods Size in Period 1
use "GKQ_Dataset.dta", clear
collapse pgsize, by(experiment cpp oneshot strangers partners period groupid)

gen part=1 if period<=9
replace part=2 if period<=18 & part==.
replace part=3 if period<=27 & part==.

* Descriptive Statistics
table experiment cpp if experiment<4 & period==1, c(mean pgsize  sd pgsize) format(%9.2f)
table experiment cpp if part==1 & experiment>1 & experiment<4, c(mean pgsize ) format(%9.2f)
table experiment cpp if part==2 & experiment>1 & experiment<4, c(mean pgsize ) format(%9.2f)
table experiment cpp if part==3 & experiment>1 & experiment<4, c(mean pgsize ) format(%9.2f)
bys experiment: table cpp if experiment<4,c(mean pgsize) format(%9.2f)

bys experiment cpp period: egen stddev_pgsize = sd(pgsize)
bys experiment: table cpp part, c(mean stddev_pgsize) format(%9.2f)
bys experiment: table cpp , c(mean stddev_pgsize) format(%9.2f)

* Test differences in Public Goods Size in Period 1
ttest pgsize if experiment==1 & period==1, by(cpp)
ttest pgsize if strangers==1 & period==1, by(cpp)
ttest pgsize if partners==1 & period==1, by(cpp)



* Test differences in Public Goods Size in later periods
use "GKQ_Dataset.dta", clear
collapse pgsize, by(experiment cpp oneshot strangers partners period mg)

gen part=1 if period<=9
replace part=2 if period<=18 & part==.
replace part=3 if period<=27 & part==.


bys experiment: xtmixed pgsize cpp if part==1 & (partners==1 | strangers==1) || mg:
bys experiment: xtmixed pgsize cpp if part==2 & (partners==1 | strangers==1) || mg:
bys experiment: xtmixed pgsize cpp if part==3 & (partners==1 | strangers==1) || mg:
bys experiment: xtmixed pgsize cpp if partners==1 | strangers==1 || mg:



******************************************
*** Table S1 Panel B: BELIEFS
******************************************
use "GKQ_Dataset.dta", clear

gen part=1 if period<=9
replace part=2 if period<=18 & part==.
replace part=3 if period<=27 & part==.

bys experiment cpp period: egen stddev_b = sd(b)
bys experiment: table cpp if period==1 & experiment<4, c(mean stddev_b) format(%9.2f)
bys experiment: table cpp part if experiment>1 & experiment<4, c(mean stddev_b) format(%9.2f)
bys experiment: table cpp if experiment<4, c(mean stddev_b) format(%9.2f)

table experiment cpp if period==1 & experiment<4, c(mean b ) format(%9.2f)
table experiment cpp if part==1 & experiment>1 & experiment<4, c(mean b ) format(%9.2f)
table experiment cpp if part==2 & experiment>1 & experiment<4, c(mean b ) format(%9.2f)
table experiment cpp if part==3 & experiment>1 & experiment<4, c(mean b ) format(%9.2f)
bys experiment: table cpp if experiment<4,c(mean b) format(%9.2f)


* Test differences in Beliefs in Period 1
ttest b if experiment==1, by(cpp)
ttest b if strangers==1 & period==1, by(cpp)
ttest b if partners==1 & period==1, by(cpp)



* Test differences in Beliefs in later periods
bys experiment: xtmixed b cpp if part==1 & (partners==1 | strangers==1) || mg:
bys experiment: xtmixed b cpp if part==2 & (partners==1 | strangers==1) || mg:
bys experiment: xtmixed b cpp if part==3 & (partners==1 | strangers==1) || mg:
bys experiment: xtmixed b cpp if partners==1 | strangers==1 || mg:





************************
********Table S2********
************************
use "GKQ_Dataset.dta", clear
collapse c pgsize, by(experiment cpp partners strangers punishment mg period)


xtmixed pgsize cpp  if  strangers==1 || mg: 
eststo PvsM_strangers
xtmixed pgsize cpp  if  partners==1 || mg: 
eststo PvsM_partners
xtmixed pgsize partners  if cpp==0 & (partners==1 | strangers==1) || mg: 
eststo SvsP_M
xtmixed pgsize partners if cpp==1 & (partners==1 | strangers==1) || mg: 
eststo SvsP_P

* Show regression output
esttab PvsM_strangers PvsM_partners SvsP_M SvsP_P using "Table_S2.rtf", replace  varwidth(25) depvars modelwidth(15) se ar2 star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Multilevel Regression") nonotes  ///
coeflabels( _cons "Constant") addnotes("Note: Robust standard errors in parentheses, * p<0.1, ** p<0.05, *** p<0.01")





************************************************************************************
***Table S3: ESTIMATED RECIPROCITY: COMPARISONS ACROSS MAINTENANCE and PROVISION
************************************************************************************
use "GKQ_Dataset.dta", clear


*** STRANGERS: CPM vs. CPP All
xi: xtmixed c i.cpp*lag_c_other i.cpp|period  if strangers==1 || mg: || id: 
estimates store rec_PvsM_strangers_All
*** PARTNERS: CPM vs. CPP All
xi: xtmixed c i.cpp*lag_c_other i.cpp|period  if partners==1 || mg: || id: 
estimates store rec_PvsM_partners_All

*** STRANGERS: CPM vs. CPP CC
xi: xtmixed c i.cpp*lag_c_other i.cpp|period   if strangers==1 & type==1 || mg: || id: 
estimates store rec_PvsM_strangers_CC
*** PARTNERS: CPM vs. CPP CC
xi: xtmixed c i.cpp*lag_c_other i.cpp|period  if partners==1 & type==1 || mg: || id: 
estimates store rec_PvsM_partners_CC

*** STRANGERS: CPM vs. CPP FR
xi: xtmixed c i.cpp*lag_c_other i.cpp|period  if strangers==1 & type==2 || mg: || id: 
estimates store rec_PvsM_strangers_FR
*** PARTNERS: CPM vs. CPP FR
xi: xtmixed c i.cpp*lag_c_other i.cpp|period  if partners==1 & type==2 || mg: || id: 
estimates store rec_PvsM_partners_FR

*** STRANGERS: CPM vs. CPP OT
xi: xtmixed c i.cpp*lag_c_other i.cpp|period if strangers==1 & type==3 || mg: || id: 
estimates store rec_PvsM_strangers_OT
*** PARTNERS: CPM vs. CPP OT
xi: xtmixed c i.cpp*lag_c_other i.cpp|period if partners==1 & type==3 || mg: || id: 
estimates store rec_PvsM_partners_OT

* Show output
esttab rec_PvsM_strangers_All   rec_PvsM_strangers_CC  rec_PvsM_strangers_FR  rec_PvsM_strangers_OT  using "Table_S3A.rtf", replace  varwidth(30)  modelwidth(10) se(%9.3f)  b(%9.3f) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Supplementary Table 3 Panel A: Strangers: Comparing estimated reciprocity between Maintenance and Provision") nonotes mtitles("All" "CC" "FR"  "OT" ) ///
coeflabels( _Icpp_1 "Provision" _IcppXlag_c_1 "Provision x lag_c_other" period "Round" _IcppXperio_1 "Provision x Round" _cons "Constant") addnotes("Notes: Multilevel linear regression. Robust standard errors clustered on the group and the individual level in parentheses, * p<0.1, ** p<0.05, *** p<0.01")

esttab  rec_PvsM_partners_All   rec_PvsM_partners_CC  rec_PvsM_partners_FR  rec_PvsM_partners_OT using "Table_S3B.rtf", replace  varwidth(30)  modelwidth(10) se(%9.3f)  b(%9.3f) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Supplementary Table 3 Panel B: Partners: Comparing estimated reciprocity between Maintenance and Provision") nonotes mtitles("All" "CC" "FR"  "OT" ) ///
coeflabels( _Icpp_1 "Provision" _IcppXlag_c_1 "Provision x lag_c_other" period "Round" _IcppXperio_1 "Provision x Round" _cons "Constant") addnotes("Notes: Multilevel linear regression. Robust standard errors clustered on the group and the individual level in parentheses, * p<0.1, ** p<0.05, *** p<0.01")





************************************************************************************
*** Table S4: ESTIMATED RECIPROCITY: COMPARISONS ACROSS PARTNERS AND STRANGERS
************************************************************************************
use "GKQ_Dataset.dta", clear

*** CPM: STRANGERS vs. PARTNERS All
xi: xtmixed c i.partners*lag_c_other i.partners|period if cpp==0 & (partners==1 | strangers==1) || mg: || id:
estimates store rec_SvsP_CPM_All
*** CPP: STRANGERS vs. PARTNERS All
xi: xtmixed c i.partners*lag_c_other i.partners|period if cpp==1 & (partners==1 | strangers==1) || mg: || id:
estimates store rec_SvsP_CPP_All

*** CPM: STRANGERS vs. PARTNERS CC
xi: xtmixed c i.partners*lag_c_other i.partners|period if cpp==0 & type==1 & (partners==1 | strangers==1) || mg: || id:
estimates store rec_SvsP_CPM_CC
*** CPP: STRANGERS vs. PARTNERS CC
xi: xtmixed c i.partners*lag_c_other i.partners|period if cpp==1 & type==1 & (partners==1 | strangers==1) || mg: || id:
estimates store rec_SvsP_CPP_CC

*** CPM: STRANGERS vs. PARTNERS FR
xi: xtmixed c i.partners*lag_c_other i.partners|period if cpp==0 & type==2 & (partners==1 | strangers==1) || mg: || id:
estimates store rec_SvsP_CPM_FR
*** CPP: STRANGERS vs. PARTNERS FR
xi: xtmixed c i.partners*lag_c_other i.partners|period if cpp==1 & type==2 & (partners==1 | strangers==1) || mg: || id:
estimates store rec_SvsP_CPP_FR

*** CPM: STRANGERS vs. PARTNERS OT
xi: xtmixed c i.partners*lag_c_other i.partners|period if cpp==0 & type==3 & (partners==1 | strangers==1) || mg: || id:
estimates store rec_SvsP_CPM_OT
*** CPP: STRANGERS vs. PARTNERS OT
xi: xtmixed c i.partners*lag_c_other i.partners|period if cpp==1 & type==3 & (partners==1 | strangers==1) || mg: || id:
estimates store rec_SvsP_CPP_OT


* Show output
esttab rec_SvsP_CPM_All rec_SvsP_CPP_All  rec_SvsP_CPM_CC rec_SvsP_CPP_CC rec_SvsP_CPM_FR rec_SvsP_CPP_FR rec_SvsP_CPM_OT rec_SvsP_CPP_OT using "Table_S4.rtf", replace  varwidth(30)  modelwidth(10) se(%9.3f) b(%9.3f) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Supplementary Table 4: Comparing estimated reciprocity between Strangers and Partners") nonotes mtitles("All Maintenance" "All Provision" "CC Maintenance" "CC Provision" "FR Maintenance" "FR Provision" "OT Maintenance" "OT Provision") ///
coeflabels( _Ipartners_1 "Partners" _IparXlag_c_1 "Partners x lag_c_other" period "Round" _IparXperio_1 "Partners x Round" _cons "Constant") addnotes("Notes: Multilevel linear regression. Robust standard errors clustered on the group and the individual level in parentheses, * p<0.1, ** p<0.05, *** p<0.01")




**************
***Table S5***
**************
use "GKQ_Dataset.dta", clear

*** Types comparison in all 4 conditions
char type [omit] 2
xi: xtmixed c i.type*lag_c_other i.type|period if cpp==0 & strangers==1 || mg: || id: 
estimates store rec_types_M_S

char type [omit] 2
xi: xtmixed c i.type*lag_c_other i.type|period if cpp==1 & strangers==1  || mg: || id:
estimates store rec_types_P_S

char type [omit] 2
xi: xtmixed c i.type*lag_c_other i.type|period if cpp==0 & partners==1  || mg: || id:
estimates store rec_types_M_P

char type [omit] 2
xi: xtmixed c i.type*lag_c_other i.type|period if cpp==1 & partners==1  || mg: || id:
estimates store rec_types_P_P
 
* Show output
esttab rec_types_M_S rec_types_P_S  rec_types_M_P rec_types_P_P using "Table_S5.rtf", replace  varwidth(30)  modelwidth(10) se(%9.3f) ar2 b(%9.3f) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Supplementary Table 5: Comparing estimated reciprocity across attitude types") nonotes mtitles("Strangers Maintenance" "Strangers Provision" "Partners Maintenance" "Partners Provision") ///
coeflabels( _Itype_1 "CC" _Itype_3 "OT" _ItypXlag_c_1 "CC x lag_c_other" _ItypXlag_c_3 "OT x lag_c_other" period "Round" _ItypXperio_1 "CC x Round" _ItypXperio_3 "OT x Round" _cons "Constant") addnotes("Notes: Multilevel linear regression. Robust standard errors clustered on the group and the individual level in parentheses, * p<0.1, ** p<0.05, *** p<0.01")






******************************
*******Table S6***************
******************************
use "GKQ_Dataset.dta", clear


char type [omit] 2
xi: xtmixed c i.type if cpp==0 & experiment==1 
estimates store contr_types_oneshot_M

char type [omit] 2
xi: xtmixed c i.type if cpp==0 & experiment==2  || mg: || id: 
estimates store contr_types_strangers_M

char type [omit] 2
xi: xtmixed c i.type if cpp==0 & experiment==3  || mg: || id: 
estimates store contr_types_partners_M


char type [omit] 2
xi: xtmixed c i.type if cpp==1 & experiment==1 
estimates store contr_types_oneshot_P

char type [omit] 2
xi: xtmixed c i.type if cpp==1 & experiment==2  || mg: || id: 
estimates store contr_types_strangers_P

char type [omit] 2
xi: xtmixed c i.type if cpp==1 & experiment==3  || mg: || id: 
estimates store contr_types_partners_P

* Show output
esttab contr_types_oneshot_M contr_types_oneshot_P contr_types_strangers_M contr_types_strangers_P  contr_types_partners_M  contr_types_partners_P using "Table_S6.rtf", replace  varwidth(30)  modelwidth(10) se(%9.3f) ar2 b(%9.3f) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Supplementary Table 6: Comparing effective contributions across attitude types") nonotes mtitles("One-shot Maintenance" "One-shot Provision" "Strangers Maintenance" "Strangers Provision" "Partners Maintenance" "Partners Provision") ///
coeflabels( _Itype_1 "CC" _Itype_3 "OT" _cons "Constant") addnotes("Notes: Multilevel linear regression. Robust standard errors clustered on the group and the individual level in parentheses, * p<0.1, ** p<0.05, *** p<0.01")


************************
********Table S7********
************************
use "GKQ_Dataset_pun.dta", clear


xi: xtmixed pun i.cpp*pos_dev i.cpp|neg_dev  i.cpp|period  i.cpp|avg_other_two, || mg: || id:
eststo conditional_pun_MvsP

xi: xtmixed pun i.cpp*pos_dev i.cpp|neg_dev  i.cpp|period  i.cpp|avg_other_two if type==1, || mg: || id:
eststo conditional_pun_MvsP_CC

xi: xtmixed pun i.cpp*pos_dev i.cpp|neg_dev  i.cpp|period  i.cpp|avg_other_two if type==2, || mg: || id:
eststo conditional_pun_MvsP_FR

xi: xtmixed pun i.cpp*pos_dev i.cpp|neg_dev  i.cpp|period  i.cpp|avg_other_two if type==3, || mg: || id:
eststo conditional_pun_MvsP_OT

esttab  conditional_pun_MvsP conditional_pun_MvsP_CC conditional_pun_MvsP_FR conditional_pun_MvsP_OT using "Table_S7.rtf", replace  varwidth(30)  modelwidth(10) b(%9.3f) se(%9.3f) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles( "All"  "CC" "FR" "OT") title("Supplementary Table 7: Comparing negative reciprocity across Maintenance and Provision") ///
coeflabels(_IcppXavg_o_1 "Provision x avg_other_two" _Icpp_1 "Provision" pos_dev "Positive deviation from c_i" neg_dev "Negative deviation from c_i" _IcppXpos_d_1 "Provision x Positive deviation from c_i" _IcppXneg_d_1 "Provision x Negative deviation from c_i" period "Round" _IcppXperio_1 "Provision x Round" _cons "Constant") ///
addnotes("Note: Multi-level regression, * p<0.1, ** p<0.05, *** p<0.01")



*************************
********Table S8********
*************************
use "GKQ_Dataset_pun.dta", clear

* Maintenance
char type [omit] 2
xi:xtmixed pun i.type*pos_dev i.type|neg_dev  i.type|period  i.type|avg_other_two if cpp==0, || mg: || id:
eststo conditional_pun_types_M
* Provision
char type [omit] 2
xi:xtmixed pun i.type*pos_dev i.type|neg_dev  i.type|period  i.type|avg_other_two if cpp==1, || mg: || id:
eststo conditional_pun_types_P

esttab  conditional_pun_types_M conditional_pun_types_P  using "Table_S8.rtf", replace  varwidth(30)  modelwidth(10) b(%9.3f) se(%9.3f) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles( "Maintenance" "Provision") title("Supplementary Table 8: Comparing negative reciprocity across attitude types") ///
coeflabels(_Itype_1 "CC" _Itype_3 "OT" pos_dev "Positive deviation from c_i" _ItypXpos_d_1 "Positive deviation from c_i x CC" _ItypXpos_d_3 "Positive deviation from c_i x OT" neg_dev "Negative deviation from c_i" ///
_ItypXneg_d_1 "Negative deviation from c_i x CC" _ItypXneg_d_3 "Negative deviation from c_i x OT" period "Round" _ItypXperio_1 "CC x Round" _ItypXperio_3 "OT x Round" _cons "Constant") ///
addnotes("Note: Multi-level regression, * p<0.1, ** p<0.05, *** p<0.01")





**************
***Table S9***
**************
use "GKQ_Dataset.dta", clear
keep if punishment==1
collapse pgsize, by(cpp mg period)

gen part=1 if period<=9
replace part=2 if period<=18 & part==.
replace part=3 if period<=27 & part==.

table cpp if period==1, c(mean pgsize sd pgsize) format(%9.2f)
table cpp if part==1, c(mean pgsize ) format(%9.2f)
table cpp if part==2, c(mean pgsize ) format(%9.2f)
table cpp if part==3, c(mean pgsize ) format(%9.2f)
table cpp, c(mean pgsize ) format(%9.2f)

bys  cpp period: egen stddev_pgsize = sd(pgsize)
table cpp part, c(mean stddev_pgsize) format(%9.2f)
table cpp , c(mean stddev_pgsize) format(%9.2f)


ttest pgsize if period==1 , by(cpp)
xtmixed pgsize cpp if part==1 || mg:
xtmixed pgsize cpp if part==2 || mg:
xtmixed pgsize cpp if part==3 || mg:
xtmixed pgsize cpp  || mg:


***************
***Table S10***
***************
use "GKQ_Dataset.dta", clear
collapse pgsize, by(experiment cpp partners strangers punishment mg period)

xtmixed pgsize cpp  if  punishment==1 || mg: 
eststo MvsP_punishment
xtmixed pgsize punishment if cpp==0 & (partners==1 | punishment==1) || mg: 
eststo M_partner_punishment
xtmixed pgsize punishment if cpp==1 & (partners==1 | punishment==1) || mg:
eststo P_partner_punishment


* Show regression output
esttab MvsP_punishment M_partner_punishment P_partner_punishment using "Table_S10.rtf", replace  varwidth(25) depvars modelwidth(15) se ar2 star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Supplementary Table 10: Comparing the public good size in Partners with Punishment") nonotes mtitles( "" "Maintenance" "Provision")  ///
coeflabels( cpp "Provision" punishment "Partners with Punishment" _cons "Constant") addnotes("Note: Robust standard errors in parentheses, * p<0.1, ** p<0.05, *** p<0.01")



************************
********Table S11********
************************
use "GKQ_Dataset.dta", clear

drop if punishment==0
xtset id period


gen lag_pun_received = l.pun_received
gen lag_c = l.c
gen change_contrib = c - lag_c
gen below = c < c_other
gen lag_below = l.below


*** ABOVE AVERAGE
xi: xtmixed change_contrib   i.cpp*lag_pun_received  i.cpp|period if lag_below==0   || mg: || id:
eststo change_MvsP_above

*** BELOW AVERAGE
xi: xtmixed change_contrib   i.cpp*lag_pun_received  i.cpp|period  if lag_below==1 || mg: || id:
eststo change_MvsP_below


* Show Results 
esttab  change_MvsP_below change_MvsP_above using "Table_S11.rtf", replace  varwidth(30)  modelwidth(10) b(%9.3f) se(%9.3f) star(* 0.1 ** 0.05 *** 0.01) ///
mtitles("Below average contribution" "Above average contribution") title("Supplementary Table 11: Comparing reaction to received punishment across Maintenance and Provision") ///
coeflabels(cpp "Provision" lag_pun_received "# received pun. points" _IcppXlag_p_1 "Provision x # received pun. points" period "Round" _IcppXperio_1 "Provision x Round"  _cons "Constant") ///
addnotes("Note: Multi-level regression, * p<0.1, ** p<0.05, *** p<0.01")




*****************
*** TABLE S12 ***
*****************
use "GKQ_Dataset.dta", clear
drop if partners!=1


xtset id period
gen lag_c = l.c
gen lag_c_cpp = lag_c * cpp
gen lag_c_other_cpp = lag_c_other * cpp
gen b_cpp = b * cpp
gen period_cpp = period * cpp

gen dev_from_own = lag_c - lag_c_other
gen pos_dev_from_own = max(lag_c_other - lag_c, 0)
gen neg_dev_from_own = max(lag_c - lag_c_other, 0)

gen dev_from_own_cpp = dev_from_own * cpp
gen pos_dev_from_own_cpp =  pos_dev_from_own * cpp
gen neg_dev_from_own_cpp =  neg_dev_from_own * cpp


* POSITIVE & NEGATIVE DEVIATIONS
xtmixed c   pos_dev_from_own neg_dev_from_own  pos_dev_from_own_cpp neg_dev_from_own_cpp lag_c lag_c_cpp  period period_cpp cpp if  partners==1 || mg: || id: 
eststo m1

esttab m1 using "Table_S12.rtf", replace  b(%9.3f) se(%9.2f) r2 varwidth(50) star(* 0.1 ** 0.05 *** 0.01) ///
title("Supplementary Table 12: Comparing reactions to positive and negative deviations from own contributions in Partners") ///
coeflabels(pos_dev_from_own "Positive deviation from ci in t-1" neg_dev_from_own "Negative deviation from ci in t-1" period "Round" period_cpp "Round x Provision" cpp "Provision" _cons "Constant" ///
pos_dev_from_own_cpp "Positive deviation from ci in t-1 x Provision" neg_dev_from_own_cpp "Negative deviation from ci in t-1 x Provision" lag_c "Lagged own contribution " lag_c_cpp "Lagged own contribution x Provision") ///
order(pos_dev_from_own neg_dev_from_own  pos_dev_from_own_cpp neg_dev_from_own_cpp lag_c period lag_c_cpp period_cpp cpp) mtitles("")



********************************************
********Table S13 **************************
********************************************
use "GKQ_Dataset.dta", clear
drop if period!=1
drop if punishment==1

reshape long cc, i(id) j(avg_other)

xi: xtmixed cc i.cpp*avg_other || id:
eststo schedule_mixed


* Show regression output
esttab schedule_mixed using "Table_S13.rtf", replace   varwidth(50)  modelwidth(10) b(%9.3f) se(%9.3f) star(* 0.1 ** 0.05 *** 0.01) ///
order(avg_other _Icpp_1 _IcppXavg_o_1 _cons) mtitle("") ///
 title("Supplementary Table 13: Comparing cooperation attitudes between Maintenance and Provision")  nonotes addnotes("Note: Robust standard errors clustered at the individual level in parentheses, * p<0.1, ** p<0.05, *** p<0.01")  ///
coeflabels( svg_other "Avg. contributions others" _IcppXavg_o_1 "Avg. contributions others x Provision" _Icpp_1 "Provision"  _cons "Constant") 



********************************************
********CLUSTER ANALYSIS *******************
********************************************
use "GKQ_Dataset.dta", clear
drop if period!=1
drop if punishment==1


cluster wardslinkage cc*, measure(L1)

*** optimal number of clusters
cluster stop _clus_1, rule(duda) groups(1/10)


**** generate number of clusters
cluster generate clus = groups(6), name(_clus_1) ties(more)

gen type_clus=1 if clus==6
replace type_clus=2 if clus==1
replace type_clus=3 if clus==3
replace type_clus=4 if clus==5
replace type_clus=5 if clus==4
replace type_clus=6 if clus==2

label define type_clus 1 "strongCC" 2 "weakCC" 3 "selfish" 4 "altruists" 5 "midrange" 6 "hump-shaped"
label values type_clus type_clus

gen strongCC=type_clus==1
gen weakCC=type_clus==2
gen selfish=type_clus==3
gen altruists=type_clus==4
gen midrange=type_clus==5
gen hump=type_clus==6

tab cpp type_clus, row chi

tab cpp strongCC, row chi
tab cpp weakCC, row chi
tab cpp selfish, row chi
tab cpp altruists, row chi
tab cpp midrange, row chi
tab cpp hump, row chi


*Comparison with our classification
tab type type_clus, row 


prog drop _all
set more off



******************************
*******Table S14**************
******************************
* Finite mixture models; code adjusted from Moffatt (2015), "Experimetrics - Econometrics fro Experimental Economists", Palgrave.

clear

* LIKELIHOOD EVALUATION PROGRAM STARTS HERE:

program define pg_mixture
	args todo b logl
	tempvar p1_1 p2_1 p3_1 p1_2 p2_2 p3_2 p1_3 p2_3 p3_3 p1 p2 p3 pp1 pp2 pp3 pp
		
	tempname xb1 xb2 sig1 sig2 p_rec p_str

* ASSIGN PARAMETER NAMES TO THE ELEMENTS OF THE PARAMETER VECTOR b:
	
	mleval `xb1' = `b', eq(1) 
	mleval `xb2' = `b', eq(2) 
	mleval `sig1' = `b', eq(3) scalar
	mleval `sig2'=`b', eq(4) scalar
	mleval `p_rec'=`b', eq(5) scalar
	mleval `p_str'=`b', eq(6) scalar
	
	quietly{
	
* INITIALISE THE p* VARIABLES WITH MISSING VALUES:
	
	gen double `p1_1'=.
	gen double `p2_1'=.
	gen double `p3_1'=.
	gen double `p1_2'=.
	gen double `p2_2'=.
	gen double `p3_2'=.
	gen double `p1_3'=.
	gen double `p2_3'=.
	gen double `p3_3'=.
	
	gen double `p1'=.
	gen double `p2'=.
	gen double `p3'=.
	
	gen double `pp1'=.
	gen double `pp2'=.
	gen double `pp3'=.
	gen double `pp'=.
	

* COMPUTE TYPE-CONDITIONAL DENSITIES UNDER REGIME 1:
			
		replace `p1_1'=normal(-`xb1'/`sig1')
		replace `p2_1'=normal(-`xb2'/`sig2')
		replace `p3_1'=1

* COMPUTE TYPE-CONDITIONAL DENSITIES UNDER REGIME 2:
		
		replace `p1_2'=(1/`sig1')*normalden((c-`xb1')/`sig1')
		replace `p2_2'=(1/`sig2')*normalden((c-`xb2')/`sig2')
		replace `p3_2'=0

* COMPUTE TYPE-CONDITIONAL DENSITIES UNDER REGIME 3:
		
		replace `p1_3'=(1-normal((20-`xb1')/`sig1'))
		replace `p2_3'=(1-normal((20-`xb2')/`sig2'))
		replace `p3_3'=0

* MATCH TYPE-CONDITIONAL DENSITIES TO ACTUAL REGIMES (d IS REGIME):		
		
		replace `p1' = (d==1)*`p1_1'+(d==2)*`p1_2'+(d==3)*`p1_3'
		replace `p2' = (d==1)*`p2_1'+(d==2)*`p2_2'+(d==3)*`p2_3'
		replace `p3' = (d==1)*`p3_1'+(d==2)*`p3_2'+(d==3)*`p3_3'

* FIND PRODUCT OF TYPE-CONDITIONAL DENSITIES FOR EACH SUBJECT:		

		by id: replace `pp1'=exp(sum(ln(max(`p1',1e-12))))
		by id: replace `pp2'=exp(sum(ln(max(`p2',1e-12))))
		by id: replace `pp3'=exp(sum(ln(max(`p3',1e-12))))

* COMBINE TYPE-CONDITIONAL DENSITIES TO OBTAIN MARGINAL DENSITY FOR EACH SUBJECT
* (ONLY REQUIRED IN FINAL ROW FOR EACH SUBJECT):
		
		replace `pp'=`p_rec'*`pp1'+`p_str'*`pp2'+(1-`p_rec'-`p_str')*`pp3'
		replace `pp'=. if last~=1

* SPECIFY (LOG-LIKELIHOOD) FUNCTION WHOSE SUM OVER SUBJECTS IS TO BE MAXIMISED
		
		mlsum `logl'=ln(`pp') if last==1

* GENERATE POSTERIOR TYPE PROBABILITIES, AND MAKE THESE AVAILABLE OUTSIDE THE PROGRAM
		
		replace postp1=`p_rec'*`pp1'/`pp'
		replace postp2=`p_str'*`pp2'/`pp'
		replace postp3=(1-`p_rec'-`p_str')*`pp3'/`pp'
		
		putmata postp1, replace
		putmata postp2, replace
		putmata postp3, replace
		
	
	}
					
	end

* END OF LOG-LIKELIHOOD EVALUATION PROGRAM
	
clear
set more off




****Linear mixture model PROVISION STRANGERS
use "GKQ_Dataset.dta", clear

keep if strangers==1
keep id period lag_c_other c cpp strangers partners type
drop if period==1
keep if cpp==1

bys id: gen last=_n==_N

gen int d=1
replace d=2 if c>0
replace d=3 if c==20


*  SPECIFY EXPLANATORY-VARIABLE LISTS FOR RECIPROCATOR (LIST1) 
*  AND STRATEGIST (LIST2) EQUATIONS:
local list1 "lag_c_other period"
local list2 "period"

* INITIALISE VARIABLES TO BE USED FOR POSTERIOR TYPE PROBABILITIES:
gen postp1=.
gen postp2=.
gen postp3=.

* SPECIFY STARTING VALUES:
mat start=(0.25,-0.10,6.1,-0.93,5.2,3.3,3.7,0.26,0.49)

* SPECIFY LIKELIHOOD EVALUATOR, PROGRAM, AND PARAMETER NAMES:
ml model d0 pg_mixture (=`list1') (=`list2') /sig1 /sig2  /p_rec /p_str
ml init start, copy

* USE ML COMMAND TO MAXIMISE LOG-LIKELIHOOD, AND STORE RESULTS:

ml max, trace search(norescale)
est store cpp_strangers
estat ic

* COMPUTE THIRD MIXING PROPORTION USING DELTA METHOD:
nlcom p_fr: 1-[p_rec]_b[_cons]-[p_str]_b[_cons]

* EXTRACT POSTERIOR TYPE PROBABILITIES 
drop postp1 postp2 postp3

getmata postp1
getmata postp2
getmata postp3

label variable postp1 "rec"
label variable postp2 "str"
label variable postp3 "fr"

gen type_fmm=1 if postp1>postp2 & postp1>postp3
replace type_fmm=2 if postp3>postp2 & postp3>postp1
replace type_fmm=3 if postp2>postp3 & postp2>postp1

keep if period==27
keep cpp type strangers partners type_fmm

save strangers_cpp.dta, replace




****Linear mixture model MAINTENANCE STRANGERS
use "GKQ_Dataset.dta", clear

keep if strangers==1
keep id period lag_c_other c cpp strangers partners type
drop if period==1
keep if cpp==0

bys id: gen last=_n==_N

gen int d=1
replace d=2 if c>0
replace d=3 if c==20


*  SPECIFY EXPLANATORY-VARIABLE LISTS FOR RECIPROCATOR (LIST1) 
*  AND STRATEGIST (LIST2) EQUATIONS:
local list1 "lag_c_other period"
local list2 "period"

* INITIALISE VARIABLES TO BE USED FOR POSTERIOR TYPE PROBABILITIES:
gen postp1=.
gen postp2=.
gen postp3=.

* SPECIFY STARTING VALUES:
mat start=(0.25,-0.10,6.1,-0.93,5.2,3.3,3.7,0.26,0.49)

* SPECIFY LIKELIHOOD EVALUATOR, PROGRAM, AND PARAMETER NAMES:
ml model d0 pg_mixture (=`list1') (=`list2') /sig1 /sig2  /p_rec /p_str
ml init start, copy

* USE ML COMMAND TO MAXIMISE LOG-LIKELIHOOD, AND STORE RESULTS:
ml max, trace search(norescale)
est store cpm_strangers
estat ic

* COMPUTE THIRD MIXING PROPORTION USING DELTA METHOD:

nlcom p_fr: 1-[p_rec]_b[_cons]-[p_str]_b[_cons]

* EXTRACT POSTERIOR TYPE PROBABILITIES AND 

drop postp1 postp2 postp3

getmata postp1
getmata postp2
getmata postp3

label variable postp1 "rec"
label variable postp2 "str"
label variable postp3 "fr"

gen type_fmm=1 if postp1>postp2 & postp1>postp3
replace type_fmm=2 if postp3>postp2 & postp3>postp1
replace type_fmm=3 if postp2>postp3 & postp2>postp1

keep if period==27
keep cpp type strangers partners type_fmm

save strangers_cpm.dta, replace



****Linear mixture model PROVISION PARTNERS
use "GKQ_Dataset.dta", clear

keep if partners==1
keep id period lag_c_other c cpp strangers partners type
drop if period==1
keep if cpp==1

bys id: gen last=_n==_N

gen int d=1
replace d=2 if c>0
replace d=3 if c==20


*  SPECIFY EXPLANATORY-VARIABLE LISTS FOR RECIPROCATOR (LIST1) 
*  AND STRATEGIST (LIST2) EQUATIONS:
local list1 "lag_c_other period"
local list2 "period"

* INITIALISE VARIABLES TO BE USED FOR POSTERIOR TYPE PROBABILITIES:
gen postp1=.
gen postp2=.
gen postp3=.

* SPECIFY STARTING VALUES:
mat start=(0.25,-0.10,6.1,-0.93,5.2,3.3,3.7,0.26,0.49)

* SPECIFY LIKELIHOOD EVALUATOR, PROGRAM, AND PARAMETER NAMES:
ml model d0 pg_mixture (=`list1') (=`list2') /sig1 /sig2  /p_rec /p_str
ml init start, copy

* USE ML COMMAND TO MAXIMISE LOG-LIKELIHOOD, AND STORE RESULTS:
ml max, trace search(norescale)
est store cpp_partners
estat ic

* COMPUTE THIRD MIXING PROPORTION USING DELTA METHOD:
nlcom p_fr: 1-[p_rec]_b[_cons]-[p_str]_b[_cons]

* EXTRACT POSTERIOR TYPE PROBABILITIES 
drop postp1 postp2 postp3

getmata postp1
getmata postp2
getmata postp3

label variable postp1 "rec"
label variable postp2 "str"
label variable postp3 "fr"

gen type_fmm=1 if postp1>postp2 & postp1>postp3
replace type_fmm=2 if postp3>postp2 & postp3>postp1
replace type_fmm=3 if postp2>postp3 & postp2>postp1

keep if period==27
keep cpp type strangers partners type_fmm

save partners_cpp.dta, replace



****Linear mixture model MAINTENANCE PARTNERS
use "GKQ_Dataset.dta", clear

keep if partners==1
keep id period lag_c_other c cpp strangers partners type
drop if period==1
keep if cpp==0

bys id: gen last=_n==_N

gen int d=1
replace d=2 if c>0
replace d=3 if c==20


*  SPECIFY EXPLANATORY-VARIABLE LISTS FOR RECIPROCATOR (LIST1) 
*  AND STRATEGIST (LIST2) EQUATIONS:
local list1 "lag_c_other period"
local list2 "period"

* INITIALISE VARIABLES TO BE USED FOR POSTERIOR TYPE PROBABILITIES:
gen postp1=.
gen postp2=.
gen postp3=.

* SPECIFY STARTING VALUES:
mat start=(0.25,-0.10,6.1,-0.93,5.2,3.3,3.7,0.26,0.49)


* SPECIFY LIKELIHOOD EVALUATOR, PROGRAM, AND PARAMETER NAMES:
ml model d0 pg_mixture (=`list1') (=`list2') /sig1 /sig2  /p_rec /p_str
ml init start, copy


* USE ML COMMAND TO MAXIMISE LOG-LIKELIHOOD, AND STORE RESULTS:
ml max, trace search(norescale)
est store cpm_partners
estat ic


* COMPUTE THIRD MIXING PROPORTION USING DELTA METHOD:
nlcom p_fr: 1-[p_rec]_b[_cons]-[p_str]_b[_cons]


* EXTRACT POSTERIOR TYPE PROBABILITIES 
drop postp1 postp2 postp3

getmata postp1
getmata postp2
getmata postp3

label variable postp1 "rec"
label variable postp2 "str"
label variable postp3 "fr"

gen type_fmm=1 if postp1>postp2 & postp1>postp3
replace type_fmm=2 if postp3>postp2 & postp3>postp1
replace type_fmm=3 if postp2>postp3 & postp2>postp1

keep if period==27
keep cpp type strangers partners type_fmm

save partners_cpm.dta, replace




**Append files 
use strangers_cpm.dta, clear
append using strangers_cpp.dta
append using partners_cpp.dta
append using partners_cpm.dta


gen CC_fmm=type_fmm==1
gen FR_fmm=type_fmm==2
gen STR_fmm=type_fmm==3

gen CC=type==1
gen FR=type==2
gen STR=type==3


* Show output
esttab cpm_strangers cpp_strangers  cpm_partners cpp_partners  using Table_S14.rtf, replace varwidth(30)  modelwidth(10) scalars(ll)  aic se(%9.3f) b(%9.3f) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Supplementary Table 14: Maximum likelihood estimation of two-limit Tobit finite mixture models") nonotes mtitles("Strangers Maintenance" "Strangers Provision" "Partners Maintenance" "Partners Provision") ///
coeflabels( lag_c_other "Others' lagged contributions (t-1)" period "Round" _cons "Constant") addnotes("Notes: Two-limits Tobit maximum likelihood estimations. Standard errors in parentheses, * p<0.1, ** p<0.05, *** p<0.01")




************************************************************************************************************************************************************************
*** Figure S1 ***
************************************************************************************************************************************************************************
use "GKQ_Dataset", clear
drop if experiment==4
collapse c, by(experiment cpp type period)

label define cpp 0 "Maintenance" 1 "Provision"
label values cpp cpp

gen xaxis = 1 


*** ONESHOT MAINTENANCE
tw ///
(sc c xaxis if type==1 & cpp==0 & experiment==1,  msize(medlarge) msym(O) color(eltblue)) ///
(sc c xaxis if type==2 & cpp==0 & experiment==1,  msize(medlarge) msym(S) color(eltblue)) ///
(sc c xaxis if type==3 & cpp==0 & experiment==1,  msize(medlarge) msym(T) mfcolor(white) color(eltblue)), ///
 subtitle("One-shot", alignment(middle) box bexpand fcolor(gs12)) ///
xscale(range(0.5 1.5))  xtitle("") xlabel(1, labsize(medlarge)) ///
ylabel(0(2)12, angle(0) nogrid gmin gmax labsize(large) format(%9.0fc))  ///
ytitle("Effective contributions", size(large)) ///
 legend(off) title("{bf:a}", pos(10) size(huge)) name(oneshot_cpm, replace)  fxsize(30)

*** STRANGERS MAINTENANCE 
tw ///
(sc c period if type==1 & cpp==0 & experiment==2, connect(l) msize(medlarge) msym(O) lpattern(solid) color(eltblue)) ///
(sc c period if type==2 & cpp==0 & experiment==2, connect(l)   msize(medlarge) msym(S) lpattern(solid) color(eltblue)) ///
(sc c period if type==3 & cpp==0 & experiment==2, connect(l)   msize(medlarge) msym(T) mfcolor(white) lpattern(solid) color(eltblue)), ///
 subtitle("Strangers", alignment(middle) box bexpand fcolor(gs12))  ytitle("") ///
 xtitle("") xlabel(1  10  20 27, labsize(medlarge)) ///
 ylabel("", angle(0) nogrid gmin gmax labsize(large) format(%9.0fc)) ///ytitle("Average contribution ({&plusminus} 1 s.e.m.)", size(large)) ////
legend(off)   name(strangers_cpm, replace)  fxsize(50)


*** PARTNERS MAINTENANCE 
tw ///
(sc c period if type==1 & cpp==0 & experiment==3, connect(l) msize(medlarge) msym(O) lpattern(solid) color(eltblue)) ///
(sc c period if type==2 & cpp==0 & experiment==3, connect(l)   msize(medlarge) msym(S) lpattern(solid) color(eltblue)) ///
(sc c period if type==3 & cpp==0 & experiment==3, connect(l)   msize(medlarge) msym(T) mfcolor(white) lpattern(solid) color(eltblue)), ///
 subtitle("Partners", alignment(middle) box bexpand fcolor(gs12))  ytitle("") ///
 xtitle("") xlabel(1  10  20 27, labsize(medlarge)) ///
 ylabel("", angle(0) nogrid gmin gmax labsize(large) format(%9.0fc)) ///ytitle("Average contribution ({&plusminus} 1 s.e.m.)", size(large)) ////
legend(order(1 "CC" 2 "FR" 3 "OT")  rows(3) ring(0) pos(2))   name(partners_cpm, replace)  fxsize(50)


* Combine Graph
graph combine oneshot_cpm strangers_cpm partners_cpm,  rows(1)  imargin(zero) b1title(Round, color(bg)) 
graph export "fig_S1a.pdf", replace


*** ONESHOT PROVISION
tw ///
(sc c xaxis if type==1 & cpp==1 & experiment==1,  msize(medlarge) msym(O) lpattern(solid) color(red)) ///
(sc c xaxis if type==2 & cpp==1 & experiment==1,  msize(medlarge) msym(S) lpattern(solid) color(red)) ///
(sc c xaxis if type==3 & cpp==1 & experiment==1,  msize(medlarge) msym(T) mfcolor(white) lpattern(solid) color(red)), ///
 subtitle("One-shot", alignment(middle) box bexpand fcolor(gs12)) ///
xscale(range(0.5 1.5))  xtitle("") xlabel(1, labsize(medlarge)) ///
ylabel(0(2)12, angle(0) nogrid gmin gmax labsize(large) format(%9.0fc))  ///
ytitle("Effective contributions", size(large)) ///
 legend(off) title("{bf:b}", pos(10) size(huge)) name(oneshot_cpp, replace)  fxsize(30)

*** STRANGERS PROVISION 
tw ///
(sc c period if type==1 & cpp==1 & experiment==2, connect(l) msize(medlarge) msym(O) lpattern(solid) color(red)) ///
(sc c period if type==2 & cpp==1 & experiment==2, connect(l)   msize(medlarge) msym(S) lpattern(solid) color(red)) ///
(sc c period if type==3 & cpp==1 & experiment==2, connect(l)   msize(medlarge) msym(T) mfcolor(white) lpattern(solid) color(red)), ///
 subtitle("Strangers", alignment(middle) box bexpand fcolor(gs12))  ytitle("") ///
 xtitle("") xlabel(1  10  20 27, labsize(medlarge)) ///
 ylabel("", angle(0) nogrid gmin gmax labsize(large) format(%9.0fc)) ///ytitle("Average contribution ({&plusminus} 1 s.e.m.)", size(large)) ////
legend(off)   name(strangers_cpp, replace)  fxsize(50)


*** PARTNERS PROVISION
tw ///
(sc c period if type==1 & cpp==1 & experiment==3, connect(l) msize(medlarge) msym(O) lpattern(solid) color(red)) ///
(sc c period if type==2 & cpp==1 & experiment==3, connect(l)   msize(medlarge) msym(S) lpattern(solid) color(red)) ///
(sc c period if type==3 & cpp==1 & experiment==3, connect(l)   msize(medlarge) msym(T) mfcolor(white) lpattern(solid) color(red)), ///
 subtitle("Partners", alignment(middle) box bexpand fcolor(gs12))  ytitle("") ///
 xtitle("") xlabel(1  10  20 27, labsize(medlarge)) ///
 ylabel("", angle(0) nogrid gmin gmax labsize(large) format(%9.0fc)) ///ytitle("Average contribution ({&plusminus} 1 s.e.m.)", size(large)) ////
legend(order(1 "CC" 2 "FR" 3 "OT")  rows(3) ring(0) pos(2))   name(partners_cpp, replace)  fxsize(50)

* Combine Graph
graph combine oneshot_cpp strangers_cpp partners_cpp,  rows(1)  imargin(zero) b1title(Round) 
graph export "fig_S1b.pdf", replace




************************************************************************************************************************************************************************
*** Figure S2 ***
************************************************************************************************************************************************************************
use "GKQ_Dataset.dta", clear

keep if punishment==1
drop if period!=1


bys cpp : egen mean_cc = mean(CC)
bys cpp : egen mean_fr = mean(FR)
bys cpp : egen mean_ot = mean(OT)
gen mean_type = .
replace mean_type = mean_cc if type==1
replace mean_type = mean_fr if type==2
replace mean_type = mean_ot if type==3

* Generate X-Axis
gen xaxis = .
replace xaxis= 1.0 if type==1 & cpp==0
replace xaxis= 3.5 if type==2 & cpp==0
replace xaxis= 6.0 if type==3 & cpp==0
replace xaxis= 2.0 if type==1 & cpp==1
replace xaxis= 4.5 if type==2 & cpp==1
replace xaxis= 7.0 if type==3 & cpp==1


tab cpp type, row chi
tab cpp CC, row chi
tab cpp FR, row chi
tab cpp OT, row chi


tw (bar mean_type xaxis if cpp==1, barwidth(0.9)  fcolor(red)  lcolor(red)  ) ///
   (bar mean_type xaxis if cpp==0, barwidth(0.9)  fcolor(eltblue) lcolor(eltblue) ) ///
   (scatteri 0.60 1.0  0.60 2.0, recast(line) lcolor(black)) ///
   (scatteri 0.38 3.5  0.38 4.5, recast(line) lcolor(black)) ///
   (scatteri 0.35 6.0  0.35 7.0, recast(line) lcolor(black)), ///
	xlabel(1.5 `""Conditional" "cooperators""' 4 `""Free" "riders""' 6.5 "Others", noticks labgap(1) labsize(large)) xscale(range(0.25 7.75)) ///
	ylabel(0 .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60" .7 "70" .8 "80" , angle(0) gmin gmax labsize(large))  ///
	ytitle("Percent", size(vlarge)) xtitle("") legend(order(2 "Maintenance" 1 "Provision") rows(2) ring(0) position(1) size(medlarge)) ///
	text(0.62 1.5 " *** ", size(vhuge)  fcolor(none)) ///
	text(0.40 4.0 " ** ", size(vhuge)  fcolor(none)) ///
	text(0.40 6.5 " ns ", size(vlarge)  fcolor(none)) ///
	text(0.285 2.0 " 57%", box fcolor(white) lcolor(white) size(medlarge) ) ///
	text(0.165 1.0 " 33%", box fcolor(white) lcolor(white) size(medlarge)) ///
	text(0.09 4.5 " 18%", box fcolor(white) lcolor(white) size(medlarge)) ///
	text(0.175 3.5 " 35%", box fcolor(white) lcolor(white) size(medlarge)) ///
	text(0.125 7.0 " 25%", box fcolor(white) lcolor(white) size(medlarge)) ///
	text(0.16 6.0 " 32%", box fcolor(white) lcolor(white) size(medlarge)) ///
	name(fig_S3, replace)
graph export "fig_S2.pdf", replace


************************************************************************************************************************************************************************
*** Figure S3 ***
************************************************************************************************************************************************************************
use "GKQ_Dataset.dta", clear
drop if punishment!=1


gen deal_punish = pun>0
bys id: egen num_pun_periods = total(deal_punish)


collapse num_pun_periods, by(cpp id)

bys cpp: su num_pun_periods

label define cpp 0 "Maintenance" 1 "Provision"
label values cpp cpp

hist num_pun_periods, by(cpp, note("")) subtitle(, alignment(middle) box fcolor(gs12)) xtitle("Number of periods each participant punishes",size(medlarge)) percent ///
xlabel(0 10 20 27, labsize(medlarge))  ylabel(, nogrid angle(0) labsize(medlarge)) ytitle(,size(medlarge))
graph export "fig_s3.pdf", replace


