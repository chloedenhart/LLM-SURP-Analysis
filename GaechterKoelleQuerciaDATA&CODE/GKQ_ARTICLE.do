*Results reported in article of Gaechter, Koelle and Quercia 'Reciprocity and the Tragedies of Maintaining and Providing the Commons'
*Documentation of analysis for Supplementary Information is in separate file
*Stata/SE 14.1 for Windows
*requires Stata package grc1leg (from http://www.stata.com/users/vwiggins)
*July 27, 2017



************************************************************************************************************************************************************************
*** ANALYSIS MAIN TEXT
************************************************************************************************************************************************************************

*** COMPARE LOW LEVELS OF PUBLIC GOOD IN ONE-SHOT
use "GKQ_Dataset.dta", clear
keep if experiment==1
collapse pgsize, by(cpp groupid)
gen low_pg = pgsize <=10
tab cpp low_pg, row chi

*** COMPARE DISTRIBUTION OF TYPES ACROSS MAINTENANCE AND PROVISION IN ONE-SHOT, STRANGERS, AND PARTNERS
use "GKQ_Dataset.dta", clear
keep if experiment<4
keep if period==1

tab cpp type, row chi
tab cpp CC, row chi
tab cpp FR, row chi
tab cpp OT, row chi


*** SPEARMAN CORRELATION BETWEEN PREDICTED AND ACTUAL CONTRIBUTIONS IN ONE-SHOT & FIRST PERIOD OF STRANGERS AND PARTNERS
use "GKQ_Dataset.dta", clear
spearman pc c if experiment==1 & cpp==0
spearman pc c if experiment==1 & cpp==1

spearman pc c if experiment==2 & cpp==0 & period==1
spearman pc c if experiment==2 & cpp==1 & period==1

spearman pc c if experiment==3 & cpp==0 & period==1
spearman pc c if experiment==3 & cpp==1 & period==1



*** DEVIATION FROM PREDICTED CONTRIBUTIONS
gen consistent = dev_pc >=-2 & dev_pc <=2

tab cpp consistent if experiment==1, row chi
tab cpp consistent if experiment==2 & period==1, row chi
tab cpp consistent if experiment==3 & period==1, row chi


*** FREE RIDERS CONTRIBUTE LESS THAN CONDITIONAL COOPERATORS AND OTHERS
* ONE-SHOT
ttest c if (FR==1 | CC==1) & experiment==1 & cpp==0, by(type)
ttest c if (FR==1 | CC==1) & experiment==1 & cpp==1, by(type)
ttest c if (FR==1 | OT==1) & experiment==1 & cpp==0, by(type)
ttest c if (FR==1 | OT==1) & experiment==1 & cpp==1, by(type)
* STRANGERS
xtmixed c FR  if (FR==1 | CC==1) & experiment==2 & cpp==0, || mg: || id:
xtmixed c FR  if (FR==1 | CC==1) & experiment==2 & cpp==1, || mg: || id:
xtmixed c FR  if (FR==1 | OT==1) & experiment==2 & cpp==0, || mg: || id:
xtmixed c FR  if (FR==1 | OT==1) & experiment==2 & cpp==1, || mg: || id:
* PARTNERS
xtmixed c FR  if (FR==1 | CC==1) & experiment==3 & cpp==0, || mg: || id:
xtmixed c FR  if (FR==1 | CC==1) & experiment==3 & cpp==1, || mg: || id:
xtmixed c FR  if (FR==1 | OT==1) & experiment==3 & cpp==0, || mg: || id:
xtmixed c FR  if (FR==1 | OT==1) & experiment==3 & cpp==1, || mg: || id:



*** COMPARE DISTRIBUTION OF TYPES ACROSS MAINTENANCE AND PROVISION IN PARTNERS WITH PUNISHMENT
use "GKQ_Dataset.dta", clear
keep if experiment==4
keep if period==1

tab cpp type, row chi
tab cpp CC, row chi
tab cpp FR, row chi
tab cpp OT, row chi


************************************************************************************
*** ESTIMATED RECIPROCITY: COMPARISONS
************************************************************************************
use "GKQ_Dataset.dta", clear
drop if oneshot==1
drop if punishment==1



*** STRANGERS: CPM vs. CPP 
xi: xtmixed c i.cpp*lag_c_other period if strangers==1 || mg: || id: 

*** PARTNERS: CPM vs. CPP   
xi: xtmixed c i.cpp*lag_c_other period if partners==1 || mg: || id: 

*** CPM: STRANGERS vs. PARTNERS
xi: xtmixed c i.partners*lag_c_other period if cpp==0 || mg: || id:

*** CPP: STRANGERS vs. PARTNERS
xi: xtmixed c i.partners*lag_c_other period if cpp==1 || mg: || id:


*** CC vs. FR in all 4 conditions
xi: xtmixed c i.CC*lag_c_other period if cpp==0 & partners==0 & type!=3 || mg: || id: 
xi: xtmixed c i.CC*lag_c_other period if cpp==1 & partners==0 & type!=3 || mg: || id: 
xi: xtmixed c i.CC*lag_c_other period if cpp==0 & partners==1 & type!=3 || mg: || id: 
xi: xtmixed c i.CC*lag_c_other period if cpp==1 & partners==1 & type!=3 || mg: || id: 


*** CC: STRANGERS vs. PARTNERS in both treatments
xi: xtmixed c i.partners*lag_c_other period if cpp==0 & type==1 || mg: || id:
xi: xtmixed c i.partners*lag_c_other period if cpp==1 & type==1 || mg: || id:

*** FR: STRANGERS vs. PARTNERS in both treatments
xi: xtmixed c i.partners*lag_c_other period if cpp==0 & type==2 || mg: || id:
xi: xtmixed c i.partners*lag_c_other period if cpp==1 & type==2 || mg: || id:

*** OT: STRANGERS vs. PARTNERS in both treatments
xi: xtmixed c i.partners*lag_c_other period if cpp==0 & type==3 || mg: || id:
xi: xtmixed c i.partners*lag_c_other period if cpp==1 & type==3 || mg: || id:






************************************************************************************************************************************************************************
*** FIGURE 2 
************************************************************************************************************************************************************************
use "GKQ_Dataset.dta", clear
collapse pgsize, by(experiment cpp mg period)
gen mean_pg = .
gen se_pg = .

* MEAN & SEM: One-shot Game
ci means pgsize if experiment==1 & cpp==0
replace mean_pg = r(mean) if experiment==1 & cpp==0
replace se_pg = r(se) if experiment==1 & cpp==0

ci means pgsize if experiment==1 & cpp==1
replace mean_pg = r(mean) if experiment==1 & cpp==1
replace se_pg = r(se) if experiment==1 & cpp==1


* MEAN & SEM: Repeated Games
foreach num of numlist  2 3 4{
foreach i of numlist 0 1{
foreach p of numlist 1/27{
quietly ci means pgsize if cpp==`i' & experiment==`num' & period==`p'
quietly replace mean_pg = r(mean) if cpp==`i' & experiment==`num' & period==`p' 
quietly replace se_pg = r(se) if cpp==`i' & experiment==`num' & period==`p' 
}
}
}

* Generate upper and lower bounds (+/- 1 SEM)
gen lowse = mean_pg - se_pg
gen hise = mean_pg + se_pg


* Collapse again for Graph
collapse mean_pg lowse hise, by(experiment cpp period)
gen xaxis = 1 


*** ONESHOT
tw ///
(rcap lowse hise xaxis if cpp==0 & experiment==1, color(black) color(eltblue)) ///
(rcap lowse hise xaxis if cpp==1 & experiment==1, color(black) color(red)) ///
(sc mean_pg xaxis if cpp==0 & experiment==1,  msize(medlarge) color(eltblue)) ///
(sc mean_pg xaxis if cpp==1 & experiment==1,  msize(medlarge) color(red)), ///
 subtitle("One-shot", alignment(middle) box bexpand fcolor(gs12)) ///
xscale(range(0.5 1.5))  xtitle("") xlabel(1, labsize(medlarge)) ///
ylabel(0(10)50, angle(0) nogrid gmin gmax labsize(large) format(%9.0fc)) yscale(range(0 50))  ///
ytitle("Public good size", size(large)) title("", pos(10) size(huge)) ///
 legend(off) name(oneshot, replace)  fxsize(30)

*** STRANGERS: 
tw ///
(rarea lowse hise period if cpp==0 & experiment==2, sort color(eltblue) lcolor(eltblue*0.22)  fintensity(22)) ///
(rarea lowse hise period if cpp==1 & experiment==2, sort color(red) lcolor(red*0.22) fintensity(22)) ///
(sc mean_pg period if cpp==0 & experiment==2, connect(l) lwidth(medthick) msize(medlarge) lpattern(solid) color(eltblue)) ///
(sc mean_pg period if cpp==1 & experiment==2, connect(l) lwidth(medthick) msize(medlarge) lpattern(solid) color(red)), ///
 subtitle("Strangers", alignment(middle) box bexpand fcolor(gs12))  ytitle("") ///
 xtitle("") xlabel(1  10  20 27, labsize(medlarge))  title("", pos(10) size(huge)) ///
yscale(range(0 50)) ylabel("", angle(0) nogrid gmin gmax labsize(large) format(%9.0fc)) ///ytitle("Average contribution ({&plusminus} 1 s.e.m.)", size(large)) ////
legend(order(3 "Maintenance" 4 "Provision") rows(2))   name(strangers, replace)  fxsize(50)

*** PARTNERS: 
tw ///
(rarea lowse hise period if cpp==0 & experiment==3, sort color(eltblue) lcolor(eltblue*0.22)  fintensity(22)) ///
(rarea lowse hise period if cpp==1 & experiment==3, sort color(red) lcolor(red*0.22) fintensity(22)) ///
(sc mean_pg period if cpp==0 & experiment==3, connect(l) lwidth(medthick) msize(medlarge) lpattern(solid)  color(eltblue)) ///
(sc mean_pg period if cpp==1 & experiment==3, connect(l) lwidth(medthick) msize(medlarge)  lpattern(solid) color(red)), ///
 subtitle("Partners", alignment(middle) box bexpand fcolor(gs12))   ytitle("") ///
 xtitle("") xlabel(1  10  20 27, labsize(medlarge))  title("", pos(10) size(huge)) ///
yscale(range(0 50)) ylabel("", angle(0) nogrid gmin gmax labsize(large) format(%9.0fc)) ///ytitle("Average contribution ({&plusminus} 1 s.e.m.)", size(large)) ///
legend(off)  name(partners, replace) fxsize(50)


* Combine Panels for Figure 2
grc1leg oneshot strangers partners,  rows(1)  imargin(zero) b1title(Round) legendfrom(strangers) pos(2) ring(0)
graph export "fig_2.pdf", replace




************************************************************************************************************************************************************************
*** FIGURE 3 PANEL A 
************************************************************************************************************************************************************************
use "GKQ_Dataset.dta", clear
drop if period!=1
drop if experiment==4

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


tw (bar mean_type xaxis if cpp==1 , barwidth(0.9)  fcolor(red)  lcolor(red)  ) ///
   (bar mean_type xaxis if cpp==0 , barwidth(0.9)  fcolor(eltblue) lcolor(eltblue) ) ///
   (scatteri 0.67 1.0  0.67 2.0, recast(line) lcolor(black)) ///
   (scatteri 0.31 3.5  0.31 4.5, recast(line) lcolor(black)) ///
   (scatteri 0.33 6.0  0.33 7.0, recast(line) lcolor(black)), ///
	xlabel(1.5 `""Conditional" "cooperators""' 4 `""Free" "riders""' 6.5 "Others", noticks labgap(1) labsize(large)) xscale(range(0.25 7.75)) ///
	ylabel(0 .1 "10" .2 "20" .3 "30" .4 "40" .5 "50" .6 "60" .7 "70" .8 "80" , angle(0) gmin gmax labsize(large))  ///
	ytitle("Percent", size(vlarge)) xtitle("") legend(order(2 "Maintenance" 1 "Provision") rows(2) ring(0) position(1) size(medlarge)) ///
	text(0.69 1.5 " *** ", size(vhuge)  fcolor(none)) ///
	text(0.33 4.0 " *** ", size(vhuge)  fcolor(none)) ///
	text(0.35 6.5 " *** ", size(vhuge)  fcolor(none)) ///
	text(0.32 2.0 " 64%", box fcolor(white) lcolor(white) size(medlarge) ) ///
	text(0.215 1.0 " 42%", box fcolor(white) lcolor(white) size(medlarge)) ///
	text(0.09 4.5 " 17%", box fcolor(white) lcolor(white) size(medlarge)) ///
	text(0.14 3.5 " 28%", box fcolor(white) lcolor(white) size(medlarge)) ///
	text(0.09 7.0 " 19%", box fcolor(white) lcolor(white) size(medlarge)) ///
	text(0.145 6.0 " 30%", box fcolor(white) lcolor(white) size(medlarge)) ///
	name(fig_3a, replace) title("{bf:a}", pos(10) size(huge))
graph export "fig_3a.pdf", replace






************************************************************************************************************************************************************************
*** FIGURE 3 PANEL B
************************************************************************************************************************************************************************
use "GKQ_Dataset.dta", clear
drop if oneshot==1
drop if punishment==1

gen est_lag_c_other = .
gen se_lag_c_other = .
gen est_lag_c_other_type = .
gen se_lag_c_other_type = .

*** STRANGERS CPP
xtmixed c lag_c_other period if cpp==1 & strangers==1 || mg: || id: 
replace est_lag_c_other = _b[lag_c_other] if cpp==1 & strangers==1
replace se_lag_c_other = _se[lag_c_other] if cpp==1 & strangers==1
xtmixed c lag_c_other period if cpp==1 & strangers==1 & type==1 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==1 & strangers==1 & type==1
replace se_lag_c_other_type = _se[lag_c_other] if cpp==1 & strangers==1 & type==1
xtmixed c lag_c_other period if cpp==1 & strangers==1 & type==2 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==1 & strangers==1 & type==2
replace se_lag_c_other_type = _se[lag_c_other] if cpp==1 & strangers==1 & type==2
xtmixed c lag_c_other period if cpp==1 & strangers==1 & type==3 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==1 & strangers==1 & type==3
replace se_lag_c_other_type = _se[lag_c_other] if cpp==1 & strangers==1 & type==3


*** STRANGERS CPM
xtmixed c lag_c_other period if cpp==0 & strangers==1 || mg: || id: 
replace est_lag_c_other = _b[lag_c_other] if cpp==0 & strangers==1
replace se_lag_c_other = _se[lag_c_other] if cpp==0 & strangers==1
xtmixed c lag_c_other period if cpp==0 & strangers==1 & type==1 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==0 & strangers==1 & type==1
replace se_lag_c_other_type = _se[lag_c_other] if cpp==0 & strangers==1 & type==1
xtmixed c lag_c_other period if cpp==0 & strangers==1 & type==2 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==0 & strangers==1 & type==2
replace se_lag_c_other_type = _se[lag_c_other] if cpp==0 & strangers==1 & type==2
xtmixed c lag_c_other period if cpp==0 & strangers==1 & type==3 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==0 & strangers==1 & type==3
replace se_lag_c_other_type = _se[lag_c_other] if cpp==0 & strangers==1 & type==3

*** PARTNERS CPP
xtmixed c lag_c_other period if cpp==1 & partners==1 || mg: || id: 
replace est_lag_c_other = _b[lag_c_other] if cpp==1 & partners==1
replace se_lag_c_other = _se[lag_c_other] if cpp==1 & partners==1
xtmixed c lag_c_other period if cpp==1 & partners==1 & type==1 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==1 & partners==1 & type==1
replace se_lag_c_other_type = _se[lag_c_other] if cpp==1 & partners==1 & type==1
xtmixed c lag_c_other period if cpp==1 & partners==1 & type==2 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==1 & partners==1 & type==2
replace se_lag_c_other_type = _se[lag_c_other] if cpp==1 & partners==1 & type==2
xtmixed c lag_c_other period if cpp==1 & partners==1 & type==3 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==1 & partners==1 & type==3
replace se_lag_c_other_type = _se[lag_c_other] if cpp==1 & partners==1 & type==3

*** PARTNERS CPM
xtmixed c lag_c_other period if cpp==0 & partners==1 || mg: || id: 
replace est_lag_c_other = _b[lag_c_other] if cpp==0 & partners==1
replace se_lag_c_other = _se[lag_c_other] if cpp==0 & partners==1
xtmixed c lag_c_other period if cpp==0 & partners==1 & type==1 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==0 & partners==1 & type==1
replace se_lag_c_other_type = _se[lag_c_other] if cpp==0 & partners==1 & type==1
xtmixed c lag_c_other period if cpp==0 & partners==1 & type==2 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==0 & partners==1 & type==2
replace se_lag_c_other_type = _se[lag_c_other] if cpp==0 & partners==1 & type==2
xtmixed c lag_c_other period if cpp==0 & partners==1 & type==3 || mg: || id: 
replace est_lag_c_other_type = _b[lag_c_other] if cpp==0 & partners==1 & type==3
replace se_lag_c_other_type = _se[lag_c_other] if cpp==0 & partners==1 & type==3




* Collapse Data
collapse est_lag_c_other se_lag_c_other est_lag_c_other_type se_lag_c_other_type, by(cpp partners type)

sort type partners cpp
set obs 16
gen id = _n
replace type = 0 if type==.
replace cpp = 0 if id==13 | id==15
replace cpp = 1 if id==14 | id==16
replace partners = 0 if id==13 | id==14
replace partners = 1 if id==15 | id==16
replace est_lag_c_other_type = est_lag_c_other[_n-4] if id>=13
replace se_lag_c_other_type = se_lag_c_other[_n-4] if id>=13
drop est_lag_c_other se_lag_c_other


label define type 0 "All" 1 `""Conditional" "cooperators""' 2 `""Free" "riders""' 3 "Others", replace
label values type type

* Generate X-Axis
gen xaxis = .
replace xaxis = 1 if partners==0 
replace xaxis = 2 if partners==1 

* Generate Upper and Lower Bounds (+/- 1SEM)
gen up  = est_lag_c_other_type + se_lag_c_other_type
gen low  = est_lag_c_other_type - se_lag_c_other_type



tw ///
(rcap up low xaxis if cpp==0, msize(vlarge) lcolor(eltblue)) ///
(rcap up low xaxis if cpp==1, msize(vlarge) lcolor(red)) ///
(sc est_lag_c_other_type xaxis if cpp==0, msize(vlarge) msymbol(T) mcolor(eltblue) connect(l) color(black) lpattern(solid) lwidth(thin)) ///
(sc est_lag_c_other_type xaxis if cpp==1, msize(vlarge) msymbol(O) mcolor(red) connect(l) color(black) lpattern(solid) lwidth(thin)), ///
by(type, note("")  cols(4) title("{bf:b}", pos(10) size(huge))) subtitle(, alignment(middle) box fcolor(gs12)) xscale(range(0.5 2.5)) xlabel(1 "S" 2 "P", labsize(medlarge) ) xtitle("") ///
ylabel(0.(0.1)0.8, angle(0) nogrid labsize(large) format(%9.1fc)) ///
ytitle("Estimated  reciprocity", size(large)) ///
legend(order(3 "Maintenance" 4 "Provision") ring(1) pos(6) rows(1)  size(medlarge))  name(fig3b, replace)
graph export "fig_3b.eps", replace
graph export "fig_3b.pdf", replace


* Combine Figure 3a & 3b
graph combine fig_3a fig3b, xsize(8)
graph export "fig_3.pdf", replace



************************************************************************************************************************************************************************
*** FIGURE 5 PANEL A
************************************************************************************************************************************************************************
use "GKQ_Dataset_pun.dta", clear


gen beta_neg_dev = .
gen se_neg_dev = .
gen beta_pos_dev = .
gen se_pos_dev = .

gen beta_neg_dev_type = .
gen se_neg_dev_type = .
gen beta_pos_dev_type = .
gen se_pos_dev_type = .


*** PROVISION
* ALL
xtmixed pun pos_dev neg_dev avg_other_two period if cpp==1, || mg: || id:
replace beta_neg_dev = _b[neg_dev] if cpp==1
replace se_neg_dev = _se[neg_dev] if cpp==1
replace beta_pos_dev = _b[pos_dev] if cpp==1
replace se_pos_dev = _se[pos_dev] if cpp==1

* CC
xtmixed pun pos_dev neg_dev avg_other_two period if cpp==1 & type==1, || mg: || id:
replace beta_neg_dev_type = _b[neg_dev] if cpp==1 & type==1
replace se_neg_dev_type = _se[neg_dev] if cpp==1 & type==1
replace beta_pos_dev_type = _b[pos_dev] if cpp==1 & type==1
replace se_pos_dev_type = _se[pos_dev] if cpp==1 & type==1

* FR
xtmixed pun pos_dev neg_dev avg_other_two period if cpp==1 & type==2, || mg: || id:
replace beta_neg_dev_type = _b[neg_dev] if cpp==1 & type==2
replace se_neg_dev_type = _se[neg_dev] if cpp==1 & type==2
replace beta_pos_dev_type = _b[pos_dev] if cpp==1 & type==2
replace se_pos_dev_type = _se[pos_dev] if cpp==1 & type==2

* OT
xtmixed pun pos_dev neg_dev avg_other_two period if cpp==1 & type==3, || mg: || id:
replace beta_neg_dev_type = _b[neg_dev] if cpp==1 & type==3
replace se_neg_dev_type = _se[neg_dev] if cpp==1 & type==3
replace beta_pos_dev_type = _b[pos_dev] if cpp==1 & type==3
replace se_pos_dev_type = _se[pos_dev] if cpp==1 & type==3



*** MAINTENANCE
* ALL
xtmixed pun pos_dev neg_dev avg_other_two period if cpp==0, || mg: || id:
replace beta_neg_dev = _b[neg_dev] if cpp==0
replace se_neg_dev = _se[neg_dev] if cpp==0
replace beta_pos_dev = _b[pos_dev] if cpp==0
replace se_pos_dev = _se[pos_dev] if cpp==0

* CC
xtmixed pun pos_dev neg_dev avg_other_two period if cpp==0 & type==1, || mg: || id:
replace beta_neg_dev_type = _b[neg_dev] if cpp==0 & type==1
replace se_neg_dev_type = _se[neg_dev] if cpp==0 & type==1
replace beta_pos_dev_type = _b[pos_dev] if cpp==0 & type==1
replace se_pos_dev_type = _se[pos_dev] if cpp==0 & type==1

* FR
xtmixed pun pos_dev neg_dev avg_other_two period if cpp==0 & type==2, || mg: || id:
replace beta_neg_dev_type = _b[neg_dev] if cpp==0 & type==2
replace se_neg_dev_type = _se[neg_dev] if cpp==0 & type==2
replace beta_pos_dev_type = _b[pos_dev] if cpp==0 & type==2
replace se_pos_dev_type = _se[pos_dev] if cpp==0 & type==2

* OT
xtmixed pun pos_dev neg_dev avg_other_two period if cpp==0 & type==3, || mg: || id:
replace beta_neg_dev_type = _b[neg_dev] if cpp==0 & type==3
replace se_neg_dev_type = _se[neg_dev] if cpp==0 & type==3
replace beta_pos_dev_type = _b[pos_dev] if cpp==0 & type==3
replace se_pos_dev_type = _se[pos_dev] if cpp==0 & type==3




* Collapse Data
collapse beta_neg_dev se_neg_dev beta_pos_dev se_pos_dev beta_neg_dev_type se_neg_dev_type beta_pos_dev_type se_pos_dev_type, by(cpp type)
sort type cpp
set obs 8
gen id = _n
replace type = 0 if type==.
replace cpp = 0 if id==7
replace cpp = 1 if id==8

replace beta_neg_dev_type = beta_neg_dev[_n-2] if id>=7
replace se_neg_dev_type = se_neg_dev[_n-2] if id>=7
replace beta_pos_dev_type = beta_pos_dev[_n-2] if id>=7
replace se_pos_dev_type = se_pos_dev[_n-2] if id>=7
drop beta_neg_dev se_neg_dev beta_pos_dev se_pos_dev
drop id

label define type 0 "All" 1 `""Conditional" "cooperators""' 2 `""Free" "riders""' 3 "Others", replace
label values type type


sort type cpp

expand 2
gen above = _n>8

* Generate x-axis
gen xaxis = 1 if cpp==0
replace xaxis = 1.2 if cpp==1
replace xaxis = 1.4 if cpp==0 & above==1
replace xaxis = 1.6 if cpp==1 & above==1


* Generate Upper & Lower Bounds
gen up_neg  = beta_neg_dev_type + se_neg_dev_type
gen low_neg  = beta_neg_dev_type - se_neg_dev_type
gen up_pos  = beta_pos_dev_type + se_pos_dev_type
gen low_pos  = beta_pos_dev_type - se_pos_dev_type



tw ///
(rcap up_neg low_neg xaxis if cpp==0 & above==0, msize(large) lcolor(eltblue)) ///
(rcap up_neg low_neg xaxis if cpp==1 & above==0, msize(large) lcolor(red)) ///
(sc beta_neg_dev_type xaxis if cpp==0 & above==0, msize(large) msymbol(T) mcolor(eltblue)  ) ///
(sc beta_neg_dev_type xaxis if cpp==1 & above==0, msize(large) msymbol(O) mcolor(red)  ) ///
(rcap up_pos low_pos xaxis if cpp==0 & above==1, msize(large) lcolor(eltblue)) ///
(rcap up_pos low_pos xaxis if cpp==1 & above==1, msize(large) lcolor(red)) ///
(sc beta_pos_dev_type xaxis if cpp==0 & above==1, msize(large) msymbol(T) mcolor(eltblue)  ) ///
(sc beta_pos_dev_type xaxis if cpp==1 & above==1, msize(large) msymbol(O) mcolor(red)  ), ///
by(type, note("") legend(off)  cols(4) title("{bf:a}", pos(10) size(huge))) subtitle(, alignment(middle) box fcolor(gs12))  ///
xlabel(1.075 `""Negative" "deviation"' 1.525 `""Positive" "deviation"', labsize(medsmall)) xscale(range(0.95 1.65)) xtitle("")  ///
ylabel(-0.025(0.025)0.15, angle(0) nogrid labsize(large) format(%9.3fc)) ///
ytitle("Estimated negative reciprocity", size(large)) ///
legend(off)   name(fig5a, replace)
graph export "fig_5a.pdf", replace





************************************************************************************************************************************************************************
*** FIGURE 5 PANEL B
************************************************************************************************************************************************************************
use "GKQ_Dataset.dta", clear
keep if punishment==1

* MEAN & SEM: Repeated Games
gen mean_pg = .
gen se_pg = .

collapse  c pgsize mean_pg se_pg, by(cpp mg period)

foreach i of numlist 0 1{
foreach p of numlist 1/27{
quietly ci means pgsize if cpp==`i'  & period==`p'
quietly replace mean_pg = r(mean) if cpp==`i'  & period==`p' 
quietly replace se_pg = r(se) if cpp==`i' & period==`p' 
}
}


gen lowse = mean_pg - se_pg
gen hise = mean_pg + se_pg

* Collapse
collapse mean_pg lowse hise , by(cpp period)


tw ///
(rarea lowse hise period if cpp==0, sort color(eltblue) lcolor(eltblue*0.22)  fintensity(22)) ///
(rarea lowse hise period if cpp==1, sort color(red) lcolor(red*0.22) fintensity(22)) ///
(sc mean_pg period if cpp==0 ,  msize(medlarge) connect(l) msymbol(T) lpattern(solid)  color(eltblue)) ///
(sc mean_pg period if cpp==1 ,  msize(medlarge) connect(l) msymbol(C) lpattern(solid)  color(red)), ///
 subtitle("Partners with Punishment", alignment(middle) box bexpand fcolor(gs12))  ytitle("") ///
xtitle("Round", size(large)) xlabel(1  10  20 27, labsize(large)) title("{bf:b}", pos(10) size(huge)) ///
yscale(range(0 60)) ylabel(0(10)60, angle(0) nogrid gmin gmax labsize(large) format(%9.0fc)) ///
ytitle("Public good size", size(large)) ///
legend(order(3 "Maintenance" 4 "Provision") ring(0)   pos(4) rows(2)  size(medlarge)) name(fig5b, replace)
graph export "fig_5b.pdf", replace



* Combine Figure 5a & 5b
graph combine fig5a fig5b, xsize(8) imargin(0)

