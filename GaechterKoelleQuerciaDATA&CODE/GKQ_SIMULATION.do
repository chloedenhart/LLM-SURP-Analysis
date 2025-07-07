*Simulation results reported in article of Gaechter, Koelle and Quercia 'Reciprocity and the Tragedies of Maintaining and Providing the Commons'
*Documentation of analysis for Supplementary Information is in separate file
*Stata/SE 14.1 for Windows
*July 27, 2017



********************************************************************
*** SIMULATION ******************************************************
*********************************************************************
set more off

use "GKQ_Dataset", clear
drop if period != 1

keep id cpp cc0-cc20 b experiment

* Only use beliefs from One-shot and Strangers
replace b = . if experiment > 2


* Set Seed
set seed 1234


* Generate Variables needed for Loop
gen obs = 60000 // simulated nr. of observation
gen sample = 60 // Sample size per simulated experiment

* Set locals for loop
local obs = obs
local sample = sample


* Generate Variables for Simulation
quietly gen SIMbelief = .
quietly gen SIMcontribution = .
foreach num of numlist 0/20{
gen SIMcc`num' = .
}

quietly gen rand_belief = .
quietly gen rand_table = .

* Sort and Save
gsort experiment id
save GKQ_simul`obs'.dta, replace



*****************************************************************************
*** PROVISION  
use GKQ_simul`obs'.dta, clear

	drop if cpp==0
	set obs `obs'
	quietly gen iid = _n
	quietly order iid
	
	* Randomly draw Contribution Schedule and Belief 
	forvalues i = 1/`obs'{
	quietly replace rand_belief = 1 + floor((276)*runiform()) if iid == `i'
	quietly replace rand_table = 1 + floor((444)*runiform()) if iid == `i' 
	
	* Get relevant Contribution Schedule and Belief 
	foreach num of numlist 0/20 {
	quietly replace SIMcc`num' = cc`num'[rand_table]
	}
	quietly replace SIMbelief = b[rand_belief] if iid == `i'
	}
		

* Predict Effective Contribution using ABC
	foreach j of numlist 0/20{
	quietly replace SIMcontribution = SIMcc`j' if SIMbelief ==`j'
	}
	
	
	

* Save Data
quietly keep iid SIMcontribution* 
gen cpp=1 
quietly save GKQ_simul`obs'_cpp.dta, replace



*****************************************************************************
***  MAINTENANCE
use GKQ_simul`obs'.dta, clear

	drop if cpp==1
	set obs `obs'
	quietly gen iid = _n
	quietly order iid
	
* Randomly draw Contribution Schedule and Belief 	
	forvalues i = 1/`obs'{
	quietly replace rand_belief = 1 + floor((268)*runiform()) if iid == `i'
	quietly replace rand_table = 1 + floor((432)*runiform()) if iid == `i'

* Get relevant Contribution Schedule and Belief 
	foreach num of numlist 0/20 {
	quietly replace SIMcc`num' = cc`num'[rand_table]
	}
	quietly replace SIMbelief = b[rand_belief] if iid == `i'
	}
		

* Predict Effective Contribution using ABC
	foreach j of numlist 0/20{
	quietly replace SIMcontribution = SIMcc`j' if SIMbelief ==`j'
	}
	
	
	

* Save Data
quietly keep iid SIMcontribution* 
gen cpp=0 
quietly save GKQ_simul`obs'_cpm.dta, replace

	


*****************************************************************************
*** MERGE DATASETS 

use GKQ_simul`obs'_cpp, clear
append using GKQ_simul`obs'_cpm.dta


gen mean_cpp = .
gen mean_cpm = .


quietly gen ngroups = floor(`obs'/`sample') - 1
quietly su ngroups, meanonly

* Calculate Mean effective contributions for each simulated experiment
forvalues i=0/`r(max)'{
quietly sum SIMcontribution if cpp == 1 & iid<=(`i'+1)*`sample' & iid>`i'*`sample'
quietly replace mean_cpp = r(mean) if cpp == 1 & iid<=(`i'+1)*`sample' & iid>`i'*`sample'

quietly sum SIMcontribution if cpp == 0 & iid<=(`i'+1)*`sample' & iid>`i'*`sample'
quietly replace mean_cpm = r(mean) if cpp == 0 & iid<=(`i'+1)*`sample' & iid>`i'*`sample'

}


* Write Provision and Maintenance observations into one row
sort iid cpp 
quietly replace  mean_cpp = mean_cpp[_n+1] 
drop if cpp==1

* Generate Id for simulated runs
gen simulated_sample = ceil(iid / `sample')

* Calculate Ratio of effective contribution between Maintenance and Provision
gen ratio_cpm  = mean_cpm / mean_cpp
gen d_ratio_cpm = ratio_cpm  > 1


* Save Merged Data
collapse ratio_cpm d_ratio_cpm, by( simulated_sample )
save GKQ_simul`obs'_cpp_cpm, replace


* Descriptive Statistics
su ratio_cpm, det
tab d_ratio_cpm



*****************************************************************************
*** FIGURE 4
hist ratio_cpm, percent  ///
ytitle("Percent", size(medlarge)) xtitle("Proportion of mean effective contributions Maintenance/Provision", size(medium) margin(medsmall)) ///
ylabel(0(5)12.5, angle(0) labsize(medlarge)) xlabel(0.25(0.25)1.75, angle(0) labsize(medlarge)) ///
fcolor(gs6) lcolor(gs1) xline(1, lcolor(black) lpattern(shortdash) lwidth(medium)) subtitle("") ///
text(12 0.30 "Provision > Maintenance", size(medium) placement(e)) text(12 1.135 "Provision < Maintenance", size(medium) placement(e)) 
graph export "fig_4_`sample'.pdf", replace
