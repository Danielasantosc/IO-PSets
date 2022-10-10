*****************
* Problem Set 1 *
*****************

clear all 
set more off 

if c(username)== "abrasm" { //insert username
cd "/Users/abrasm/Dropbox/PhD Year 2/IO-PSets" // insert root path
}

* Upload data
use data/PS1_Data, clear

* Label variables
la var firm "Firm ID"
la var year "Year"
la var L "Log of Labor"
la var I "Log of Investment"
la var K "Log of Capital"
la var A "Age of the firm"
la var X "Continuation Dummy"
la var Y "Log of Output"


********************************************************************************

* Question 1 *
* Report sample statistics for the full sample, balanced sub-panel, exiters

bysort firm: egen years_sample = count(firm)
gen balanced = (years_sample == 10)

* Full Sample Table 
est clear 
estpost tabstat Y L I K A, c(stat) stat(mean p50 sd min max n)
ereturn list 

esttab using "./out/summary_stats_full.tex", replace ////
cells("mean(fmt(%6.2fc)) p50(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count(fmt(%6.0fc))") nonumber ///
nomtitle nonote noobs label booktabs ///
collabels("Mean" "Median" "SD" "Min" "Max" "N")

* Balanced Sample Table 
est clear 
estpost tabstat Y L I K A if balanced == 1, c(stat) stat(mean p50 sd min max n) 
ereturn list 

esttab using "./out/summary_stats_balanced.tex", replace ////
cells("mean(fmt(%6.2fc)) p50(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count(fmt(%6.0fc))") nonumber ///
nomtitle nonote noobs label booktabs ///
collabels("Mean" "Median" "SD" "Min" "Max" "N")

* Exiters Sample Table 
est clear 
estpost tabstat Y L I K A if balanced == 0, c(stat) stat(mean p50 sd min max n) 
ereturn list 

esttab using "./out/summary_stats_exiters.tex", replace ////
cells("mean(fmt(%6.2fc)) p50(fmt(%6.2fc)) sd(fmt(%6.2fc)) min max count(fmt(%6.0fc))") nonumber ///
nomtitle nonote noobs label booktabs ///
collabels("Mean" "Median" "SD" "Min" "Max" "N")

********************************************************************************

* Question 2 *
* Using only balanced panel, compute between, within and RE estimators

xtset firm year

* Total/Pooled
eststo: reg Y A K L i.year if balanced==1

* Between
eststo: xtreg Y A K L i.year if balanced==1, be

* Within
eststo: xtreg Y A K L i.year if balanced==1, fe
estimates store fe

* Random Effects 
eststo: xtreg Y A K L i.year if balanced==1, re
estimates store re

* Final regression table
esttab using "./out/table_question2.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("Pooled" "Between" "Within" "Random Effects") /// 
 keep(A K L) ///
 booktabs nonotes

* Hausman 
hausman fe re 


********************************************************************************

* Question 3 *
* Compute differenced estimators 
tsset firm year
tab year, g(year_)
drop year_1

* First difference
eststo: reg S1.Y S1.A S1.K S1.L S1.year_* if balanced==1, nocons // need to get rid of constant due to collinearity

* Second difference
eststo: reg S2.Y S2.A S2.K S2.L S2.year_* if balanced==1, nocons 

* Third difference 
eststo: reg S3.Y S3.A S3.K S3.L S3.year_* if balanced==1, nocons 

* Table
esttab using "./out/table_question3.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("First" "Second" "Third") /// 
 keep(A K L) ///
 booktabs nonotes
 
********************************************************************************

* Question 4 *

** a) Using the full panel, compute pooled and FE estimators 

* Total/Pooled
eststo: reg Y A K L i.year

* Within
eststo: xtreg Y A K L i.year, fe

* Table 
esttab using "./out/table_question4a.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("Pooled" "Within") /// 
 keep(A K L) ///
 booktabs nonotes

** b) Probit model 
* Agus solution includes squared variables and interactions. How do I know what specification to use? 
eststo: probit X I A K

* Table 
esttab using "./out/table_question4b.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("Probit") /// 
 keep(I K A) ///
 booktabs nonotes
 
* Inverse Mills Ratio 
predict phat, xb
gen mills = exp(-.5*phat^2)/(sqrt(2*_pi)*normprob(phat))

* Running pooled and FE with mills 
eststo: reg Y A K L i.year mills // pooled
eststo: xtreg Y A K L i.year mills, fe // FE 

* Table 
esttab using "./out/table_question4b_mills.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("Pooled" "Within") /// 
 keep(A K L) ///
 booktabs nonotes


********************************************************************************

* Question 5 *

** a) Run the first stage 
reg Y L i.year 
 
 











