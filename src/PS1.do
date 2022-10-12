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
est clear 
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

est clear 
* First difference
eststo: reg S1.Y S1.A S1.K S1.L S1.year_* if balanced==1, nocons // need to get rid of constant due to collinearity

* Second difference
eststo: reg S2.Y S2.A S2.K S2.L S2.year_* if balanced==1, nocons 

* Third difference 
eststo: reg S3.Y S3.A S3.K S3.L S3.year_* if balanced==1, nocons 

esttab using "./out/table_question3.tex", replace ///
rename(S2.A S.A S3.A S.A S2.K S.K S3.K S.K S2.L S.L S3.L S.L) ///
b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("First" "Second" "Third")  ///
 keep(S.A S.K S.L) coeflabel(S.A "Age of the firm" S.K "Log of Capital" S.L "Log of Labor") ///
 booktabs nonotes 
********************************************************************************

* Question 4 *

** a) Using the full panel, compute pooled and FE estimators 
est clear 
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
est clear 
eststo: probit X I A K

* Table 
esttab using "./out/table_question4b.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("Probit") /// 
 keep(I K A) ///
 booktabs nonotes
 
* Inverse Mills Ratio 
predict phat_xb, xb
predict phat
gen pdf_probit = normalden(phat_xb)
gen cdf_probit = normprob(phat_xb)
gen inv_mills = pdf_probit/cdf_probit 

* Running pooled and FE with mills 
est clear 
eststo: reg Y A K L i.year L1.inv_mills // pooled
eststo: xtreg Y A K L i.year L1.inv_mills, fe // FE 

* Table 
esttab using "./out/table_question4b_mills.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("Pooled" "Within") /// 
 keep(A K L) ///
 booktabs nonotes


********************************************************************************

* Question 5 *

** a) Run the first stage 
* Create squared variables 
gen ksq = K^2
gen isq = I^2
gen asq = A^2
gen K_I = K*I
gen K_A = K*A
gen A_I = A*I 

est clear 
eststo: reg Y L i.year K I A ksq isq asq K_I K_A A_I

* Table 
esttab using "./out/table_question5a.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 keep(L) ///
 booktabs nonotes
 
** b) 
gen phi_hat = _b[_cons] + _b[K]*K + _b[A]*A + _b[I]*I + _b[ksq]*ksq + _b[asq]*asq + _b[isq]*isq + _b[K_A]*K_A + _b[K_I]*K_I + _b[A_I]*A_I

** c) 
gen Y_ss = Y - _b[L]*L 
nl (Y_ss = {b0} + {bK}*K + {bA}*A + {by2}*year_2 + {by3}*year_3 + {by4}*year_4 + {by5}*year_5 + {by6}*year_6 + {by7}*year_7 + {by8}*year_8 + {by9}*year_9 + {by10}*year_10 + {bh}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K) + {bh_sq}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K)^2) if L1.phi_hat != .
estimates store nls 

* Table 
 esttab nls using "./out/table_question5c.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) noeqlines eqlabels(none) /// 
 booktabs nonotes keep(bK:_cons bA:_cons) coeflabel(bK:_cons "Log of Capital" bA:_cons "Age of the firm")
 
** d)
gen phat_sq = phat^2

nl (Y_ss = {b0} + {bK}*K + {bA}*A + {by2}*year_2 + {by3}*year_3 + {by4}*year_4 + {by5}*year_5 + {by6}*year_6 + {by7}*year_7 + {by8}*year_8 + {by9}*year_9 + {by10}*year_10 + {bh}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K) + {bh_sq}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K)^2 + {bphat}*L1.phat + {bphat_sq}*L1.phat_sq + {b_phat_h}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K)*L1.phat) if L1.phi_hat != .
estimates store nls_d

* Table 
 esttab nls_d using "./out/table_question5d.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) noeqlines eqlabels(none) /// 
 booktabs nonotes keep(bK:_cons bA:_cons) coeflabel(bK:_cons "Log of Capital" bA:_cons "Age of the firm")
 
** e) 
net install st0145_2

gen exit = (X == 0)
opreg Y, exit(exit) state(A K) proxy(I) free(L) cvars(year_*) 
estimates store opreg 

esttab opreg using "./out/table_question5e.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 booktabs nonotes keep(Y:A Y:K Y:L)  eqlabels(none)





 












