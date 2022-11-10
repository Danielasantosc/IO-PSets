**************************************************************
* Problem Set 1 - Industrial Organization 
* Ana, Dani, Rafa
* 13/10/2022
* Input: ./data/PS1_data.dta
* Output: all tables stored in ./out
* You can also find on github a .tex file with all the tables
**************************************************************

clear all 
set more off 

if c(username)== "abrasm" { //insert username
cd "/Users/abrasm/Dropbox/PhD Year 2/IO-PSets/Dani/IO-PSets" // insert root path
globa path "/Users/abrasm/Dropbox/PhD Year 2/IO-PSets/Dani/IO-PSets"
}

if c(username)== "Daniela" { //insert username
cd "C:/Users/Daniela/Documents/dropbox_trabajo/Dropbox/UZH/Fall_2022/IO/ps1/repo" // insert root path
globa path "C:/Users/Daniela/Documents/dropbox_trabajo/Dropbox/UZH/Fall_2022/IO/ps1/repo"

}

* Upload data
use "$path/data/PS1_Data", clear

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
                                                
local statsvars "Y L I K A"

* Full Sample Table 
est clear 
estpost tabstat `statsvars', c(stat) stat(mean sd min p25 p50 p75 max n)
ereturn list 

esttab using "$path/out/summary_stats_full.tex", replace ////
cells("mean(fmt(%6.2fc)) sd(fmt(%6.1fc))  min p25(fmt(%6.2fc)) p50(fmt(%6.2fc)) p75(fmt(%6.2fc)) max count(fmt(%6.0fc))")  ///
nonumber nomtitle nonote noobs label booktabs ///
title("Summary Statistics for the Full Sample" \label{tab:fullstats}) ///
collabels("Mean" "SD" "Min" "Perc. 25" "Median" "Perc. 75" "Max" "N")

* Balanced Sample Table 
est clear 
estpost tabstat `statsvars' if balanced == 1, c(stat) stat(mean sd min p25 p50 p75 max n)
ereturn list 

esttab using "$path/out/summary_stats_balanced.tex", replace ////
cells("mean(fmt(%6.2fc)) sd(fmt(%6.1fc))  min p25(fmt(%6.2fc)) p50(fmt(%6.2fc)) p75(fmt(%6.2fc)) max count(fmt(%6.0fc))")  ///
nonumber nomtitle nonote noobs label booktabs ///
title("Summary Statistics for the Balanced Sample" \label{tab:balstats}) ///
collabels("Mean" "SD" "Min" "Perc. 25" "Median" "Perc. 75" "Max" "N")

* Exiters Sample Table 
est clear 
estpost tabstat `statsvars' if balanced == 0, c(stat) stat(mean sd min p25 p50 p75 max n)
ereturn list 

esttab using "$path/out/summary_stats_exiters.tex", replace ////
cells("mean(fmt(%6.2fc)) sd(fmt(%6.1fc))  min p25(fmt(%6.2fc)) p50(fmt(%6.2fc)) p75(fmt(%6.2fc)) max count(fmt(%6.0fc))")  ///
nonumber nomtitle nonote noobs label booktabs ///
title("Summary Statistics for the Exiters Sample" \label{tab:exitstats}) ///
collabels("Mean" "SD" "Min" "Perc. 25" "Median" "Perc. 75" "Max" "N")

foreach var of local statsvars {
    if `var'==Y local w=0.5
    if `var'==K local w=0.5
    if `var'==L local w=0.3
    if `var'==I local w=0.3
    dis "`var'"
    ttest `var', by(balanced)

    twoway (histogram `var', color(gs14) w(`w')) ///
    (histogram `var' if balanced==1, fcolor(none) lcolor(black) graphregion(color(white)) bgcolor(white) w(`w')) ///
    (histogram `var' if balanced==0, fcolor(none) lcolor(cranberry) w(`w')) ///
    , legend(order (1 "Full" 2 "Balanced" 3 "Exiters" ) pos(11) ring(0) cols(1) region(lcolor(white))) ylabel(,nogrid)
    graph export "$path/out/hist`var'.eps", replace
}

sum years_sample if balanced==0, det /// in year 6 most of them are still in the market

foreach var in Y K L I {

   egen av_`var'=mean(`var'), by(year balanced)

    twoway (scatter av_`var' year if balanced==1, connect(l) sort) ///
    (scatter av_`var' year if balanced==0, connect(l) sort), ///
    graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
    ytitle(" `:variable label `var'' ") xtitle("Years") ///
    legend(lab(1 "Balanced") lab(2 "Exiters") pos(11) cols(1) ring(0) region(lcolor(white))) ///
    xline(7) xlabel(1(1)10)
    graph export "$path/out/time`var'.eps", replace

}


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
eststo: xtreg Y A K L i.year if balanced==1, re theta
estimates store re

* Final regression table
esttab using "$path/out/table_question2.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("Pooled" "Between" "Within" "Random Effects") /// 
 keep(A K L) stats(N,fmt("%9.0fc")) ///
 title("Total, Between, Within and Random Effects Estimators" \label{tab:q2}) ///
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

qui: reg S3.Y S3.A S3.K S3.L S3.year_* if balanced==1, nocons 
g diff_sample=e(sample) /// in order to have comparable estimates (same sample)

* First difference
eststo: reg S1.Y S1.A S1.K S1.L S1.year_* if balanced==1 & diff_sample==1, nocons // need to get rid of constant due to collinearity

* Second difference
eststo: reg S2.Y S2.A S2.K S2.L S2.year_* if balanced==1 & diff_sample==1, nocons 

* Third difference 
eststo: reg S3.Y S3.A S3.K S3.L S3.year_* if balanced==1 & diff_sample==1, nocons 

esttab using "./out/table_question3.tex", replace ///
rename(S2.A S.A S3.A S.A S2.K S.K S3.K S.K S2.L S.L S3.L S.L) ///
b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("First" "Second" "Third")  stats(N,fmt("%9.0fc")) ///
 title("Difference Estimators" \label{tab:q3}) ///
 keep(S.A S.K S.L) coeflabel(S.A "Age of the firm" S.K "Log of Capital" S.L "Log of Labor") ///
 booktabs nonotes 
********************************************************************************

* Question 4 *

** a) Using the full panel, compute pooled and FE estimators 
est clear 
* Total/Pooled
eststo full: reg Y A K L i.year 
eststo balan: reg Y A K L i.year if balanced==1
suest full balan
test [full_mean]K = [balan_mean]K // different
test [full_mean]A = [balan_mean]A // not different
* Within
eststo: xtreg Y A K L i.year, fe 
eststo: xtreg Y A K L i.year if balanced==1, fe

* Table 
esttab using "$path/out/table_question4a.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles( "Pooled" "balanced" "Within" "balanced") stats(N,fmt("%9.0fc")) /// 
 title("Total and Within Estimators for Full Sample" \label{tab:q4a}) ///
 keep(A K L) ///
 booktabs nonotes

** b) Probit model 
est clear 
eststo: probit X I A K

* Table 
esttab using "$path/out/table_question4b.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 title("Probit Model for Exiting Probability" \label{tab:q4b1}) ///
 keep(I K A) eqlabels(none) stats(N,fmt("%9.0fc")) ///
 booktabs nonotes
 
* Inverse Mills Ratio 
predict phat_xb, xb
predict phat
gen pdf_probit = normalden(phat_xb)
gen cdf_probit = normprob(phat_xb)
gen inv_mills = pdf_probit/cdf_probit 
la var inv_mills "Mills-Ratio"

* Running pooled and FE with mills -- HInt is wrong, we don't need to lag the mills
est clear
eststo nomill: reg Y A K L i.year // pooled
eststo mill: reg Y A K L i.year inv_mills // pooled
suest nomill mill
test [nomill_mean]K = [mill_mean]K // not different
test [nomill_mean]A = [mill_mean]A // different

eststo:  xtreg Y A K L i.year, fe // FE 
eststo:  xtreg Y A K L i.year inv_mills, fe // FE 

* Table 
esttab using "$path/out/table_question4b_mills.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 mtitles("Pooled" "Corrected" "Within" "Corrected") /// 
 title("Total and Within Estimators correcting for Selection" \label{tab:q4b2}) ///
 keep(A K L inv_mills) stats(N,fmt("%9.0fc")) ///
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
esttab using "$path/out/table_question5a.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) /// 
 keep(L) stats(N,fmt("%9.0fc")) ///
 title("OP First Stage" \label{tab:q5a}) ///
 booktabs nonotes nomtitles
 
** b) 
gen phi_hat = _b[_cons] + _b[K]*K + _b[A]*A + _b[I]*I + ///
_b[ksq]*ksq + _b[asq]*asq + _b[isq]*isq + _b[K_A]*K_A + _b[K_I]*K_I + _b[A_I]*A_I

** c) 
gen Y_ss = Y - _b[L]*L 
nl (Y_ss = {b0} + {bK}*K + {bA}*A + {by2}*year_2 + {by3}*year_3 + ///
{by4}*year_4 + {by5}*year_5 + {by6}*year_6 + {by7}*year_7 + {by8}*year_8 /// 
+ {by9}*year_9 + {by10}*year_10 + ///
{bh}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K) /// 
+ {bh_sq}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K)^2) if L1.phi_hat != .
estimates store nls 

* Table 
 esttab nls using "$path/out/table_question5c.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) noeqlines eqlabels(none) ///
 title("OP Second Stage" \label{tab:q5c}) nomtitles stats(N,fmt("%9.0fc")) ///
 booktabs nonotes keep(bK:_cons bA:_cons) coeflabel(bK:_cons "Log of Capital" bA:_cons "Age of the firm")
 
** d)
gen phat_sq = phat^2

nl (Y_ss = {b0} + {bK}*K + {bA}*A + {by2}*year_2 + {by3}*year_3 + ///
{by4}*year_4 + {by5}*year_5 + {by6}*year_6 + {by7}*year_7 + {by8}*year_8 + ///
{by9}*year_9 + {by10}*year_10 + {bh}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K) + ///
{bh_sq}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K)^2 + {bphat}*L1.phat + /// 
{bphat_sq}*L1.phat_sq + {b_phat_h}*(L1.phi_hat - {bA}*L1.A - {bK}*L1.K)*L1.phat) if L1.phi_hat != .
estimates store nls_d

* Table 
 esttab nls_d using "$path/out/table_question5d.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) noeqlines eqlabels(none) stats(N,fmt("%9.0fc")) /// 
 title("OP Second Stage correcting for Selection" \label{tab:q5d}) nomtitles ///
 booktabs nonotes keep(bK:_cons bA:_cons) coeflabel(bK:_cons "Log of Capital" bA:_cons "Age of the firm")
 
** e) 
net install st0145_2 // for some reason this stopped working for me out of the blue. 
/// but hopefully it will work for somebody that doesn't have the package installed

gen exit = (X == 0)
opreg Y, exit(exit) state(A K) proxy(I) free(L) cvars(year_*) 
estimates store opreg 

esttab opreg using "$path/out/table_question5e.tex", replace   ///
 b(3) se(3) label star(* 0.10 ** 0.05 *** 0.01) stats(N,fmt("%9.0fc")) /// 
 title("OP Estimation correcting for Endogeneity and Selection" \label{tab:q5e}) ///
 booktabs nonotes keep(Y:A Y:K Y:L)  eqlabels(none)





 












