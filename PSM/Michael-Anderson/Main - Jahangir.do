*cd "~/Desktop/Research/College Sports/Data Archive"

cd "/Users/jalam/Library/CloudStorage/GoogleDrive-jalam@tamu.edu/My Drive/Courses/Spring 2024/ECMT 680/Code/Michael Anderson/Raw Data"

set more off
set matsize 11000

// The following "condition" exists to allow one to easily fold all the data processing and program definition code, contained in brackets, out of view


*** Open college data

use "college data.dta", clear
sort teamname year

*** Keep variables we intend to use

keep teamname year athletics_total alumni_ops_athletics alumni_ops_total ops_athletics_total_grand usnews_academic_rep_new acceptance_rate appdate satdate satmt75 satvr75 satmt25 satvr25 applicants_male applicants_female enrolled_male enrolled_female vse_alum_giving_rate first_time_students_total first_time_outofstate first_time_instate total_giving_pre_2004 alumni_total_giving asian hispanic black control
save temp.dta, replace

use "covers_data.dta", clear

*** Fix errors in data

drop if team==68 & date==14568 & line==.
drop if team==136 & date==14568 & line==.

*** Generate a week variable

sort team season date
gen week = 1 in 1

local obs_counter = 2
local week_counter = 2
local end_counter = _N

while `obs_counter'<=`end_counter' {
	if season[`obs_counter']!=season[`obs_counter'-1] | team[`obs_counter']!=team[`obs_counter'-1] {
		local week_counter = 1
	}
	quietly replace week = `week_counter' in `obs_counter'
	local week_counter = `week_counter' + 1
	local obs_counter = `obs_counter' + 1
}

*** Drop out any games that could potentially be conference championship games and games with no line

keep if week<=12 & month!=12 & month!=1
sort teamname season week
by teamname season: egen total_obs = count(line)
keep if total_obs>=8
replace win = . if line==.

*** Estimate the single game propensity score as a function of the betting line

forvalues i = 2(1)5 {
	gen line`i' = line^`i'
}
logit win line line2 line3 line4 line5
predict pscore
sum pscore, det

// Generate measure of how much the team over/underperforms the spread

gen outperform = realspread+line

// Generate variables to hold outperform measure (and quadratic in line for comparison) for each week of the season
// Treat obs with missing lines as neither under nor overperforming

sort teamname season week
forvalues i=1(1)11 {
	gen outperform_wk`i'_temp = outperform if week==`i'
	by teamname season: egen outperform_wk`i' = mean(outperform_wk`i'_temp)
	replace outperform_wk`i' = 0 if outperform_wk`i'==.
	drop outperform_wk`i'_temp
	gen outperformwk`i'_2 = outperform_wk`i'^2
	gen outperformwk`i'_3 = outperform_wk`i'^3	
}

// Regress line in week i on cubic of outperform in all previous weeks and take residuals to condition out the portion of the line in week i that is due to team's over/underperformance in previous weeks

gen line_clean = line if week==1
forvalues i=2(1)12 {
	local lag = `i' - 1
	reg line outperform_wk1-outperformwk`lag'_3 if week==`i'
	predict line_clean`i' if week==`i', resid
	replace line_clean`i' = line_clean`i' + _b[_cons]
	replace line_clean = line_clean`i' if week==`i'
}

// Estimate propensity score using the "clean" betting line that has been purged of team over/underperformance

forvalues i = 2(1)5 {
	gen line_clean_p`i' = line_clean^`i'
}
logit win line_clean line_clean_p2 line_clean_p3 line_clean_p4 line_clean_p5
predict pscore_clean_line
sum pscore_clean_line, det

// Generate season aggregate measures

sort teamname season week
by teamname season: egen seasonwins = total(win)
by teamname season: egen seasongames = count(win)
by teamname season: egen seasonspread = total(realspread)
by teamname season: egen seasonline = total(line)
by teamname season: egen seasonoutperform = total(outperform)
by teamname season: egen seasonwins_5 = total(win) if week>=5
by teamname season: egen seasongames_5 = count(win) if week>=5
gen pct_win = seasonwins/seasongames
assert pct_win>=0 & pct_win<=1 if pct_win!=.

// Generate expected win measures (clean and naive)

by teamname season: egen exp_wins_naive = total(pscore)
by teamname season: egen exp_wins = total(pscore_clean_line)
gen exp_win_pct = exp_wins/seasongames
forvalues w = 1(1)11 {
	by teamname season: egen exp_wins_wk`w' = total(pscore) if week>`w'
}
gen exp_wins_wk12 = 0
by teamname season: egen exp_wins_wk0 = total(pscore) if week>0

// Generate weekly measures of wins, pscores, etc.

sort teamname season week
forvalues i=1(1)12 {
	gen pscore_wk`i'_temp = pscore if week==`i'
	by teamname season: egen pscore_wk`i' = mean(pscore_wk`i'_temp)
	drop pscore_wk`i'_temp
	gen win_wk`i'_temp = win if week==`i'
	by teamname season: egen win_wk`i' = mean(win_wk`i'_temp)
	drop win_wk`i'_temp
}
forvalues i=1(1)12 {
	gen pscore_wk`i'_temp_clean = pscore_clean_line if week==`i'
	by teamname season: egen pscore_clean_wk`i' = mean(pscore_wk`i'_temp_clean)
	drop pscore_wk`i'_temp_clean
}

collapse (mean) seasonwins-pct_win exp_wins_naive exp_wins exp_win_pct exp_wins_wk1-exp_wins_wk12 exp_wins_wk0 bcs pscore_wk1-win_wk12 pscore_clean_wk1-pscore_clean_wk12, by(teamname season)
rename season year
sort teamname year
merge teamname year using temp.dta
!rm temp.dta
tab _merge
drop if _merge==1 | _merge==2
drop _merge
sort teamname year

*** Variables of interest

*** Generate variables for analysis

sort teamname year
foreach varname of varlist seasonwins-pct_win exp_wins exp_wins_wk1-exp_wins_wk12 exp_wins_wk0 exp_win_pct pscore_wk1-win_wk12 pscore_clean_wk1-pscore_clean_wk12 {
	gen lag_`varname' = `varname'[_n-1] if teamname==teamname[_n-1] & year==year[_n-1]+1
	gen lag2_`varname' = `varname'[_n-2] if teamname==teamname[_n-2] & year==year[_n-1]+1 & year==year[_n-2]+2
	gen lag3_`varname' = `varname'[_n-3] if teamname==teamname[_n-3] & year==year[_n-1]+1 & year==year[_n-2]+2 & year==year[_n-3]+3
	gen lag4_`varname' = `varname'[_n-4] if teamname==teamname[_n-4] & year==year[_n-1]+1 & year==year[_n-2]+2 & year==year[_n-3]+3 & year==year[_n-4]+4
	gen lead_`varname' = `varname'[_n+1] if teamname==teamname[_n+1] & year==year[_n+1]-1
	gen lead2_`varname' = `varname'[_n+2] if teamname==teamname[_n+2] & year==year[_n+1]-1 & year==year[_n+2]-2
}

egen school_id = group(teamname)
xi i.year
xtset school_id

// Deal with special reporting dates for SAT scores and applicants

sort teamname year
foreach varname of varlist satmt25 satmt75 satvr25 satvr75 {
	gen `varname'_temp = `varname'
	replace `varname'_temp = . if satdate==1
	replace `varname'_temp = `varname'[_n+1] if `varname'_temp==. & satdate[_n+1]==1 & teamname[_n+1]==teamname & year[_n+1]==year+1
	drop `varname'
	rename `varname'_temp `varname'
} 

foreach varname of varlist applicants_male applicants_female enrolled_male enrolled_female {
	gen `varname'_temp = `varname'
	replace `varname'_temp = . if appdate==1
	replace `varname'_temp = `varname'[_n+1] if `varname'_temp==. & appdate[_n+1]==1 & teamname[_n+1]==teamname & year[_n+1]==year+1
	drop `varname'
	rename `varname'_temp `varname'
} 

sort teamname year
foreach varname of varlist satmt25 satmt75 satvr25 satvr75 {
	gen `varname'_temp = `varname'
	replace `varname'_temp = `varname'[_n+1] if teamname[_n+1]==teamname & year[_n+1]==year+1
	drop `varname'
	rename `varname'_temp `varname'
} 

foreach varname of varlist applicants_male applicants_female enrolled_male enrolled_female {
	gen `varname'_temp = .
	replace `varname'_temp = `varname'[_n+1] if teamname[_n+1]==teamname & year[_n+1]==year+1
	drop `varname'
	rename `varname'_temp `varname'
} 

gen athletics_share = alumni_ops_athletics/alumni_ops_total if alumni_ops_athletics/alumni_ops_total>.05 & alumni_ops_athletics/alumni_ops_total<.8
gen alum_non_athl_ops = alumni_ops_total - alumni_ops_athletics
gen sat_75 = satmt75 + satvr75
gen sat_25 = satmt25 + satvr25
gen applicants = applicants_male + applicants_female
drop appdate satdate

rename ops_athletics_total_grand ops_athl_grndtotal
rename first_time_students_total firsttime_total
rename first_time_outofstate firsttime_outofstate

sort teamname year
foreach varname of varlist alumni_ops_athletics alum_non_athl_ops alumni_total_giving vse_alum_giving_rate usnews_academic_rep_new applicants acceptance_rate firsttime_outofstate first_time_instate sat_25 sat_75 {
	gen lag_`varname' = `varname'[_n-1] if teamname==teamname[_n-1] & year==year[_n-1]+1
	gen lag2_`varname' = `varname'[_n-2] if teamname==teamname[_n-2] & year==year[_n-2]+2
	gen lag3_`varname' = `varname'[_n-3] if teamname==teamname[_n-3] & year==year[_n-3]+3
	gen lag4_`varname' = `varname'[_n-4] if teamname==teamname[_n-4] & year==year[_n-4]+4
}

// Recode Cincinnati, Louisville, South Florida, and UConn as non-BCS since they only joined in 2002 or 2005. Recode Temple as BCS since it was BCS in 1991.

replace bcs=0 if teamname=="Cincinnati" | teamname=="Louisville" | teamname=="South Florida" | teamname=="Connecticut"
replace bcs=1 if teamname=="Temple"

// Generate variables that code missing values as non-existent games

forvalues w = 1(1)12 {
	gen lag_win_wk_edmiss`w' = lag_win_wk`w' if lag_seasongames!=.
	replace lag_win_wk_edmiss`w' = 1 if (lag_win_wk`w'==. | lag_pscore_wk`w'==.) & lag_seasongames!=.
	gen lag_pscore_wk_edmiss`w' = lag_pscore_wk`w' if lag_seasongames!=.
	replace lag_pscore_wk_edmiss`w' = 1 if (lag_win_wk`w'==. | lag_pscore_wk`w'==.) & lag_seasongames!=.
	gen lead2_win_wk_edmiss`w' = lead2_win_wk`w' if lead2_seasongames!=.
	replace lead2_win_wk_edmiss`w' = 1 if (lead2_win_wk`w'==. | lead2_pscore_wk`w'==.) & lead2_seasongames!=.
	gen lead2_pscore_wk_edmiss`w' = lead2_pscore_wk`w' if lead2_seasongames!=.
	replace lead2_pscore_wk_edmiss`w' = 1 if (lead2_win_wk`w'==. | lead2_pscore_wk`w'==.) & lead2_seasongames!=.
	gen lag2_win_wk_edmiss`w' = lag2_win_wk`w' if lag2_seasongames!=.
	replace lag2_win_wk_edmiss`w' = 1 if (lag2_win_wk`w'==. | lag2_pscore_wk`w'==.) & lag2_seasongames!=.
	gen lag2_pscore_wk_edmiss`w' = lag2_pscore_wk`w' if lag2_seasongames!=.
	replace lag2_pscore_wk_edmiss`w' = 1 if (lag2_win_wk`w'==. | lag2_pscore_wk`w'==.) & lag2_seasongames!=.
}

// Calculate IPW weights for dynamic sequential treatment effects

gen lag_ipw_weight = 1/((lag_win_wk_edmiss1*lag_pscore_wk_edmiss1+(1-lag_win_wk_edmiss1)*(1-lag_pscore_wk_edmiss1)) * (lag_win_wk_edmiss2*lag_pscore_wk_edmiss2+(1-lag_win_wk_edmiss2)*(1-lag_pscore_wk_edmiss2)) * (lag_win_wk_edmiss3*lag_pscore_wk_edmiss3+(1-lag_win_wk_edmiss3)*(1-lag_pscore_wk_edmiss3)) * (lag_win_wk_edmiss4*lag_pscore_wk_edmiss4+(1-lag_win_wk_edmiss4)*(1-lag_pscore_wk_edmiss4)) * (lag_win_wk_edmiss5*lag_pscore_wk_edmiss5+(1-lag_win_wk_edmiss5)*(1-lag_pscore_wk_edmiss5)) * (lag_win_wk_edmiss6*lag_pscore_wk_edmiss6+(1-lag_win_wk_edmiss6)*(1-lag_pscore_wk_edmiss6)) * (lag_win_wk_edmiss7*lag_pscore_wk_edmiss7+(1-lag_win_wk_edmiss7)*(1-lag_pscore_wk_edmiss7)) * (lag_win_wk_edmiss8*lag_pscore_wk_edmiss8+(1-lag_win_wk_edmiss8)*(1-lag_pscore_wk_edmiss8)) * (lag_win_wk_edmiss9*lag_pscore_wk_edmiss9+(1-lag_win_wk_edmiss9)*(1-lag_pscore_wk_edmiss9)) * (lag_win_wk_edmiss10*lag_pscore_wk_edmiss10+(1-lag_win_wk_edmiss10)*(1-lag_pscore_wk_edmiss10)) * (lag_win_wk_edmiss11*lag_pscore_wk_edmiss11+(1-lag_win_wk_edmiss11)*(1-lag_pscore_wk_edmiss11))* (lag_win_wk_edmiss12*lag_pscore_wk_edmiss12+(1-lag_win_wk_edmiss12)*(1-lag_pscore_wk_edmiss12))) if lag_seasongames!=.

gen lead2_ipw_weight = 1/((lead2_win_wk_edmiss1*lead2_pscore_wk_edmiss1+(1-lead2_win_wk_edmiss1)*(1-lead2_pscore_wk_edmiss1)) * (lead2_win_wk_edmiss2*lead2_pscore_wk_edmiss2+(1-lead2_win_wk_edmiss2)*(1-lead2_pscore_wk_edmiss2)) * (lead2_win_wk_edmiss3*lead2_pscore_wk_edmiss3+(1-lead2_win_wk_edmiss3)*(1-lead2_pscore_wk_edmiss3)) * (lead2_win_wk_edmiss4*lead2_pscore_wk_edmiss4+(1-lead2_win_wk_edmiss4)*(1-lead2_pscore_wk_edmiss4)) * (lead2_win_wk_edmiss5*lead2_pscore_wk_edmiss5+(1-lead2_win_wk_edmiss5)*(1-lead2_pscore_wk_edmiss5)) * (lead2_win_wk_edmiss6*lead2_pscore_wk_edmiss6+(1-lead2_win_wk_edmiss6)*(1-lead2_pscore_wk_edmiss6)) * (lead2_win_wk_edmiss7*lead2_pscore_wk_edmiss7+(1-lead2_win_wk_edmiss7)*(1-lead2_pscore_wk_edmiss7)) * (lead2_win_wk_edmiss8*lead2_pscore_wk_edmiss8+(1-lead2_win_wk_edmiss8)*(1-lead2_pscore_wk_edmiss8)) * (lead2_win_wk_edmiss9*lead2_pscore_wk_edmiss9+(1-lead2_win_wk_edmiss9)*(1-lead2_pscore_wk_edmiss9)) * (lead2_win_wk_edmiss10*lead2_pscore_wk_edmiss10+(1-lead2_win_wk_edmiss10)*(1-lead2_pscore_wk_edmiss10)) * (lead2_win_wk_edmiss11*lead2_pscore_wk_edmiss11+(1-lead2_win_wk_edmiss11)*(1-lead2_pscore_wk_edmiss11))* (lead2_win_wk_edmiss12*lead2_pscore_wk_edmiss12+(1-lead2_win_wk_edmiss12)*(1-lead2_pscore_wk_edmiss12))) if lead2_seasongames!=.

gen lag_ipw_weight_5 = 1/((lag_win_wk_edmiss5*lag_pscore_wk_edmiss5+(1-lag_win_wk_edmiss5)*(1-lag_pscore_wk_edmiss5)) * (lag_win_wk_edmiss6*lag_pscore_wk_edmiss6+(1-lag_win_wk_edmiss6)*(1-lag_pscore_wk_edmiss6)) * (lag_win_wk_edmiss7*lag_pscore_wk_edmiss7+(1-lag_win_wk_edmiss7)*(1-lag_pscore_wk_edmiss7)) * (lag_win_wk_edmiss8*lag_pscore_wk_edmiss8+(1-lag_win_wk_edmiss8)*(1-lag_pscore_wk_edmiss8)) * (lag_win_wk_edmiss9*lag_pscore_wk_edmiss9+(1-lag_win_wk_edmiss9)*(1-lag_pscore_wk_edmiss9)) * (lag_win_wk_edmiss10*lag_pscore_wk_edmiss10+(1-lag_win_wk_edmiss10)*(1-lag_pscore_wk_edmiss10)) * (lag_win_wk_edmiss11*lag_pscore_wk_edmiss11+(1-lag_win_wk_edmiss11)*(1-lag_pscore_wk_edmiss11))* (lag_win_wk_edmiss12*lag_pscore_wk_edmiss12+(1-lag_win_wk_edmiss12)*(1-lag_pscore_wk_edmiss12)))

gen lag2_ipw_weight = 1/((lag2_win_wk_edmiss1*lag2_pscore_wk_edmiss1+(1-lag2_win_wk_edmiss1)*(1-lag2_pscore_wk_edmiss1)) * (lag2_win_wk_edmiss2*lag2_pscore_wk_edmiss2+(1-lag2_win_wk_edmiss2)*(1-lag2_pscore_wk_edmiss2)) * (lag2_win_wk_edmiss3*lag2_pscore_wk_edmiss3+(1-lag2_win_wk_edmiss3)*(1-lag2_pscore_wk_edmiss3)) * (lag2_win_wk_edmiss4*lag2_pscore_wk_edmiss4+(1-lag2_win_wk_edmiss4)*(1-lag2_pscore_wk_edmiss4)) * (lag2_win_wk_edmiss5*lag2_pscore_wk_edmiss5+(1-lag2_win_wk_edmiss5)*(1-lag2_pscore_wk_edmiss5)) * (lag2_win_wk_edmiss6*lag2_pscore_wk_edmiss6+(1-lag2_win_wk_edmiss6)*(1-lag2_pscore_wk_edmiss6)) * (lag2_win_wk_edmiss7*lag2_pscore_wk_edmiss7+(1-lag2_win_wk_edmiss7)*(1-lag2_pscore_wk_edmiss7)) * (lag2_win_wk_edmiss8*lag2_pscore_wk_edmiss8+(1-lag2_win_wk_edmiss8)*(1-lag2_pscore_wk_edmiss8)) * (lag2_win_wk_edmiss9*lag2_pscore_wk_edmiss9+(1-lag2_win_wk_edmiss9)*(1-lag2_pscore_wk_edmiss9)) * (lag2_win_wk_edmiss10*lag2_pscore_wk_edmiss10+(1-lag2_win_wk_edmiss10)*(1-lag2_pscore_wk_edmiss10)) * (lag2_win_wk_edmiss11*lag2_pscore_wk_edmiss11+(1-lag2_win_wk_edmiss11)*(1-lag2_pscore_wk_edmiss11))* (lag2_win_wk_edmiss12*lag2_pscore_wk_edmiss12+(1-lag2_win_wk_edmiss12)*(1-lag2_pscore_wk_edmiss12))) if lag2_seasongames!=.


save "College_Athletic_RES.dta", replace


