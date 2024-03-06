*** Bill of Materials

File:							Description:
README.txt						This "read me" text file
generate tables.do				Stata code to read in analytic datasets and generate all main and appendix tables
covers_data.dta					Analytic Stata dataset containing data on college football games (righthand side variables)
college data.dta				Analytic Stata dataset containing data on college outcomes (lefthand side variables)
import college data.do			Stata code to generate analytic dataset "college data.dta" from raw VSE, IPEDS, and US News data
import football data.do			Stata code to generate analytic dataset "covers_data.dta" from raw Covers data
Raw Data.tar.gz					Raw IPEDS, US News, VSE, and Covers data and associated processing .do files and crosswalks

*** Data Sources

Covers - http://www.covers.com/
Voluntary Support of Education (VSE) - http://vse.cae.org/
Integrated Postsecondary Education Data System (IPEDS) - http://nces.ed.gov/ipeds/
US News and World Report (US News) - N/A (downloaded through library e-resources)

*** Data Dictionary For Analytic Datasets (relevant variables)

Variable Name:				Source:		Description:

* Dataset: college data.dta (level of observations is the college-by-year)

teamname        			N/A			College name                  
year            			N/A			Year (in which outcomes are measured)
acceptance_rate 			US News     Acceptance rate (percentage points)           
usnews_academic_rep_new 	US News     US News academic reputation score            
alumni_ops_athletics		VSE			Alumni donations to athletic current operations (dollars, FY)
alumni_ops_total			VSE			Alumni donations to current operations, all categories (dollars, FY)
alumni_total_giving			VSE			Total alumni donations (dollars, FY)
athletics_total				VSE         Total donations to athletics (dollars, FY)
ops_athletics_total_grand	VSE			Total donations to athletic current operations (dollars, FY)
total_giving_pre_2004		VSE			Total donations, pre-2004 (dollars, FY)
appdate         			IPEDS		Fall reporting period for IPEDS applicants and admissions (1 = Enrolled Fall lagged year, 2 = Enrolled Fall current year, negative values missing)
applicants_male				IPEDS       First-time first-year degree-seeking applicants - Men (number)
applicants_female			IPEDS		First-time first-year degree-seeking applicants - Women (number)
enrolled_male				IPEDS		First-time first-year degree-seeking men enrolled full-time (number)
enrolled_female				IPEDS		First-time first-year degree-seeking women enrolled full-time (number)
satdate						IPEDS		Fall reporting period for SAT/ACT test scores (1 = Enrolled Fall of previous year, 2 = Enrolled Fall of current year, negative values = missing)
satvr25						IPEDS		SAT I Verbal 25th percentile score (points)
satvr75						IPEDS 		SAT 1 Verbal 75th percentile score (points)
satmt25						IPEDS		SAT 1 Math 25th percentile score (points)
satmt75						IPEDS		SAT 1 Math 75th percentile score (points)
control						IPEDS       University type (1 = Public, 2 = Private)
black						IPEDS		Percent Black, non-Hispanic (percentage points)
asian						IPEDS		Percent Asian/Pacific Islander (points)
hispanic					IPEDS		Percent Hispanic (points)
vse_alum_giving_rate        VSE			VSE alumni giving rate (alumni donors/alumni of record)
first_time_students_total	IPEDS		First-time first-year students 
first_time_outofstate		IPEDS		First-time first-year students from out-of-state
first_time_instate 			IPEDS		First-time first-year students from in-state

* Dataset: covers_data.dta (level of observation is the game)

teamname	    			N/A			College name            
bcs      					N/A			BCS conference team (1 = yes, 0 = no)
team            			N/A			Numerical team identifier               
opponent					Covers		Team's opponent (numerical identifier or team name)              
line						Covers		Betting line (points)            
overunder					Covers		Over-under on total points scored (points)              
ownscore					Covers		Actual points scored by team (points)            
opponentscore				Covers		Actual points scored by team's opponent (points)
vseteamname					VSE			VSE college name (for merge purposes)             
date						Covers		Date of game (day month year)			                  
day							Covers		Day of game
month						Covers		Month of game          
year						Covers		Year of game
season						Covers		Football of season of game (season==year unless game occurs in January)              
realspread					N/A			Actual difference in scores between team and its opponent (points)
win							N/A			Indicator for team winning (1 = won, 0 = did not win)
tie							N/A			Indicator for team tying (1 = tied, 0 = did not tie)

*** Software version on which code was tested:

Stata/MP 13.1 running on Mac OS X El Capitan 10.11.2

*** Instructions to reproduce tables:

1. Update Line 1 in "generate tables.do" to cd to the local directory containing "covers_data.dta" and "college data.dta".
2. Execute code from Line 1 to "TABLE CODE" comment (approximately Line 1718). This part of the code processes the data and defines the programs that get called to generate most of the individual table results.
3. Execute code for each table separately. Results will appear in Stata Browser window.
4. Please see the special note in the code for reproducing Table 7. Reproducing all of Table A1 requires running the code when setting the "unexpected wins" to 0 and when setting it to 1.

*** Instructions to reproduce analytic datasets ("covers_data.dta" and "college data.dta") from raw data:

1. Unpack "Raw Data.tar.gz" archive.
2. Update Lines 1 in "import college data.do" and "import football data.do" to cd to the local directory containing "Raw College Data" and "Raw Football Data".
3. NOTE: It is *critical* to maintain the original directory and subdirectory structure in "Raw College Data" and "Raw Football Data". Changes to this structure will likely cause the code listed below to fail to properly execute.
4. Execute "import college data.do".
5. Execute "import football data.do".
