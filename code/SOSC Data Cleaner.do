// The first time you run, install estout
// ssc install estout, replace

clear all
set more off


* Downloading and saving additional demographic data

import delimited "../Datasets/assignments_556e59b7-a2b0-402b-a29b-4bc811e68329", clear
save "../Datasets/SOSC Connect Cloud Research Demographics", replace
clear

* Importing data from Qualtrics and saving

import delimited "../Datasets/SOSC 13300 Severson and Coleman Extension Survey_May 1, 2024_17.48.csv", varnames(1) clear 
save "../Datasets/Severson and Coleman Main Raw Data", replace

* Removing unnecessary variables and observations

drop startdate enddate status ipaddress progress duration finished recordeddate recipientlastname recipientfirstname recipientemail  locationlatitude locationlongitude distributionchannel userlanguage

drop if responseid == "Response ID" | responseid == `"{"ImportId":"_recordId"}"'
keep if infcons == "I agree to participate in the research"

* Merging Qualtrics and additional demographics

merge 1:1 participantid using "../Datasets/SOSC Connect Cloud Research Demographics"

keep if countryofresidence == "United States"
drop if missing(responseid)

* Removing more unnecessary variables

drop responseid externalreference assignmentid projectid createnewfieldorchoosefromdropdo starttimeamericachicago completiontimeamericachicago _merge completioncode completiontype party_id

* Fixing variable names and changing strings to integers

rename v25 party_idr
rename v26 party_idd
rename v27 party_idi
destring party_idd, replace
destring party_idi, replace
destring party_idr, replace

* Creating a single variable for party identification

gen party_id = .
replace party_id = 3 if party_idr == 6
replace party_id = 2 if party_idr == 5
replace party_id = 1 if party_idi == 4
replace party_id = 0 if party_idi == 3
replace party_id = -1 if party_idi == 2
replace party_id = -2 if party_idd == 1
replace party_id = -3 if party_idd == 0

gen party = 0
replace party = -1 if party_idd == 0 | party_idd == 1 | party_idi == 2
replace party = 0 if party_idi == 3
replace party = 1 if party_idi == 4 | party_idr == 5 | party_idr == 6

* Converting more strings to integers

destring prosociality_1, replace
destring prosociality_2, replace
destring prosociality_3, replace
destring prosociality_4, replace
destring prosociality_5, replace
destring prosociality_6, replace
destring prosociality_7, replace
destring prosociality_8, replace
destring prosociality_9, replace

* Creating the overall prosociality score variable

gen prosociality = 0
gen pro_prosocial = 0
gen pro_individual = 0
gen pro_competitive = 0

* Adding each response for every prosociality question to the proper category

replace pro_prosocial = pro_prosocial+1 if prosociality_1 == 3
replace pro_individual = pro_individual+1 if prosociality_1 == 2
replace pro_competitive = pro_competitive+1 if prosociality_1 == 1

replace pro_prosocial = pro_prosocial+1 if prosociality_2 == 2
replace pro_individual = pro_individual+1 if prosociality_2 == 1
replace pro_competitive = pro_competitive+1 if prosociality_2 == 3

replace pro_prosocial = pro_prosocial+1 if prosociality_3 == 1
replace pro_individual = pro_individual+1 if prosociality_3 == 3
replace pro_competitive = pro_competitive+1 if prosociality_3 == 2

replace pro_prosocial = pro_prosocial+1 if prosociality_4 == 3
replace pro_individual = pro_individual+1 if prosociality_4 == 2
replace pro_competitive = pro_competitive+1 if prosociality_4 == 1

replace pro_prosocial = pro_prosocial+1 if prosociality_5 == 2
replace pro_individual = pro_individual+1 if prosociality_5 == 1
replace pro_competitive = pro_competitive+1 if prosociality_5 == 3

replace pro_prosocial = pro_prosocial+1 if prosociality_6 == 1
replace pro_individual = pro_individual+1 if prosociality_6 == 3
replace pro_competitive = pro_competitive+1 if prosociality_6 == 2

replace pro_prosocial = pro_prosocial+1 if prosociality_7 == 1
replace pro_individual = pro_individual+1 if prosociality_7 == 2
replace pro_competitive = pro_competitive+1 if prosociality_7 == 3

replace pro_prosocial = pro_prosocial+1 if prosociality_8 == 3
replace pro_individual = pro_individual+1 if prosociality_8 == 1
replace pro_competitive = pro_competitive+1 if prosociality_8 == 2

replace pro_prosocial = pro_prosocial+1 if prosociality_9 == 2
replace pro_individual = pro_individual+1 if prosociality_9 == 3
replace pro_competitive = pro_competitive+1 if prosociality_9 == 1

* Analyzing the distribution and assigning on overall result for prosociality

replace prosociality = 1 if pro_prosocial >= 6
replace prosociality = 2 if pro_individual >= 6
replace prosociality = 3 if pro_competitive >=6


* Attention checks
gen attention_check_1pass = 1*(attention_check_1 == "Strongly like")
gen attention_check_2pass = 1*(attention_check_2 == "1,3")

**Quantifying variables

**gender
** 0 = prefer not to say
gen sex_id = 0
replace sex_id = 0 if sex == "Male"
replace sex_id = 1 if sex == "Female"

**education level
** 0 = prefer not to say
gen college = 0
replace college = 1 if education == "Professional degree (for example: MD, DDS, DVM, LLB, JD)"
replace college = 1 if education == "Bachelor's degree (for example: BA, AB, BS)"
replace college = 1 if education == "Master's degree (for example: MA, MS, MEng, MEd, MSW, MBA)"
replace college = 1 if education == "Doctorate degree (for example: PhD, EdD)"

**occupation
** 0 = rather not say
gen occupationNum = 0
replace occupationNum = 1 if occupationfield == "Architecture and Construction"
replace occupationNum = 2 if occupationfield == "Arts"
replace occupationNum = 3 if occupationfield == "Business Management & Administration"
replace occupationNum = 4 if occupationfield == "Education & Training"
replace occupationNum = 5 if occupationfield == "Finance"
replace occupationNum = 6 if occupationfield == "Government & Public Administration"
replace occupationNum = 7 if occupationfield == "Hospitality & Tourism"
replace occupationNum = 8 if occupationfield == "Information Technology"
replace occupationNum = 9 if occupationfield == "Legal"
replace occupationNum = 10 if occupationfield == "Manufacturing"
replace occupationNum = 11 if occupationfield == "Marketing and Sales"
replace occupationNum = 12 if occupationfield == "Medicine"
replace occupationNum = 13 if occupationfield == "Retail"
replace occupationNum = 14 if occupationfield == "Science, Technology, Engineering & Mathematics"
replace occupationNum = 15 if occupationfield == "Social Sciences"
replace occupationNum = 16 if occupationfield == "Retired"
replace occupationNum = 17 if occupationfield == "Other"

**relationship status
** 0 = rather not say
gen relationship = 0
replace relationship = 1 if relationshipmaritalstatus == "In a civil union/partnership"
replace relationship = 1 if relationshipmaritalstatus == "Married"

** household income
** 0 = prefer not to say
gen income_level = 0
replace income_level = 0 if householdincome == "Less than $10,000"
replace income_level = 1 if householdincome == "$10,000-$19,999"
replace income_level = 2 if householdincome == "$20,000-$29,999"
replace income_level = 3 if householdincome == "$30,000-$39,999"
replace income_level = 4 if householdincome == "$40,000-$49,999"
replace income_level = 5 if householdincome == "$50,000-$59,999"
replace income_level = 6 if householdincome == "$60,000-$69,999"
replace income_level = 7 if householdincome == "$70,000-$79,999"
replace income_level = 8 if householdincome == "$80,000-$89,999"
replace income_level = 9 if householdincome == "$90,000-$99,999"
replace income_level = 10 if householdincome == "$100,000-$124,999"
replace income_level = 15 if householdincome == "$150,0000-$174,999"
replace income_level = 18 if householdincome == "$175,000-$199,999"
replace income_level = 20 if householdincome == "$200,000-$224,999"
replace income_level = 23 if householdincome == "$225,000-$249,999"
replace income_level = 25 if householdincome == "$250,000 or more"

** race
** 0 = prefer not to say / no answer
gen race_id = 0
replace race_id = 1 if race == "An ethnicity not listed here" | race == "Other"
replace race_id = 2 if race == "Asian Indian"
replace race_id = 3 if race == "Black or African American"
replace race_id = 4 if race == "Chinese"
replace race_id = 5 if race == "Filipino"
replace race_id = 6 if race == "Vietnamese"
replace race_id = 7 if race == "White"
gen race_white = 1*(race_id == 7)

** employment status
** 0 = prefer not to say
gen employment_status = 0
replace employment_status = 1 if employmentstatus == "Full-time"

rename scientificconfidence scientific_confidence
rename rewardconsequence reward_consequence
rename political_views ideology
keep treatment party_id party employment_status race_id race_white income_level relationship college sex_id age prosociality  gastax carbtax treaty regcarb ideology scientific_confidence reward_consequence religiosity economic_reasoning attention_check_1pass attention_check_2pass gastax_after carbtax_after treaty_after regcarb_after

** rel freq
gen rel_freq = 1
replace rel_freq = church_freq if !missing(church_freq)
replace rel_freq = syn_freq if !missing(syn_freq)
replace rel_freq = mosque_freq if !missing(mosque_freq)

* Other cleaning
destring scientific_confidence, replace
destring reward_consequence, replace
destring religiosity, replace
destring economic_reasoning, replace
destring ideology, replace


* pre- and post-test response
destring gastax, replace
destring carbtax, replace
destring treaty, replace
destring regcarb, replace
destring gastax_after, replace
destring carbtax_after, replace
destring treaty_after, replace
destring regcarb_after, replace
gen post_test = 0
gen pre_test = 0
replace post_test = (gastax_after + carbtax_after + treaty_after + regcarb_after)/4
replace pre_test = (gastax + carbtax + treaty + regcarb)/4


* Replace missing pre-treatment covariates with mean values

* Loop through each variable in the list
* List your variables
local varlist  age party_id employment_status race_white income_level race_white relationship college sex_id  prosociality gastax carbtax treaty regcarb ideology scientific_confidence reward_consequence religiosity rel_freq economic_reasoning
replace rel_freq = 1 if missing(rel_freq)

* Loop through each variable in the list
foreach var of local varlist {
    * Replace other missing values with the mean
    summ `var'
	replace `var' = r(mean) if missing(`var')

}

* create table
summarize age party_id employment_status race_white income_level relationship college sex_id prosociality gastax carbtax treaty regcarb ideology scientific_confidence reward_consequence religiosity rel_freq economic_reasoning attention_check_1pass attention_check_2pass

estpost summarize age party_id employment_status race_white income_level relationship college sex_id prosociality gastax carbtax treaty regcarb ideology scientific_confidence reward_consequence religiosity rel_freq economic_reasoning attention_check_1pass attention_check_2pass

esttab using "../Datasets/summary_stats.csv", cells("count mean sd min max") nomtitle nonumber noobs replace


gen unique_id = _n

gen treatment_0 = 1*(treatment_value == "0")
gen treatment_1 = 1*(treatment_value == "1")
gen treatment_2 = 1*(treatment_value == "2")
gen treatment_3 = 1*(treatment_value == "3")
gen treatment_4 = 1*(treatment_value == "4")
gen treatment_5 = 1*(treatment_value == "5")
drop if treatment_value == "6"

* Saving the new dataset

save "../Datasets/ssi-data-cleaned", replace
outsheet using "../Datasets/ssi-data-cleaned.csv", comma replace
