* Example for basic survival analysis. ; 
* Data used is from Synthea, which develops opensource synthetic patient records. ; 

options compress=no ;
%let spath=	%str(C:\Users\nhadley\Documents\); 
libname synthea	"&spath.synthea" ;

proc datasets nolist kill memtype=data library=work; quit;

* Import dataset ; 
proc import datafile="C:\Users\nhadley\Documents\synthea\patients.csv"
     out=patients
     dbms=csv
     replace;
     getnames=yes;
run;

* Examine initial dataset and variable values ; 
proc contents data = patients ; run; 

proc freq data = patients ; 
table ethnicity ; 
table race ; 
run ; 

* Subset data/format - going to run analyses based on gender and race ; 
* Going to insert a randomized dummy "event" variable between birth and death (or censor) to represent a medical procedure ; 

%macro calc_date(dt1, dt2, event_date);
&event_date. = &dt1 + int((&dt2-&dt1)*ranuni(0));
%mend calc_date;

data pat_analysis ; 
set patients (keep = id ssn birthdate race gender deathdate) ; 

CensorDate = "31DEC2018"d  ; * censor at 12/31/2018 for random event generation;
survival_end_date=min(deathdate, CensorDate);
%calc_date(birthdate, survival_end_date, med_event);  

CensorDate_1Y = med_event + 365 ; * censor at 1 year post-procedure ;
survival_end_date_1Y=min(deathdate, CensorDate_1Y);
survmonths_1Y = (survival_end_date_1Y - med_event)/(365.25/12);
died_1Y=(. lt deathdate le CensorDate_1Y);

CensorDate_3Y = med_event + 1095 ; * censor at 3 years post-procedure ;
survival_end_date_3Y=min(deathdate, CensorDate_3Y);
survmonths_3Y = (survival_end_date_3Y - med_event)/(1095/12);
died_3Y=(. lt deathdate le CensorDate_3Y);

format survival_end_date CensorDate survival_end_date_1Y CensorDate_1Y survival_end_date_3Y CensorDate_3Y med_event YYMMDD10. ; 

run ; 

* Create tables to get overall counts for each by variable, and counts per level of variable ; 

proc freq data = pat_analysis noprint ; 
table race / out = overall_race_table ; 
table gender / out = overall_gender_table ; 
table race*died_1Y / out = race_table ; 
table gender*died_3Y / out = gender_table ; 
run; 

* Run basic survival analysis macro - what is the patient survival rate one year after medical procedure, by race? ; 

%macro km(d,t,maxt,e,s,n) ;

	proc datasets lib=work nolist;
		delete &e. km&e._By_&s. pe_&s.;
	run; quit;

	ods output productlimitestimates=productlimitestimates_&s.;
	proc lifetest data = &d. outsurv=&e. (where=(_censor_ ne 1)) method=pl;
		time &t.*&e.(0) ;
		strata &s. ;
	run ;
	
	data pe_&s.;
		set productlimitestimates_&s.(where=(&t. <= &maxt. and survival ne .));
		by stratum;
		if last.stratum;
	run;

	proc sort data=&e. ; by stratum &t. ; run; 

	data &e._2obs_&t. ; 
	set &e. ; 
	by stratum &t.; 
	if last.stratum;
	run; 

	data SR_&e._&d._&s. ; 
	merge &e._2obs_&t. pe_&s. (keep=stratum failed left); 
	by stratum ;
	surv = 100*survival;   
	ucl = 100*sdf_ucl ; 
	lcl = 100*sdf_lcl ;
	run; 
%mend ;
%km(pat_analysis,survmonths_1Y,12,died_1Y,race,6) ;

* Run basic survival analysis macro again - what is the patient survival rate three years after medical procedure, by gender? ; 

%macro km(d,t,maxt,e,s,n) ;

	proc datasets lib=work nolist;
		delete &e. km&e._By_&s. pe_&s.;
	run; quit;

	ods output productlimitestimates=productlimitestimates_&s.;
	proc lifetest data = &d. outsurv=&e. (where=(_censor_ ne 1)) method=pl;
		time &t.*&e.(0) ;
		strata &s. ;
	run ;
	
	data pe_&s.;
		set productlimitestimates_&s.(where=(&t. <= &maxt. and survival ne .));
		by stratum;
		if last.stratum;
	run;

	proc sort data=&e. ; by stratum &t. ; run; 

	data &e._2obs_&t. ; 
	set &e. ; 
	by stratum &t.; 
	if last.stratum;
	run; 

	data SR_&e._&d._&s. ; 
	merge &e._2obs_&t. pe_&s. (keep=stratum failed left); 
	by stratum ;
	surv = 100*survival;
	ucl = 100*sdf_ucl ; 
	lcl = 100*sdf_lcl ;  
	run; 
%mend ;
%km(pat_analysis,survmonths_3Y,36,died_3Y,gender,2) ;

* Creating and outputting results excel file ; 
%macro Output(level, id) ; 

proc transpose data = &level._table out = &level._table2 ; 
by &level. ; 
id died_&id. ;
var count ; 
run ;  

data ForPrint_&level. ; 
merge sr_died_&id._pat_analysis_&level. (keep = &level. surv lcl ucl) overall_&level._table &level._table2 ; 
by &level. ; 
run; 

options missing = 0 ; 
filename excel dde "Excel|[Surv_Analysis.xlsx]&level.!R4C1:R10C7" ;
	data _null_ ;
		set ForPrint_&level.  ;
	   file excel ;
	   put  &level. count _0 _1 surv lcl ucl ;
	run ;

%mend; 
%Output(Race, 1Y); 
%Output(Gender, 3Y) ; 
