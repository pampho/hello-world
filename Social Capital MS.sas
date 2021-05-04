/**TITLE: SOCIAL CAPITAL MS
/**AUTHOR: PAM PHOJANAKONG (PP)
/**PURPOSE: Analysis of BWHN Phase II data for publication (2019) and APHA 2019 presentation
/**DATE CREATED: 4-JUN-2018 (PP)
/**LAST UPDATE: 19-JAN-2020 (PP)
/**MAJOR EDITS: 26-OCT-18: reversed depression slope calculation to show 'improvement' in depression symptoms per meeting with MMC
				28-NOV-18: reduced models to look at baseline, 3-month, and 12-month paths only
			    28-JAN-19: changed models from scores to slopes in scores per meeting with MMC and SLW
				

/**NOTES: This file program comtains ALL analyses related to social capital, including prelimiary examinations of both bridging and bonding social capital scores, correlations with outcomes
	of interest, preliminary mixed models, and all iterations of path models for publication. Data processing and analyses for the submitted publication are included with active code. All other 
	analyses are archived (see below). 
		**Social Capital: Per Williams' (2006) scale, Bonding and Bridging scores were considered separately.
		**OUTCOMES/PATH MODELS: effect of attending 4+ classes (eff_dose) on depression, employment (yes/no), TANF receipt (yes/no)
	**LINES 22-394: data processing, variable parameterizations
	**LINES 396-421: preliminary/crude logistic regressions
	**LINES 422-701: path analysis models used for publication (hypothesized paths and re-parameterized paths: lines 427-682), correlation checkswith attendance (lines 685-701)
	**LINES 707-2581: Archived analyses
**/

/*************************************************DATA PROCESSING & FORMATS********************************************************/

libname bwhnp2 '\\drexel.edu\encrypted\CHFC\BWHN\Research\Data Analysis\Analyses\PhaseII\Projects\bwhnp2' ;
/*need to macro these data and sorting steps*/
data baseline (drop=socsup);
	set bwhnp2.baseline_JUN17 ;
	survey=1 ;
run ;
data M3 (drop=socsup);
	set bwhnp2.M3_oct17 ;
	survey=2 ;
run ;
data M6 (drop=socsup);
	set bwhnp2.M6_DEC17 ;
	survey=3 ;
run ;
data M9 (drop=socsup);
	set bwhnp2.M9_MAR18 ;
	survey=4 ;
run ;
data M12 (drop=socsup);
	set bwhnp2.M12_JUN18 ;
	survey=5 ;
run ;
proc sort data=baseline ;
	by subject ;
run ;
proc sort data=M3 ;
	by subject ;
run ;
proc sort data=M6 ;
	by subject ;
run ;
proc sort data=M9 ;
	by subject ;
run ;
proc sort data=M12 ;
	by subject ;
run ;
data prepost ;
	set baseline M3 M6 M9 M12 ;
	by subject ;
	if cohort=9 then TANF=0 ; /*messed up in other codes*/
/*WIC, SSI, and currently receiving TANF (need to double check definitions)--- check with ACASI*/
	/*WIC*/
	if secu_1 ge 7 then WIC=. ;
	else if secu_1=1 then WIC=1 ;
	else if secu_1=2 then WIC=0 ;
	else if secu_1=3 then WIC=0 ;
	label WIC="Receive WIC" ;
	if secu_2a=1 then WIC_cut=0 ;
	else if secu_2a=0 then WIC_cut=1 ;
	else WIC_cut=. ;
	/*SNAP*/
	if secu_3 ge 7 then SNAP=. ;
	else if secu_3=2 then SNAP=0 ;
	else if secu_3=1 then SNAP=1 ;
	else SNAP=1 ;
	/*TANF (cash benefits)*/
	if secu_8 ge 7 then cash_cat=. ;
	else if secu_8=2 then cash_cat=1 ;
	else if secu_8=5 then cash_cat=1 ;
	else cash_cat=0 ;
	label cash_cat="Receive TANF" ;
/*fix missings globally for banking accounts and employment and benefits*/
	if secl_20 gt 1 then checking=9 ;
	else checking=secl_20 ;
	if secl_22 gt 1 then savings=9 ;
	else savings=secl_22 ;
	if secl_21 gt 50000 then checkbal= . ;
	else checkbal=secl_21 ;
	if secl_23 gt 50000 then savbal= . ;
	else savbal=secl_23 ;
	if secj_2 lt 97 then secj_2=secj_2 ;
	else if secj_2 ge 97 then secj_2=. ;
	if secj_4 lt 97 then secj_4=secj_4 ;
	else if secj_4 ge 97 then secj_4=. ;
	if secu_6 lt 99997 then secu_6=secu_6 ;
	else if secu_6 ge 99997 then secu_6=. ;
/*fix yes/no with accounts*/
	if secl_20 gt 1 then secl_20=. ;
	if secl_22 gt 1 then secl_22=. ;
/*employment = 20+ hrs/week*/
	if 20 le secj_2 lt 98 then employ_20=1 ;
	else if secj_2 lt 20 then employ_20=0 ;
	else employ_20=. ;
/*dichotomize health and food variables*/
	if phealth=1 then phealthcat=1 ;
	else if phealth gt 1 then phealthcat=0 ;
	else phealthcat=. ;
	if chealth=1 then chealthcat=1 ;
	else if chealth gt 1 then chealthcat=0 ;
	else chealthcat=. ;
	if fdsecure=1 then foodsec_cat=1 ;
	else if fdsecure gt 1 then foodsec_cat=0 ;
	else foodsec_cat=. ;
run ;
/*ANALYSES - COMPLETE SETS only */
data prepost_c11 ;
	set prepost ;
	if cohort gt 11 then delete ;
run ;
proc freq data=prepost_c11 ;
	table survey/chisq ;
	title 'headers' ;
	format survey survey. ;
run ; 
data socialcap_c11 ;
	set prepost_c11 ;
	if secd_1 gt 5 then secd_1m=. ;
	else secd_1m=secd_1 ;
	if secd_2 gt 5 then secd_2m=. ;
	else secd_2m=secd_2 ;
	if secd_3 gt 5 then secd_3m=. ;
	else secd_3m=secd_3 ;
	if secd_4 gt 5 then secd_4m=. ;
	else secd_4m=secd_4 ;
	if secd_5 gt 5 then secd_5m=. ;
	else secd_5m=secd_5 ;
	if secd_6 gt 5 then secd_6m=. ;
	else secd_6m=secd_6 ;
	if secd_7 gt 5 then secd_7m=. ;
	else secd_7m=secd_7 ;
	if secd_8 gt 5 then secd_8m=. ;
	else secd_8m=secd_8 ;
	if secd_9 gt 5 then secd_9m=. ;
	else secd_9m=secd_9 ;
	if secd_10 gt 5 then secd_10m=. ;
	if secd_11 gt 5 then secd_11m=. ;
	else secd_11m=secd_11 ;
	if secd_12 gt 5 then secd_12m=. ;
	else secd_12m=secd_12 ;
	if secd_13 gt 5 then secd_13m=. ;
	else secd_13m=secd_13 ;
	if secd_14 gt 5 then secd_14m=. ;
	else secd_14m=secd_14 ;	
	if secd_15 gt 5 then secd_15m=. ;
	else secd_15m=secd_15 ;
	if secd_16 gt 5 then secd_16m=. ;
	else secd_16m=secd_16 ;
	if secd_17 gt 5 then secd_17m=. ;
	else secd_17m=secd_17 ;
	if secd_18 gt 5 then secd_18m=. ;
	else secd_18m=secd_18 ;
	if secd_19 gt 5 then secd_19m=. ;
	else secd_19m=secd_19 ;
	if secd_20 gt 5 then secd_20m=. ;
	else secd_20m=secd_20 ;
	bonding=sum(of secd_1m--secd_10m) ;
	bridging=sum(of secd_11m--secd_20m) ;
run ;	
/*merge with attendence*/
proc sort data=socialcap_c11 ;
	by subject ;
run ;
proc sort data=bwhnp2.attendance_clean ;
	by subject ;
run ;
proc sort data=socialcap_c11 ;
	by subject ;
run ;
data socialcap_c11_attend ;
	merge bwhnp2.attendance_clean socialcap_c11 ;
	by subject ;
	if total_attend=0 then ever_attend=0 ;
	if total_attend gt 0 then ever_attend=1 ;
	if run_attend ge 3 then eff_dose=1 ;
	else if run_attend lt 3 then eff_dose=0 ;
	else eff_dose=. ;
	if fdsecure gt 1 then foodinsecure=1 ; /*1-JUL-18: add outcomes*/
	else if fdsecure=1 then foodinsecure=0; 
	if secj_2 lt 20 then employ20=0 ;
	else if 20 le secj_2 lt 97 then employ20=1 ;
	else employ20=. ;
run ;

/*PATH ANALYSIS DATA SETUP*/
/*rename variables for timepoints*/
data demos ;
	set baseline ;
	keep subject gender race phealth seca_1 seca_9 seca_7 seca_11 aces ;
run ;
data socialcap_path (keep=subject survey aces eff_dose ever_attend bonding bridging dpsscore dps secj_1 foodsec_cat phealthcat chealthcat cash_cat);
	set socialcap_c11_attend ;
run ;
proc sql ;
	create table bond0 as
	select subject, survey, bonding as bonding0
	from socialcap_path 
	where survey=1 ;
	create table bond3 as
	select subject, survey, bonding as bonding3
	from socialcap_path 
	where survey=2 ;
	create table bond6 as
	select subject, survey, bonding as bonding6
	from socialcap_path 
	where survey=3 ;
	create table bond9 as
	select subject, survey, bonding as bonding9
	from socialcap_path 
	where survey=4 ;
	create table bond12 as
	select subject, survey, bonding as bonding12
	from socialcap_path 
	where survey=5 ;
	create table bridg0 as
	select subject, survey, bridging as bridging0
	from socialcap_path 
	where survey=1 ;
	create table bridg3 as
	select subject, survey, bridging as bridging3
	from socialcap_path 
	where survey=2 ;
	create table bridg6 as
	select subject, survey, bridging as bridging6
	from socialcap_path 
	where survey=3 ;
	create table bridg9 as
	select subject, survey, bridging as bridging9
	from socialcap_path 
	where survey=4 ;
	create table bridg12 as
	select subject, survey, bridging as bridging12
	from socialcap_path 
	where survey=5 ;
	create table dps0 as
	select subject, survey, dpsscore as dpsscore0, dps as dps0
	from socialcap_path 
	where survey=1 ;
	create table dps3 as
	select subject, survey, dpsscore as dpsscore3, dps as dps3
	from socialcap_path 
	where survey=2 ;
	create table dps6 as
	select subject, survey, dpsscore as dpsscore6, dps as dps6
	from socialcap_path 
	where survey=3 ;
	create table dps9 as
	select subject, survey, dpsscore as dpsscore9, dps as dps9
	from socialcap_path 
	where survey=4 ;
	create table dps12 as
	select subject, survey, dpsscore as dpsscore12, dps as dps12
	from socialcap_path 
	where survey=5 ;
	create table employ0 as
	select subject, survey, secj_1 as employ0
	from socialcap_path 
	where survey=1 ;
	create table employ3 as
	select subject, survey, secj_1 as employ3
	from socialcap_path 
	where survey=2 ;
	create table employ6 as
	select subject, survey, secj_1 as employ6
	from socialcap_path 
	where survey=3 ;
	create table employ9 as
	select subject, survey, secj_1 as employ9
	from socialcap_path 
	where survey=4 ;
	create table employ12 as
	select subject, survey, secj_1 as employ12
	from socialcap_path 
	where survey=5 ;
	create table foodsec0 as
	select subject, survey, foodsec_cat as foodsec0
	from socialcap_path 
	where survey=1 ;
	create table foodsec3 as
	select subject, survey, foodsec_cat as foodsec3
	from socialcap_path 
	where survey=2 ;
	create table foodsec6 as
	select subject, survey, foodsec_cat as foodsec6
	from socialcap_path 
	where survey=3 ;
	create table foodsec9 as
	select subject, survey, foodsec_cat as foodsec9
	from socialcap_path 
	where survey=4 ;
	create table foodsec12 as
	select subject, survey, foodsec_cat as foodsec12
	from socialcap_path 
	where survey=5 ;
	create table phealthcat0 as
	select subject, survey, phealthcat as phealthcat0
	from socialcap_path 
	where survey=1 ;
	create table phealthcat3 as
	select subject, survey, phealthcat as phealthcat3
	from socialcap_path 
	where survey=2 ;
	create table phealthcat6 as
	select subject, survey, phealthcat as phealthcat6
	from socialcap_path 
	where survey=3 ;
	create table phealthcat9 as
	select subject, survey, phealthcat as phealthcat9
	from socialcap_path 
	where survey=4 ;
	create table phealthcat12 as
	select subject, survey, phealthcat as phealthcat12
	from socialcap_path 
	where survey=5 ;
	create table chealthcat0 as
	select subject, survey, chealthcat as chealthcat0
	from socialcap_path 
	where survey=1 ;
	create table chealthcat3 as
	select subject, survey, chealthcat as chealthcat3
	from socialcap_path 
	where survey=2 ;
	create table chealthcat6 as
	select subject, survey, chealthcat as chealthcat6
	from socialcap_path 
	where survey=3 ;
	create table chealthcat9 as
	select subject, survey, chealthcat as chealthcat9
	from socialcap_path 
	where survey=4 ;
	create table chealthcat12 as
	select subject, survey, chealthcat as chealthcat12
	from socialcap_path 
	where survey=5 ;
	create table cash_cat0 as
	select subject, survey, cash_cat as cash_cat0
	from socialcap_path 
	where survey=1 ;
	create table cash_cat3 as
	select subject, survey, cash_cat as cash_cat3
	from socialcap_path 
	where survey=2 ;
	create table cash_cat6 as
	select subject, survey, cash_cat as cash_cat6
	from socialcap_path 
	where survey=3 ;
	create table cash_cat9 as
	select subject, survey, cash_cat as cash_cat9
	from socialcap_path 
	where survey=4 ;
	create table cash_cat12 as
	select subject, survey, cash_cat as cash_cat12
	from socialcap_path 
	where survey=5 ;
run ;
data classes (keep=subject eff_dose);
	set socialcap_path ;
run ;
data path (drop=survey) ;
	merge demos classes bond0 bond3 bond6 bond9 bond12 bridg0 bridg3 bridg6 bridg9 bridg12 dps0 dps3 dps6 dps9 dps12 employ0 employ3 employ6 employ9 employ12
		  foodsec0 foodsec3 foodsec6 foodsec9 foodsec12 phealthcat0 phealthcat3 phealthcat6 phealthcat9 phealthcat12 chealthcat0 chealthcat3 
		  chealthcat6 chealthcat9 chealthcat12 cash_cat0 cash_cat3 cash_cat6 cash_cat9 cash_cat12 ;
	by subject ;
	if subject=. then delete ;
	if subject ge 1200 then delete ;
	if ever_atttend=0 then delete ; /*drop 0 classes from analysis (28-JAN-19)*/
/*change dpsscore to mental health improvement # per meeting with MMC 26-Oct-18*/
	deltadps3=dpsscore3-dpsscore0 ;
	deltadps6=dpsscore6-dpsscore0 ;
	deltadps9=dpsscore9-dpsscore0 ;
	deltadps12=dpsscore12-dpsscore0 ;
/*add slopes for bridging and bonding per 28-JAN-19 meeting with MMC and SW*/
	deltabond3=bonding3-bonding0 ;
	deltabond6=bonding6-bonding0 ;
	deltabond9=bonding9-bonding0 ;
	deltabond12=bonding12-bonding0 ;
	deltabridge3=bridging3-bridging0 ;
	deltabridge6=bridging6-bridging0 ;
	deltabridge9=bridging9-bridging0 ;
	deltabridge12=bridging12-bridging0 ;
run ;

/****************************************************EXPLORATORY CRUDE ANALYSIS: Logistic regression (Attend 4+ sessions ---> depression , employment, TANF outcomes)**************************/
proc logistic data=path descending ;
	model dps12=eff_dose aces eff_dose|aces dps0 ; 
	oddsratio 'effect of attendance across no ACEs' eff_dose /at(aces=(1));
	oddsratio 'effect of attendance across 1-3 ACEs' eff_dose /at(aces=(2));
	oddsratio 'effect of attendance across 4+ ACEs' eff_dose /at(aces=(3));
	oddsratio 'effect of attendance' eff_dose/at (aces=(1.5)) ;
	oddsratio 'effect of attendance' eff_dose ;
	title 'depression crude analysis' ;
run ;
proc logistic data=path (where=(employ12 ne 8)) descending ;
	model employ12=eff_dose aces eff_dose|aces employ0 ; 
	oddsratio 'effect of attendance across no ACEs' eff_dose /at(aces=(1));
	oddsratio 'effect of attendance across 1-3 ACEs' eff_dose /at(aces=(2));
	oddsratio 'effect of attendance across 4+ ACEs' eff_dose /at(aces=(3));
	oddsratio 'effect of attendance' eff_dose ;
	title 'employment crude analysis' ;
run ;
proc logistic data=path descending ;
	model cash_cat12=eff_dose aces eff_dose|aces cash_cat0 ; 
	oddsratio 'effect of attendance across no ACEs' eff_dose /at(aces=(1));
	oddsratio 'effect of attendance across 1-3 ACEs' eff_dose /at(aces=(2));
	oddsratio 'effect of attendance across 4+ ACEs' eff_dose /at(aces=(3));
	title 'TANF crude analysis' ;
run ;

/************************************************************************************PATH ANALYSIS*******************************************************************************/
/*28-JAN-19: subset eff_dose --> social capital-->depression*/
/*********MULTIPLE IMPUTATIONS DON'T CONVERGE, USE FIML: With structural equation modeling, it's often easier to use Full Information Maximum Likelihood (FIML) as the estimator instead of doing MI (reference Paul Allison). */
ods graphics on ;
ods listing sge=on ;
proc calis data=path method=fiml ;
	path deltabond3 -> deltabond12,
 		 bonding0 -> deltabond3,
 		 eff_dose -> deltabond3,
 		 deltabond12 -> deltadps12,
 		 deltadps3 -> deltadps12,
		 deltadps3 -> deltabond12,
 		 deltabond3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> deltabond12,
 		 eff_dose -> deltadps12,
		 bonding0 <-> dpsscore0 ;
	effpart deltabond3 <- eff_dose bonding0 dpsscore0,
			deltadps3 <- eff_dose deltabond3 dpsscore0,
			deltabond12 <- eff_dose bonding0 deltabond3 deltadps3,
			deltadps12 <- eff_dose deltabond12 deltabond3 deltadps3 ;	
	title 'FIML: bonding and depression deltas' ;
run ;

proc calis data=path method=fiml ;
	path deltabond3 -> deltabond12,
 		 bonding0 -> deltabond3,
 		 eff_dose -> deltabond3,
 		 deltabond12 -> deltadps12,
 		 deltadps3 -> deltadps12,
 		 deltabond3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
		 bonding0 <-> dpsscore0 ;
	effpart deltabond3 <- eff_dose bonding0 dpsscore0,
			deltadps3 <- eff_dose deltabond3 dpsscore0,
			deltabond12 <- eff_dose bonding0 deltabond3 deltadps3,
			deltadps12 <- eff_dose deltabond12 deltabond3 deltadps3 ;	
	title 'FIML: bonding and depression deltas - REFIT' ;
run ;
proc calis data=path method=fiml ;
	path deltabridge3 -> deltabridge12,
 		 bridging0 -> deltabridge3,
 		 eff_dose -> deltabridge3,
 		 deltabridge12 -> deltadps12,
 		 deltadps3 -> deltadps12,
		 deltadps3 -> deltabridge12,
 		 deltabridge3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> deltabridge12,
 		 eff_dose -> deltadps12,
		 bridging0 <-> dpsscore0 ;
	effpart deltabridge3 <- eff_dose bridging0 dpsscore0,
			deltadps3 <- eff_dose deltabridge3 dpsscore0,
			deltabridge12 <- eff_dose bridging0 deltabridge3 deltadps3,
			deltadps12 <- eff_dose deltabridge12 deltabridge3 deltadps3 ;		
	title 'FIML bridging and depression deltas' ;
run ;
proc calis data=path method=fiml ;
	path deltabridge3 -> deltabridge12,
 		 bridging0 -> deltabridge3,
 		 eff_dose -> deltabridge3,
 		 deltadps3 -> deltadps12,
 		 deltabridge3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltabridge12,
		 bridging0 <-> dpsscore0 ;
	effpart deltabridge3 <- eff_dose bridging0 dpsscore0,
			deltadps3 <- eff_dose deltabridge3 dpsscore0,
			deltabridge12 <- eff_dose bridging0 deltabridge3 deltadps3,
			deltadps12 <- eff_dose deltabridge12 deltabridge3 deltadps3 ;		
	title 'FIML bridging and depression deltas - REFIT' ;
run ;

/*28-JAN-19: focus on employment and TANF*/
proc calis data=path method=FIML ;
	path deltabond12 -> employ12,
 		 deltadps12 -> employ12,
 		 employ3 -> employ12,
 		 deltabond3 -> employ3,
 		 deltadps3 -> employ3,
 		 employ0 -> employ3,
 		 eff_dose -> employ3,
 		 deltabond3 -> deltabond12,
 		 bonding0 -> deltabond3,
 		 eff_dose -> deltabond3,
 		 deltabond12 -> deltadps12,
 		 deltadps3 -> deltadps12,
		 deltadps3 -> deltabond12,
 		 deltabond3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> deltabond12,
 		 eff_dose -> employ12,
 		 eff_dose -> deltadps12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> employ0 ;
	effpart employ3 <- eff_dose deltabond3 deltadps3 employ0,
			employ12 <- eff_dose deltabond3 deltadps3 deltabond12 deltadps12 employ0 employ3 ;
	title 'FIML bonding and employment (deltas)' ;
run ;
proc calis data=path method=FIML ;
	path employ3 -> employ12,
 		 deltabond3 -> employ3,
 		 employ0 -> employ3,
 		 eff_dose -> employ3,
 		 deltabond3 -> deltabond12,
 		 bonding0 -> deltabond3,
 		 eff_dose -> deltabond3,
 		 deltabond12 -> deltadps12,
 		 deltadps3 -> deltadps12,
 		 deltabond3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> employ12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> employ0 ;
	effpart employ3 <- eff_dose deltabond3 deltadps3 employ0,
			employ12 <- eff_dose deltabond3 deltadps3 deltabond12 deltadps12 employ0 employ3 ;
	title 'FIML bonding and employment (deltas) - REFIT' ;
run ;
proc calis data=path method=fiml ;
	path deltabridge12 -> employ12,
 		 deltadps12 -> employ12,
 		 employ3 -> employ12,
 		 deltabridge3 -> employ3,
 		 deltadps3 -> employ3,
 		 employ0 -> employ3,
 		 eff_dose -> employ3,
 		 deltabridge3 -> deltabridge12,
 		 bridging0 -> deltabridge3,
 		 eff_dose -> deltabridge3,
 		 deltabridge12 -> deltadps12,
 		 deltadps3 -> deltadps12,
		 deltadps3 -> deltabridge12,
 		 deltabrideg3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> deltabridge12,
 		 eff_dose -> employ12,
 		 eff_dose -> deltadps12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> employ0,
		 bridging0 <-> employ0 ;
	effpart employ3 <- eff_dose deltabridge3 deltadps3 employ0,
			employ12 <- eff_dose deltabridge3 deltadps3 deltabridge12 deltadps12 employ0 employ3 ;
	title 'FIML: bridging and employment (deltas)' ;
run ;
proc calis data=path method=fiml ;
	path employ3 -> employ12,
 		 deltabridge3 -> employ3,
 		 employ0 -> employ3,
 		 eff_dose -> employ3,
 		 deltabridge3 -> deltabridge12,
 		 bridging0 -> deltabridge3,
 		 eff_dose -> deltabridge3,
 		 deltadps3 -> deltadps12,
 		 deltabrideg3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> deltabridge12,
 		 eff_dose -> employ12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> employ0,
		 bridging0 <-> employ0 ;
	effpart employ3 <- eff_dose deltabridge3 deltadps3 employ0,
			employ12 <- eff_dose deltabridge3 deltadps3 deltabridge12 deltadps12 employ0 employ3 ;
	title 'FIML: bridging and employment (deltas) - REFIT' ;
run ;
/*TANF*/
proc calis data=path method=fiml ;
	path deltabond12 -> cash_cat12,
 		 deltadps12 -> cash_cat12,
 		 cash_cat3 -> cash_cat12,
 		 deltabond3 -> cash_cat3,
 		 deltadps3 -> cash_cat3,
 		 cash_cat0 -> cash_cat3,
 		 eff_dose -> cash_cat3,
 		 deltabond3 -> deltabond12,
 		 bonding0 -> deltabond3,
 		 eff_dose -> deltabond3,
 		 deltabond12 -> deltadps12,
 		 deltadps3 -> deltadps12,
		 deltadps3 -> deltabond12,
 		 deltabond3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> deltabond12,
 		 eff_dose -> cash_cat12,
 		 eff_dose -> deltadps12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> cash_cat0,
		 bonding0 <-> cash_cat0 ;
	effpart cash_cat3 <- eff_dose deltabond3 deltadps3 cash_cat0,
			cash_cat12 <- eff_dose deltabond3 deltadps3 deltabond12 deltadps12 cash_cat0 cash_cat3 ;
	title 'FIML bonding and TANF (deltas)' ;
run ;
proc calis data=path method=fiml ;
	path deltabond12 -> cash_cat12,
 		 cash_cat3 -> cash_cat12,
 		 deltabond3 -> cash_cat3,
 		 cash_cat0 -> cash_cat3,
 		 deltabond3 -> deltabond12,
 		 bonding0 -> deltabond3,
 		 eff_dose -> deltabond3,
 		 deltabond12 -> deltadps12,
 		 deltadps3 -> deltadps12,
 		 deltabond3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> cash_cat0,
		 bonding0 <-> cash_cat0 ;
	effpart cash_cat3 <- eff_dose deltabond3 deltadps3 cash_cat0,
			cash_cat12 <- eff_dose deltabond3 deltadps3 deltabond12 deltadps12 cash_cat0 cash_cat3 ;
	title 'FIML bonding and TANF (deltas) - REFIT' ;
run ;
proc calis data=path method=fiml ;
	path deltabridge12 -> cash_cat12,
 		 deltadps12 -> cash_cat12,
 		 cash_cat3 -> cash_cat12,
 		 deltabridge3 -> cash_cat3,
 		 deltadps3 -> cash_cat3,
 		 cash_cat0 -> cash_cat3,
 		 eff_dose -> cash_cat3,
 		 deltabridge3 -> deltabridge12,
 		 bridging0 -> deltabridge3,
 		 eff_dose -> deltabridge3,
 		 deltabridge12 -> deltadps12,
 		 deltadps3 -> deltadps12,
		 deltadps3 -> deltabridge12,
 		 deltabridge3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> deltabridge12,
 		 eff_dose -> cash_cat12,
 		 eff_dose -> deltadps12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> cash_cat0,
		 bridging0 <-> cash_cat0 ;
	effpart cash_cat3 <- eff_dose deltabridge3 deltadps3 cash_cat0,
			cash_cat12 <- eff_dose deltabridge3 deltadps3 deltabridge12 deltadps12 cash_cat0 cash_cat3 ;
	title 'FIML: bridging and TANF (deltas)' ;
run ;
proc calis data=path method=fiml ;
	path deltadps12 -> cash_cat12,
 		 cash_cat3 -> cash_cat12,
 		 deltabridge3 -> cash_cat3,
 		 cash_cat0 -> cash_cat3,
 		 deltabridge3 -> deltabridge12,
 		 bridging0 -> deltabridge3,
 		 eff_dose -> deltabridge3,
 		 deltadps3 -> deltadps12,
 		 deltabridge3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltabridge12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> cash_cat0,
		 bridging0 <-> cash_cat0 ;
	effpart cash_cat3 <- eff_dose deltabridge3 deltadps3 cash_cat0,
			cash_cat12 <- eff_dose deltabridge3 deltadps3 deltabridge12 deltadps12 cash_cat0 cash_cat3 ;
	title 'FIML: bridging and TANF (deltas) - REFIT' ;
run ;

/*check correlation between social capital scores and depression symptoms in different attendance groups*/
data path_0 ;
	set path ;
	where eff_dose=0 ;
run ;
data path_1 ;
	set path ;
	where eff_dose=1 ;
run ;
proc corr data=path_0 spearman ;
	var dpsscore3 bonding3 bridging3 bonding12 bridging12 dpsscore12;
	title 'correlation b/w social capital and dps scores-non attenders' ;
run ;
proc corr data=path_1 spearman ;
	var dpsscore3 bonding3 bridging3 bonding12 bridging12 dpsscore12;
	title 'correlation b/w social capital and dps scores-attenders' ;
run ;


/**********************************************************END OF ACTIVE CODE**********************************************************************/


/********************************************************ARCHIVED PATHS NOT PUBLISHED (Food insecurity, parent health, child health)***************************************************************************************/
proc calis data=path omethod=none ;
	path bonding12 -> foodsec12,
 		 dpsscore12 -> foodsec12,
 		 foodsec3 -> foodsec12,
 		 bonding3 -> foodsec3,
 		 dpsscore3 -> foodsec3,
 		 foodsec0 -> foodsec3,
 		 eff_dose -> foodsec3,
 		 bonding3 -> bonding12,
 		 bonding0 -> bonding3,
 		 eff_dose -> bonding3,
 		 bonding12 -> dpsscore12,
 		 dpsscore3 -> dpsscore12,
		 dpsscore3 -> bonding12,
 		 bonding3 -> dpsscore3,
 		 dpsscore0 -> dps3,
 		 eff_dose -> dps3,
 		 eff_dose -> bonding12,
 		 eff_dose -> foodsec12,
 		 eff_dose -> dps12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> foodsec0,
		 bonding0 <-> foodsec0 ;
	effpart foodsec3 <- eff_dose deltabond3 deltadps3 foodsec0,
			foodsec12 <- eff_dose deltabond3 deltadps3 deltabond12 deltadps12 foodsec0 foodsec3 ;
	title 'bonding and foodsec' ;
run ;
proc calis data=path omethod=none ;
	path bridging12 -> foodsec12,
 		 dpsscore12 -> foodsec12,
 		 foodsec3 -> foodsec12,
 		 bridging3 -> foodsec3,
 		 dpsscore3 -> foodsec3,
 		 foodsec0 -> foodsec3,
 		 eff_dose -> foodsec3,
 		 bridging3 -> bridging12,
 		 bridging0 -> bridging3,
 		 eff_dose -> bridging3,
 		 bridging12 -> dpsscore12,
 		 dpsscore3 -> dpsscore12,
		 dpsscore3 -> bridging12,
 		 bridging3 -> dpsscore3,
 		 dpsscore0 -> dps3,
 		 eff_dose -> dps3,
 		 eff_dose -> bridging12,
 		 eff_dose -> foodsec12,
 		 eff_dose -> dps12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> foodsec0,
		 bridging0 <-> foodsec0 ;
	effpart dpsscore3 <- eff_dose bridging3,
			dpsscore12 <- eff_dose bridging12 bridging3 dpsscore3,
			foodsec12 <- eff_dose bridging12 dpsscore12,
			foodsec3 <- eff_dose bridging3 dpsscore3,
			foodsec12 <- eff_dose bridging12 bridging3 dpsscore12 dpsscore3 foodsec3 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bridging and foodsec' ;
run ;
proc calis data=path omethod=none ;
	path bonding12 -> phealthcat12,
 		 dpsscore12 -> phealthcat12,
 		 phealthcat3 -> phealthcat12,
 		 bonding3 -> phealthcat3,
 		 dpsscore3 -> phealthcat3,
 		 phealthcat0 -> phealthcat3,
 		 eff_dose -> phealthcat3,
 		 bonding3 -> bonding12,
 		 bonding0 -> bonding3,
 		 eff_dose -> bonding3,
 		 bonding12 -> dpsscore12,
 		 dpsscore3 -> dpsscore12,
		 dpsscore3 -> bonding12,
 		 bonding3 -> dpsscore3,
 		 dpsscore0 -> dps3,
 		 eff_dose -> dps3,
 		 eff_dose -> bonding12,
 		 eff_dose -> phealthcat12,
 		 eff_dose -> dps12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> phealthcat0,
		 bonding0 <-> phealthcat0 ;
	effpart dpsscore3 <- eff_dose bonding3,
			dpsscore12 <- eff_dose bonding12 bonding3 dpsscore3,
			phealthcat12 <- eff_dose bonding12 dpsscore12,
			phealthcat3 <- eff_dose bonding3 dpsscore3,
			phealthcat12 <- eff_dose bonding12 bonding3 dpsscore12 dpsscore3 phealthcat3 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bonding and phealthcat' ;
run ;
proc calis data=path omethod=none ;
	path bridging12 -> phealthcat12,
 		 dpsscore12 -> phealthcat12,
 		 phealthcat3 -> phealthcat12,
 		 bridging3 -> phealthcat3,
 		 dpsscore3 -> phealthcat3,
 		 phealthcat0 -> phealthcat3,
 		 eff_dose -> phealthcat3,
 		 bridging3 -> bridging12,
 		 bridging0 -> bridging3,
 		 eff_dose -> bridging3,
 		 bridging12 -> dpsscore12,
 		 dpsscore3 -> dpsscore12,
		 dpsscore3 -> bonding12,
 		 bridging3 -> dpsscore3,
 		 dpsscore0 -> dps3,
 		 eff_dose -> dps3,
 		 eff_dose -> bridging12,
 		 eff_dose -> phealthcat12,
 		 eff_dose -> dps12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> phealthcat0,
		 bridging0 <-> phealthcat0 ;
	effpart dpsscore3 <- eff_dose bridging3,
			dpsscore12 <- eff_dose bridging12  bridging3 dpsscore3,
			phealthcat12 <- eff_dose bridging12 dpsscore12,
			phealthcat3 <- eff_dose bridging3 dpsscore3,
			phealthcat12 <- eff_dose bridging12 bridging3 dpsscore12 dpsscore3 phealthcat3 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bridging and phealthcat' ;
run ;
proc calis data=path omethod=none ;
	path bonding12 -> chealthcat12,
 		 dpsscore12 -> chealthcat12,
 		 chealthcat3 -> chealthcat12,
 		 bonding3 -> chealthcat3,
 		 dpsscore3 -> chealthcat3,
 		 chealthcat0 -> chealthcat3,
 		 eff_dose -> chealthcat3,
 		 bonding3 -> bonding12,
 		 bonding0 -> bonding3,
 		 eff_dose -> bonding3,
 		 bonding12 -> dpsscore12,
 		 dpsscore3 -> dpsscore12,
		 dpsscore3 -> bonding12,
 		 bonding3 -> dpsscore3,
 		 dpsscore0 -> dps3,
 		 eff_dose -> dps3,
 		 eff_dose -> bonding12,
 		 eff_dose -> chealthcat12,
 		 eff_dose -> dps12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> chealthcat0,
		 bonding0 <-> chealthcat0 ;
	effpart dpsscore3 <- eff_dose bonding3,
			dpsscore12 <- eff_dose bonding12 bonding3 dpsscore3,
			chealthcat12 <- eff_dose bonding12 dpsscore12,
			chealthcat3 <- eff_dose bonding3 dpsscore3,
			chealthcat12 <- eff_dose bonding12 bonding3 dpsscore12 dpsscore3 chealthcat3 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bonding and chealthcat' ;
run ;
proc calis data=path omethod=none ;
	path bridging12 -> chealthcat12,
 		 dpsscore12 -> chealthcat12,
 		 chealthcat3 -> chealthcat12,
 		 bridging3 -> chealthcat3,
 		 dpsscore3 -> chealthcat3,
 		 chealthcat0 -> chealthcat3,
 		 eff_dose -> chealthcat3,
 		 bridging3 -> bridging12,
 		 bridging0 -> bridging3,
 		 eff_dose -> bridging3,
 		 bridging12 -> dpsscore12,
 		 dpsscore3 -> dpsscore12,
		 dpsscore3 -> bridging12,
 		 bridging3 -> dpsscore3,
 		 dpsscore0 -> dps3,
 		 eff_dose -> dps3,
 		 eff_dose -> bridging12,
 		 eff_dose -> chealthcat12,
 		 eff_dose -> dps12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> chealthcat0,
		 bridging0 <-> chealthcat0 ;
	effpart dpsscore3 <- eff_dose bridging3,
			dpsscore12 <- eff_dose bridging12 bridging3 dpsscore3,
			chealthcat12 <- eff_dose bridging12 dpsscore12,
			chealthcat3 <- eff_dose bridging3 dpsscore3,
			chealthcat12 <- eff_dose bridging12 bridging3 dpsscore12 dpsscore3 chealthcat3 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bridging and chealthcat' ;
run ;

/*****************************************************ARCHIVE: SUMMER 2018 ANALYSES (Distributions, Preliminary Models using all timepoints)**************************************************/

libname bwhnp2 '\\drexel.edu\encrypted\CHFC\BWHN\Research\Data Analysis\Data Analysis-PhaseII\Analysis\bwhnp2' ;
/*need to macro these data and sorting steps*/
/*socsupport already done in each warehouse processing program (socsup) ---> need to edit*/
data baseline (drop=socsup);
	set bwhnp2.baseline_JUN17 ;
	survey=1 ;
run ;
data M3 (drop=socsup);
	set bwhnp2.M3_oct17 ;
	survey=2 ;
run ;
data M6 (drop=socsup);
	set bwhnp2.M6_DEC17 ;
	survey=3 ;
run ;
data M9 (drop=socsup);
	set bwhnp2.M9_MAR18 ;
	survey=4 ;
run ;
data M12 (drop=socsup);
	set bwhnp2.M12_JUN18 ;
	survey=5 ;
run ;
proc sort data=baseline ;
	by subject ;
run ;
proc sort data=M3 ;
	by subject ;
run ;
proc sort data=M6 ;
	by subject ;
run ;

proc sort data=M9 ;
	by subject ;
run ;

proc sort data=M12 ;
	by subject ;
run ;

data prepost ;
	set baseline M3 M6 M9 M12 ;
	by subject ;
	if cohort=9 then TANF=0 ; /*messed up in other codes*/
/*WIC, SSI, and currently receiving TANF (need to double check definitions)--- check with ACASI*/
	/*WIC*/
	if secu_1 ge 7 then WIC=. ;
	else if secu_1=1 then WIC=1 ;
	else if secu_1=2 then WIC=0 ;
	else if secu_1=3 then WIC=0 ;
	label WIC="Receive WIC" ;
	if secu_2a=1 then WIC_cut=0 ;
	else if secu_2a=0 then WIC_cut=1 ;
	else WIC_cut=. ;
	/*SNAP*/
	if secu_3 ge 7 then SNAP=. ;
	else if secu_3=2 then SNAP=0 ;
	else if secu_3=1 then SNAP=1 ;
	else SNAP=1 ;
	/*TANF (cash benefits)*/
	if secu_8 ge 7 then cash_cat=. ;
	else if secu_8=2 then cash_cat=1 ;
	else if secu_8=5 then cash_cat=1 ;
	else cash_cat=0 ;
	label cash_cat="Receive TANF" ;
/*fix missings globally for banking accounts and employment and benefits*/
	if secl_20 gt 1 then checking=9 ;
	else checking=secl_20 ;
	if secl_22 gt 1 then savings=9 ;
	else savings=secl_22 ;
	if secl_21 gt 50000 then checkbal= . ;
	else checkbal=secl_21 ;
	if secl_23 gt 50000 then savbal= . ;
	else savbal=secl_23 ;
	if secj_2 lt 97 then secj_2=secj_2 ;
	else if secj_2 ge 97 then secj_2=. ;
	if secj_4 lt 97 then secj_4=secj_4 ;
	else if secj_4 ge 97 then secj_4=. ;
	if secu_6 lt 99997 then secu_6=secu_6 ;
	else if secu_6 ge 99997 then secu_6=. ;
/*fix yes/no with accounts*/
	if secl_20 gt 1 then secl_20=. ;
	if secl_22 gt 1 then secl_22=. ;
/*employment = 20+ hrs/week*/
	if 20 le secj_2 lt 98 then employ_20=1 ;
	else if secj_2 lt 20 then employ_20=0 ;
	else employ_20=. ;
/*dichotomize health and food variables*/
	if phealth=1 then phealthcat=1 ;
	else if phealth gt 1 then phealthcat=0 ;
	else phealthcat=. ;
	if chealth=1 then chealthcat=1 ;
	else if chealth gt 1 then chealthcat=0 ;
	else chealthcat=. ;
	if fdsecure=1 then foodsec_cat=1 ;
	else if fdsecure gt 1 then foodsec_cat=0 ;
	else foodsec_cat=. ;
run ;
/*ANALYSES - PHASE II SETS only */
data prepost_c11 ;
	set prepost ;
	if cohort gt 11 then delete ;
run ;
proc freq data=prepost_c11 ;
	table survey/chisq ;
	title 'headers' ;
	format survey survey. ;
run ;
 
data socialcap_c11 ;
	set prepost_c11 ;
	if secd_1 gt 5 then secd_1m=. ;
	else secd_1m=secd_1 ;
	if secd_2 gt 5 then secd_2m=. ;
	else secd_2m=secd_2 ;
	if secd_3 gt 5 then secd_3m=. ;
	else secd_3m=secd_3 ;
	if secd_4 gt 5 then secd_4m=. ;
	else secd_4m=secd_4 ;
	if secd_5 gt 5 then secd_5m=. ;
	else secd_5m=secd_5 ;
	if secd_6 gt 5 then secd_6m=. ;
	else secd_6m=secd_6 ;
	if secd_7 gt 5 then secd_7m=. ;
	else secd_7m=secd_7 ;
	if secd_8 gt 5 then secd_8m=. ;
	else secd_8m=secd_8 ;
	if secd_9 gt 5 then secd_9m=. ;
	else secd_9m=secd_9 ;
	if secd_10 gt 5 then secd_10m=. ;
	if secd_11 gt 5 then secd_11m=. ;
	else secd_11m=secd_11 ;
	if secd_12 gt 5 then secd_12m=. ;
	else secd_12m=secd_12 ;
	if secd_13 gt 5 then secd_13m=. ;
	else secd_13m=secd_13 ;
	if secd_14 gt 5 then secd_14m=. ;
	else secd_14m=secd_14 ;	
	if secd_15 gt 5 then secd_15m=. ;
	else secd_15m=secd_15 ;
	if secd_16 gt 5 then secd_16m=. ;
	else secd_16m=secd_16 ;
	if secd_17 gt 5 then secd_17m=. ;
	else secd_17m=secd_17 ;
	if secd_18 gt 5 then secd_18m=. ;
	else secd_18m=secd_18 ;
	if secd_19 gt 5 then secd_19m=. ;
	else secd_19m=secd_19 ;
	if secd_20 gt 5 then secd_20m=. ;
	else secd_20m=secd_20 ;
	bonding=sum(of secd_1m--secd_10m) ;
	bridging=sum(of secd_11m--secd_20m) ;
run ;

/*check correlation b/w social capital domains and support*/
data socialsup_c11 ;
	set socialcap_c11 (keep=subject survey aces secc_1-secc_12);
/*rescale questions and missings for total score*/
	if secc_1 gt 5 then secc_1m=. ;
	else secc_1m=secc_1 ;
	if secc_2 gt 5 then secc_2m=. ;
	else secc_2m=secc_2 ;
	if secc_3 gt 5 then secc_3m=. ;
	else secc_3m=secc_3 ;
	if secc_4 gt 5 then secc_4m=. ;
	else secc_4m=secc_4 ;
	if secc_5 gt 5 then secc_5m=. ;
	else secc_5m=secc_5 ;
	if secc_6 gt 5 then secc_6m=. ;
	else secc_6m=secc_6 ;
	if secc_7 gt 5 then secc_7m=. ;
	else secc_7m=secc_7 ;
	if secc_8 gt 5 then secc_8m=. ;
	else secc_8m=secc_8 ;
	if secc_9 gt 5 then secc_9m=. ;
	else secc_9m=secc_9 ;
	if secc_10 gt 5 then secc_10m=. ;
	if secc_11 gt 5 then secc_11m=. ;
	else secc_11m=secc_11 ;
	if secc_12 gt 5 then secc_12m=. ;
	if secc_10=0 then secc_10m=1 ;
	else if secc_10=1 then secc_10m=0 ;
	else secc_10m=secc_10 ;
	if secc_12=1 then secc_12m=0 ;
	else if secc_12=0 then secc_12m=1 ;
	else secc_12m=secc_12 ;
	socsupport= sum(of secc_1m--secc_12m) ;
run ;

proc sort data=socialcap_c11 ;
	by subject ;
run ;
proc sort data=socialsup_c11 ;
	by subject ;
run ;
data cap (keep=subject survey bridging bonding);
	set socialcap_c11 ;
run ;
data sup (keep=subject survey secc_1-secc_12 secc_1m--secc_12m socsupport);
	set socialsup_c11 ;
run ;
proc means data=cap ;
	var bonding bridging ;
	class survey ;
	title 'social capital means - ALL' ;
run ;
proc glm data=cap ;
	model bonding=survey ;
	manova h=_all_ ;
	title 'bonding' ;
run ;
proc glm data=cap ;
	model bridging=survey ;
	manova h=_all_ ;
	title 'bridging' ;
run ;
data aces (keep=subject aces) ;
	set baseline ;
run ;
data cap_aces ;
	merge aces cap ;
	by subject ;
run ;
proc means data=cap_aces (where=(aces=1)) ;
	var bonding bridging ;
	class survey ;
	title 'social capital means - no ACEs' ;
run ;
proc glm data=cap_aces (where=(aces=1)) ;
	model bonding=survey ;
	manova h=_all_ ;
	title 'bonding - no ACEs' ;
run ;
proc glm data=cap_aces (where=(aces=1)) ;
	model bridging=survey ;
	manova h=_all_ ;
	title 'bridging - no ACEs' ;
run ;
proc means data=cap_aces (where=(aces=2)) ;
	var bonding bridging ;
	class survey ;
	title 'social capital means - 1-3 ACEs' ;
run ;
proc glm data=cap_aces (where=(aces=2)) ;
	model bonding=survey ;
	manova h=_all_ ;
	title 'bonding - 1-3 ACEs' ;
run ;
proc glm data=cap_aces (where=(aces=2)) ;
	model bridging=survey ;
	manova h=_all_ ;
	title 'bridging - 1-3 ACEs' ;
run ;
proc means data=cap_aces (where=(aces=3)) ;
	var bonding bridging ;
	class survey ;
	title 'social capital means - 4+ ACEs' ;
run ;
proc glm data=cap_aces (where=(aces=3)) ;
	model bonding=survey ;
	manova h=_all_ ;
	title 'bonding - 4+ ACEs' ;
run ;
proc glm data=cap_aces (where=(aces=3)) ;
	model bridging=survey ;
	manova h=_all_ ;
	title 'bridging - 4+ ACEs' ;
run ;
proc freq data=sup ;
	table aces*survey*(secc_1m--secc_12m)/nocol nopercent jt ;
	title 'social support' ;
run ;
data check ;
	merge cap sup ;
	by subject ;
run ;
proc corr data=check (where=(survey=1)) spearman ;
	var bonding socsupport ;
	title 'correlation b/w bonding and socsupport - baseline' ;
run ;
proc corr data=check (where=(survey=1))spearman ;
	var bridging socsupport ;
	title 'correlation b/w bridging and socsupport - baseline' ;
run ;
proc corr data=check (where=(survey=2)) spearman ;
	var bonding socsupport ;
	title 'correlation b/w bonding and socsupport - 3 mon' ;
run ;
proc corr data=check (where=(survey=2))spearman ;
	var bridging socsupport ;
	title 'correlation b/w bridging and socsupport - 3 mon' ;
run ;
proc corr data=check (where=(survey=3)) spearman ;
	var bonding socsupport ;
	title 'correlation b/w bonding and socsupport - 6 mon' ;
run ;
proc corr data=check (where=(survey=3))spearman ;
	var bridging socsupport ;
	title 'correlation b/w bridging and socsupport - 6 mon' ;
run ;
proc corr data=check (where=(survey=4)) spearman ;
	var bonding socsupport ;
	title 'correlation b/w bonding and socsupport - 9 mon' ;
run ;
proc corr data=check (where=(survey=4))spearman ;
	var bridging socsupport ;
	title 'correlation b/w bridging and socsupport - 9 mon' ;
run ;
proc corr data=check (where=(survey=5)) spearman ;
	var bonding socsupport ;
	title 'correlation b/w bonding and socsupport - 12 mon' ;
run ;
proc corr data=check (where=(survey=5))spearman ;
	var bridging socsupport ;
	title 'correlation b/w bridging and socsupport - 12 mon' ;
run ;
	
	
/*merge with attendence*/
proc sort data=bwhnp2.attendance_clean ;
	by subject ;
run ;
proc sort data=socialcap_c11 ;
	by subject ;
run ;
data socialcap_c11_attend ;
	merge bwhnp2.attendance_clean  socialcap_c11 ;
	by subject ;
	if total_attend=0 then ever_attend=0 ;
	if total_attend gt 0 then ever_attend=1 ;
	if run_attend ge 3 then eff_dose=1 ;
	else if run_attend lt 3 then eff_dose=0 ;
	else eff_dose=. ;
	if fdsecure gt 1 then foodinsecure=1 ; /*1-JUL-18: add outcomes*/
	else if fdsecure=1 then foodinsecure=0; 
	if secj_2 lt 20 then employ20=0 ;
	else if 20 le secj_2 lt 97 then employ20=1 ;
	else employ20=. ;
run ;
/*Slopes modelling per 1-June meeting*/
proc mixed data=socialcap_c11_attend order=formatted ;
	class eff_dose ;
	model bonding=eff_dose survey eff_dose*survey ;
	lsmeans eff_dose/pdiff at survey=2 diff=control('No');
	lsmeans eff_dose/pdiff at survey=3 diff=control('No');
	lsmeans eff_dose/pdiff at survey=4 diff=control('No');
	lsmeans eff_dose/pdiff at survey=5 diff=control('No');
	format survey survey. eff_dose yesno. ;
	title 'test bonding slopes by attendance (eff dose)' ;
run ;
proc mixed data=socialcap_c11_attend ;
	class eff_dose ;
	model bonding=eff_dose survey eff_dose*survey/Noint solution ;
	title 'test bonding slopes by attendance (eff dose) - reparameterized' ;
run ;
proc mixed data=socialcap_c11_attend ;
	class eff_dose ;
	model bridging=eff_dose survey eff_dose*survey ;
	lsmeans eff_dose/pdiff at survey=2 diff=control('No');
	lsmeans eff_dose/pdiff at survey=3 diff=control('No');
	lsmeans eff_dose/pdiff at survey=4 diff=control('No');
	lsmeans eff_dose/pdiff at survey=5 diff=control('No');
	format survey survey. eff_dose yesno. ;
	title 'test bridging slopes by attendance (eff dose)' ;
run ;
proc mixed data=socialcap_c11_attend ;
	class eff_dose ;
	model bridging=eff_dose survey eff_dose*survey/Noint solution ;
	ods select solutionf ;
	title 'test bridging slopes by attendance (eff dose) - reparameterized' ;
run ;
proc mixed data=socialcap_c11_attend ;
	class ever_attend ;
	model bonding=ever_attend survey ever_attend*survey ;
	lsmeans ever_attend/pdiff at survey=2 diff=control('No');
	lsmeans ever_attend/pdiff at survey=3 diff=control('No');
	lsmeans ever_attend/pdiff at survey=4 diff=control('No');
	lsmeans ever_attend/pdiff at survey=5 diff=control('No');
	format survey survey. ever_attend yesno. ;
	title 'test bonding slopes by attendance (ever attend)' ;
run ;
proc mixed data=socialcap_c11_attend ;
	class ever_attend ;
	model bonding=ever_attend survey ever_attend*survey/Noint solution ;
	ods select solutionf ;
	title 'test bonding slopes by attendance (ever attend) - reparameterized' ;
run ;
proc mixed data=socialcap_c11_attend ;
	class ever_attend ;
	model bridging=ever_attend survey ever_attend*survey ;
	lsmeans ever_attend/pdiff at survey=2 diff=control('No');
	lsmeans ever_attend/pdiff at survey=3 diff=control('No');
	lsmeans ever_attend/pdiff at survey=4 diff=control('No');
	lsmeans ever_attend/pdiff at survey=5 diff=control('No');
	format survey survey. ever_attend yesno. ;
	title 'test bridging slopes by attendance (ever attend)' ;
run ;
proc mixed data=socialcap_c11_attend ;
	class ever_attend ;
	model bridging=ever_attend survey ever_attend*survey/noint solution ;
	ods select solutionf ;
	title 'test bridging slopes by attendance (ever attend) - reparameterized' ;
run ;

/*boxplots over time*/
proc sort data=socialcap_c11_attend  ;
	by survey ;
run ;
ods graphics on;
title 'Box Plot for Social Capital';
proc boxplot data=socialcap_c11_attend  ;
	plot bonding*survey /boxstyle = schematic ;
    format survey survey. ;
	title 'Bonding Over Time - ALL' ;
run;
proc boxplot data=socialcap_c11_attend  ;
	plot bridging*survey /boxstyle = schematic ;
    format survey survey. ;
	title 'Bridging Over Time - ALL' ; 
run;
proc sort data=socialcap_c11_attend  ;
	by eff_dose ;
run ;
ods graphics on;
proc boxplot data=socialcap_c11_attend  ;
	plot bonding*survey /boxstyle = schematic ;
	by eff_dose ;
    format survey survey. eff_dose yesno. ;
	title 'Bonding Over Time - by attendance' ;
run;
proc boxplot data=socialcap_c11_attend  ;
	plot bridging*survey /boxstyle = schematic ;
	by eff_dose ;
    format survey survey. eff_dose yesno. ;
	title 'Bridging Over Time - by attendance' ;
run;

/*Test association with outcomes (food, job, health) independent of attendence
		 analyze with dps, stratify*/
/*Slopes modelling per 26-June meeting with DPS*/

proc mixed data=socialcap_c11_attend order=formatted ;
	class eff_dose ;
	model dpsscore=eff_dose survey eff_dose*survey ;
	lsmeans eff_dose/pdiff at survey=2 diff=control('No');
	lsmeans eff_dose/pdiff at survey=3 diff=control('No');
	lsmeans eff_dose/pdiff at survey=4 diff=control('No');
	lsmeans eff_dose/pdiff at survey=5 diff=control('No');
	format survey survey. eff_dose yesno. ;
run ;
proc mixed data=socialcap_c11_attend order=formatted ;
	model dpsscore=eff_dose survey /noint solution ;
	format survey survey. eff_dose yesno. ;
	title '1. depression score and attendance slopes' ;
run ;
proc mixed data=socialcap_c11_attend order=formatted ;
	model dpsscore=eff_dose bonding survey /noint solution ;
	format survey survey. eff_dose yesno. ;
	title '2a.bonding mediator model' ;
run ;
proc mixed data=socialcap_c11_attend order=formatted ;
	model dpsscore=eff_dose bridging survey /noint solution ;
	format survey survey. eff_dose yesno. ;
	title '2b.bridging mediator model' ;
run ;
proc mixed data=socialcap_c11_attend order=formatted ;
	model bonding=eff_dose survey /noint solution ;
	format survey survey. eff_dose yesno. ;
	title '3a.bonding and attendance' ;
run ;
proc mixed data=socialcap_c11_attend order=formatted ;
	model bridging=eff_dose survey /noint solution ;
	format survey survey. eff_dose yesno. ;
	title '3b.bridging and attendance' ;
run ;

/*Depression slopes model with social capital*/
proc mixed data=socialcap_c11_attend order=formatted ;
	class eff_dose ;
	model dpsscore=eff_dose survey eff_dose*survey bonding bonding*survey bonding*dpsscore ;
	lsmeans eff_dose/pdiff at survey=2 diff=control('No');
	lsmeans eff_dose/pdiff at survey=3 diff=control('No');
	lsmeans eff_dose/pdiff at survey=4 diff=control('No');
	lsmeans eff_dose/pdiff at survey=5 diff=control('No');
	format survey survey. eff_dose yesno. ;
	title 'depression score slopes and attendance adjust for bonding' ;
run ;
proc mixed data=socialcap_c11_attend order=formatted ;
	class eff_dose ;
	model dpsscore=eff_dose survey eff_dose*survey bridging bridging*survey bridging*dpsscore ;
	lsmeans eff_dose/pdiff at survey=2 diff=control('No');
	lsmeans eff_dose/pdiff at survey=3 diff=control('No');
	lsmeans eff_dose/pdiff at survey=4 diff=control('No');
	lsmeans eff_dose/pdiff at survey=5 diff=control('No');
	format survey survey. eff_dose yesno. ;
	title 'depression score slopes and attendance adjust for bridging' ;
run ;

/*TEST ASSOCIATION W/FOOD SECURITY*/
proc means data=socialcap_c11_attend (where=(survey=1));
	class fdsecure2cat ;
	var bonding bridging ;
	format fdsecure2cat foodsec_cat. ;
	title 'baseline scores by food security' ;
run ;
proc npar1way data=socialcap_c11_attend wilcoxon ;
	where survey=1 ;
	class fdsecure2cat ;
	var bonding bridging ;
	format fdsecure2cat foodsec_cat. ;
	title 'baseline scores by food security' ;
run ;
proc means data=socialcap_c11_attend (where=(survey=1));
	class fdsecure ;
	var bonding bridging ;
	format fdsecure foodsec. ;
	title 'baseline scores by food security levels' ;
run ;
proc npar1way data=socialcap_c11_attend wilcoxon ;
	where survey=1 ;
	class fdsecure ;
	var bonding bridging ;
	format fdsecure foodsec. ;
	title 'baseline scores by food security levels' ;
run ;
proc means data=socialcap_c11_attend (where=(survey=5));
	class fdsecure2cat ;
	var bonding bridging ;
	format fdsecure2cat foodsec_cat. ;
	title 'M12 scores by food security' ;
run ;
proc npar1way data=socialcap_c11_attend wilcoxon ;
	where survey=5 ;
	class fdsecure2cat ;
	var bonding bridging ;
	format fdsecure2cat foodsec_cat. ;
	title 'M12 scores by food security' ;
run ;
proc means data=socialcap_c11_attend (where=(survey=5));
	class fdsecure ;
	var bonding bridging ;
	format fdsecure foodsec. ;
	title 'M12 scores by food security levels' ;
run ;
proc npar1way data=socialcap_c11_attend wilcoxon ;
	where survey=5 ;
	class fdsecure ;
	var bonding bridging ;
	format fdsecure foodsec. ;
	title 'M12 scores by food security levels' ;
run ;
/*model scores with food insecurity*/
proc glimmix data=socialcap_c11_attend ;
	class fdsecure2cat survey ;
	model fdsecure2cat=bonding/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	title 'test bonding and food sec over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend ;
	class fdsecure2cat survey ;
	model fdsecure2cat=bridging/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	title 'test bridging and food sec over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend (where=(bonding ge 39)) ;
	class fdsecure2cat survey ;
	model fdsecure2cat=dpsscore/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	title 'test bonding and food sec over time - mixed model - high tert' ;
run ;
proc glimmix data=socialcap_c11_attend (where=(bonding lt 39)) ;
	class fdsecure2cat survey ;
	model fdsecure2cat=dpsscore/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	title 'test bonding and food sec over time - mixed model - low tert' ;
run ;
proc glimmix data=socialcap_c11_attend ;
	class fdsecure2cat survey ;
	model fdsecure2cat=bridging/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	title 'test bridging and food sec over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend (where=(bridging ge 39)) ;
	class fdsecure2cat survey ;
	model fdsecure2cat=dpsscore/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	title 'test bridging and food sec over time - mixed model - high tert' ;
run ;
proc glimmix data=socialcap_c11_attend (where=(bridging lt 39)) ;
	class fdsecure2cat survey ;
	model fdsecure2cat=dpsscore/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	title 'test bridging and food sec over time - mixed model - low tert' ;
run ;
/*test with employment and employed 20+*/
proc glimmix data=socialcap_c11_attend (where=(secj_1 le 1));
	class secj_1 survey ;
	model secj_1(descending)=bonding/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	title 'test bonding and employment over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend (where=(secj_1 le 1));
	class secj_1 survey ;
	model secj_1(descending)=bridging/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	title 'test bridging and employment over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend ;
	class employ20 survey ;
	model employ20(descending)=bonding/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	title 'test bonding and employed 20+ hrs over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend ;
	class employ20 survey ;
	model employ20(descending)=bridging/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	title 'test bridging and employed 20+ hrs over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend (where=(bonding ge 39)) ;
	class secj_1 survey ;
	model secj_1=dpsscore/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	title 'test bonding and employment over time - mixed model - high tert' ;
run ;
proc glimmix data=socialcap_c11_attend (where=(bonding lt 39)) ;
	class secj_1 survey ;
	model secj_1=dpsscore/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	title 'test bonding and employment over time - mixed model - low tert' ;
run ;

proc glimmix data=socialcap_c11_attend (where=(bridging ge 39)) ;
	class secj_1 survey ;
	model secj_1=dpsscore/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	title 'test bridging and employment over time - mixed model - high tert' ;
run ;
proc glimmix data=socialcap_c11_attend (where=(bridging lt 39)) ;
	class secj_1 survey ;
	model secj_1=dpsscore/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	title 'test bridging and employment over time - mixed model - low tert' ;
run ;
/*test with caregiver and child health*/
proc glimmix data=socialcap_c11_attend ;
	class phealth survey ;
	model phealth=bonding/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	format phealth health. ;
	title 'test bonding and caregiver health over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend ;
	class phealth survey ;
	model phealth=bridging/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	format phealth health. ;
	title 'test bridging and caregiver health over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend ;
	class chealth survey ;
	model chealth=bonding/solution oddsratio(at bonding=0 unit bonding=1)  ;
	random intercept/subject=survey ;
	format chealth health. ;
	title 'test bonding and child health over time - mixed model' ;
run ;
proc glimmix data=socialcap_c11_attend ;
	class chealth survey ;
	model chealth=bridging/solution oddsratio(at bridging=0 unit bridging=1)  ;
	random intercept/subject=survey ;
	format chealth health. ;
	title 'test bridging and child health over time - mixed model' ;
run ;

/*PATH ANALYSIS*/
/*rename variables for timepoints*/
data socialcap_path (keep=subject survey aces eff_dose bonding bridging dpsscore dps secj_1 foodsec_cat phealthcat chealthcat cash_cat);
	set socialcap_c11_attend ;
run ;

proc sql ;
	create table bond0 as
	select subject, survey, bonding as bonding0
	from socialcap_path 
	where survey=1 ;
	create table bond3 as
	select subject, survey, bonding as bonding3
	from socialcap_path 
	where survey=2 ;
	create table bond6 as
	select subject, survey, bonding as bonding6
	from socialcap_path 
	where survey=3 ;
	create table bond9 as
	select subject, survey, bonding as bonding9
	from socialcap_path 
	where survey=4 ;
	create table bond12 as
	select subject, survey, bonding as bonding12
	from socialcap_path 
	where survey=5 ;
	create table bridg0 as
	select subject, survey, bridging as bridging0
	from socialcap_path 
	where survey=1 ;
	create table bridg3 as
	select subject, survey, bridging as bridging3
	from socialcap_path 
	where survey=2 ;
	create table bridg6 as
	select subject, survey, bridging as bridging6
	from socialcap_path 
	where survey=3 ;
	create table bridg9 as
	select subject, survey, bridging as bridging9
	from socialcap_path 
	where survey=4 ;
	create table bridg12 as
	select subject, survey, bridging as bridging12
	from socialcap_path 
	where survey=5 ;
	create table dps0 as
	select subject, survey, dpsscore as dpsscore0, dps as dps0
	from socialcap_path 
	where survey=1 ;
	create table dps3 as
	select subject, survey, dpsscore as dpsscore3, dps as dps3
	from socialcap_path 
	where survey=2 ;
	create table dps6 as
	select subject, survey, dpsscore as dpsscore6, dps as dps6
	from socialcap_path 
	where survey=3 ;
	create table dps9 as
	select subject, survey, dpsscore as dpsscore9, dps as dps9
	from socialcap_path 
	where survey=4 ;
	create table dps12 as
	select subject, survey, dpsscore as dpsscore12, dps as dps12
	from socialcap_path 
	where survey=5 ;
	create table employ0 as
	select subject, survey, secj_1 as employ0
	from socialcap_path 
	where survey=1 ;
	create table employ3 as
	select subject, survey, secj_1 as employ3
	from socialcap_path 
	where survey=2 ;
	create table employ6 as
	select subject, survey, secj_1 as employ6
	from socialcap_path 
	where survey=3 ;
	create table employ9 as
	select subject, survey, secj_1 as employ9
	from socialcap_path 
	where survey=4 ;
	create table employ12 as
	select subject, survey, secj_1 as employ12
	from socialcap_path 
	where survey=5 ;
	create table foodsec0 as
	select subject, survey, foodsec_cat as foodsec0
	from socialcap_path 
	where survey=1 ;
	create table foodsec3 as
	select subject, survey, foodsec_cat as foodsec3
	from socialcap_path 
	where survey=2 ;
	create table foodsec6 as
	select subject, survey, foodsec_cat as foodsec6
	from socialcap_path 
	where survey=3 ;
	create table foodsec9 as
	select subject, survey, foodsec_cat as foodsec9
	from socialcap_path 
	where survey=4 ;
	create table foodsec12 as
	select subject, survey, foodsec_cat as foodsec12
	from socialcap_path 
	where survey=5 ;
	create table phealthcat0 as
	select subject, survey, phealthcat as phealthcat0
	from socialcap_path 
	where survey=1 ;
	create table phealthcat3 as
	select subject, survey, phealthcat as phealthcat3
	from socialcap_path 
	where survey=2 ;
	create table phealthcat6 as
	select subject, survey, phealthcat as phealthcat6
	from socialcap_path 
	where survey=3 ;
	create table phealthcat9 as
	select subject, survey, phealthcat as phealthcat9
	from socialcap_path 
	where survey=4 ;
	create table phealthcat12 as
	select subject, survey, phealthcat as phealthcat12
	from socialcap_path 
	where survey=5 ;
	create table chealthcat0 as
	select subject, survey, chealthcat as chealthcat0
	from socialcap_path 
	where survey=1 ;
	create table chealthcat3 as
	select subject, survey, chealthcat as chealthcat3
	from socialcap_path 
	where survey=2 ;
	create table chealthcat6 as
	select subject, survey, chealthcat as chealthcat6
	from socialcap_path 
	where survey=3 ;
	create table chealthcat9 as
	select subject, survey, chealthcat as chealthcat9
	from socialcap_path 
	where survey=4 ;
	create table chealthcat12 as
	select subject, survey, chealthcat as chealthcat12
	from socialcap_path 
	where survey=5 ;
	create table cash_cat0 as
	select subject, survey, cash_cat as cash_cat0
	from socialcap_path 
	where survey=1 ;
	create table cash_cat3 as
	select subject, survey, cash_cat as cash_cat3
	from socialcap_path 
	where survey=2 ;
	create table cash_cat6 as
	select subject, survey, cash_cat as cash_cat6
	from socialcap_path 
	where survey=3 ;
	create table cash_cat9 as
	select subject, survey, cash_cat as cash_cat9
	from socialcap_path 
	where survey=4 ;
	create table cash_cat12 as
	select subject, survey, cash_cat as cash_cat12
	from socialcap_path 
	where survey=5 ;
run ;
data dps (drop=survey) ;
	merge dps0 dps3 dps6 dps9 dps12 ;
	by subject ;
	if subject=. then delete ;
	deltadps3=dpsscore0-dpsscore3 ;
	deltadps6=dpsscore0-dpsscore6 ;
	deltadps9=dpsscore0-dpsscore9 ;
	deltadps12=dpsscore0-dpsscore12 ;
run ;
data classes (keep=subject eff_dose);
	set socialcap_path ;
run ;
data path (drop=survey) ;
	merge classes bond0 bond3 bond6 bond9 bond12 bridg0 bridg3 bridg6 bridg9 bridg12 dps0 dps3 dps6 dps9 dps12 employ0 employ3 employ6 employ9 employ12
		  foodsec0 foodsec3 foodsec6 foodsec9 foodsec12 phealthcat0 phealthcat3 phealthcat6 phealthcat9 phealthcat12 chealthcat0 chealthcat3 
		  chealthcat6 chealthcat9 chealthcat12 cash_cat0 cash_cat3 cash_cat6 cash_cat9 cash_cat12 ;
	by subject ;
	if subject=. then delete ;
	if subject ge 1200 then delete ;
/*change dpsscore to mental health improvement # per meeting with MMC 26-Oct*/
	deltadps3=dpsscore0-dpsscore3 ;
	deltadps6=dpsscore0-dpsscore6 ;
	deltadps9=dpsscore0-dpsscore9 ;
	deltadps12=dpsscore0-dpsscore12 ;
run ;

/*OUTCOME 1: EMPLOYMENT*/
/*bonding scale*/
ods graphics on ;
ods listing sge=on ;
proc calis data=path omethod=none ;
	path bonding12 -> employ12,
 		 dps12 -> employ12,
 		 employ9 -> employ12,
 		 bonding9 -> employ9,
 		 dps9 -> employ9,
 		 employ6 -> employ9,
 		 bonding6 -> employ6,
 		 dps6 -> employ6,
 		 employ3 -> employ6,
 		 bonding3 -> employ3,
 		 dps3 -> employ3,
 		 employ0 -> employ3,
 		 eff_dose -> employ3,
 		 bonding9 -> bonding12,
 		 bonding6 -> bonding9,
 		 bonding3 -> bonding6,
 		 bonding0 -> bonding3,
 		 eff_dose -> bonding3,
 		 bonding12 -> dps12,
 		 dps9 -> dps12,
 		 bonding9 -> dps9,
 		 dps6 -> dps9,
 		 bonding6 -> dps6,
 		 dps3 -> dps6,
 		 bonding3 -> dps3,
 		 dpsscore0 -> dps3,
 		 eff_dose -> dps3,
 		 eff_dose -> bonding12,
 		 eff_dose -> employ12,
 		 eff_dose -> dps12,
		 bonding3 -> dps6,
		 bonding6 -> dps9,
		 bonding9 -> dps12,
		 dps3 ->employ6,
		 dps6 ->employ9,
		 dps9 ->employ12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> employ0,
		 bonding0 <-> employ0 ;
	effpart dps3 <- eff_dose bonding3,
			dps12 <- eff_dose bonding12 dps3,
			employ12 <- eff_dose,
			employ12 <- eff_dose bonding12 dps12,
			employ3 <- eff_dose bonding3 dps3,
			employ12 <- eff_dose bonding12 dps12 employ3 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bonding and employment' ;
run ;
proc calis data=path omethod=none ;
	path bridging12 -> employ12,
 		 dps12 -> employ12,
 		 employ9 -> employ12,
 		 bridging9 -> employ9,
 		 dps9 -> employ9,
 		 employ6 -> employ9,
 		 bridging6 -> employ6,
 		 dps6 -> employ6,
 		 employ3 -> employ6,
 		 bridging3 -> employ3,
 		 dps3 -> employ3,
 		 employ0 -> employ3,
 		 eff_dose -> employ3,
 		 bridging9 -> bridging12,
 		 bridging6 -> bridging9,
 		 bridging3 -> bridging6,
 		 bridging0 -> bridging3,
 		 eff_dose -> bridging3,
 		 bridging12 -> dps12,
 		 dps9 -> dps12,
 		 bridging9 -> dps9,
 		 dps6 -> dps9,
 		 bridging6 -> dps6,
 		 dps3 -> dps6,
 		 bridging3 -> dps3,
 		 dpsscore0 -> dps3,
 		 eff_dose -> dps3,
 		 eff_dose -> bridging12,
 		 eff_dose -> employ12,
 		 eff_dose -> dps12,
		 bridging3 -> dps6,
		 bridging6 -> dps9,
		 bridging9 -> dps12,
		 dps3 ->employ6,
		 dps6 ->employ9,
		 dps9 ->employ12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> employ0,
		 bridging0 <-> employ0 ;
	effpart dps3 <- eff_dose bridging3,
			dps12 <- eff_dose bridging12 dps3,
			employ12 <- eff_dose,
			employ12 <- eff_dose bridging12 dps12,
			employ3 <- eff_dose bridging3 dps3,
			employ12 <- eff_dose bridging12 dps12 employ3 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bridging and employment' ;
run ;
proc calis data=path omethod=none ;
	path bonding12 -> foodsec12,
 		 deltadps12 -> foodsec12,
 		 foodsec9 -> foodsec12,
 		 bonding9 -> foodsec9,
 		 deltadps9 -> foodsec9,
 		 foodsec6 -> foodsec9,
 		 bonding6 -> foodsec6,
 		 deltadps6 -> foodsec6,
 		 foodsec3 -> foodsec6,
 		 bonding3 -> foodsec3,
 		 deltadps3 -> foodsec3,
 		 foodsec0 -> foodsec3,
 		 eff_dose -> foodsec3,
 		 bonding9 -> bonding12,
 		 bonding6 -> bonding9,
 		 bonding3 -> bonding6,
 		 bonding0 -> bonding3,
 		 eff_dose -> bonding3,
 		 bonding12 -> deltadps12,
 		 deltadps9 -> deltadps12,
 		 bonding9 -> deltadps9,
 		 deltadps6 -> deltadps9,
 		 bonding6 -> deltadps6,
 		 deltadps3 -> deltadps6,
 		 bonding3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> bonding12,
 		 eff_dose -> foodsec12,
 		 eff_dose -> deltadps12,
		 bonding3 -> deltadps6,
		 bonding6 -> deltadps9,
		 bonding9 -> deltadps12,
		 deltadps3 ->foodsec6,
		 deltadps6 ->foodsec9,
		 deltadps9 ->foodsec12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> foodsec0,
		 bonding0 <-> foodsec0 ;
	effpart foodsec12 <- eff_dose bonding12 deltadps12 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bonding and food security' ;
run ;
proc calis data=path omethod=none ;
		path bridging12 -> foodsec12,
 		 deltadps12 -> foodsec12,
 		 foodsec9 -> foodsec12,
 		 bridging9 -> foodsec9,
 		 deltadps9 -> foodsec9,
 		 foodsec6 -> foodsec9,
 		 bridging6 -> foodsec6,
 		 deltadps6 -> foodsec6,
 		 foodsec3 -> foodsec6,
 		 bridging3 -> foodsec3,
 		 deltadps3 -> foodsec3,
 		 foodsec0 -> foodsec3,
 		 eff_dose -> foodsec3,
 		 bridging9 -> bridging12,
 		 bridging6 -> bridging9,
 		 bridging3 -> bridging6,
 		 bridging0 -> bridging3,
 		 eff_dose -> bridging3,
 		 bridging12 -> deltadps12,
 		 deltadps9 -> deltadps12,
 		 bridging9 -> deltadps9,
 		 deltadps6 -> deltadps9,
 		 bridging6 -> deltadps6,
 		 deltadps3 -> deltadps6,
 		 bridging3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> bridging12,
 		 eff_dose -> foodsec12,
 		 eff_dose -> deltadps12,
		 bridging3 -> deltadps6,
		 bridging6 -> deltadps9,
		 bridging9 -> deltadps12,
		 deltadps3 ->foodsec6,
		 deltadps6 ->foodsec9,
		 deltadps9 ->foodsec12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> foodsec0,
		 bridging0 <-> foodsec0 ;
	effpart foodsec12 <- eff_dose bridging12 deltadps12 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bridging and food security' ;
run ;
proc calis data=path omethod=none ;
	path bonding12 -> phealthcat12,
 		 deltadps12 -> phealthcat12,
 		 phealthcat9 -> phealthcat12,
 		 bonding9 -> phealthcat9,
 		 deltadps9 -> phealthcat9,
 		 phealthcat6 -> phealthcat9,
 		 bonding6 -> phealthcat6,
 		 deltadps6 -> phealthcat6,
 		 phealthcat3 -> phealthcat6,
 		 bonding3 -> phealthcat3,
 		 deltadps3 -> phealthcat3,
 		 phealthcat0 -> phealthcat3,
 		 eff_dose -> phealthcat3,
 		 bonding9 -> bonding12,
 		 bonding6 -> bonding9,
 		 bonding3 -> bonding6,
 		 bonding0 -> bonding3,
 		 eff_dose -> bonding3,
 		 bonding12 -> deltadps12,
 		 deltadps9 -> deltadps12,
 		 bonding9 -> deltadps9,
 		 deltadps6 -> deltadps9,
 		 bonding6 -> deltadps6,
 		 deltadps3 -> deltadps6,
 		 bonding3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> bonding12,
 		 eff_dose -> phealthcat12,
 		 eff_dose -> deltadps12,
		 bonding3 -> deltadps6,
		 bonding6 -> deltadps9,
		 bonding9 -> deltadps12,
		 deltadps3 ->phealthcat6,
		 deltadps6 ->phealthcat9,
		 deltadps9 ->phealthcat12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> phealthcat0,
		 bonding0 <-> phealthcat0 ;
	effpart phealthcat12 <- eff_dose bonding12 deltadps12 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bonding and caregiver health' ;
run ;
proc calis data=path omethod=none ;
	path bridging12 -> phealthcat12,
 		 deltadps12 -> phealthcat12,
 		 phealthcat9 -> phealthcat12,
 		 bridging9 -> phealthcat9,
 		 deltadps9 -> phealthcat9,
 		 phealthcat6 -> phealthcat9,
 		 bridging6 -> phealthcat6,
 		 deltadps6 -> phealthcat6,
 		 phealthcat3 -> phealthcat6,
 		 bridging3 -> phealthcat3,
 		 deltadps3 -> phealthcat3,
 		 phealthcat0 -> phealthcat3,
 		 eff_dose -> phealthcat3,
 		 bridging9 -> bridging12,
 		 bridging6 -> bridging9,
 		 bridging3 -> bridging6,
 		 bridging0 -> bridging3,
 		 eff_dose -> bridging3,
 		 bridging12 -> deltadps12,
 		 deltadps9 -> deltadps12,
 		 bridging9 -> deltadps9,
 		 deltadps6 -> deltadps9,
 		 bridging6 -> deltadps6,
 		 deltadps3 -> deltadps6,
 		 bridging3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> bridging12,
 		 eff_dose -> phealthcat12,
 		 eff_dose -> deltadps12,
		 bridging3 -> deltadps6,
		 bridging6 -> deltadps9,
		 bridging9 -> deltadps12,
		 deltadps3 ->phealthcat6,
		 deltadps6 ->phealthcat9,
		 deltadps9 ->phealthcat12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> phealthcat0,
		 bridging0 <-> phealthcat0 ;
	effpart phealthcat12 <- eff_dose bridging12 deltadps12 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bridging and caregiver health' ;
run ;
proc calis data=path omethod=none ;
	path bonding12 -> chealthcat12,
 		 deltadps12 -> chealthcat12,
 		 chealthcat9 -> chealthcat12,
 		 bonding9 -> chealthcat9,
 		 deltadps9 -> chealthcat9,
 		 chealthcat6 -> chealthcat9,
 		 bonding6 -> chealthcat6,
 		 deltadps6 -> chealthcat6,
 		 chealthcat3 -> chealthcat6,
 		 bonding3 -> chealthcat3,
 		 deltadps3 -> chealthcat3,
 		 chealthcat0 -> chealthcat3,
 		 eff_dose -> chealthcat3,
 		 bonding9 -> bonding12,
 		 bonding6 -> bonding9,
 		 bonding3 -> bonding6,
 		 bonding0 -> bonding3,
 		 eff_dose -> bonding3,
 		 bonding12 -> deltadps12,
 		 deltadps9 -> deltadps12,
 		 bonding9 -> deltadps9,
 		 deltadps6 -> deltadps9,
 		 bonding6 -> deltadps6,
 		 deltadps3 -> deltadps6,
 		 bonding3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> bonding12,
 		 eff_dose -> chealthcat12,
 		 eff_dose -> deltadps12,
		 bonding3 -> deltadps6,
		 bonding6 -> deltadps9,
		 bonding9 -> deltadps12,
		 deltadps3 ->chealthcat6,
		 deltadps6 ->chealthcat9,
		 deltadps9 ->chealthcat12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> chealthcat0,
		 bonding0 <-> chealthcat0 ;
	effpart chealthcat12 <- eff_dose bonding12 deltadps12 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bonding and child health' ;
run ;
proc calis data=path omethod=none ;
	path bridging12 -> chealthcat12,
 		 deltadps12 -> chealthcat12,
 		 chealthcat9 -> chealthcat12,
 		 bridging9 -> chealthcat9,
 		 deltadps9 -> chealthcat9,
 		 chealthcat6 -> chealthcat9,
 		 bridging6 -> chealthcat6,
 		 deltadps6 -> chealthcat6,
 		 chealthcat3 -> chealthcat6,
 		 bridging3 -> chealthcat3,
 		 deltadps3 -> chealthcat3,
 		 chealthcat0 -> chealthcat3,
 		 eff_dose -> chealthcat3,
 		 bridging9 -> bridging12,
 		 bridging6 -> bridging9,
 		 bridging3 -> bridging6,
 		 bridging0 -> bridging3,
 		 eff_dose -> bridging3,
 		 bridging12 -> deltadps12,
 		 deltadps9 -> deltadps12,
 		 bridging9 -> deltadps9,
 		 deltadps6 -> deltadps9,
 		 bridging6 -> deltadps6,
 		 deltadps3 -> deltadps6,
 		 bridging3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> bridging12,
 		 eff_dose -> chealthcat12,
 		 eff_dose -> deltadps12,
		 bridging3 -> deltadps6,
		 bridging6 -> deltadps9,
		 bridging9 -> deltadps12,
		 deltadps3 ->chealthcat6,
		 deltadps6 ->chealthcat9,
		 deltadps9 ->chealthcat12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> chealthcat0,
		 bridging0 <-> chealthcat0 ;
	effpart chealthcat12 <- eff_dose bridging12 deltadps12 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bridging and child health' ;
run ;
proc calis data=path omethod=none ;
	path bonding12 -> cash_cat12,
 		 deltadps12 -> cash_cat12,
 		 cash_cat9 -> cash_cat12,
 		 bonding9 -> cash_cat9,
 		 deltadps9 -> cash_cat9,
 		 cash_cat6 -> cash_cat9,
 		 bonding6 -> cash_cat6,
 		 deltadps6 -> cash_cat6,
 		 cash_cat3 -> cash_cat6,
 		 bonding3 -> cash_cat3,
 		 deltadps3 -> cash_cat3,
 		 cash_cat0 -> cash_cat3,
 		 eff_dose -> cash_cat3,
 		 bonding9 -> bonding12,
 		 bonding6 -> bonding9,
 		 bonding3 -> bonding6,
 		 bonding0 -> bonding3,
 		 eff_dose -> bonding3,
 		 bonding12 -> deltadps12,
 		 deltadps9 -> deltadps12,
 		 bonding9 -> deltadps9,
 		 deltadps6 -> deltadps9,
 		 bonding6 -> deltadps6,
 		 deltadps3 -> deltadps6,
 		 bonding3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> bonding12,
 		 eff_dose -> cash_cat12,
 		 eff_dose -> deltadps12,
		 bonding3 -> deltadps6,
		 bonding6 -> deltadps9,
		 bonding9 -> deltadps12,
		 deltadps3 ->cash_cat6,
		 deltadps6 ->cash_cat9,
		 deltadps9 ->cash_cat12,
		 bonding0 <-> dpsscore0,
		 dpsscore0 <-> cash_cat0,
		 bonding0 <-> cash_cat0 ;
	effpart cash_cat12 <- eff_dose bonding12 deltadps12 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bonding and TANF' ;
run ;
proc calis data=path omethod=none ;
	path bridging12 -> cash_cat12,
 		 deltadps12 -> cash_cat12,
 		 cash_cat9 -> cash_cat12,
 		 bridging9 -> cash_cat9,
 		 deltadps9 -> cash_cat9,
 		 cash_cat6 -> cash_cat9,
 		 bridging6 -> cash_cat6,
 		 deltadps6 -> cash_cat6,
 		 cash_cat3 -> cash_cat6,
 		 bridging3 -> cash_cat3,
 		 deltadps3 -> cash_cat3,
 		 cash_cat0 -> cash_cat3,
 		 eff_dose -> cash_cat3,
 		 bridging9 -> bridging12,
 		 bridging6 -> bridging9,
 		 bridging3 -> bridging6,
 		 bridging0 -> bridging3,
 		 eff_dose -> bridging3,
 		 bridging12 -> deltadps12,
 		 deltadps9 -> deltadps12,
 		 bridging9 -> deltadps9,
 		 deltadps6 -> deltadps9,
 		 bridging6 -> deltadps6,
 		 deltadps3 -> deltadps6,
 		 bridging3 -> deltadps3,
 		 dpsscore0 -> deltadps3,
 		 eff_dose -> deltadps3,
 		 eff_dose -> bridging12,
 		 eff_dose -> cash_cat12,
 		 eff_dose -> deltadps12,
		 bridging3 -> deltadps6,
		 bridging6 -> deltadps9,
		 bridging9 -> deltadps12,
		 deltadps3 ->cash_cat6,
		 deltadps6 ->cash_cat9,
		 deltadps9 ->cash_cat12,
		 bridging0 <-> dpsscore0,
		 dpsscore0 <-> cash_cat0,
		 bridging0 <-> cash_cat0 ;
	effpart cash_cat12 <- eff_dose bridging12 deltadps12 ;
	pathdiagram diagram=standard notitle nofittable ;
	title 'bridging and TANF' ;
run ;

/*correlations for paths*/	 
proc corr data=path spearman ;
	var bonding0 bridging0 dpsscore0 employ0 foodsec0 phealth0 chealth0 ;
	title 'baseline correlations' ;
run ;

/*OLD CRAP*/ 
data m12 ;
	set m12 ;
	/*rescale section D responses for summing scores*/
	if secd_1 gt 5 then secd_1=. ;
	if secd_2 gt 5 then secd_2=. ;
	if secd_3 gt 5 then secd_3=. ;
	if secd_4 gt 5 then secd_4=. ;
	if secd_5 gt 5 then secd_5=. ;
	if secd_6 gt 5 then secd_6=. ;
	if secd_7 gt 5 then secd_7=. ;
	if secd_8 gt 5 then secd_8=. ;
	if secd_9 gt 5 then secd_9=. ;
	if secd_10 gt 5 then secd_10=. ;
	if secd_11 gt 5 then secd_11=. ;
	if secd_12 gt 5 then secd_12=. ;
	if secd_13 gt 5 then secd_13=. ;
	if secd_14 gt 5 then secd_14=. ;
	if secd_15 gt 5 then secd_15=. ;
	if secd_16 gt 5 then secd_16=. ;
	if secd_17 gt 5 then secd_17=. ;
	if secd_18 gt 5 then secd_18=. ;
	if secd_19 gt 5 then secd_19=. ;
	if secd_20 gt 5 then secd_20=. ;
	bonding_m12=sum(of secd_1-secd_10) ; /*per Williams (2006): 2 separate domains*/
	bridging_m12=sum(of secd_11-secd_20) ;
run ;
proc univariate data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	histogram bonding bridging ;
	title 'social capital distribution' ;
run ;
data socialcap_c11_scale (where=(survey=1));
	set socialcap_c11 ;
	if bridging le 38 then bridge_tert=1 ;
	else if 38 lt bridging le 45 then bridge_tert=2 ;
	else if 45 lt bridging then bridge_tert=3 ;
run ;
data socialcap_c11_scale_12 ;
	merge socialcap_c11_scale (in=a) m12 (in=b) ;
	by subject ;
run ;
proc sort data=socialcap_c11 ;
	by aces ;
run ;
proc means data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	by aces ;   
	title 'Mean score by aces' ;
run ;
proc reg data=socialcap_c11 (where=(survey=1));
	model bonding=aces ;
run ;
proc reg data=socialcap_c11 (where=(survey=1));
	model bridging=aces ;
run ;
proc sort data=socialcap_c11 ;
	by race ;
run ;
proc ttest data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	by race ;   
	format race race. ;
	title 'Mean score by race' ;
run ;
proc sort data=socialcap_c11 ;
	by seca_7 ;
run ;
proc ttest data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	by seca_7;
	format seca_7 married. ;
	title 'Mean score by marital status' ;
run ;
proc ttest data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	class employ_20 ;
	format employ_20 yesno. ;
	title 'Mean score by employed' ;
run ;
proc ttest data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	class secl_22 ;
	format secl_22 yesno. ;
	title 'Mean score by savings account' ;
run ;
proc ttest data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	class dps;
	format dps yesno. ;
	title 'Mean score by depression' ;
run ;
proc ttest data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	class fdsecure2cat ;
	format fdsecure2cat foodsec_cat. ;
	title 'Mean score by food security' ;
run ;
proc sort data=socialcap_c11 ;
	by houseinsecure ;
run ;
proc ttest data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	by houseinsecure ;
	format houseinsecure econsec. ;
	title 'Mean score by housing security' ;
run ;
proc sort data=socialcap_c11 ;
	by energy ;
run ;
proc ttest data=socialcap_c11 (where=(survey=1));
	var bonding bridging ;
	by energy ;
	format energy econsec. ;
	title 'Mean score by energy security' ;
run ;

	
/*by ACEs*/
data aces_c11 (keep=subject aces cohort) ;
	set baseline ;
run ;	
data socialcap_c11_aces ;
	merge socialcap_c11 aces_c11 ;
	by subject ;
run ;
proc sort data=socialcap_c11 ;
	by race ;
run ;
proc ttest data=socialcap_c11 (where=(aces=3));
	var bonding bridging ;
	by race ;   
	format race race. ;
	title 'Mean score by race' ;
run ;
proc sort data=socialcap_c11 ;
	by seca_7 ;
run ;
proc ttest data=socialcap_c11 (where=(aces=3));
	var bonding bridging ;
	by seca_7;
	format seca_7 married. ;
	title 'Mean score by marital status' ;
run ;
proc ttest data=socialcap_c11 (where=(aces=3));
	var bonding bridging ;
	class employ_20 ;
	format employ_20 yesno. ;
	title 'Mean score by employed' ;
run ;
proc ttest data=socialcap_c11 (where=(aces=3));
	var bonding bridging ;
	class secl_22 ;
	format secl_22 yesno. ;
	title 'Mean score by savings account' ;
run ;
proc ttest data=socialcap_c11 (where=(aces=3));
	var bonding bridging ;
	class dps;
	format dps yesno. ;
	title 'Mean score by depression' ;
run ;
proc ttest data=socialcap_c11 (where=(aces=3));
	var bonding bridging ;
	class fdsecure2cat ;
	format fdsecure2cat foodsec_cat. ;
	title 'Mean score by food security' ;
run ;
proc sort data=socialcap_c11 ;
	by houseinsecure ;
run ;
proc ttest data=socialcap_c11 (where=(aces=3));
	var bonding bridging ;
	by houseinsecure ;
	format houseinsecure econsec. ;
	title 'Mean score by housing security' ;
run ;
proc sort data=socialcap_c11 ;
	by energy ;
run ;
proc ttest data=socialcap_c11 (where=(aces=3));
	var bonding bridging ;
	by energy ;
	format energy econsec. ;
	title 'Mean score by energy security' ;
run ;

/*over time, everyone*/
proc freq data=socialcap_c11 ;
	table survey*(secd_1-secd_20)/ nopercent nocol jt ;
	format secd_1 likert. secd_2 likert. secd_3 likert. secd_4 likert. secd_5 likert. secd_6 likert. secd_7 likert. secd_8 likert. secd_9 likert.
		   secd_10 likert. secd_11 likert. secd_12 likert. secd_13 likert. secd_14 likert. secd_15 likert. secd_16 likert. secd_17 likert. secd_18 likert. secd_19 likert. secd_20 likert. ;
	title 'Social Capitol Over Time' ;
run ;
proc means data=socialcap_c11 ;
	var bonding bridging ;
	class survey ;
	title 'Social Capitol Over Time' ;
run ;
proc npar1way wilcoxon data=socialcap_c11 ;
	class survey ;
	var bridging ;
	title 'bridging over time' ;
run ;
proc npar1way wilcoxon data=socialcap_c11 ;
	class survey ;
	var bonding ;
	title 'bonding' ;
run ;
data socialcap_c11_lowtert (where=(bridge_tert=1));
	set socialcap_c11_scale_12 ;
run ;
proc npar1way wilcoxon data=socialcap_c11_lowtert ;
	class survey ;
	var bridging ;
	exact wilcoxon ;
	title 'bridging over time' ;
run ;
proc npar1way wilcoxon data=socialcap_c11 ;
	class survey ;
	var bonding ;
	exact wilcoxon ;
	title 'bonding' ;
run ;


/*social support*/
proc freq data=socialcap_c11 ;
	table survey*(secc_1-secc_12)/ nopercent nocol jt ;
	title 'Social support Over Time' ;
	format secc_1 yesno. secc_2 yesno. secc_3 yesno. secc_4 yesno. secc_5 yesno. secc_6 yesno. secc_7 yesno. secc_8 yesno. secc_9 yesno.
		   secc_10 yesno. secc_11 yesno. secc_12 yesno. ;
run ;

data socialsup_c11 ;
	set socialcap_c11 (keep=subject survey aces secc_1-secc_12);
/*rescale questions and missings for total score*/
	if secc_1 gt 5 then secc_1m=. ;
	else secc_1m=secc_1 ;
	if secc_2 gt 5 then secc_2m=. ;
	else secc_2m=secc_2 ;
	if secc_3 gt 5 then secc_3m=. ;
	else secc_3m=secc_3 ;
	if secc_4 gt 5 then secc_4m=. ;
	else secc_4m=secc_4 ;
	if secc_5 gt 5 then secc_5m=. ;
	else secc_5m=secc_5 ;
	if secc_6 gt 5 then secc_6m=. ;
	else secc_6m=secc_6 ;
	if secc_7 gt 5 then secc_7m=. ;
	else secc_7m=secc_7 ;
	if secc_8 gt 5 then secc_8m=. ;
	else secc_8m=secc_8 ;
	if secc_9 gt 5 then secc_9m=. ;
	else secc_9m=secc_9 ;
	if secc_10 gt 5 then secc_10m=. ;
	if secc_11 gt 5 then secc_11m=. ;
	else secc_11m=secc_11 ;
	if secc_12 gt 5 then secc_12m=. ;
	if secc_10=0 then secc_10m=1 ;
	else if secc_10=1 then secc_10m=0 ;
	else secc_10m=secc_10 ;
	if secc_12=1 then secc_12m=0 ;
	else if secc_12=0 then secc_12m=1 ;
	else secc_12m=secc_12 ;
	socsupport= sum(of secc_1m--secc_12m) ;
run ;

proc means data=socialsup_c11 ;
	var socsupport ;
	class survey ;
	title 'Social support Over Time' ;
run ;
proc reg data=socialsup_c11 ;
	model socsupport=survey ;
run ;

proc sort data=socialcap_c11 ;
	by subject ;
run ;
data socialcap_attend_outcomes (where=(cohort le 9)) ;
	merge socialcap_c11  bwhnp2.attendance_clean ;
	by subject ;
	if run_attend ge 3 then eff_dose=1 ;
	else if run_attend lt 3 then eff_dose=0 ;
	else eff_dose=. ;
/*rescale section D responses for summing scores*/
	if secd_1 gt 5 then secd_1=. ;
	if secd_2 gt 5 then secd_2=. ;
	if secd_3 gt 5 then secd_3=. ;
	if secd_4 gt 5 then secd_4=. ;
	if secd_5 gt 5 then secd_5=. ;
	if secd_6 gt 5 then secd_6=. ;
	if secd_7 gt 5 then secd_7=. ;
	if secd_8 gt 5 then secd_8=. ;
	if secd_9 gt 5 then secd_9=. ;
	if secd_10 gt 5 then secd_10=. ;
	if secd_11 gt 5 then secd_11=. ;
	if secd_12 gt 5 then secd_12=. ;
	if secd_13 gt 5 then secd_13=. ;
	if secd_14 gt 5 then secd_14=. ;
	if secd_15 gt 5 then secd_15=. ;
	if secd_16 gt 5 then secd_16=. ;
	if secd_17 gt 5 then secd_17=. ;
	if secd_18 gt 5 then secd_18=. ;
	if secd_19 gt 5 then secd_19=. ;
	if secd_20 gt 5 then secd_20=. ;
	bonding=sum(of secd_1-secd_10) ; /*per Williams (2006): 2 separate domains*/
	bridging=sum(of secd_11-secd_20) ;
run ;	
proc sort data=baseline_attend_outcomes ;
	by survey ;
run ;
proc ttest data=socialcap_attend_outcomes  ;
	var bonding bridging ;
	by survey ;
	class eff_dose ;
	title 'Mean score by attendance' ;
run ;
proc reg data=socialcap_attend_outcomes  (where=(eff_dose=0));
	model bridging=survey ;
run ;

/*network satisfaction*/

data satisfaction_c11 (keep=subject sect_1-sect_7);
	set reconc.M3_oct17 ;
	if subject gt 1000 then delete ;
	label SECT_1="How comfortable do you feel sharing your experiences with other people in the Drexel network?"
        SECT_2="How comfortable do you feel interacting with other people in the Drexel Network?"
        SECT_3="How much do you trust the other members in the Drexel Network?"
        SECT_4="How helpful is the financial education instructor in the Drexel Network?"
        SECT_5="How helpful is the group support facilitator in the Drexel Network?"
        SECT_6="In your opinion, how difficult are the tasks assigned in the Drexel Network?"
        SECT_7="Overall, how productive would you say the Drexel Network is?" ;
run ;

proc freq data=satisfaction_c11 ;
	table sect_1-sect_7 /chisq ;
	title 'Network Satisfaction' ;
	format sect_1 satis. sect_2 satis. sect_3 trust. sect_4 facil. sect_5 facil. sect_6 ease. sect_7 product. ;
run ;
proc sort data=satisfaction_c11 ;
	by subject ;
run ;
data satisfaction_attend_outcomes (where=(cohort le 9)) ;
	merge satisfaction_c11  bwhnp2.attendance_clean ;
	by subject ;
	if run_attend ge 3 then eff_dose=1 ;
	else if run_attend lt 3 then eff_dose=0 ;
	else eff_dose=. ;
run ;

proc freq data=satisfaction_attend_outcomes  ;
	table eff_dose*(sect_1-sect_7) /chisq nocol nopercent ;
	exact fisher/mc ;
	title 'Network Satisfaction by attendance' ;
	format sect_1 satis. sect_2 satis. sect_3 trust. sect_4 facil. sect_5 facil. sect_6 ease. sect_7 product. ;
run ;
