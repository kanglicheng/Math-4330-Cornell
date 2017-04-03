/*summary statistics*/
proc freq data=midterm1;
	tables dsmdephr*pardep;
	tables dsmdephr*ptsex;
	tables dsmdephr*dsmsubhr;
	tables dsmdephr*sesclass;
	tables dsmdephr*msparent;
run;

proc ttest data=midterm1;
	var ptage;
	class dsmdephr;
run;

proc ttest data=midterm1;
	var bedepon;
	class dsmdephr;
run; 

proc ttest data=midterm1;
	var besubon;
	class dsmdephr;
run;

data midterm1; set midterm1; 
if BEDEPON >= PTAGE or BEDEPON=-1 then DEP = PTAGE; else DEP = BEDEPON;
run;
/*Testing for PH assumption*/
proc phreg data=midterm1;
model dep*dsmdephr (0)=pardep ptsex dsmsubhr sesclass msparent/ties=efron;
assess ph/resample;
output out=out1 xbeta=xb resmart=mart resdev=dev;
title "Adjusted Cox Model 1: Depression of Offspring Dependent on Depression of Parent";
run;
ods graphics off;
/*Add time and PARDEP interaction term for nonproportionality*/
ods graphics on;
proc phreg data=midterm1;
model dep*dsmdephr (0)=pardep ptsex dsmsubhr sesclass msparent pardepdep/ties=efron;
pardepdep = pardep*dep;
title "Adjusted Cox Model 1: Depression of Offspring Dependent on Depression of Parent with pardep*dep interaction";
run;
ods graphics off;
/*Interaction with parent depression status and prepubertal onset of depression*/
ods graphics on;
proc phreg data=midterm1;
model dep*dsmdephr(0)=pardep pardepdeptime / ties=efron;
pardepdep=pardep*dep;
pardepdeptime=pardepdep*time;
if BEDEPON>=13 or BEDEPON=-1 then time=0; else time = 1;
title "UnAdjusted Cox Model 1 Depresison of offspring dependent on depression of parent with pardep*dep and time interaction";
run;
ods graphics off;
/*Interaction with parent depression status and prepubertal onset of depression*/
ods graphics on;
proc phreg data=midterm1;
model dep*dsmdephr(0)=pardep ptsex dsmsubhr sesclass msparent pardepdeptime / ties=efron;
pardepdep=pardep*dep;
pardepdeptime=pardepdep*time;
if BEDEPON>=13 or BEDEPON=-1 then time=0; else time = 1;
title "Adjusted Cox Model 1 Depresison of offspring dependent on depression of parent with pardep*dep and time interaction";
run;
ods graphics off;
/*imputation for missing variables*/
data midterm2; set midterm;
	if dsmsubhr=0 then besubon=ptage;
run;

data midterm3; set midterm2;
	if besubon =-1 then besubon=.;
run;

proc mi data=midterm3 nimpute=1 out=midterm4;
mcmc chain = multiple plots =trace  plots=acf;
	var besubon ptage;
run;


/*undajusted model for question 3 and create the variables needed for analysis*/
data midterm4; set midterm4; 
	if Besubon >= PTAGE then sub = PTAGE; else sub = BEsubon;
run;

ods graphics on;
proc phreg data=midterm4;
	model sub*dsmsubhr (0)  = sa pardep /ties=efron;
	if bedepon<besubon then sa=1; else sa=0;
	title "Unadjusted Cox Model 5: Substance abuse of Offspring Dependent on Depression of Parent";
run;
ods graphics off;


/*adjusted model for question 3 */
proc phreg data=midterm4 plots (cl)=s;
	class sesclass (ref=first)/param=ref;
	class msparent (ref=first)/param=ref;
	class ptsex (ref=first)/param=ref;
	model sub*dsmsubhr (0)  = sa pardep msparent sesclass ptsex/ties=efron;
	if bedepon<besubon then sa=1; else sa=0;
	title "adjusted Cox Model 7: substance abuse of Offspring Dependent on Depression of Child";
run;
