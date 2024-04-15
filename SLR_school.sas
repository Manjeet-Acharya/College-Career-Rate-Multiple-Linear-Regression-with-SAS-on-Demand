/* Importing the .xlsx file into SAS program*/
FILENAME SCHOLL '/home/u63548264/IE 5318 - Group Project/school.csv';
PROC IMPORT DATAFILE=SCHOLL
	DBMS=CSV
	replace
	OUT=WORK.SCHOLL;
	GETNAMES=YES;
	GUESSINGROWS=MAX;
	
PROC CONTENTS DATA=WORK.SCHOLL;

AXIS7 LABEL=(ANGLE = 90);
PROC GPLOT;
PLOT college_career_rate*graduation_rate / VAXIS=AXIS7;

proc corr data=SCHOLL;
var college_career_rate graduation_rate;
proc means;

proc reg;
	model college_career_rate = graduation_rate;
	output out=school_forever predicted=yhat residual=e;

	
goptions reset=all;
symbol1 v=dot c=black;
symbol2 v=none i=join c=black;
axis1 label=(angle=90);
proc gplot data = school_forever;
	plot college_career_rate*graduation_rate yhat*graduation_rate/ overlay vaxis=axis1;
	
proc univariate plot data=school_forever;
var e;
run;	

proc rank normal=blom out=enrm data=school_forever;
	var e;
	ranks enrm;
	label enrm = 'Normal Scores';


data school_forevernew;
	set SCHOLL; set school_forever; set enrm;
	id = _n_;
	label id = 'Observation Number';

proc corr data=school_forevernew;
	var e enrm;
	
goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = school_forevernew;
	plot e*yhat /vref = 0 vaxis = axis1;

symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = school_forevernew;
	plot e*graduation_rate /vref = 0 vaxis = axis1;
	
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = school_forevernew;
	plot e*enrm / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot i=join c=black;
axis1 label=(angle = 90);
proc gplot data = school_forevernew;
	plot e*id / vaxis = axis1;
run;

data calcBP; set SCHOLL; set school_forever;
e_sq = e**2;
label e_sq = 'Squared Residual';

proc reg data=calcBP;
model e_sq = graduation_rate;
run;

