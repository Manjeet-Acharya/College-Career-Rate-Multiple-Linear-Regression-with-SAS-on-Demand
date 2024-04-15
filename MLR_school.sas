/* Importing the .CSV file into SAS program*/
FILENAME IMPUSCH '/home/u63548264/IE 5318 - Group Project/Imputed_school.csv';
PROC IMPORT DATAFILE=IMPUSCH
	DBMS=CSV
	replace
	OUT=imputed_school;
	GETNAMES=YES;
	GUESSINGROWS=MAX;

/* Plotting the Matrix scatterplot to analyze
response to predictor variable association and
predictor to predictor variable association*/
PROC SGSCATTER DATA=imputed_school; 
     MATRIX college_career_rate graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;
RUN;

proc corr data=imputed_school noprob;
	var college_career_rate graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;

/* Buildiing the full model with all 4 predictor variables*/
/* checking for the VIF for serious multicolinearity*/
proc reg data=imputed_school;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety / ss1 vif i;
output out=school_out predicted=yhat residual=e rstudent=tres h=hii cookd=cookdi dffits=dfittsi;

/* normal scores for normality plot*/
proc rank normal=blom out=enrm data=school_out;
var e;
ranks enrm;
	
data school_new; set school_out; set enrm;
label enrm = 'Normal Scores';
id = _n_;
label id = 'Observation Number';

proc corr data=school_new;
var e enrm;

/* Plotting Residual vs. Y-hat for residual analysis*/
goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = school_new;
	plot e*yhat /vref = 0 vaxis = axis1;

/* Normality Plot*/
goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = school_new;
	plot e*enrm / vaxis = axis1;
run;

/* Bruesch-Pagen Test for Constant Variance*/
data school_BP; set imputed_school; set school_out;
e_sq = e**2;
label e_sq = 'Squared Residual';

proc reg data=school_BP;
model e_sq = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;
run;

proc reg data=imputed_school;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety / vif influence;
output out=school_out2 predicted=yhat2 residual=e2 cookd=cookdi2;
proc print ; var college_career_rate graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety cookdi2;

/* Creating a copy of variables for standardizing */
data school_out2a; set school_out2;
stdx1 = graduation_rate;
stdx2 = attendance_rate;
stdx3 = pct_stu_safe;
stdx4 = pct_stu_enough_variety;

/* standardizing the variables */
proc standard data = school_out2a mean=0 std=1 out=school_out2std;
var stdx1 stdx2 stdx3 stdx4;

/* Creating interaction terms */
data school_out2intr; set school_out2std;
x1x2 = graduation_rate*attendance_rate;
x1x3 = graduation_rate*attendance_rate;
x1x4 = graduation_rate*attendance_rate;
x2x3 = attendance_rate*pct_stu_safe;
x2x4 = attendance_rate*pct_stu_enough_variety;
x3x4 = pct_stu_safe*pct_stu_enough_variety;

/* Creating standardized interaction terms */
stdx1x2 = stdx1*stdx2;
stdx1x3 = stdx1*stdx3;
stdx1x4 = stdx1*stdx4;
stdx2x3 = stdx2*stdx3;
stdx2x4 = stdx2*stdx4;
stdx3x4 = stdx3*stdx4;

/* Partial Regression */
proc reg data=school_out2intr;
model x1x2 = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;
output out=outint residual=ex1x2;

proc reg data=outint;
model x1x2 = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;
output out=outint residual=ex1x3;

proc reg data=outint;
model x1x4 = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;
output out=outint residual=ex1x4;

proc reg data=outint;
model x2x3 = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;
output out=outint residual=ex2x3;

proc reg data=outint;
model x2x4 = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;
output out=outint residual=ex2x4;

proc reg data=outint;
model x3x4 = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety;
output out=outint residual=ex3x4;

proc corr data=school_out2intr noprob;
var college_career_rate graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety x1x2 x1x3 x1x4 x2x3 x2x4 x3x4;

proc corr data=school_out2intr noprob;
var college_career_rate graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4;

data part_reg; set outint;
label e2 = 'e(Y | x1,x2,x3,x4)';
label ex1x2 = 'e(x1x2 | x1,x2,x3,x4)';
label ex1x3 = 'e(x1x3 | x1,x2,x3,x4)';
label ex1x4 = 'e(x1x4 | x1,x2,x3,x4)';
label ex2x3 = 'e(x2x3 | x1,x2,x3,x4)';
label ex2x4 = 'e(x2x4 | x1,x2,x3,x4)';
label ex3x4 = 'e(x3x4 | x1,x2,x3,x4)';


/* ================ Residual Vs. Interaction Terms ============*/

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*x1x2 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*x1x3 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*x1x4 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*x2x3 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*x2x4 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*x3x4 / vaxis = axis1;
run;

/* ================ Residual vs. Standardized Interaction ============*/

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*stdx1x2 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*stdx1x3 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*stdx1x4 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*stdx2x3 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*stdx2x4 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*stdx3x4 / vaxis = axis1;
run;

/* ================ Partial Regression Plots ============*/

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*ex1x2 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*ex1x3 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*ex1x4 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*ex2x3 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*ex2x4 / vaxis = axis1;
run;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data=part_reg;
	plot e2*ex3x4 / vaxis = axis1;
run;
/*

/*============= Best Subset Regression ===============*/

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=1 stop=1 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=2 stop=2 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=3 stop=3 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=4 stop=4 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=5 stop=5 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=5 stop=5 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=6 stop=6 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=7 stop=7 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=8 stop=8 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=9 stop=9 best=2;

Proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = adjrsq cp aic sbc start=1 stop=10 best=14;

/* =============== Backward Deletion ============ */

proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = backward slstay=.05;

/* =============== Stepwise Regression ============ */

proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe pct_stu_enough_variety stdx1x2 stdx1x3 stdx1x4 stdx2x3 stdx2x4 stdx3x4 / selection = stepwise slentry=.05 slstay=0.05;

/* fitting the best 2 models and 
looking at their ANOVA tables for parameter estimates */
proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe stdx1x2 stdx1x3 stdx2x3 / vif influence;
output out=final_model6 predicted=yhatf1 residual=ef1 cookd=cookdif1;

proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe stdx1x2 stdx1x3 stdx1x4 stdx2x3 / vif influence;
output out=final_model7 predicted=yhatf2 residual=ef2 cookd=cookdif2;


/* resudial vs. y-hat and normality plot for model 6 */
goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = final_model6;
	plot ef1*yhatf1 /vref = 0 vaxis = axis1;

proc rank normal=blom out=enrm_f1 data=final_model6;
var ef1;
ranks enrm_f1;

data col_outnew1; set final_model6; set enrm_f1;
label enrm_f1 = 'Normal Scores';

proc corr data=col_outnew1;
var ef1 enrm_f1;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = col_outnew1;
	plot ef1*enrm_f1 / vaxis = axis1;
run;

/* resudial vs. y-hat and normality plot for model 7 */
goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = final_model7;
	plot ef2*yhatf2 /vref = 0 vaxis = axis1;

proc rank normal=blom out=enrm_f2 data=final_model7;
var ef2;
ranks enrm_f2;

data col_outnew2; set final_model7; set enrm_f2;
label enrm_f2 = 'Normal Scores';

proc corr data=col_outnew2;
var ef2 enrm_f2;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = col_outnew2;
	plot ef2*enrm_f2 / vaxis = axis1;
run;

/* Picking the overall best Model*/
proc reg data=school_out2intr;
model college_career_rate = graduation_rate attendance_rate pct_stu_safe stdx1x2 stdx1x3 stdx2x3 / ss1 vif i;
output out=col_out predicted=yhatbest residual=ebest rstudent=tresbest h=hiibest cookd=cookdibest dffits=dffitsibest;

/* Plotting residual vs. Y-hat and normality plot
for the overall best Model*/
goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = col_out;
	plot ebest*yhatbest /vref = 0 vaxis = axis1;

proc rank normal=blom out=enrm_best data=col_out;
var ebest;
ranks enrm_best;

data col_outnew; set col_out; set enrm_best;
label enrm_best = 'Normal Scores';

proc corr data=col_outnew;
var ebest enrm_best;

goptions reset = all;
symbol1 v=dot c=black;
axis1 label=(angle = 90);
proc gplot data = col_outnew;
	plot ebest*enrm_best / vaxis = axis1;
run;
