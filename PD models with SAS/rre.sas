PROC IMPORT OUT= WORK.rre 
            DATAFILE= "C:\secondPillar\private bank CIBC bank\PD Models\
rre.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
proc reg data=RRE;
model rrepddiff1 = lagrrepddiff1 dji1qrdiff1 uediff2lag1 ndigdiff2lag2/white;
output out=res r=res;
run;
proc univariate data=res normal;
var res;
qqplot res;
run;
proc model data=rre;
parms a0 a1 a2 a3 a4;
rrepddiff1 = a0+ a1*lagrrepddiff1 +a2*dji1qrdiff1 + a3*uediff2lag1 + a4* ndigdiff2lag2;
fit rrepddiff1 /gmm kernel = (bart, 0 ,0) vardef = n out=resid normal ;
run;

proc autoreg data=rre;
model rrepddiff1 = lagrrepddiff1 dji1qrdiff1 uediff2lag1 ndigdiff2lag2;
output out=pdgraph p=pdhat;
run;
proc sgplot data=pdgraph ;
title 'Actual vs. Fitted';
series x=year_qtr  y=rrepddiff1 ;
series x=year_qtr y=pdhat /legendlabel = ' Predicted Value of Diff1 RRE PD';
run;
/*test for heteroskedasticity   */
proc model data=rre;
parms a0 a1 a2 a3 a4;
rrepddiff1 = a0+ a1*lagrrepddiff1 +a2*dji1qrdiff1 + a3*uediff2lag1 + a4* ndigdiff2lag2;
fit rrepddiff1 / white pagan=(1 lagrrepddiff1 dji1qrdiff1 uediff2lag1 ndigdiff2lag2) ;
run;
proc autoreg data=rre;
model rrepddiff1 = lagrrepddiff1 dji1qrdiff1 uediff2lag1 ndigdiff2lag2 / archtest plots(unpack);
run;
/*test for autocorrelation */
proc model data=rre;
parms a0 a1 a2 a3 a4;
rrepddiff1 = a0+ a1*lagrrepddiff1 +a2*dji1qrdiff1 + a3*uediff2lag1 + a4* ndigdiff2lag2;
fit rrepddiff1 / gmm godfrey =8 ;
run;
proc autoreg data=rre;
model rrepddiff1 = lagrrepddiff1 dji1qrdiff1 uediff2lag1 ndigdiff2lag2 / lagdep = lagrrepddiff1 plots(unpack);
run;
/*test for stationary*/
proc reg data=rre;
model rrepddiff1 = lagrrepddiff1 dji1qrdiff1 uediff2lag1 ndigdiff2lag2;
output out=pdres
p=pdhat
r=pdres;
run;
proc autoreg data=pdres;
model pdres= /stationarity = (kpss=(kernel=qs auto) )stationarity=(phillips) stationarity=(adf=1) plots(unpack);
run;
/*testfor seasonality*/
proc reg data=rre;
model rrepddiff1 = lagrrepddiff1 dji1qrdiff1 uediff2lag1 ndigdiff2lag2 q1 q2 q3;
test q1=q2=q3=0;
run;
proc spectra data=rre
out=gra;
var rrepddiff1;
run;
title 'Periodogram';
proc sgplot data=gra;
series x=freq y=p_01;
run;
/*test for collinearity   */
proc reg data=rre;
model rrepddiff1 = lagrrepddiff1 dji1qrdiff1 uediff2lag1 ndigdiff2lag2 / vif tol collin;
run;
