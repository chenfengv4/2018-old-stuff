PROC IMPORT OUT= WORK.cre 
            DATAFILE= "C:\secondPillar\private bank CIBC bank\PD Models\
cre.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
proc reg data=cre;
model crepd = lagcrepd bbbpremiumdiff1lag3 creidiff2lag4/white;
output out=res r=res;
run;
proc univariate data=res normal;
var res;
qqplot res;
run;
/*correct for heteroskedasticity*/
proc model data=cre;
parms a0 a1 a2 a3;
crepd = a0 +a1*lagcrepd +a2*bbbpremiumdiff1lag3 + a3*creidiff2lag4;
fit crepd/gmm kernel=(bart, 5,0) vardef=n out=resid normal;
run;
proc autoreg data=cre;
model crepd = lagcrepd bbbpremiumdiff1lag3 creidiff2lag4;
output out=pdgraph p=pdhat;
run;
proc sgplot data=pdgraph ;
title 'Actual vs. Fitted';
series x=year_qtr  y=crepd ;
series x=year_qtr y=pdhat /legendlabel = ' Predicted Value of CRE PD';
run;

/*test for heteroskedasticity   */
proc model data=cre;
parms a0 a1 a2 a3;
crepd = a0 +a1*lagcrepd +a2*bbbpremiumdiff1lag3 + a3*creidiff2lag4;
fit crepd/ gmm white pagan = (1 lagcrepd bbbpremiumdiff1lag3 creidiff2lag4);
run;
proc autoreg data=cre;
model crepd = lagcrepd bbbpremiumdiff1lag3 creidiff2lag4/archtest plots(unpack);
run;

/*test for autocorrelation*/
proc model data=cre;
parms a0 a1 a2 a3;
crepd = a0 +a1*lagcrepd +a2*bbbpremiumdiff1lag3 + a3*creidiff2lag4;
fit crepd/gmm godfrey = 8;
run;
proc autoreg data=cre;
model crepd = lagcrepd bbbpremiumdiff1lag3 creidiff2lag4/lagdep=lagcrepd plots(unpack);
run;


/*test for stationary*/
proc reg data=cre;
model crepd = lagcrepd bbbpremiumdiff1lag3 creidiff2lag4;
output out=pdres
p=pdhat
r=pdres;
run;
proc autoreg data=pdres;
model pdres= /stationarity = (kpss=(kernel=qs auto) )stationarity=(phillips) stationarity=(adf=1) plots(unpack);
run;


/*test for seasonality*/
proc reg data=cre;
model crepd = lagcrepd bbbpremiumdiff1lag3 creidiff2lag4 q1 q2 q3;
test q1=q2=q3=0;
run;
proc spectra data=cre
out=gra;
var crepd;
run;
title 'Periodogram';
proc sgplot data=gra;
series x=freq y=p_01;
run;
/*test for collinearity   */
proc reg data=cre;
model crepd = lagcrepd bbbpremiumdiff1lag3 creidiff2lag4 /vif tol collin;
run;
