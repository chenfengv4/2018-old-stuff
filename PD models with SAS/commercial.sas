proc model data=commercial  ;
parms a0 a1 a2 a3;
logcompd = a0 + a1*laglogcompd + a2*bbbpremiumdiff1lag4 + a3*uelogdiff1lag4;
fit logcompd /gmm   KERNEL=(bart,0,0) VARDEF=N out=resid  ;
run;
proc reg data=commercial;
model logcompd = laglogcompd bbbpremiumdiff1lag4 uelogdiff1lag4/white ;
output out=res r=res;
run;
proc univariate data=res normal;
var res;
qqplot res;
run;
/*plot fitted value*/
proc autoreg data=commercial;
model logcompd = laglogcompd bbbpremiumdiff1lag4 uelogdiff1lag4;
output out=pdgraph p=pdhat;
run;
proc sgplot data=pdgraph ;
title 'Actual vs. Fitted';
series x=year_qtr  y=logcompd ;
series x=year_qtr y=pdhat /legendlabel = ' Predicted Value of Commercial PD';
run;
/*test for normality*/
proc model data=commercial;
parms a0 a1 a2 a3;
logcompd = a0 + a1*laglogcompd + a2*bbbpremiumdiff1lag4 + a3*uelogdiff1lag4;
fit logcompd /normal;
run;
/*TEST for heteroskedasticity*/
proc model data=commercial;
parms a0 a1 a2 a3;
logcompd = a0 + a1*laglogcompd + a2*bbbpremiumdiff1lag4 + a3*uelogdiff1lag4;
fit logcompd /white pagan=(1 laglogcompd bbbpremiumdiff1lag4 uelogdiff1lag4);
run;
proc autoreg data=commercial;
model logcompd = laglogcompd bbbpremiumdiff1lag4 uelogdiff1lag4/archtest plots(unpack);
run;
/*test for autocorrelation   */
proc model data=commercial;
parms a0 a1 a2 a3;
logcompd = a0 + a1*laglogcompd + a2*bbbpremiumdiff1lag4 + a3*uelogdiff1lag4;
fit logcompd /godfrey = 8;
run;
/*durbin h test for autocorrelation*/
proc autoreg data=commercial;
model logcompd = laglogcompd bbbpremiumdiff1lag4 uelogdiff1lag4/lagdep =laglogcompd  plots(unpack);
run;

/*test for stationary*/
proc reg data=commercial;
model logcompd = laglogcompd bbbpremiumdiff1lag4 uelogdiff1lag4;
output out=pdres
p=pdhat
r=pdres;
run;
proc autoreg data=pdres;
model pdres= /stationarity = (kpss=(kernel=qs auto) )stationarity=(phillips) stationarity=(adf) plots(unpack);
run;

/*test for seasonality*/
proc reg data=commercial;
model logcompd = laglogcompd bbbpremiumdiff1lag4 uelogdiff1lag4 q1 q2 q3;
test q1=q2=q3=0;
run;
/*seasonality graph   */
proc spectra data=commercial out=gra;
var logcompd;
run;
title 'Periodogram';
proc sgplot data=gra;
series x=freq y=p_01;
run;
/*test collinearity*/
proc reg data=commercial;
model logcompd = laglogcompd bbbpremiumdiff1lag4 uelogdiff1lag4 /vif tol collin;
run;

