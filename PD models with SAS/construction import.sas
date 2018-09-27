PROC IMPORT OUT= WORK.COnstruction 
            DATAFILE= "C:\secondPillar\private bank CIBC bank\PD Models\
construction.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
