data correlation_matrix(type=corr);
input 
	_TYPE_ $1-5 _NAME_ $6-16 ARMAGNAC 17-24 
	CALVADOS 25-32 COGNAC 33-40 KIRSCH 41-49 
	MARC 50-57 MIRABELLE 58-65 RUM 66-73 WHISKEY 74-79;
datalines;
CORR ARMAGNAC   1.000   0.210   0.370   -0.320   0.000   -0.310  -0.260  0.090 
CORR CALVADOS   0.210   1.000   0.090   -0.290   0.120   -0.300  -0.140  0.010 
CORR COGNAC     0.370   0.090   1.000   -0.310   -0.040  -0.300  -0.110  0.120 
CORR KIRSCH     -0.320  -0.290  -0.310  1.000    -0.160  0.250   -0.130  -0.140
CORR MARC       0.000   0.120   -0.040  -0.160   1.000   -0.200  -0.030  -0.080
CORR MIRABELLE  -0.310  -0.300  -0.300  0.250    -0.200  1.000   -0.240  -0.160
CORR RUM        -0.260  -0.140  -0.110  -0.130   -0.030  -0.240  1.000   -0.200
CORR WHISKEY    0.090   0.010   0.120   -0.140   -0.080  -0.160  -0.200  1.000 
N    .          2014.0  2014.0  2014.0  2014.0   2014.0  2014.0  2014.0  2014.0
;
run;
quit;


proc factor data=correlation_matrix 
	method=principal priors=smc nfactors=3 rotate=varimax;
run; quit;

proc factor data=correlation_matrix 
	method=ml heywood priors=smc nfactors=3 rotate=varimax;
run; quit;
