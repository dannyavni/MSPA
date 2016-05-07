libname mydata 'c:\mspa\410' access=readonly;

ods graphics on;
ods noproctitle;
title 'Assignment 6';

data temp;
set mydata.stock_portfolio_data;
run;

proc sort data=temp; by date; run; quit;

data temp;
set temp;
* Compute the log-returns - log of the ratio of today's price to yesterday's price;
* Note that the data needs to be sorted in the correct direction in order for us to compute the correct return;
	return_AA  = log(AA/lag1(AA));
	return_BAC = log(BAC/lag1(BAC));
	return_BHI = log(BHI/lag1(BHI));
	return_CVX = log(CVX/lag1(CVX));
	return_DD  = log(DD/lag1(DD));
	return_DOW = log(DOW/lag1(DOW));
	return_DPS = log(DPS/lag1(DPS));
	return_GS  = log(GS/lag1(GS));
	return_HAL = log(HAL/lag1(HAL));
	return_HES = log(HES/lag1(HES));
	return_HON = log(HON/lag1(HON));
	return_HUN = log(HUN/lag1(HUN));
	return_JPM = log(JPM/lag1(JPM));
	return_KO  = log(KO/lag1(KO));
	return_MMM = log(MMM/lag1(MMM));
	return_MPC = log(MPC/lag1(MPC));
	return_PEP = log(PEP/lag1(PEP));
	return_SLB = log(SLB/lag1(SLB));
	return_WFC = log(WFC/lag1(WFC));
	return_XOM = log(XOM/lag1(XOM));
* Name the log-return for VV as the response variable;
	response_VV = log(VV/lag1(VV));
run;

* We can use ODS TRACE to print out all of the data sets available to ODS for a particular SAS procedure.;
* We can also look these data sets up in the SAS User's Guide in the chapter for the selected procedure.;
ods output PearsonCorr=portfolio_correlations;
proc corr data=temp;
	var return_:;
	with response_VV;
	title 'Portfolio Correlations';
run; quit;

proc print data=portfolio_correlations; 
	title 'Portfolio Correlations';
run; quit;

data wide_correlations;
set portfolio_correlations (keep=return_:);
run;
* Note that wide_correlations is a 'wide' data set and we need a 'long' data set;
* We can use PROC TRANSPOSE to convert data from one format to the other;
proc transpose data=wide_correlations out=long_correlations;
run; quit;
data long_correlations;
set long_correlations;
tkr = substr(_NAME_,8,3);
drop _NAME_;
rename COL1=correlation;
run;
proc print data=long_correlations; run; quit;

* Merge on sector id and make a colored bar plot;
data sector;
input tkr $ 1-3 sector $ 4-35;
datalines;
AA Industrial - Metals
BAC Banking
BHI Oil Field Services
CVX Oil Refining
DD Industrial - Chemical
DOW Industrial - Chemical
DPS Soft Drinks
GS Banking
HAL Oil Field Services
HES Oil Refining
HON Manufacturing
HUN Industrial - Chemical
JPM Banking
KO Soft Drinks
MMM Manufacturing
MPC Oil Refining
PEP Soft Drinks
SLB Oil Field Services
WFC Banking
XOM Oil Refining
VV Market Index
;
run;
proc print data=sector; run; quit;
proc sort data=sector; by tkr; run;
proc sort data=long_correlations; by tkr; run;
data long_correlations;
merge long_correlations (in=a) sector (in=b);
by tkr;
if (a=1) and (b=1);
run;
proc print data=long_correlations; run; quit;
* Make Grouped Bar Plot;
* p. 48 Statistical Graphics Procedures By Example;
title 'Correlations with the Market Index';
proc sgplot data=long_correlations;
	format correlation 3.2;
	vbar tkr / response=correlation group=sector groupdisplay=cluster datalabel;
run; quit;

proc means data=long_correlations;
	class Sector;
	title 'Correlations by Market Sector';
run; quit;

data return_data;
set temp (keep= return_:);
* What happens when I put this keep statement in the set statement?;
* Look it up in The Little SAS Book;
run;

proc print data=return_data(obs=10); run;

title 'Principal Components Analysis';

proc princomp data=return_data out=pca_output outstat=eigenvectors plots=scree(unpackpanel);
run; quit;

* Notice that PROC PRINCOMP produces a lot of output;
* How many principal components should we keep?;
* Do the principal components have any interpretability?;
* Can we display that interpretability using graphics?;
proc print data=pca_output(obs=10); run;
proc print data=eigenvectors(where=(_TYPE_='SCORE')); run;
* Display the two plots and the Eigenvalue table from the output;
data pca2;
set eigenvectors(where=(_NAME_ in ('Prin1','Prin2')));
drop _TYPE_ ;
run;
proc print data=pca2; run;
proc transpose data=pca2 out=long_pca; run; quit;
proc print data=long_pca; run;
data long_pca;
set long_pca;
format tkr $3.;
tkr = substr(_NAME_,8,3);
drop _NAME_;
run;

proc print data=long_pca; run;
* Plot the first two eigenvectors;
* Note that SAS has been calling them Prin* but giving them type SCORE;

proc sort data=long_pca; by tkr; run;
data long_pca;
merge long_pca (in=a) sector (in=b);
by tkr;
if (a=1) and (b=1);
run;

proc sgplot data=long_pca;
scatter x=Prin1 y=Prin2 / datalabel=tkr group=sector;
run; quit;
* Do we see anything interesting here? Why would we make such a plot?;

********************************************************************;
* Create a training data set and a testing data set from the PCA output;
* Note that we will use a SAS shortcut to keep both of these 'datasets' in one data set that we will call cv_data (cross-validation data). ;
********************************************************************;
data cv_data;
merge pca_output temp(keep=response_VV);
* No BY statement needed here. We are going to append a column in its current order;
* generate a uniform(0,1) random variable with seed set to 123;
u = uniform(123);
if (u < 0.70) then train = 1;
else train = 0;
if (train=1) then train_response=response_VV;
else train_response=.;
run;
proc print data=cv_data(obs=10); run;

* create macros for printing the estimators and calculating MSE and MAE;
%macro data_validation(indata,outdata);
data &outdata.;
  set &indata.;
  err = response_VV - yhat;
  abserr = abs(err);
  sqerr  = (err) ** 2;
%mend;

%macro print_mse_mae(indata,mytitle);
proc means data=&indata. mean min max median sum;
  var abserr sqerr;
  class train;
  title &mytitle;
run; quit;
%mend;

proc reg data = cv_data plots =  diagnostics(unpack) outest = Model_Stocks_est;
  Model_Stocks: model train_response = 
	return_AA return_BAC return_BHI return_CVX 
	return_DD return_DOW return_DPS return_GS 
	return_HAL return_HES return_HON return_HUN 
	return_JPM return_KO return_MMM return_MPC 
	return_PEP return_SLB return_WFC return_XOM 
    / vif;
  output out=Model_Stocks_out (keep=train response_VV yhat) predicted=yhat;
  title 'Model_Stocks';
run;

proc reg data = cv_data plots =  diagnostics(unpack)outest = Model_PCA_est;
  Model_PCA: model train_response = 
	Prin1 Prin2 Prin3 Prin4 
	Prin5 Prin6 Prin7 Prin8 
    / vif;
  output out=Model_PCA_out (keep=train response_VV yhat) predicted=yhat;
  title 'Model_PCA';
run;

%data_validation(indata=%str(Model_Stocks_out),outdata=%str(Model_Stocks_validation));
%print_mse_mae(indata=%str(Model_Stocks_validation),mytitle=%str('Model_Stocks MSE MAE'));

%data_validation(indata=%str(Model_PCA_out),outdata=%str(Model_PCA_validation));
%print_mse_mae(indata=%str(Model_PCA_validation),mytitle=%str('Model_PCA MSE MAE'));
