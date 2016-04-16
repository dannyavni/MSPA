libname mydata 'c:\mspa\410' access=readonly;

ods graphic on;
ods noproctitle;
title 'Assignment 3';

* create a local copy of the data, add additional variables for future work ;
data temp;
  set mydata.ames_housing_data;
  logSalePrice = log(SalePrice);
  AgeAtSale = YrSold - YearBuilt;
  PriceSqft = SalePrice / GrLivArea;
  if (AgeAtSale = 0) then 
    NewBldg = 'Yes';
  else 
    NewBldg = 'No';
run; quit;

* compare sale price of new construction versus rest using boxplot;
proc sgplot data=temp;
  title 'Sale Price by New Construction versus Used';
  vbox SalePrice / category=NewBldg;
run; quit;

proc sgplot data=temp;
  title 'Price per Sqf by New Construction versus Used';
  vbox PriceSqft / category=NewBldg;
run; quit;

* produce water fall of drop off criteria ;
data drop;
	set temp;
	format drop_condition $40.;
	if      (BldgType ne '1Fam')                      then drop_condition='01: Not a Single Family';
	else if (Zoning not in ('RH','RL','RM','FV'))     then drop_condition='02: Non-Residential Zoning';
	else if (GrLivArea > 4000 and SalePrice < 200000) then drop_condition='03: Large Area and Low Sale Price';
    else if (FullBath < 1)                            then drop_condition='04: No Bath';
    else if (LotArea > 20000)                         then drop_condition='05: Lot Area Over 20,000sqf';
    else if (SalePrice > 700000)                      then drop_condition='06: Sale Price Over $700,000';
	else if (NewBldg = 'Yes')                         then drop_condition='07: New Construction';
    else drop_condition='08: Sample Population';
run;

proc freq data = drop;
 tables drop_condition;
 title 'Sample Waterfall';
run; quit;

data keep;
  set drop
  (where = (drop_condition = '08: Sample Population'));
run; quit;

* produce a scatter plot of candidate continous variables and pearson correlation values;
proc sgscatter data=keep;
    compare x=(AgeAtSale GrLivArea LotArea TotalBsmtSF) y=saleprice / loess reg;
	title 'Scatter Plot of Sale Price by Age, Living Area, Lot Area, And Basement Area';
run; quit;

proc corr data=keep;
	var  SalePrice;
	with AgeAtSale GrLivArea LotArea TotalBsmtSF;
run; quit;

proc corr data=keep;
	var  AgeAtSale GrLivArea LotArea TotalBsmtSF;
	with AgeAtSale GrLivArea LotArea TotalBsmtSF;
run; quit;

proc sgscatter data=keep;
    compare x=(AgeAtSale GrLivArea LotArea TotalBsmtSF) y=SalePrice / loess reg;
	title 'Scatter Plot of Sale Price by Age, Living Area, Lot Area, And Basement Area';
run; quit;

* build regression model with paneled results supressed;
* model 1;
proc reg data = keep plots =  diagnostics(unpack);
  model SalePrice = GrLivArea;
  title 'Sale Price by Living Area';
run; quit;

* model 2;
proc reg data = keep plots =  diagnostics(unpack);
  model SalePrice = AgeAtSale;
  title 'Sale Price by Building Age';
run; quit;

* model 3;
* produce leverage and studentized r plot for outlier analysis;
proc reg data = keep plots =  diagnostics(unpack);
  model SalePrice = GrLivArea AgeAtSale / vif;
  output out=outlier (keep=GrLivArea AgeAtSale SalePrice r lev dfitt) h=lev dffits=dfitt rstudent=r;
  title 'Sale Price by Living Area and Bulding Age';
run; quit;

* outlier section - 2213 observations;
data outlier1;
  set outlier;
  absr = abs(r);
  absd = abs(dfitt);
run; quit; 

proc sort data=outlier1;
  by lev;
run; quit;

proc print data=outlier1 (firstobs=2188);
  var lev r dfitt GrLivArea AgeAtSale SalePrice;
  title 'Top 25 Leverage Points for GrLivArea and AgeAtSale';
run; quit;

proc sort data=outlier1;
  by absr;
run; quit;

proc print data=outlier1 (firstobs=2188);
  var r lev dfitt GrLivArea AgeAtSale SalePrice;
  title 'Top 25 Abs(R) Points for GrLivArea and AgeAtSale';
run; quit;

proc sort data=outlier1;
  by absd;
run; quit;

proc print data=outlier1 (firstobs=2188);
  var dfitt lev r GrLivArea AgeAtSale SalePrice;
  title 'Top Abs(DFITTS) Points for GrLivArea';
run; quit;

* produce water fall of outlier drop off criteria ;
data outlier2;
	set outlier1;
	format outlier_def $40.;
	if (absr > 2 and lev > 0.0018) then do;
	  outlier_def = '1. Outlier and Leverage';
	  outlier_code = 1;
	end;
	else if (absr > 2) then do;
	  outlier_def = '2. Outlier';
	  outlier_code = 1;
	end;
	else if (lev > 0.0018) then do;
	  outlier_def = '3. Leverage';
	  outlier_code = 1;
	end;
	else if (dfitt > 0.06) then do;
	  outlier_def = '4. Large DFITTS';
	  outlier_code = 1;
	end;
	else do;
	  outlier_def = '5. Regular Observation';
	  outlier_code = 0;
	end;
run;

proc freq data = outlier2;
  tables outlier_def;
  title 'Sample Waterfall for Outlier Detection';
run; quit;

proc sgscatter data=outlier2;
   plot SalePrice*GrLivArea / group=outlier_def transparency=0.5;
   title 'Scatter Plot of Sale Price to Living Area by Outlier Code';
run;

proc sgscatter data=outlier2;
   plot SalePrice*AgeAtSale / group=outlier_def transparency=0.5;
   title 'Scatter Plot of Sale Price to Age by Outlier Code';
run;

proc sgscatter data=outlier2;
    compare x=(AgeAtSale GrLivArea) y=SalePrice / group=outlier_def transparency=0.5 reg;
	title 'Scatter Plot of Sale Price by Age, Living Area, Lot Area, And Basement Area';
run; quit;

data sans_outlier;
  set outlier2
  (where = (outlier_code = 0));
  logSalePrice = log(SalePrice);
run; quit;

* model 4;
proc reg data = sans_outlier plots =  diagnostics(unpack);
  model SalePrice = GrLivArea AgeAtSale;
  title 'Outlier Removed: Sale Price by Living Area and Bulding Age';
run; quit;

* comparing SalePrice to log(SalePrice) ;

* model 5;
proc reg data = keep plots =  diagnostics(unpack);
  model logSalePrice = GrLivArea AgeAtSale;
  title 'Log Sale Price by Living Area and Bulding Age';
run; quit;

