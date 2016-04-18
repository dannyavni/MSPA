* source code for assignment #3 in predictive analytics 410;
libname mydata 'c:\mspa\410' access=readonly;

ods graphic on;
ods noproctitle;
title 'Assignment 3';

* create a local copy of the data, add additional variables for future work ;
data temp;
  set mydata.ames_housing_data;
  PriceSqft = SalePrice / GrLivArea;
  if (AgeAtSale = 0) then 
    NewBldg = 'Yes';
  else 
    NewBldg = 'No';
run; quit;

* scatter plot of living area to sales price;
proc sgscatter data=temp;
    compare x=GrLivArea y=saleprice / loess reg;
	title 'Scatter Plot of Sale Price by Living Area';
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

* create a data set of remaining observations with additional variables;
data keep;
  set drop
  (where = (drop_condition = '08: Sample Population'));
  logSalePrice    = log(SalePrice);
  logGrLivArea    = log(GrLivArea);
  logLotArea      = log(LotArea);
  AgeAtSale = YrSold - YearBuilt;
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

* outlier section - 2315 observations;
data outlier1;
  set outlier;
  absr = abs(r);
  absd = abs(dfitt);
  rprodlev = absr * lev;
run; quit; 

proc sort data=outlier1;
  by lev;
run; quit;

proc print data=outlier1 (firstobs=2306);
  var lev r dfitt GrLivArea AgeAtSale SalePrice;
  title 'Top 10 Leverage Points for GrLivArea and AgeAtSale';
run; quit;

proc sort data=outlier1;
  by rprodlev;
run; quit;

proc print data=outlier1 (firstobs=2306);
  var r lev dfitt GrLivArea AgeAtSale SalePrice;
  title 'Top 10 Abs(R) Points for GrLivArea and AgeAtSale';
run; quit;

proc sort data=outlier1;
  by absr*lev;
run; quit;

proc print data=outlier1 (firstobs=2306);
  var dfitt lev r GrLivArea AgeAtSale SalePrice;
  title 'Top 10 Abs(R)*Leverage Points for GrLivArea';
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

proc sgscatter data=outlier2 datasymbols=(CircleFilled);
   plot SalePrice*GrLivArea / group=outlier_def transparency=0.5 markerattrs=(size=8);
   title 'Scatter Plot of Sale Price to Living Area by Outlier Code';
run;

proc sgscatter data=outlier2 datasymbols=(CircleFilled);
   plot SalePrice*AgeAtSale / group=outlier_def transparency=0.5 markerattrs=(size=8);
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

* comparing SalePrice to log(SalePrice) and keeping only building with age over 15 years ;
* model 5;
proc reg data = keep plots =  diagnostics(unpack);
  model logSalePrice = GrLivArea AgeAtSale;
  title 'Log Sale Price by Living Area and Bulding Age';
run; quit;

* introduce indicators variables ;
data ind;
	set keep;

    total_baths = max(FullBath,0) + max(BsmtFullBath,0);
	total_halfbaths = max(HalfBath,0) + max(BsmtHalfBath,0);
	total_baths_calc = total_baths + total_halfbaths;

    if (Neighborhood in ('StoneBr','NridgHt','Greens','GrnHill')) then 
      highend_ind = 1;
    else
      highend_ind = 0; 
    if (Neighborhood in ('BrDale','IDOTRR','MeadowV','OldTown','SWISU')) then 
      midend_ind = 0;
	else
	  midend_ind = 1;
	if (SaleCondition = 'Normal') then normal_sale=1; else normal_sale=0;
	if (CentralAir='Y') then central_air=1; else central_air=0;
	if (Fireplaces>0) then fireplace_ind=1; else fireplace_ind=0;
	if (GarageCars>0) then garage_ind=1; else garage_ind=0;
	if (BsmtQual in ('Ex','Gd')) or (BsmtCond in ('Ex','Gd')) then good_basement_ind=1; else good_basement_ind=0;
	if (ExterQual='Ex') then ExterQual_Ex=1; else ExterQual_Ex=0;
	if (ExterQual='Gd') then ExterQual_Gd=1; else ExterQual_Gd=0;
	if (ExterQual='TA') then ExterQual_TA=1; else ExterQual_TA=0;
	if (ExterQual='Fa') then ExterQual_Fa=1; else ExterQual_Fa=0;
	if (ExterQual='Po') then ExterQual_Po=1; else ExterQual_Po=0;
	if (Exterior1 in ('BrkComm','BrkFace')) or (Exterior2 in ('BrkComm','BrkFace')) 
	  then brick_exterior=1; else brick_exterior=0;
	if (LotShape in ('Reg','IR1')) then regular_lot=1; else regular_lot=0;
	if (LotConfig='Inside') then lot_inside=1; else lot_inside=0;
	if (LotConfig='Corner') then lot_corner=1; else lot_corner=0;
	if (LotConfig='CulDSac') then lot_culdsac=1; else lot_culdsac=0;
	if (LotConfig in ('FR2','FR3')) then lot_frontage=1; else lot_frontage=0;
	quality_index = OverallCond*OverallQual;
run; quit;

* model 6;
proc reg data = ind plots =  diagnostics(unpack);
  model logSalePrice = GrLivArea LotArea AgeAtSale
    total_baths_calc TotRmsAbvGrd highend_ind midend_ind 
	central_air fireplace_ind garage_ind good_basement_ind
	quality_index  brick_exterior lot_frontage
  / vif;
  title 'Log Sale Price with Original Living Area and Lot Area';
run; quit;

* model 7;
proc reg data = ind plots =  diagnostics(unpack);
  model logSalePrice = logGrLivArea logLotArea AgeAtSale
    total_baths_calc TotRmsAbvGrd highend_ind midend_ind 
	central_air fireplace_ind garage_ind good_basement_ind
	quality_index  brick_exterior lot_frontage
  / vif;
  title 'Log Sale Price by Log Transformed Living Area and Lot Area';
run; quit;

* additional misc plots for appendix;
proc sgplot data=keep;
title 'Sale Price by LotConfig';
vbox SalePrice / category=LotConfig;
run; quit;

proc sgplot data=ind;
title 'Sale Price by High End Ind';
vbox SalePrice / category=highend_ind;
run; quit;

proc sgplot data=ind;
title 'Sale Price by Mid End Ind';
vbox SalePrice / category=midend_ind;
run; quit;
