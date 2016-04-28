libname mydata 'c:\mspa\410' access=readonly;

ods graphic on;
ods noproctitle;
title 'Assignment 5';

* create water drop condition to establish sample;
data drop;
	set mydata.ames_housing_data;
	format drop_condition $40.;
	if      (BldgType ne '1Fam')                      then drop_condition='01: Not a Single Family';
	else if (Zoning not in ('RH','RL','RM','FV'))     then drop_condition='02: Non-Residential Zoning';
	else if (GrLivArea > 4000 and SalePrice < 200000) then drop_condition='03: Large Area and Low Sale Price';
    else if (FullBath < 1)                            then drop_condition='04: No Bath';
    else if (LotArea > 20000)                         then drop_condition='05: Lot Area Over 20,000sqf';
    else if (SalePrice > 700000)                      then drop_condition='06: Sale Price Over $700,000';
    else if (SalePrice < 15000)                       then drop_condition='07: Sale Price Below $15,000';
    else if (GrLivArea < 1000)                        then drop_condition='08: Living Area Below 1000sqf';
    else if (GrLivArea > 3000)                        then drop_condition='09: Living Area Above 3000sqf';
    else if (AgeAtSale > 100)                         then drop_condition='10: Age Over 100';
    else if (Functional ne 'Typ')                     then drop_condition='11: Non-Typcial Function';
    else if (SaleCondition ne 'Normal')               then drop_condition='12: Non-Normal Sale'; 
    else drop_condition='99: Sample Population';
    AgeAtSale = YrSold - YearBuilt;
    logSalePrice = log(SalePrice);
run;

proc freq data=drop;
 tables drop_condition;
 title 'Sample Waterfall';
run;

* create a data set of remaining observations with additional dummy variables;
* split the sample population to a 70/30 training/validation observations;
data ind;
    set drop
      (where = (drop_condition = '99: Sample Population'));

    u = uniform(123);
    if (u < 0.70) then train = 1; else train = 0;
    if (train=1) then train_response=SalePrice; else train_response=.;

    total_baths = max(FullBath,0) + max(BsmtFullBath,0);
	total_halfbaths = max(HalfBath,0) + max(BsmtHalfBath,0);
	total_baths_calc = total_baths + total_halfbaths;

	if (AgeAtSale < 5) then new_bldg = 1; else new_bldg = 0;
	if (AgeAtSale > 85) then old_bldg = 1; else old_bldg = 0;

    if (Neighborhood in ('StoneBr','NridgHt','Greens','GrnHill')) then 
      highend_ind = 1;
    else
      highend_ind = 0; 
    if (Neighborhood in ('BrDale','IDOTRR','MeadowV','OldTown','SWISU')) then 
      midend_ind = 0;
	else
	  midend_ind = 1;

	if (Condition1 in ('PosA','PosN') or Condition2 in ('PosA','PosN')) then pos_cond = 1; else pos_cond = 0;
    if (Foundation = 'PConc') then concr_foundation = 1; else concr_foundation = 0;
	if (HeatingQC = 'Ex') then good_heating = 1; else good_heating = 0;
    if (KitchenQual = 'Ex') then excl_kitchen = 1; else excl_kitchen = 0;
	YearsSinceRemodel =  YrSold - YearRemodel;
    if (YearsSinceRemodel >= 0 and YearsSinceRemodel <= 10) then recent_remodel = 1; else recent_remodel = 0;  

	if (SaleCondition = 'Normal') then normal_sale=1; else normal_sale=0;
	if (Functional='Typ') then typ_func=1; else typ_func=0; 
	if (CentralAir='Y') then central_air=1; else central_air=0;
	if (Fireplaces>0) then fireplace_ind=1; else fireplace_ind=0;
	if (GarageCars>0) then garage_ind=1; else garage_ind=0;
    if (BsmtFinType1 = 'GLQ') then good_basement_ind=1; else good_basement_ind=0;

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
run;

* create macros for printing the estimators and calculating MSE and MAE;
%macro print_est(indata,mytitle);
proc print data=&indata.;
title &mytitle.;
run; quit;
%mend;

%macro data_validation(indata,outdata);
data &outdata.;
  set &indata.;
  abserr = abs(SalePrice - yhat);
  sqerr  = (SalePrice - yhat) ** 2;
  if (abserr <= SalePrice * 0.1) then
    Prediction_Grade = 'Grade 1';
  else if (abserr <= SalePrice * 0.15) then
    Prediction_Grade = 'Grade 2';
  else 
    Prediction_Grade = 'Grade 3';
run; quit;
%mend;

%macro print_grade(indata,mytitle);
proc freq data=&indata.;
  tables Prediction_Grade;
  where train=0;
  title &mytitle.;
run; quit;
%mend;

%macro print_mse_mae(indata,mytitle);
proc means data=&indata. mean min max median sum;
  var abserr sqerr;
  class train;
  title &mytitle;
run; quit;
%mend;

* Model_AdjR2: use automatic variable selection with adjusted R squre ;
proc reg data = ind plots =  diagnostics(unpack) outest = Model_AdjR2_est;
  Model_AdjR2: model train_response = 
    GrLivArea LotArea AgeAtSale TotalBsmtSF total_baths_calc 
    TotRmsAbvGrd highend_ind midend_ind good_heating excl_kitchen
	central_air fireplace_ind garage_ind good_basement_ind concr_foundation 
	quality_index  brick_exterior lot_frontage new_bldg old_bldg pos_cond recent_remodel 
    / selection=adjrsq best=1 aic bic cp vif;
  output out=Model_AdjR2_out (keep=train SalePrice yhat) predicted=yhat;
  title 'Model_AdjR2';
run;

* Model_MaxR: use automatic variable selection with MaxR ;
proc reg data = ind plots =  diagnostics(unpack) outest = Model_MaxR_est;
  Model_MaxR: model train_response = 
    GrLivArea LotArea AgeAtSale TotalBsmtSF total_baths_calc 
    TotRmsAbvGrd highend_ind midend_ind good_heating excl_kitchen
	central_air fireplace_ind garage_ind good_basement_ind concr_foundation 
	quality_index  brick_exterior lot_frontage new_bldg old_bldg pos_cond recent_remodel 
    / best=1 selection=maxr aic bic cp vif;
  output out=Model_MaxR_out (keep=train SalePrice yhat) predicted=yhat;
  title 'Model_MaxR';
run;

* Model_MCp: use automatic variable selection with Minimal Mallow's Cp ;
proc reg data = ind plots =  diagnostics(unpack) outest = Model_MCp_est;
  Model_MCp: model train_response = 
    GrLivArea LotArea AgeAtSale TotalBsmtSF total_baths_calc 
    TotRmsAbvGrd highend_ind midend_ind good_heating excl_kitchen
	central_air fireplace_ind garage_ind good_basement_ind concr_foundation 
	quality_index  brick_exterior lot_frontage new_bldg old_bldg pos_cond recent_remodel 
    / best=1 selection=cp best=1 aic bic cp vif;
  output out=Model_MCp_out (keep=train SalePrice yhat) predicted=yhat;
  title 'Model_MCp';
run;

* Model_F: use automatic variable selection with forward selection ;
proc reg data = ind plots =  diagnostics(unpack) outest = Model_F_est;
  Model_F: model train_response = 
    GrLivArea LotArea AgeAtSale TotalBsmtSF total_baths_calc 
    TotRmsAbvGrd highend_ind midend_ind good_heating excl_kitchen
	central_air fireplace_ind garage_ind good_basement_ind concr_foundation 
	quality_index  brick_exterior lot_frontage new_bldg old_bldg pos_cond recent_remodel 
    / best=1 selection=forward slentry=0.1 aic bic cp vif;
  output out=Model_F_out (keep=train SalePrice yhat) predicted=yhat;
  title 'Model_F';
run;

* Model_B: use automatic variable selection with backward selection ;
proc reg data = ind plots =  diagnostics(unpack) outest = Model_B_est;
  Model_B: model train_response = 
    GrLivArea LotArea AgeAtSale TotalBsmtSF total_baths_calc 
    TotRmsAbvGrd highend_ind midend_ind good_heating excl_kitchen
	central_air fireplace_ind garage_ind good_basement_ind concr_foundation 
	quality_index  brick_exterior lot_frontage new_bldg old_bldg pos_cond recent_remodel 
    / selection=backward slstay=0.1 best=1 aic bic cp vif;
  output out=Model_B_out (keep=train SalePrice yhat) predicted=yhat;
  title 'Model_B';
run;

* Model_S: use automatic variable selection with stepwise selection ;
proc reg data = ind plots =  diagnostics(unpack) outest = Model_S_est;
  Model_S: model train_response = 
    GrLivArea LotArea AgeAtSale TotalBsmtSF total_baths_calc 
    TotRmsAbvGrd highend_ind midend_ind good_heating excl_kitchen
	central_air fireplace_ind garage_ind good_basement_ind concr_foundation 
	quality_index  brick_exterior lot_frontage new_bldg old_bldg pos_cond recent_remodel 
    / best=1 selection=stepwise slentry=0.1 slstay=0.1 aic bic cp vif;
  output out=Model_S_out (keep=train SalePrice yhat) predicted=yhat;
  title 'Model_S';
run;

* print model estimates for all six models ;

%print_est(indata=%str(Model_AdjR2_est),mytitle=%str('Model_AdjR2 estimator'));

%print_est(indata=%str(Model_MaxR_est),mytitle=%str('Model_MaxR estimator'));

%print_est(indata=%str(Model_MCp_est),mytitle=%str('Model_MCp estimator'));

%print_est(indata=%str(Model_F_est),mytitle=%str('Model_F estimator'));

%print_est(indata=%str(Model_B_est),mytitle=%str('Model_B estimator'));

%print_est(indata=%str(Model_S_est),mytitle=%str('Model_S estimator'));

* run model in-sample and out-of-sample validation and print results ;

%data_validation(indata=%str(Model_AdjR2_out),outdata=%str(Model_AdjR2_validation));
%print_mse_mae(indata=%str(Model_AdjR2_validation),mytitle=%str('Model_AdjR2 MSE MAE'));

%data_validation(indata=%str(Model_MaxR_out),outdata=%str(Model_MaxR_validation));
%print_mse_mae(indata=%str(Model_MaxR_validation),mytitle=%str('Model_MaxR MSE MAE'));

%data_validation(indata=%str(Model_MCp_out),outdata=%str(Model_MCp_validation));
%print_mse_mae(indata=%str(Model_MCp_validation),mytitle=%str('Model_MCp MSE MAE'));

%data_validation(indata=%str(Model_F_out),outdata=%str(Model_F_validation));
%print_mse_mae(indata=%str(Model_F_validation),mytitle=%str('Model_F MSE MAE'));

%data_validation(indata=%str(Model_B_out),outdata=%str(Model_B_validation));
%print_mse_mae(indata=%str(Model_B_validation),mytitle=%str('Model_B MSE MAE'));

%data_validation(indata=%str(Model_S_out),outdata=%str(Model_S_validation));
%print_mse_mae(indata=%str(Model_S_validation),mytitle=%str('Model_S MSE MAE'));

* print model operational grades ;

%print_grade(indata=%str(Model_AdjR2_validation),mytitle=%str('Model_AdjR2 Prediction Grade'));

%print_grade(indata=%str(Model_MaxR_validation),mytitle=%str('Model_MaxR Prediction Grade'));

%print_grade(indata=%str(Model_MCp_validation),mytitle=%str('Model_MCp Prediction Grade'));

%print_grade(indata=%str(Model_F_validation),mytitle=%str('Model_F Prediction Grade'));

%print_grade(indata=%str(Model_B_validation),mytitle=%str('Model_B Prediction Grade'));

%print_grade(indata=%str(Model_S_validation),mytitle=%str('Model_S Prediction Grade'));


