PathName = cd;

fname_Best_Fit_Param_Trn =strcat(PathName,'/','Best_fit_parameter_',ModelUsed,'_Trn_',ActSelFun,'.doc');
fname_Best_Qvalues_Param_Trn = strcat(PathName,'/','Best_fit_Qvalues_',ModelUsed,'_Trn_',ActSelFun,'.doc');
fname_trn = strcat(PathName,'/',ModelUsed,'_Trn_',ActSelFun,'.doc');
fname_disp_trn = strcat(ModelUsed,'_Trn_',ActSelFun);
fname_Best_Fit_Param_Tst =strcat(PathName,'/','Best_fit_parameter_',ModelUsed,'_Tst_',ActSelFun,'.doc');
fname_Best_Qvalues_Param_Tst = strcat(PathName,'/','Best_fit_Qvalues_',ModelUsed,'_Tst_',ActSelFun,'.doc');
fname_tst = strcat(PathName,'/',ModelUsed,'_Tst_',ActSelFun,'.doc');
fname_disp_tst = strcat(ModelUsed,'_Tst_',ActSelFun);

fname_mat = strcat(PathName,'/',ModelUsed,'_',ActSelFun,'.mat');
