%clear all

%PathName ='/home/frankmj/behav/TC/riskpie/';
%File_or_Mat_Name = 'data.txt'; % this is formatted data, one merged file w/ headers Subject, Session, TrialType,RT,score

Use_Drug_Data=0;  % set to 1 if running subjects on/off drugs

% ReadDataFile  %-- for reading new TC file -- can then do "save tc_eeg
% v_trn" and just load matlab matrix from then on

global j k o p l q z r;
j=0;k=0;o=0;p=0;q=0;z=0;l=0; r=0;
global CEV_misc1 CEVR_misc1 DEV_misc1 IEV_misc1;
global CEV_misc2 CEVR_misc2 DEV_misc2 IEV_misc2;
global CEV_misc3 CEVR_misc3 DEV_misc3 IEV_misc3;
global CEV_misc4 CEVR_misc4 DEV_misc4 IEV_misc4;
global CEV_misc5 CEVR_misc5 DEV_misc5 IEV_misc5;

ii=0; % index

for jj=2000:2035 % subjects
    
    for kk = 1:1 % session
    
CEV_misc1 = [];  
CEV_misc2 = [];  
CEV_misc3 = [];   
CEV_misc4 = []; 
CEV_misc5 = [];  
   
CEVR_misc1 = []; 
CEVR_misc2 = []; 
CEVR_misc3 = []; 
CEVR_misc4 = []; 
CEVR_misc5 = [];  
  
DEV_misc1 = []; 
DEV_misc2 = []; 
DEV_misc3 = []; 
DEV_misc4 = []; 
DEV_misc5 = []; 
 
IEV_misc1 = []; 
IEV_misc2 = []; 
IEV_misc3 = []; 
IEV_misc4 = []; 
IEV_misc5 = []; 
 
    
    

if (kk==1)
    if (exist (strcat(num2str(jj),'_tc.mat')) >0)
        load(strcat(num2str(jj),'_tc'))
        ii=ii+1;
    else continue;
    end
else
    if (exist (strcat(num2str(jj),'b_tc.mat')) >0)
        load(strcat(num2str(jj),'b_tc'))
        ii=ii+1;
    else continue;
    end
end
   
     
v_trn = zeros(size(order,1),8);
v_trn(:,1) = subject.cb_num;
v_trn(:,2) = 1; % session

for(i=1:size(order,1)) % convert trialtypes to same format;  
    if order(i,1) == 1 % cev
        v_trn(i,4) = 12;
    elseif  order(i,1) == 2 % dev
        v_trn(i,4) = 56;
elseif  order(i,1) == 3  % iev
        v_trn(i,4) = 78;
elseif  order(i,1) == 4 % cevr
        v_trn(i,4) = 34;
    end;
        
    v_trn(i,3) = order(i,11); % rt
    v_trn(i,5) = order(i,8); % score
    
end

Variables
v_trn(:,comt_colmn)=1; % just init all gene data if dna not avail
v_trn(:,d2_colmn)=1;
v_trn(:,d32_colmn)=1;

generative =0; % generative model that makes its own choices and gets reward rather than fitting subject data.
multstart=1; % multiple starting points 

 

SubjNumbers = unique(v_trn(:,1));
Subj_Sess = unique(v_trn(:,1:2), 'rows');

 

ModelUsed = 'TC';
Filenames_TC
options = optimset(@fmincon);
%options = optimset(options, 'LargeScale', 'off');
 




gp_fit =0; % gp_fit =1 if want to find one best fitting set of params across all subjects

maxloop =  size(Subj_Sess,1);
if gp_fit ==1 maxloop = 1;
end


for subsessnum = 1:maxloop

    %identify the subject and session
    this_subj = Subj_Sess(subsessnum, 1);


    s = Subj_Sess(subsessnum, 2);
    if maxloop>1 disp(['Subject ' num2str(this_subj)]);
    else disp(['Group fit']);
    end

    % pick out the trials corresponding to this subject
    subj_trn = v_trn(find(v_trn(:,1) == this_subj), :);

    % pick out the trials corresponding to this session
    sess_trn = subj_trn(find(subj_trn(:,2) == s), :);


    SubjTrials = sess_trn(:,TrlType_Colmn);
    if(Use_Drug_Data ==1)    d=  sess_trn(1,Drug_Colmn);
        disp(['Drug ' num2str(d)]);
    end;

    init_params = [0.3;0;0.2;0.2;1000;0.6;0.5;300];
    lower_limits = [-1;-50000;0.01;0.01;.1;0;.1;0];
    upper_limits = [1;50000;5;5;5000;5000;5000;10000]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)

     
    if generative ==0
  
        if gp_fit==0
 
            params =[];

            
            if multstart ==1
                num_start_pts =5; % number of initial starting points
                DiffFmOptimal(subsessnum,:) = zeros(num_start_pts,1);

                opts = optimset('fmincon');
                opts.LargeScale = 'off';
            opts.Algorithm = 'active-set';
                opts.Display = 'none';
% 
%                 [params, SE, exitflag,xstart] = rmsearch(@(params) TC_minSE(params, sess_trn), 'fmincon', init_params,lower_limits, upper_limits,'initialsample',num_start_pts,'options',opts) ;
% SEmin(subsessnum)= min(SE);
    
parms_bst = [2001.00000	0.35014	239.21673	0.47988	0.01000 ...
             1401.24889	0.30460	400.42359	1139.30452	33260059.61104
             2003.00000      0.29834 3601.58215      0.27999 0.36464 1620.07181      0.09868 1396.77109      106.27450       62829445.11757
2004.00000	0.26641	1448.04899	0.16763	0.01000	1202.55275	0.07969	3552.16444	944.06564	91782823.53420
2005.00000	0.73545	4625.88768	0.13917	0.01000	559.49915	0.01282	4328.05133	179.12923	87313833.50236
2006.00000	0.19535	0.00000	0.13856	0.80036	1704.75074	0.31480	1517.88383	978.19429	43064247.26618
2008.00000	0.39192	0.00000	0.01000	0.28112	1177.38789	0.08558	1852.58131	363.49138	90234360.62144
2009.00000	0.48645	4080.73564	0.01000	0.01000	1536.18487	0.32005	603.50718	23.16753	42460088.63128
2010.00000	0.23233	916.81294	0.19410	0.05893	1611.08026	0.06346	4294.18671	0.00000	38451019.04105
2011.00000	0.52551	533.02465	0.01000	0.01000	1145.84261	0.19592	2005.37615	125.22060	41217707.96034
2013.00000	0.30835	767.29163	0.01000	0.01000	1616.90245	0.11738	963.14758	1290.39867	51547150.15659
2015.00000	0.49917	546.99993	0.26580	0.08088	1016.43461	0.40270	490.79183	257.75276	13538988.69345
2016.00000	0.27255	0.00000	0.13949	0.01000	1503.86347	0.22107	1003.57758	985.08411	44887396.22423
2018.00000	0.37350	0.00000	0.01000	0.20049	1808.52414	0.27238	395.44447	196.37266	27288142.17081
2019.00000	0.32030	0.00000	0.13790	0.12328	1176.81634	0.30664	1264.41556	480.14083	86188676.28751
2020.00000	0.17256	0.00000	0.01000	0.15515	2058.15022	0.00000	2734.75273	1068.98153	71033726.22891
2021.00000	0.53989	39.78359	0.01000	0.30558	1183.98156	0.23115	479.39926	215.19124	79343042.78347
2023.00000	0.44956	0.00000	0.01000	0.01000	1152.02897	0.07843	2107.33439	644.51079	83457310.83794
2024.00000	0.26117	0.00000	0.03335	0.01000	1390.37245	0.00000	1715.94380	0.00000	85593694.85419
2025.00000	0.35600	0.00000	0.16817	0.01000	1605.54335	0.24686	1950.27808	0.00000	61750704.49395
2026.00000	0.42031	3364.58750	0.25222	0.12839	1507.95437 ...
             0.20300	2084.29446	904.66187	47006832.56536
             2027.00000      0.43906 0.00000 0.01000 0.07049 1596.75573      0.38733 3005.67838      0.00000 57199007.17565
2029.00000      0.39728 1614.93386      0.46586 1.15973 1212.63174      0.11584 594.30765       2288.20687      81598158.29712
2030.00000      0.50387 0.00000 0.46727 0.69712 633.50031       0.18898 2540.12003      414.40487       54201734.65930
2031.00000      0.18360 1858.79425      0.58380 0.21959 1971.99642      0.66512 3251.72706      0.00000 30173398.01854
2032.00000      0.39702 0.00000 0.13551 0.44475 1361.34180      0.02333 3802.73799      0.00000 68354784.63013
];
    

% 101.00000   0.42600 0.00000 0.12518 0.01000 1072.49740  0.00000 485.74919   542.90399   10175.16029
% 102.00000   0.32295 0.00000 0.21060 0.01000 1615.77103  0.00000 3032.89173  956.76891   11059.47214
% 103.00000   0.25954 1498.25705  0.05883 0.08252 1826.56547  0.01726 3449.79422  37.97483    10170.25842
% 104.00000   0.52624 0.00000 0.01000 0.11480 1038.83376  0.01880 442.67229   67.41274    11898.84540
% 105.00000   0.45158 2847.49004  0.35818 0.33658 1188.05566  0.12946 2347.21490  539.27822   12254.55196
% 106.00000   0.32320 2700.21351  0.01000 0.58864 1044.41886  0.04405 4341.03711  766.43023   10547.77151
% 107.00000   0.30207 436.27311   0.01000 0.10475 1745.76958  0.20225 3120.27310  583.72459   11904.77641
% 108.00000   0.29744 3988.31686  0.01000 0.12718 1565.94145  0.17808 1031.26421  95.48134    9656.47728
% 109.00000   0.33794 2135.75682  0.01000 0.03330 1475.04448  0.01210 2277.60551  712.21533   11809.04873
% 110.00000   0.45436 0.00000 0.01000 0.01000 831.04225   0.01946 768.18888   0.00000 13332.35011
% 111.00000   0.12825 184.53849   0.01000 0.10955 1316.40316  0.09377 888.52004   0.00000 5323.88450
% 112.00000   0.30604 0.00000 0.12687 0.01000 1214.28760  0.25035 1128.40644  292.46490   9952.91056
% 113.00000   0.21651 0.00000 0.01000 0.01000 1789.38007  0.38712 2.03347 124.78596   12334.16353
% 114.00000   0.37464 0.00000 0.02708 0.01000 1509.26358  0.20028 4363.41170  295.81201   10180.74388
% 115.00000   0.17357 893.93599   0.37568 0.01000 2173.80788  0.19764 749.59706   1535.87909  8996.05309];

params = parms_bst(ii,2:end-1); SE = parms_bst(ii,end);SEmin(ii)= min(SE);

DiffFmOptimal(ii,:) = SE - SEmin(ii) % how different are the SSE values for each starting pt from optimal one
  
            [SE1 PE exp std_f std_s mn_f mn_s Go NoGo ] =    SavePredsFmBest(params, sess_trn); % save predictions from best run of rmsearch  
              save(strcat('modelVars_',num2str(jj)), 'subject',  'PE', 'exp', 'std_f', 'std_s', 'mn_f', 'mn_s', 'Go', 'NoGo');
            %save PE PE;
            
             
        
                
            else
                % use below line if just want to run one starting point (faster and
                % usually not far off from optimal..)
                [params, SE(subsessnum), exitflag] = fmincon(@(params) TC_minSE(params, sess_trn), init_params, [], [], [], [], lower_limits, upper_limits, [],options) ;
                SavePredsFmBest(params, sess_trn);
               
            end

            RTGene_preds; % save rts and predictions for each genotype and condition... 
            %% (Note here Cev etc get overridden and only count 2nd cev block - if want this need to add code to combine or average blocks of same type)
            
        else
            [params, SE(subsessnum), exitflag, output,lambda,grad,hessian] = fmincon(@(params) TC_minSE(params, v_trn), init_params, [], [], [], [],lower_limits, upper_limits, [],options);
          SavePredsFmBest(params, sess_trn);
               
        end
        
        if(Use_Drug_Data ==1)
            Best_fit_params_Trn(subsessnum, :) = [this_subj s d params' sqrt(SE(subsessnum))]
        else

            if multstart==1
                Best_fit_params_Trn(ii, :) = [str2num(subject.subj_id(1:4)) params(min(find(SE == min(SE))),:) (SEmin(subsessnum))] 

            else
                Best_fit_params_Trn(subsessnum, :) = [this_subj s params' sqrt(SE(subsessnum))] % note these are sqrt of sum!
                SEmin(subsessnum) = SE(subsessnum);
            end

        end;

    else % generative model

        SE = TC_minSE(init_params, sess_trn);
        RTGene_preds;
        Best_fit_params_Trn(subsessnum, :) = [this_subj s sqrt(SE)] % note these are sqrt of sum!

        SEmin = SE;
    end



end
%% Save params

end
end

hdr = {'Subject','Session','lambda','explore','alphaG','alphaL','K','nu','ignore','rho','SSE'};
txt=sprintf('%s\t',hdr{:});
txt(end)='';
dlmwrite(fname_Best_Fit_Param_Trn,txt,'');
dlmwrite(fname_Best_Fit_Param_Trn, Best_fit_params_Trn,'-append','delimiter','\t','precision', '%6.5f');

fid_Trn =fopen(fname_trn,'w');
rSE_Trn_mean = mean(sqrt(SEmin))
rSE_Trn_std = std(sqrt(SEmin))

fprintf(fid_Trn,'%s \t', 'rSE_Trn_mean = ');
fprintf(fid_Trn,'%f \n', rSE_Trn_mean);
fprintf(fid_Trn,'%s \t', 'rSE_Trn_std = ');
fprintf(fid_Trn,'%f \n', rSE_Trn_std);


fclose(fid_Trn);



if gp_fit==0
    MakeFigs_noDNA; %% this automatically generates a bunch of relevant figs
else
    MakeFigs_met;
end

