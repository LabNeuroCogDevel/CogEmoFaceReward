clear all


Use_Drug_Data=0;  % set to 1 if running subjects on/off drugs

% ReadDataFile  %-- for reading new TC file -- can then do "save tc_gene v_trn" and just load matlab matrix from then on
load anson;
Variables

generative =0; % generative model that makes its own choices and gets reward rather than fitting subject data.
multstart=1; % multiple starting points for fmincon

SubjNumbers = unique(v_trn(:,1));
Subj_Sess = unique(v_trn(:,1:2), 'rows');



ModelUsed = 'TC';
Filenames_TC
options = optimset(@fmincon);

global j k o p l q z r;
j=0;k=0;o=0;p=0;q=0;z=0;l=0; r=0;

gp_fit =0; % gp_fit =1 if want to find one best fitting set of params across all subjects

maxloop =  length(Subj_Sess);
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

    init_params = [0.3;2000;0.2;0.2;1000;0.1;0.5;300]; % rmsearch will set different init params throughout interval
    lower_limits = [0;0;0.01;0.01;.1;0;.1;0];
    upper_limits = [1;100000;5;5;5000;5000;5000;10000];


    if generative ==0

        if gp_fit==0

            params =[];


            if multstart ==1
                num_start_pts =3; % number of initial starting points
                DiffFmOptimal(subsessnum,:) = zeros(num_start_pts,1); % this will indicate  how different each squared error is from the best one found

                opts = optimset('fmincon');
                opts.LargeScale = 'off';
                opts.Algorithm = 'active-set';
                opts.Display = 'none';

                [params, SE, exitflag,xstart] = rmsearch(@(params) TC_minSE(params, sess_trn), 'fmincon', init_params,lower_limits, upper_limits,'initialsample',num_start_pts,'options',opts) ;

                SavePredsFmBest(params(min(find(SE == min(SE))),:), sess_trn); % save predictions from best run of rmsearch

                SEmin(subsessnum)= min(SE);
                DiffFmOptimal(subsessnum,:) = SE - SEmin(subsessnum) % how different are the SSE values for each starting pt from optimal one



            else
                % use below line if just want to run one starting point (faster and
                % usually not far off from optimal..)
                [params, SE(subsessnum), exitflag] = fmincon(@(params) TC_minSE(params, sess_trn), init_params, [], [], [], [], lower_limits, upper_limits, [],options) ;
                SavePredsFmBest(params, sess_trn);

            end

            RTGene_preds; % save rts and predictions for each genotype and condition...

        else
            [params, SE(subsessnum), exitflag, output,lambda,grad,hessian] = fmincon(@(params) TC_minSE(params, v_trn), init_params, [], [], [], [],lower_limits, upper_limits, [],options);
            SavePredsFmBest(params, sess_trn);

        end


        if multstart==1
            Best_fit_params_Trn(subsessnum, :) = [this_subj s params(min(find(SE == min(SE))),:) sqrt(SEmin(subsessnum))] % note these are sqrt of sum!

        else
            Best_fit_params_Trn(subsessnum, :) = [this_subj s params' SE(subsessnum)]
            SEmin(subsessnum) = SE(subsessnum);
        end


    else % generative model

        SE = TC_minSE(init_params, sess_trn);
        RTGene_preds;
        Best_fit_params_Trn(subsessnum, :) = [this_subj s SE] 

        SEmin = SE;
    end



end
%% Save params

hdr = {'Subject','Session','lambda','explore','alphaG','alphaL','K','nu','extra','rho','SSE'};
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
    MakeFigs; %% this automatically generates a bunch of relevant figs
else
    MakeFigs_met;
end

