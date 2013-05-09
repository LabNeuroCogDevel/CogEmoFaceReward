%clear all

%initialize global definitions
globdefs

%initialize behavior of fitting function
ModelUsed = 'TC';
generative = 0; % generative model that makes its own choices and gets reward rather than fitting subject data.
multstart = 1; % use multiple starting points for gradient descent.

%initialize optimizer settings
options = optimset(@fmincon);
%options = optimset(options, 'LargeScale', 'off');

%get a list of subject data to fit

%subject directory is relative to this fit_behavior directory.
subjdir='../subjects/';
subjfiles=dir(strcat(subjdir, '*.mat'));

if isempty(subjfiles)
    error(strcat('Could not find any subject data files in: ', subjdir));
end

subjdata=cell(length(subjfiles),1);
subjconcat=[]; %stores all subject data in one array
subjids=[];
for f = 1:length(subjfiles)
    [subjdata{f}, subjids(f)] = loadTCRun(strcat(subjdir, subjfiles(f).name));
    subjconcat = vertcat(subjconcat, subjdata{f});  
end

%for now, just fit per subject
for f = 1:size(subjdata,1)
    %model parameter initialization
    %Params 8 x 1 <numeric>
    %   ,1:  lambda           #weight for previous trial RT (autocorrelation of RT_t with RT_t-1)
    %   ,2:  explore          #epsilon parameter: how much should RT be modulated by greater relative uncertainty
    %                         #    about fast vs. slow responses
    %   ,3:  alpha1           #learning rate for positive prediction errors (approach)
    %   ,4:  alpha2           #learning rate for negative prediction errors (avoid)
    %   ,5:  K                #baseline response speed (person mean RT?)    
    %   ,6:  scale            #nu: going for the gold (modulating RT toward highest payoff)
    %   ,7:  exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
    %   ,8:  meandiff         #rho parameter: weight for expected reward of fast versus slow
    init_params = [ 0.3 ; 2000 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
    lower_limits = [ 0 ; 0 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
    upper_limits = [1 ; 100000 ; 5 ; 5 ; 5000 ; 5000 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)
    
    if generative == 0
        
        %use multiple starting values?
        if multstart == 1
            num_start_pts = 5; % number of initial starting points
            DiffFmOptimal(f,:) = zeros(num_start_pts,1);
            
            opts = optimset('fmincon');
            opts.LargeScale = 'off';
            opts.Algorithm = 'active-set';
            opts.Display = 'none';
            
            %core fitting function -- returns results of length num_start_pts (5) above.
            %These contain fit estimates for each starting point.
            %Then identify the best-fitting output for use in analyses.
            [params, SE, exitflag, xstart] = rmsearch(@(params) TC_minSE(params, subjdata{f}), 'fmincon', init_params, ...
                lower_limits, upper_limits, 'initialsample', num_start_pts, 'options', opts);
            
            SEmin(f)= min(SE);
            
            DiffFmOptimal(f,:) = SE - SEmin(f); % how different are the SSE values for each starting pt from optimal one
            
            %re-run TC_alg for all blocks with optimal parameters
            [totalSqErr, ret_all] = TC_minSE(params(find(SE == min(SE), 1 ),:), subjdata{f});
            
            %[SEbest, PE, exp, std_f, std_s, mn_f, mn_s, Go, NoGo ] = SavePredsFmBest(params(find(SE == min(SE), 1 ),:), subjdata{f}); % save predictions from best run of rmsearch
            
            %for now, create vectors that correspond to what was present before (for checking against vetted code)
            subject = subjids(f);
            PE = reshape([ret_all.rpe], [], 1);
            exp = reshape([ret_all.explore], [], 1);
            std_f = reshape([ret_all.sdShort], [], 1);
            std_s = reshape([ret_all.sdLong], [], 1);
            mn_f = reshape([ret_all.meanShort], [], 1);
            mn_s = reshape([ret_all.meanLong], [], 1);
            Go = reshape([ret_all.go], [], 1);
            NoGo = reshape([ret_all.noGo], [], 1);
            save(strcat('modelVars_', num2str(subjids(f))), 'subject',  'PE', 'exp', 'std_f', 'std_s', 'mn_f', 'mn_s', 'Go', 'NoGo');
            
        else
            % use below line if just want to run one starting point (faster and
            % usually not far off from optimal.)
            [params, SE(f), exitflag] = fmincon(@(params) TC_minSE(params, subjdata{f}), init_params, [], [], [], [], lower_limits, upper_limits, [], options) ;
            [totalSqErr, ret_all] = TC_minSE(params(find(SE == min(SE), 1 ),:), subjdata{f}); %run once with best params... (can we get all this back from fmincon?)
            %SavePredsFmBest(params, subjdata{f});
            
        end
    else % generative model, used for generating agent-based behavior
        
        SE = TC_minSE(init_params, subjdata{f});
        RTGene_preds;
        bestFit_all(f, :) = [this_subj s sqrt(SE)] % note these are sqrt of sum!
        
        SEmin = SE;
    end
    
    if multstart==1
        %1 in this vector represents a dummy code for session (just have one session at the moment)
        bestFit_all(f, :) = [subjids(f) 1 params(find(SE == min(SE), 1 ),:) sqrt(SEmin(f))]; % note errors are sqrt of sum!
    else
        bestFit_all(f, :) = [subjids(f) 1 params' sqrt(SE(f))];
    end
    
    plotSubjData(subjids(f), ret_all);
end

fname_bestFit='SubjsSummary.txt';
hdr = {'Subject','Session','lambda','explore','alphaG','alphaL','K','nu','ignore','rho','SSE'};
txt=sprintf('%s\t',hdr{:});
txt(end)='';
dlmwrite(fname_bestFit,txt,'');
dlmwrite(fname_bestFit, bestFit_all,'-append','delimiter','\t','precision', '%6.5f');


fname_trn = 'GroupStats.doc';

fid_Trn =fopen(fname_trn,'w');
rSE_Trn_mean = mean(sqrt(SEmin))
rSE_Trn_std = std(sqrt(SEmin))

fprintf(fid_Trn,'%s \t', 'rSE_Trn_mean = ');
fprintf(fid_Trn,'%f \n', rSE_Trn_mean);
fprintf(fid_Trn,'%s \t', 'rSE_Trn_std = ');
fprintf(fid_Trn,'%f \n', rSE_Trn_std);

fclose(fid_Trn);