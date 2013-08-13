%clear all

%initialize global definitions
globdefs

%initialize behavior of fitting function
%model = 'noemo'; %no parameters vary by emotion
%model = 'emoexplore'; %explore epsilon varies by emotion
%model = 'emogonogo'; %go and no go learning rates vary by emotion

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

refit=0; %whether to refit a subject who has already been run.

%loop over and fit all models
models = { 'noemo', 'emoexplore', 'emogonogo', 'emonogo' };

%matlabpool(4)

for m = 1:length(models)
    model = models{m};
    bestFit_all = [];
    
    %for now, just fit per subject
    for f = 1:size(subjdata,1)
    %parfor f = 1:size(subjdata,1)
        %skip
        if (exist(strcat('../outputs/parameter_mat/modelVars_', num2str(subjids(f)), model, '.mat'), 'file') > 0 && ~refit)
            fprintf('Skipping: modelVars_%d%s.mat\n', subjids(f), model);
            load(strcat('../outputs/parameter_mat/modelVars_', num2str(subjids(f)), model));
            bestFit_all(f, :) = bestFit;
            SEmin(f) = bestSE;
            continue
        end
        
        fprintf('Fitting: %d %s\n', subjids(f), model);
        
        %model parameter initialization
        if strcmp(model, 'noemo')
            %NOEMO: basic explore model from Frank NN paper.
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
        elseif strcmp(model, 'emoexplore')
            %EMOEXPLORE: exploration parameter varies by emotion (scrambled, fear, happy)
            %Params 10 x 1 <numeric>
            %   ,1:  lambda           #weight for previous trial RT (autocorrelation of RT_t with RT_t-1)
            %   ,2:  explore_scram    #epsilon parameter for scrambled: how much should RT be modulated by greater relative uncertainty
            %   ,3:  explore_fear     #epsilon parameter for fearful: how much should RT be modulated by greater relative uncertainty
            %   ,4:  explore_happy    #epsilon parameter for happy: how much should RT be modulated by greater relative uncertainty
            %   ,5:  alpha1           #learning rate for positive prediction errors (approach)
            %   ,6:  alpha2           #learning rate for negative prediction errors (avoid)
            %   ,7:  K                #baseline response speed (person mean RT?)
            %   ,8:  scale            #nu: going for the gold (modulating RT toward highest payoff)
            %   ,9:  exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
            %   ,10: meandiff         #rho parameter: weight for expected reward of fast versus slow
            init_params = [ 0.3 ; 2000 ; 2000 ; 2000 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
            lower_limits = [ 0 ; 0 ; 0 ; 0 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
            upper_limits = [1 ; 100000 ; 100000 ; 100000 ; 5 ; 5 ; 5000 ; 5000 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)
        elseif strcmp(model, 'emogonogo')
            %EMOGONOGO: learning rates (go and no go) vary by emotion (scrambled, fear, happy)
            %Params 12 x 1 <numeric>
            %   ,1:  lambda           #weight for previous trial RT (autocorrelation of RT_t with RT_t-1)
            %   ,2:  explore          #epsilon parameter: how much should RT be modulated by greater relative uncertainty
            %   ,3:  alpha1_scram     #learning rate for scrambled positive prediction errors (approach)
            %   ,4:  alpha1_fear      #learning rate for fear positive prediction errors (approach)
            %   ,5:  alpha1_happy     #learning rate for happy positive prediction errors (approach)
            %   ,6:  alpha2_scram     #learning rate for scrambled negative prediction errors (avoid)
            %   ,7:  alpha2_fear      #learning rate for fear negative prediction errors (avoid)
            %   ,8:  alpha2_happy     #learning rate for happy negative prediction errors (avoid)
            %   ,9:  K                #baseline response speed (person mean RT?)
            %   ,10:  scale            #nu: going for the gold (modulating RT toward highest payoff)
            %   ,11:  exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
            %   ,12: meandiff         #rho parameter: weight for expected reward of fast versus slow
            init_params = [ 0.3 ; 2000 ; 0.2 ; 0.2 ; 0.2 ; 0.2 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
            lower_limits = [ 0 ; 0 ; 0.01 ; 0.01 ; 0.01 ; 0.01 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
            upper_limits = [1 ; 100000 ; 5 ; 5 ; 5 ; 5 ; 5 ; 5 ; 5000 ; 5000 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)
        elseif strcmp(model, 'emonogo')
            %EMONOGO: initial fit of emogonogo suggested that the go parameters (PPE) had little variation.
            %         Thus, allow just no go learning rate to vary by condition (scrambled, fear, happy)
            %Params 10 x 1 <numeric>
            %   ,1:  lambda           #weight for previous trial RT (autocorrelation of RT_t with RT_t-1)
            %   ,2:  explore          #epsilon parameter: how much should RT be modulated by greater relative uncertainty
            %   ,3:  alpha1           #learning rate for positive prediction errors (approach)
            %   ,4:  alpha2_scram     #learning rate for scrambled negative prediction errors (avoid)
            %   ,5:  alpha2_fear      #learning rate for fear negative prediction errors (avoid)
            %   ,6:  alpha2_happy     #learning rate for happy negative prediction errors (avoid)
            %   ,7:  K                #baseline response speed (person mean RT?)
            %   ,8:  scale            #nu: going for the gold (modulating RT toward highest payoff)
            %   ,9:  exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
            %   ,10: meandiff         #rho parameter: weight for expected reward of fast versus slow
            init_params = [ 0.3 ; 2000 ; 0.2 ; 0.2 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
            lower_limits = [ 0 ; 0 ; 0.01 ; 0.01 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
            upper_limits = [1 ; 100000 ; 5 ; 5 ; 5 ; 5 ; 5000 ; 5000 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)
        end
        
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
                [params, SE, exitflag, xstart] = rmsearch(@(params) TC_minSE(params, subjdata{f}, model), 'fmincon', init_params, ...
                    lower_limits, upper_limits, 'initialsample', num_start_pts, 'options', opts);
                
                SEmin(f)= min(SE);
                
                DiffFmOptimal(f,:) = SE - SEmin(f); % how different are the SSE values for each starting pt from optimal one
                
                %re-run TC_alg for all blocks with optimal parameters
                [totalSqErr, ret_all] = TC_minSE(params(find(SE == min(SE), 1 ),:), subjdata{f}, model);
                
                %[SEbest, PE, exp, std_f, std_s, mn_f, mn_s, Go, NoGo ] = SavePredsFmBest(params(find(SE == min(SE), 1 ),:), subjdata{f}); % save predictions from best run of rmsearch
                
            else
                % use below line if just want to run one starting point (faster and
                % usually not far off from optimal.)
                [params, SE(f), exitflag] = fmincon(@(params) TC_minSE(params, subjdata{f}, model), init_params, [], [], [], [], lower_limits, upper_limits, [], options) ;
                [totalSqErr, ret_all] = TC_minSE(params(find(SE == min(SE), 1 ),:), subjdata{f}, model); %run once with best params... (can we get all this back from fmincon?)
                %SavePredsFmBest(params, subjdata{f});
                
                SEmin(f) = SE;
            end
        else % generative model, used for generating agent-based behavior
            
            SE = TC_minSE(init_params, subjdata{f}, model);
            %RTGene_preds;
            bestFit_all(f, :) = [this_subj s sqrt(SE)] % note these are sqrt of sum!
            
            SEmin(f) = SE;
        end
        
        if multstart==1
            %1 in this vector represents a dummy code for session (just have one session at the moment)
            bestFit_all(f, :) = [subjids(f) 1 params(find(SE == min(SE), 1 ),:) sqrt(SEmin(f))]; % note errors are sqrt of sum!
        else
            bestFit_all(f, :) = [subjids(f) 1 params' sqrt(SE(f))];
        end
        
        
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
        bestFit = bestFit_all(f, :);
        bestSE = SEmin(f);
        save(strcat('../outputs/parameter_mat/modelVars_', num2str(subjids(f)), model), 'subject',  'PE', 'exp', 'std_f', 'std_s', 'mn_f', 'mn_s', 'Go', 'NoGo', 'ret_all', 'bestFit', 'bestSE');
        
        plotSubjData(subjids(f), ret_all, model);
    end
    
    fname_bestFit=sprintf('SubjsSummary_%s.txt', model);
    if strcmp(model, 'noemo')
        hdr = {'Subject','Session','lambda','explore','alphaG','alphaL','K','nu','ignore','rho','SSE'};
    elseif strcmp(model, 'emoexplore')
        hdr = {'Subject','Session','lambda','explore_scram', 'explore_fear', 'explore_happy', 'alphaG','alphaN','K','nu','ignore','rho','SSE'};
    elseif strcmp(model, 'emogonogo')
        hdr = {'Subject','Session','lambda','explore','alphaG_scram','alphaG_fear','alphaG_happy','alphaN_scram','alphaN_fear', 'alphaN_happy', 'K','nu','ignore','rho','SSE'};
    end
    
    txt=sprintf('%s\t',hdr{:});
    txt(end)='';
    dlmwrite(fname_bestFit,txt,'');
    dlmwrite(fname_bestFit, bestFit_all,'-append','delimiter','\t','precision', '%6.5f');
    
    fname_trn = sprintf('GroupStats_%s.doc', model);
    
    fid_Trn =fopen(fname_trn,'w');
    rSE_Trn_mean = mean(sqrt(SEmin))
    rSE_Trn_std = std(sqrt(SEmin))
    
    fprintf(fid_Trn,'%s \t', 'rSE_Trn_mean = ');
    fprintf(fid_Trn,'%f \n', rSE_Trn_mean);
    fprintf(fid_Trn,'%s \t', 'rSE_Trn_std = ');
    fprintf(fid_Trn,'%f \n', rSE_Trn_std);
    
    fclose(fid_Trn);
    
end

%clear
%matlabpool close