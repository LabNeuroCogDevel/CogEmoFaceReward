function [totalSqErr, ret_all] = TC_minSE(params, trialData, model, emoSubset)

if nargin < 4, emoSubset = -1; end

global tdef;
global trialsPerBlock;

totalSqErr=0;

%if requested, subset the trialData to only include certain emotions
if emoSubset(1) > -1
    trialData = trialData(find(ismember(trialData(:,tdef.emo), emoSubset)), :);
end

%function can fit either single subject data with one or more sessions or
%group data with many subjects (potentially with many sessions per subject)

%Identify the number of subjects and sessions.
%function will loop over each session separately, summing the squared errors.

Subj_Sess = unique(trialData(:,[tdef.subj tdef.sess]), 'rows'); % number of subjects and sessions

%how many blocks do we have to be fitted
blocks = unique(trialData(:,tdef.block));

ret_all = [];

for s = 1:size(Subj_Sess,1)   % for fitting across group of subs, loop through each one, resetting V, Go/NoGo etc.
    
    %for each subject, reset expected value, go, and no-go to zero
    priors = [];
    priors.V = 0;
    priors.Go = 0;
    priors.NoGo = 0;

    %identify the subject and session (for group fits)
    this_subj =     Subj_Sess(s, 1);
    this_session =  Subj_Sess(s, 2);

    %grand mean RT across all blocks
    avgRT = mean(trialData(trialData(:,tdef.subj) == this_subj & ...
        trialData(:,tdef.sess) == this_session, ...
        tdef.rt));
    
    %fit each block
    for b = 1:length(blocks)
        toFit = trialData( ...
            trialData(:,tdef.subj) == this_subj & ...
            trialData(:,tdef.sess) == this_session & ...
            trialData(:,tdef.block) == blocks(b) ...
            , :);
        
        fitTrials = max(trialsPerBlock, size(toFit,1)); %by default, fit all trials
        
        %rt vector
        rt = toFit(1:min(fitTrials, size(toFit,1)), tdef.rt);
        reward = toFit(1:min(fitTrials, size(toFit,1)), tdef.score);
        %avgBlockRT = mean(rt); %block-specific mean
        rewFunc = toFit(1, tdef.rewFunc); %reward function
        emo = toFit(1, tdef.emo); %emotion
        
        fit_smooth = 0; % fit smoothed data
        window=5;
        mean_dev = 0;

        %can fit deviations from mean RT    
        if mean_dev == 1
            rt = rt - avgRT;
            avgRT = 0; % if mean deviating then average should now be 0.
        end
        
        if(fit_smooth == 1) 
            fitRT = smooth(rt, window);            
        else
            fitRT = rt;
        end
        
        [RTpred, ret] = TC_Alg(fitRT, reward, params, priors, avgRT, rewFunc, emo, model);
        
        ret.block = blocks(b);
        
        % When fitting multiple blocks within a given subject, use expected value from last trial
        % of block t as the expected value of the first trial for block t + 1.
        priors.V = ret.ev(end);
        
        ret_all = [ret_all; ret];
        
        RTSqErr = (fitRT - RTpred).^2;
        
        totalSqErr = totalSqErr + sum(RTSqErr);
    end
       
    %if(length(trialData)>1000) RTGene_preds_Gp; end % only applies if doing group fits..
end