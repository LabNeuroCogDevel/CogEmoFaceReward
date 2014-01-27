function [subj, subjid]=loadTCRun(fname)
%function to load single-subject behavior data from emotion clock task.
%
%input:
%   fname:  the filename of the .mat file containing subject data
%
%output:
%   subj:   a struct containing relevant behavioral data to be fit

global tdef;

if (exist(fname, 'file') > 0)
    load(fname);
    if ~exist('order','var'); error(strcat('Unable to load order structure from file: ', fname)); end
else
    error(strcat('File does not exist: ', fname));
end


%subject .mat files contain the following elements:
%
%subject <struct>
%   .subj_id: <char>
%   .gender: 'male' or 'female'
%   .age: <numeric>
%   .cb_num: <numeric>           #block counterbalance number
%
%t <numeric>                     #number of trials (504)
%

%order ntrials x 12 <cell array>
%   ,1:  reward function (1 2 3 4)  #1 = CEV; 2 = DEV; 3 = IEV; 4 = CEVR
%   ,2:  session number           #For distinct runs/sessions where each consists of several blocks.
%   ,3:  trial number (1:504)
%   ,4:  block number (1:12)      #Which block within the session. For us blocks consist of 40 trials.
%   ,5:  feedback?                #labeled FeedBck_Colmn in Variables.m, used in SavePredsFmBest.m
%                                    unclear what this does. Currently all 0 in our data.
%   ,6:  trial onset (secs)       #onset of trial, number of seconds since beginning of experiment.
%                                 #N.B. This is actually elapsed seconds / TR (so gives in scan units),
%                                    but for our data, we set TR = 1.0 to eliminate this.
%   ,7:  reward magnitude         #Reward magnitude computed based on response time.
%   ,8:  points obtained          #Payoff reflects mag * prob, so will be zero if prob = 0
%   ,9:  reward probability       #Probability of reward based on response time [0..1]
%   ,10: expected value           #True magnitude * Probability (note that EV doesn't quite match
%                                    col7 x col9 because col7 includes random -5--+5 noise, whereas
%                                    EV is the true magnitude x probability from underlying function.
%   ,11: RT (ms)
%   ,12: emotion                  #1 = happy; 2 = fear; 3 = scrambled


if ~iscell(order)
    error('this function expects a cell array for the order');
end

%initialize a fit matrix that is trials (504) x 12
%trialData: ntrials x 12 <numeric>
%   ,1:  subject number
%   ,2:  counterbalance condition (not used at the moment)
%   ,3:  session (not used at the moment)
%   ,4:  block number (currently 1-12)
%   ,5:  trial number (currently 1-504)
%   ,6:  reaction time
%   ,7:  trial type (contingency)
%   ,8:  score/points obtained; order(:,8)
%   ,9:  emotion; order(:,12)
%   ,10: comt_colmn (not used at the moment)
%   ,11: d2_colmn (not used at the moment)
%   ,12: d32_colmn (not used at the moment)

trialData = NaN(size(order,1), length(fieldnames(tdef)));
trialData(:,tdef.subj) = str2num(subject.subj_id); %subject id
trialData(:,tdef.cb) = 1; %subject.cb_num; %counterbalance condition (not used for now)
trialData(:,tdef.sess) = 1; % session (not used by us at the moment)

%gene data not used currently
%dummy fill with ones
trialData(:, tdef.comt) = 1;
trialData(:, tdef.d2) = 1;
trialData(:, tdef.d32) = 1;

rowDrop = [];
for i=1:size(order,1) % convert trialtypes to same format;
    %fprintf('trial: %d\n', i);
    if ~iscell(order{i})
        %if there are empty fields in the order array, drop them
        rowDrop = [rowDrop; i];
        continue
    end
    
    if strcmp(order{i}{1}, 'CEV')
        trialData(i,tdef.rewFunc) = 1;
    elseif strcmp(order{i}{1}, 'CEVR')
        trialData(i,tdef.rewFunc) = 2;
    elseif strcmp(order{i}{1}, 'DEV')
        trialData(i,tdef.rewFunc) = 3;
    elseif strcmp(order{i}{1}, 'IEV')
        trialData(i,tdef.rewFunc) = 4;    
    end;
    
    %copy reaction times, scores, and emotion to trialData
    trialData(i,tdef.block) = order{i}{4}; % block number
    trialData(i,tdef.trial) = order{i}{3}; % trial number
    trialData(i,tdef.rt) = order{i}{11};   % rt
    trialData(i,tdef.score) = order{i}{8}; % score
    
    % emot, 1=happy, 2= fear, 3=scrambled
    if strcmp(order{i}{12}, 'happy')
        trialData(i,tdef.emo) = 1;
    elseif strcmp(order{i}{12}, 'fear')
        trialData(i,tdef.emo) = 2;
    elseif strcmp(order{i}{12}, 'scram')
        trialData(i,tdef.emo) = 3;
    end
    
end

trialData(rowDrop,:) = []; %drop from trialData structure

%N.B. Even/odd subject ids have opposite order (counter-balance)
%Consequently, blocks are numbered 12..1 or 1..12
%To ensure that the data are fit from the first to last block, re-sort this if needed
if (trialData(1,tdef.block) > trialData(end,tdef.block))
    trialData(:,tdef.block) = trialData(end:-1:1,tdef.block);
end

subjid = str2num(subject.subj_id); %subject id
subj=trialData;

end