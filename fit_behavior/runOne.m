
FeedBck_Colmn = 5;
Blk_Colmn = 9;
Resp_Colmn = 3;

subnum = 1000;

subjdir='../subjects/orgfmt/';
if (exist (strcat(subjdir,num2str(subnum),'_tc.mat'), 'file') >0)
    load(strcat(subjdir,num2str(subnum),'_tc'))
else
    fprintf('blow up');
    return
end

v_trn = zeros(size(order,1),8);
v_trn(:,1) = subject.cb_num; %counterbalance condition
v_trn(:,2) = 1; % session (not used by us at the moment)

for i=1:size(order,1) % convert trialtypes to same format;
    if order(i,1) == 1 % cev
        v_trn(i,4) = 1;
    elseif  order(i,1) == 2 % dev
        v_trn(i,4) = 3;
    elseif  order(i,1) == 3  % iev
        v_trn(i,4) = 4;
    elseif  order(i,1) == 4 % cevr
        v_trn(i,4) = 2;
    end;
    
    %copy reaction times, scores, and emotion to v_trn
    v_trn(i,3) = order(i,11);   % rt
    v_trn(i,5) = order(i,8);    % score
    v_trn(i,10) = order(i,12);  % emot, 1=happy, 2= fear, 3=scrambled
end

Variables %load column definitions from Variables.m
v_trn(:,comt_colmn)=1; % just init all gene data if dna not avail
v_trn(:,d2_colmn)=1;
v_trn(:,d32_colmn)=1;

FitTrls= max(42, size(v_trn,1)); % by default fit all trials

trn_blk1 = v_trn(find(v_trn(:,Blk_Colmn) == 1), :); % first block

RT_1 = trn_blk1(1:min(FitTrls,length(trn_blk1)), Resp_Colmn);
Reward_1= trn_blk1(1:min(FitTrls,length(trn_blk1)),FeedBck_Colmn);

avgRT = mean(v_trn(:,Resp_Colmn));


fit_smooth =1; % fit smoothed data
window=5;

fitRT1 = smooth(RT_1,window);

RTpred=[];

cond= trn_blk1(1,TrlType_Colmn);

init_params = [ 0.3 ; 2000 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
lower_limits = [ 0 ; 0 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
upper_limits = [1 ; 100000 ; 5 ; 5 ; 5000 ; 5000 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)

%params based on vetted good fit for pilot subject 1000
good_params = [ 0.60583; 2162.8; .06214; .11034; 809.87557; .27036; 483.7405; 192.7772];
sseExpected = 41044307.29940;

Params=[];
global Go NoGo misc;
global Vstart;
Vstart=0;
Go = zeros(1,1);
NoGo = zeros(1,1);
misc = ones(1,1);
priors.V = 0;
priors.Go = 0;
priors.NoGo = 0;

[RTpred_new, ret_val] = TC_Alg(RT_1, Reward_1, good_params, priors, avgRT, cond, -1);

%[RTpred, misc_1a, misc_1b, misc_1c, misc_1d, misc_1e, misc_1f] = TC_Alg(RT_1, Reward_1, good_params, avgRT, cond);

RTdiffs = (fitRT1 - RTpred).^2; %squared residuals

%mySE = mySE + sum(RTdiffs);
