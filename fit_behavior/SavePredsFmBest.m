
%% re-runs TC_minSE but for best fitting params and saves the output
function [mySE misc_a misc_b misc_c misc_d misc_e misc_f misc_g misc_h] = SavePredsFmBest(Params, sess_trn); 
 
global TrlType_Colmn Resp_Colmn FeedBck_Colmn Blk_Colmn;   
global Vstart   
global sess_trn1
global Go NoGo misc; 
global FitTrls;
    
mySE=0;
  
 

Subj_Sess = unique(sess_trn(:,1:2), 'rows'); % number of subject sessions in input vector (this is just 1 if fitting each subject individually)

for subsessnum = 1:size(Subj_Sess,1)   % for fitting across group of subs, loop through each one, resetting V, Go/NoGo etc.

    Vstart=0;
    Go = zeros(1, 1);
    NoGo = zeros(1,1);
    misc =ones(1,1);


    %identify the subject and session (for group fits)
    this_subj = Subj_Sess(subsessnum, 1);
    s = Subj_Sess(subsessnum, 2); 
    subj_trn = sess_trn(find(sess_trn(:,1) == this_subj), :);
    % pick out the trials corresponding to this session 
    sess_trn1 = subj_trn(find(subj_trn(:,2) == s), :);
 
    
    trn_blk1 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 1), :); % first block
    trn_blk2 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 2), :);
    trn_blk3 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 3), :);
    trn_blk4 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 4), :);
    trn_blk5 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 5), :);
    trn_blk6 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 6), :);
    trn_blk7 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 7), :);
    trn_blk8 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 8), :);
    trn_blk9 = sess_trn1(find(sess_trn1(:,Blk_Colmn) == 9), :);
    
    FitTrls= max(40, size(trn_blk1,1)); % by default fit all trials
    
     
    % construct reward and rt vectors
    RT_1 = trn_blk1(1:min(FitTrls,length(trn_blk1)), Resp_Colmn);
    Reward_1= trn_blk1(1:min(FitTrls,length(trn_blk1)),FeedBck_Colmn);

    RT_2 = trn_blk2(1:min(FitTrls,length(trn_blk2)), Resp_Colmn);
    Reward_2= trn_blk2(1:min(FitTrls,length(trn_blk2)),FeedBck_Colmn);

    RT_3 = trn_blk3(1:min(FitTrls,length(trn_blk3)), Resp_Colmn);
    Reward_3= trn_blk3(1:min(FitTrls,length(trn_blk3)),FeedBck_Colmn);

    RT_4 = trn_blk4(1:min(FitTrls,length(trn_blk4)), Resp_Colmn);
    Reward_4= trn_blk4(1:min(FitTrls,length(trn_blk4)),FeedBck_Colmn);

    RT_5 = trn_blk5(1:min(FitTrls,length(trn_blk5)), Resp_Colmn);
    Reward_5= trn_blk5(1:min(FitTrls,length(trn_blk5)),FeedBck_Colmn);

    RT_6 = trn_blk6(1:min(FitTrls,length(trn_blk6)), Resp_Colmn);
    Reward_6= trn_blk6(1:min(FitTrls,length(trn_blk6)),FeedBck_Colmn);

    RT_7 = trn_blk7(1:min(FitTrls,length(trn_blk7)), Resp_Colmn);
    Reward_7= trn_blk7(1:min(FitTrls,length(trn_blk7)),FeedBck_Colmn);

    RT_8 = trn_blk8(1:min(FitTrls,length(trn_blk8)), Resp_Colmn);
    Reward_8= trn_blk8(1:min(FitTrls,length(trn_blk8)),FeedBck_Colmn);


    RT_9 = trn_blk9(1:min(FitTrls,length(trn_blk9)), Resp_Colmn);
    Reward_9= trn_blk9(1:min(FitTrls,length(trn_blk9)),FeedBck_Colmn);
    
    
    
     avgRT = mean(sess_trn1(:,Resp_Colmn));

    fit_smooth =1; % fit smoothed data
    window=5;
    mean_dev =0;


    if(mean_dev==1) RT_1 = RT_1 - avgRT;
        RT_2 = RT_2 - avgRT;
        RT_3 = RT_3 - avgRT;
        RT_4 = RT_4 - avgRT;
        RT_5 = RT_5 - avgRT;
        RT_6 = RT_6 - avgRT;
        RT_7 = RT_7 - avgRT;
        RT_8 = RT_8 - avgRT; 
        RT_9 = RT_9 - avgRT;
    end;

    if mean_dev ==1 avgRT =0; % if mean deviating then average should now be 0.
    end
    
    if(fit_smooth==1) fitRT1 = smooth(RT_1,window);
        fitRT2 = smooth(RT_2,window);
        fitRT3 = smooth(RT_3,window);
        fitRT4 = smooth(RT_4,window);
        fitRT5 = smooth(RT_5,window);
        fitRT6 = smooth(RT_6,window);
        fitRT7 = smooth(RT_7,window);
        fitRT8 = smooth(RT_8,window);
        fitRT9 = smooth(RT_9,window);
        
  
    else fitRT1 =RT_1;
        fitRT2 =RT_2;
        fitRT3 =RT_3;
        fitRT4 =RT_4;
        fitRT5 =RT_5;
        fitRT6 =RT_6;
        fitRT7 =RT_7;
        fitRT8 =RT_8;
        fitRT9 =RT_9;
    end;

    global RTpred;
    RTpred=[];
    global RTpredCEV RTpredCEVR RTpredDEV RTpredIEV;

    global CEV_misc CEVR_misc DEV_misc IEV_misc;

    RTpredCEV=[]; RTpredCEVR= []; RTpredDEV= []; RTpredIEV=[];
    CEV_misc=[]; CEVR_misc= []; DEV_misc= []; IEV_misc=[];

    misc_a =[]; misc_b =[]; misc_c =[]; misc_d =[]; misc_e =[]; misc_f =[];  misc_g =[];  misc_h =[]; 
    
    
    if ~(isempty(trn_blk1))
        
        cond= trn_blk1(1,TrlType_Colmn);
        [RTpred_1  misc_1a misc_1b misc_1c misc_1d misc_1e misc_1f misc_1g misc_1h] = TC_Alg(RT_1, Reward_1, Params, avgRT, cond);
        misc_a = [misc_a; misc_1a];
        misc_b = [misc_b; misc_1b];
        misc_c = [misc_c; misc_1c];
        misc_d = [misc_d; misc_1d];
        misc_e = [misc_e; misc_1e];
        misc_f = [misc_f; misc_1f];
        misc_g = [misc_g; misc_1g];
        misc_h = [misc_h; misc_1h];
        
        RTdiffs1 = (fitRT1 - RTpred_1).^2;
        
        mySE = mySE + sum(RTdiffs1);
        RTpred =[RTpred; RTpred_1];
        ConvertRTs(trn_blk1, RTpred_1, misc_1a, misc_1b, misc_1c, misc_1d, misc_1e);
    end
    
    if ~(isempty(trn_blk2))
        
        cond= trn_blk2(1,TrlType_Colmn);
        [RTpred_2  misc_2a misc_2b misc_2c misc_2d misc_2e misc_2f misc_2g misc_2h] = TC_Alg(RT_2, Reward_2, Params, avgRT, cond);
        misc_a = [misc_a; misc_2a];
        misc_b = [misc_b; misc_2b];
        misc_c = [misc_c; misc_2c];
        misc_d = [misc_d; misc_2d];
        misc_e = [misc_e; misc_2e];
        misc_f = [misc_f; misc_2f];
        misc_g = [misc_g; misc_2g];
        misc_h = [misc_h; misc_2h];
        
        RTdiffs2 = (fitRT2 - RTpred_2).^2;
        
        mySE = mySE + sum(RTdiffs2);
        RTpred =[RTpred; RTpred_2];
        ConvertRTs(trn_blk2, RTpred_2, misc_2a, misc_2b, misc_2c, misc_2d, misc_2e);
    end
    
    if ~(isempty(trn_blk3))
        
        cond= trn_blk3(1,TrlType_Colmn);
        [RTpred_3 misc_3a misc_3b misc_3c misc_3d misc_3e misc_3f misc_3g misc_3h] = TC_Alg(RT_3, Reward_3, Params, avgRT, cond);
        misc_a = [misc_a; misc_3a];
        misc_b = [misc_b; misc_3b];
        misc_c = [misc_c; misc_3c];
        misc_d = [misc_d; misc_3d];
        misc_e = [misc_e; misc_3e];
        misc_f = [misc_f; misc_3f];
        misc_g = [misc_g; misc_3g];
        misc_h = [misc_h; misc_3h];
        
        RTdiffs3 = (fitRT3 - RTpred_3).^2;
        
        mySE = mySE + sum(RTdiffs3);
        RTpred =[RTpred; RTpred_3];
        
        ConvertRTs(trn_blk3, RTpred_3, misc_3a, misc_3b,misc_3c, misc_3d, misc_3e );
    end
    
    
    if ~(isempty(trn_blk4))
        
        cond= trn_blk4(1,TrlType_Colmn);
        [RTpred_4 misc_4a misc_4b misc_4c misc_4d misc_4e misc_4f misc_4g misc_4h] = TC_Alg(RT_4, Reward_4, Params, avgRT, cond);
        misc_a = [misc_a; misc_4a];
        misc_b = [misc_b; misc_4b];
        misc_c = [misc_c; misc_4c];
        misc_d = [misc_d; misc_4d];
        misc_e = [misc_e; misc_4e];
        misc_f = [misc_f; misc_4f];
        misc_g = [misc_g; misc_4g];
        misc_h = [misc_h; misc_4h];
        
        
        RTdiffs4 = (fitRT4 - RTpred_4).^2;
        
        mySE = mySE + sum(RTdiffs4);
        RTpred =[RTpred; RTpred_4];
        
        ConvertRTs(trn_blk4, RTpred_4, misc_4a, misc_4b,misc_4c, misc_4d, misc_4e );
        
        
    end
    
    
    
    
    if ~(isempty(trn_blk5))
        
        cond= trn_blk5(1,TrlType_Colmn);
        [RTpred_5  misc_5a misc_5b misc_5c misc_5d misc_5e misc_5f misc_5g misc_5h] = TC_Alg(RT_5, Reward_5, Params, avgRT, cond);
        misc_a = [misc_a; misc_5a];
        misc_b = [misc_b; misc_5b];
        misc_c = [misc_c; misc_5c];
        misc_d = [misc_d; misc_5d];
        misc_e = [misc_e; misc_5e];
        misc_f = [misc_f; misc_5f];
        misc_g = [misc_g; misc_5g];
        misc_h = [misc_h; misc_5h];
        
        RTdiffs5 = (fitRT5 - RTpred_5).^2;
        
        mySE = mySE + sum(RTdiffs5);
        RTpred =[RTpred; RTpred_5];
        
        ConvertRTs(trn_blk5, RTpred_5, misc_5a, misc_5b,misc_5c, misc_5d, misc_5e );
        
        
    end
    
    if ~(isempty(trn_blk6))
        
        cond= trn_blk6(1,TrlType_Colmn);
        [RTpred_6  misc_6a misc_6b misc_6c misc_6d misc_6e misc_6f misc_6g misc_6h] = TC_Alg(RT_6, Reward_6, Params, avgRT, cond);
        misc_a = [misc_a; misc_6a];
        misc_b = [misc_b; misc_6b];
        misc_c = [misc_c; misc_6c];
        misc_d = [misc_d; misc_6d];
        misc_e = [misc_e; misc_6e];
        misc_f = [misc_f; misc_6f];
        misc_g = [misc_g; misc_6g];
        misc_h = [misc_h; misc_6h];
        
        
        RTdiffs6 = (fitRT6 - RTpred_6).^2;
        
        mySE = mySE + sum(RTdiffs6);
        RTpred =[RTpred; RTpred_6];
        
        ConvertRTs(trn_blk6, RTpred_6, misc_6a, misc_6b,misc_6c, misc_6d, misc_6e );
        
    end
    
    if ~(isempty(trn_blk7))
        
        cond= trn_blk7(1,TrlType_Colmn);
        [RTpred_7 misc_7a misc_7b misc_7c misc_7d misc_7e misc_7f misc_7g misc_7h]  = TC_Alg(RT_7, Reward_7, Params, avgRT, cond);
        misc_a = [misc_a; misc_7a];
        misc_b = [misc_b; misc_7b];
        misc_c = [misc_c; misc_7c];
        misc_d = [misc_d; misc_7d];
        misc_e = [misc_e; misc_7e];
        misc_f = [misc_f; misc_7f];
        misc_g = [misc_g; misc_7g];
        misc_h = [misc_h; misc_7h];
        
        RTdiffs7 = (fitRT7 - RTpred_7).^2;
        
        mySE = mySE + sum(RTdiffs7);
        RTpred =[RTpred; RTpred_7];
        
        ConvertRTs(trn_blk7, RTpred_7, misc_7a, misc_7b,misc_7c, misc_7d, misc_7e );
        
        
    end
    
    
    if ~(isempty(trn_blk8))
        
        cond= trn_blk8(1,TrlType_Colmn);
        [RTpred_8 misc_8a misc_8b misc_8c misc_8d misc_8e misc_8f misc_8g misc_8h] = TC_Alg(RT_8, Reward_8, Params, avgRT, cond);
        
        misc_a = [misc_a; misc_8a];
        misc_b = [misc_b; misc_8b];
        misc_c = [misc_c; misc_8c];
        misc_d = [misc_d; misc_8d];
        misc_e = [misc_e; misc_8e];
        misc_f = [misc_f; misc_8f];
        
        misc_g = [misc_g; misc_8g];
        misc_h = [misc_h; misc_8h];
        
        RTdiffs8 = (fitRT8 - RTpred_8).^2;
        
        mySE = mySE + sum(RTdiffs8);
        RTpred =[RTpred; RTpred_8];
        
        ConvertRTs(trn_blk8, RTpred_8, misc_8a, misc_8b,misc_8c, misc_8d, misc_8e );
        
        
        
    end
    
      if ~(isempty(trn_blk9))
        
        cond= trn_blk9(1,TrlType_Colmn);
        [RTpred_9 misc_9a misc_9b misc_9c misc_9d misc_9e misc_9f misc_9g misc_9h] = TC_Alg(RT_9, Reward_9, Params, avgRT, cond);
        
        misc_a = [misc_a; misc_9a];
        misc_b = [misc_b; misc_9b];
        misc_c = [misc_c; misc_9c];
        misc_d = [misc_d; misc_9d];
        misc_e = [misc_e; misc_9e];
        misc_f = [misc_f; misc_9f];
        
        misc_g = [misc_g; misc_9g];
        misc_h = [misc_h; misc_9h];
        
        RTdiffs9 = (fitRT9 - RTpred_9).^2;
        
        mySE = mySE + sum(RTdiffs9);
        RTpred =[RTpred; RTpred_9];
        
        ConvertRTs(trn_blk9, RTpred_9, misc_9a, misc_9b,misc_9c, misc_9d, misc_9e );
        
        
        
    end
    
    if(length(sess_trn)>1000) RTGene_preds_Gp; end % only applies if doing group fits..
end

mySE = mySE;




