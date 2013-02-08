
global Prob_Colmn;
global match_Colmn;
global Resp_Colmn;
global TrlType_Colmn;
global FeedBck_Colmn;
global Blk_Colmn;
global Drug_Colmn;
global ModelUsed; 
global ActSelFun; 
global avg_RT;
global comt_colmn d32_colmn d2_colmn;
global generative;

Prob_Colmn = 6;
Resp_Colmn = 3;
TrlType_Colmn = 4;     
FeedBck_Colmn = 5;
Drug_Colmn  = 6;
comt_colmn =6;
d2_colmn=7;
d32_colmn=8;

Group = 'emot';


blk=1; % add block column to data so that order is taken into account for Value comparisons..
        Blk_Colmn = FeedBck_Colmn+4; % +4 because of 3 gene columns: comt (met =1), d2 (t=1), d32 (A=1), even if no gene data these are used as placeholders
emot_colmn = Blk_Colmn+1;

        v_trn(:,Blk_Colmn)=blk;
         for(i=2:length(v_trn))  %% increase block num at start of each cond
            if (v_trn(i,TrlType_Colmn)~=v_trn(i-1,TrlType_Colmn) || v_trn(i,emot_colmn)~=v_trn(i-1,emot_colmn)) blk=blk+1;
            end;
            if v_trn(i,1)~=v_trn(i-1,1) blk=1; % reset blk for new subject (applies only if fitting across group of subs)
            end;
            v_trn(i,Blk_Colmn) = blk;
        end
