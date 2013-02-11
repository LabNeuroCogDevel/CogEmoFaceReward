

function [RTpred misc1 misc2 misc3 misc4 misc5 misc6 misc7 misc8]=TC_Alg_mine(Response, Reward, Params, avg_RT, cond);

%Response is a t x 1 column vector of reaction times for t trials.
%Reward is a t x 1 column vector of rewards obtained for t trials.
%Params is an 8 x 1 columnn vector of model parameters to be used to fit behavior.
%  
%Params 8 x 1 <numeric>
%   ,1:  lambda           #weight for previous trial RT (autocorrelation of RT_t with RT_t-1)
%   ,2:  explore          #epsilon parameter: how much should RT be modulated by greater relative uncertainty
%                         #    about fast vs. slow responses
%   ,3:  alpha1           #learning rate for positive prediction errors (approach)
%   ,4:  alpha2           #learning rate for negative prediction errors (avoid)
%   ,5:  K                #baseline response speed (person mean RT?)    
%   ,6:  scale            #nu: going for the gold (modulating RT toward highest payoff)
%   ,7:  exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
%   ,8:  meandiff         #rho parameter: 

global Vstart
global Go NoGo
NumTrls_TrlType = length(Response);
RTpred = Response(1)*ones(1,1); % can't predict first choice due to
% learning so just set it to actual
% subject RT and then predict
% starting trial 2

V = Vstart; V_fast = V; V_slow =V;
misc = 0;
joint_ent=1.0;

lambda = Params(1);
explore = Params(2);
alpha1 =  Params(3);
alpha2 = Params(4);
alphaV =  0.1; % just set this to avoid degeneracy
K = Params(5);
scale = Params(6);
decay = 1;  % decay counts for beta distribution 1= nodecay
exp_alt = Params(7); % param for alternative exp models of rt swings
meandiff = Params(8);


Q = 0;

Noise=0;

dist_type = 'beta';

mean_s = 0; mean_f = 0;

% GENERATIVE model just pick some params to generate data
global generative;
if (generative ==1)
    lambda = 0.2;
    explore = 3000;
    alpha1 = .3;
    alpha2 = .3;
    K = 1500;
    exp_alt =500;
    scale = .25;
    meandiff = 1000;
    if dist_type == 'Gauss'
        meandiff=20;
        explore = 10;
    end
    Noise=2000;
end

exp=0;exp1=0; exp1a=0;lose_switch=0; regress=0; mean_short = 0.5; mean_long = 0.5;
RT_avg = avg_RT; % set avg on first trial..

alph_long=1.01; b_long=1.01; % init counters and beta distribution hyperparams..
alph_short=1.01; b_short=1.01;
cnt_short=0; cnt_long=0;
cnt_speed=0; cnt_slow=0;

RT_new = Response(1); % just for init;
RT_last =  Response(1); % just for init
RT_last2 = Response(1); % just for init
RT_last3 = Response(1); % just for init
bestRT= avg_RT; % just for init

var_short = alph_short*b_short/(((alph_short+b_short)^2)*(alph_short+b_short+1));
var_long = alph_long*b_long/(((alph_long+b_long)^2)*(alph_long+b_long+1));
for (trial = 2:NumTrls_TrlType)
    lasttrial = trial-1;
    
    exp1_last = exp1; exp_last = exp; exp1a_last = exp1a;
    means_last = mean_short;
    meanl_last = mean_long;
    vars_last = var_short;
    varl_last = var_long;
    
    if strcmp(dist_type,'Gauss')
        
        vars = vars+ Q; varf = varf + Q; % add process noise to
        % kalman variances (only for
        % kalman filter model)
        
    end
    
    if(generative==1) % if generating responses make last rt the prev predicted rt
        
        if trial > 2 RT_last2= RT_last; end
        RT_last = RT_new;
    else
        
        RT_last = Response(lasttrial);
        if trial > 2 RT_last2 = Response(trial-2); end
        if trial > 3 RT_last3 = Response(trial-3); end
        
    end
    
    
    mom = (RT_last - RT_last2); % momentum
    if mom==0 mom=1; end
    
    Rew_last = Reward(lasttrial);
    if (generative==1)
        Rew_last = RewFunction(RT_last, cond); % calculate reward if model generating own rt's
    end
    
    if RT_last>RT_last2   % Reverse-momentum model
        cnt_slow = cnt_slow +1; cnt_speed =0; % count number of responses slower than previous
    else
        cnt_speed = cnt_speed +1; cnt_slow =0;
    end
    
    V_last = V(lasttrial);
    V_new = V_last +alphaV*(Rew_last - V_last); % update critic value
    
    rew_max = max(Reward(1:lasttrial)); % max reward received in block thus far -- used for v scaling v[RT_best - RT_avg]
    rew_std = std(Reward(1:lasttrial)); % stddev of rewards in block thus far
    
    if Rew_last >V_last && Rew_last>= (rew_max-rew_std) bestRT=RT_last; ...
            % save Rt corresponding to most recent reward within one sd
        % of max
    end;
    
    Go_last = Go(lasttrial);
    NoGo_last = NoGo(lasttrial);
    Go_new = Go_last; NoGo_new = NoGo_last; %  unless updated below by PE
    
    if(Rew_last> V_last)
        
        Go_new = Go_last + alpha1*(Rew_last - V_last);
        
    elseif(Rew_last<=V_last)
        NoGo_new = NoGo_last +alpha2*(V_last - Rew_last);
        
    end
    
    if(RT_last> RT_avg) % last response was slow/long
        cnt_long = cnt_long +1; cnt_short =0; % for sutton exp
        % bonus control model
        
        regress = -exp_alt;  % for simple oscillation regression to
        % mean explore control model
        
        
        
        if strcmp(dist_type,'beta')
            
            if(Rew_last> V_last)
                
                
                alph_long = alph_long +1; % increment count for beta distribution
                
            else
                b_long = b_long+1;
                lose_switch = -exp_alt; % if was slow go fast after neg PE
            end
            
            alph_long=decay*alph_long; % if decay <1 then this decays
            % counts, making beta dists less
            % confident
            b_long=decay*b_long;
            alph_short=decay*alph_short;
            b_short=decay*b_short;
            
            % these are mode and variances of beta dists
            var_short = alph_short*b_short/(((alph_short+b_short)^2)*(alph_short+b_short+1));
            var_long = alph_long*b_long/(((alph_long+b_long)^2)*(alph_long+b_long+1));
            mode_long = (alph_long -1) / (alph_long + b_long -2);
            mode_short = (alph_short -1) / (alph_short + b_short -2);
            mean_long = (alph_long) / (alph_long + b_long);
            mean_short = (alph_short) / (alph_short + b_short);
            
            exp1 = - explore*(sqrt(var_short) - sqrt(var_long));  % speed up if more uncertain about fast responses
            
        elseif strcmp(dist_type,'Gauss')
            
            rewvar = (std(Reward))^2;
             
                alphaKs = vars/(vars+rewvar); % Kalman gain for slow responses
                vars = (1 - alphaKs)*vars; % Kalaman variance for slow resps
        
            
            
            
            mean_s = mean_s + alphaKs*((Rew_last - 0*V_last) - mean_s); ...
                % kalman mean
            
            
            mean_long = mean_s; mean_short=mean_f;
            var_short =varf; var_long = vars;
            
            exp1 = - explore*(sqrt(varf) - sqrt(vars));  % using kalman filter gaussian distributions.
            
        end
        
        if RT_last<RT_last2   && exp1<0 exp1=0; %-exp1; % reset if
            %already explored
            %in this direction
            %last trial (see
            %supplement of  Frank et
            %al 09)
        elseif RT_last>RT_last2   &&exp1>0 exp1=0;
        end;
        
        
    elseif (RT_last<= RT_avg)  % last resp was fast/short
        
        if(RT_last > 0) % only update rew statistics if actually
            % responded, non response is counted as 0
            % in e-prime version
            
            cnt_short = cnt_short +1; cnt_long =0; % for sutton exp
            % bonus control model
            
            regress = +exp_alt; %  regress to mean control model
            
            if strcmp(dist_type,'beta')
                
                if(Rew_last> V_last)
                    
                    
                    alph_short = alph_short +1;
                else
                    b_short = b_short +1;
                    lose_switch =exp_alt; % if was fast, slow down after
                    % neg PE (lose switch control model)
                end
                alph_long=decay*alph_long;
                b_long=decay*b_long;
                alph_short=decay*alph_short;
                b_short=decay*b_short;
                
                % mode and variances of beta distribution
                var_short = alph_short*b_short/(((alph_short+b_short)^2)*(alph_short+b_short+1));
                var_long = alph_long*b_long/(((alph_long+b_long)^2)*(alph_long+b_long+1));
                mode_long = (alph_long -1) / (alph_long + b_long -2);
                mode_short = (alph_short -1) / (alph_short + b_short -2);
                mean_long = (alph_long) / (alph_long + b_long);
                mean_short = (alph_short) / (alph_short + b_short);
                
                
                
                exp1 = + explore*(sqrt(var_long) - sqrt(var_short));
                
            elseif strcmp(dist_type,'Gauss') % for kalman filter model
                
                rewvar = (std(Reward))^2;
                alphaKf = varf / (varf +rewvar);
                varf = (1 - alphaKf)*varf;
                mean_f = mean_f + alphaKf*((Rew_last - 0*V_last) - mean_f);
                mean_short = mean_f;mean_long = mean_s;
                var_short =varf; var_long = vars;
                
                exp1 = + explore*(sqrt(vars) - sqrt(varf));  % using kalman filter normal distributions.
            end
            
            
            if RT_last<RT_last2   && exp1<0 exp1=0;   % reset if
                % already
                % explored in
                % this direction
                % last trial (see
                % supplement of
                % Frank et al 09)
            elseif RT_last>RT_last2   && exp1>0 exp1=0;
            end;
            
        end;
    end;
    
    revmom=0; % for reverse momentum control model
    if cnt_speed > scale revmom=exp_alt*cnt_speed;
    elseif cnt_slow > scale revmom = -exp_alt*cnt_slow;
    end
    
    
    exp = exp_alt*(sqrt(cnt_short)-sqrt(cnt_long)); % sutton
    % exploration
    % bonus model,
    % for control
    % model in
    % supplement
    
    
    
    if (RT_last==0) RT_last = RT_last2; end; %% if last trial there
    %% was no response, use trial before that for updating RT avg and autocorrelation effects (otherwise counted as 0)
    
    RT_avg = RT_avg +alphaV*(RT_last-RT_avg); %update average RT locally...
    
    RT_new = K+ lambda*RT_last - Go_new + NoGo_new  +exp1 + 0*regress ...
        + meandiff*(mean_long-mean_short) + scale*(bestRT-avg_RT)+ Noise*(rand-0.5);
    
    if Response(trial)==0 RT_new = 0; end; %% don't try to predict
    %% non-RTs which are
    %% counted as 0 in e-prime
    
    rtvar = (std(Response))^2;
   
    
    
    if strcmp(dist_type,'Gauss')
        alphaK = varK/(varK+rtvar); % alphaK = kalman gain;
        varK = (1 - alphaK)*varK;
    end
    
    
    RTpred = [RTpred; RT_new]; % add new RT pred to vector
    V = [V; V_new];
    Go = [Go; Go_new];
    NoGo = [NoGo; NoGo_new];
    
    
    % store misc variables (e.g., PE and explore, etc)
    misc1a = Rew_last - V_last; %prediction error
    misc2a =  exp1_last; %explore product: epsilon * (sd diff [slow - fast])
    misc3a = sqrt(vars_last); %sd of short RT
    misc4a = sqrt(varl_last); %sd of long RT
    misc5a = means_last; %mean of short RT
    misc6a = meanl_last; %mean of long RT
    misc7a = Go_last; %mean of Gos
    misc8a = NoGo_last; %mean of NoGos
    
    
    if trial ==2
        misc1 =misc1a;
        misc2=misc2a;
        misc3 =misc3a;
        misc4=misc4a;
        misc5 =misc5a;
        misc6 =misc6a;
        misc7 =misc7a;
        misc8 =misc8a;
        
    else
        misc1=[misc1;  misc1a];
        misc2=[misc2; misc2a];
        misc3=[misc3;  misc3a];
        misc4=[misc4; misc4a];
        misc5=[misc5;  misc5a];
        misc6=[misc6;  misc6a];
        misc7=[misc7;  misc7a];
        misc8=[misc8;  misc8a];
        
        
    end
    
end

% after looping through trials add one last value for last trial to misc


misc1 = [misc1; Reward(trial) - V_new];
misc2 = [misc2; exp1];
misc3 = [misc3; sqrt(var_short)];
misc4 = [misc4; sqrt(var_long)];
misc5 = [misc5; mean_short];
misc6 = [misc6; mean_long];
misc7 = [misc7; Go_new];
misc8 = [misc8; NoGo_new];


Vstart=  V_new;  % set V_start for next block to set up expectation
% for rew values.

Go =  0;  % reinit GN values for next block
NoGo = 0;






