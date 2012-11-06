

function [RTpred misc1 misc2 misc3 misc4 misc5]=TC_Alg(Response,Reward, Params, avg_RT, cond);

global Vstart
global Go NoGo
NumTrls_TrlType = length(Response);
RTpred = Response(1)*ones(1,1); % can't predict first choice due to learning so just set it to actual sub RT
V = Vstart; V_fast = V; V_slow =V;
misc = 0;
joint_ent=1.0;

lambda = Params(1);
explore = Params(2);
alpha1 =  Params(3);
alpha2 = Params(4);
alphaV =  0.1; % just set this
K = Params(5);
nu = Params(6);
exp_alt = Params(7); % param for alternative models of rt swings (revmom, regress, lose-switch)
rho = Params(8);
 

Q = 0; 



 

Noise=0;
dist_type = 'beta'; % can change to Gauss if want to do Kalman filter

vars = (std(Reward))^2;varf = (std(Reward))^2; 
% initial variances of fast/slow resps for kalman filter
% just init to rewvar so initial lr = 0.5
mean_s = 0; mean_f = 0;

% GENERATIVE model
 global generative;

if (generative ==1)
lambda = 0.2;
explore = 3000;
alpha1 = .3;
alpha2 = .3;
K = 1500;
exp_alt =500;
nu = .25;
rho = 1000;
if dist_type == 'Gauss'
    rho=20;
    explore = 10;
end
Noise=2000; % add noise in RT for generative model
end

sutton_exp=0;exp1=0; lose_switch=0; regress=0;

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

for (trial = 2:NumTrls_TrlType)
    lasttrial = trial-1;

    exp1_last = exp1; 
      
     
    vars = vars+ Q; varf = varf + Q; % add process noise to kalman variances..
    
    
    if(generative==1) % if generating responses make last rt the prev predicted rt
    
       if trial > 2 RT_last2= RT_last; end
        RT_last = RT_new;  
    else  
        
    RT_last = Response(lasttrial);     
    if trial > 2 RT_last2 = Response(trial-2); end
    if trial > 3 RT_last3 = Response(trial-3); end

    end
     
     
    mom = (RT_last - RT_last2); % momentum-
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

    rew_max = max(Reward(1:lasttrial)); % max reward received in block thus far
    rew_std = std(Reward(1:lasttrial)); % stddev of rewards in block thus far

    if Rew_last >V_last && Rew_last>= (rew_max-rew_std) bestRT=RT_last; % save Rt corresponding to best reward
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
        cnt_long = cnt_long +1; cnt_short =0; % for sutton exp bonus count how many trials in a row have been long

        regress = -exp_alt;  % regression to the mean


      if strcmp(dist_type,'beta')
          
        if(Rew_last> V_last)
     
            alph_long = alph_long +1; % increment count for beta distribution

        else
            b_long = b_long+1;
            lose_switch = -exp_alt; % if was slow go fast after neg PE
        end


        % these are mode and variances of beta dists
        var_short = alph_short*b_short/(((alph_short+b_short)^2)*(alph_short+b_short+1));
        var_long = alph_long*b_long/(((alph_long+b_long)^2)*(alph_long+b_long+1));
        mode_long = (alph_long -1) / (alph_long + b_long -2);
        mode_short = (alph_short -1) / (alph_short + b_short -2);
        mean_long = (alph_long) / (alph_long + b_long);
        mean_short = (alph_short) / (alph_short + b_short);
 
exp1 = - explore*(sqrt(var_short) - sqrt(var_long));  %explore:  speed up if more uncertain about fast responses
     

      elseif strcmp(dist_type,'Gauss')
        
        rewvar = (std(Reward))^2;
        alphaKs = vars/(vars+rewvar); % Kalman gain for slow responses
        vars = (1 - alphaKs)*vars; % Kalman variance for slow resps
        mean_s = mean_s + alphaKs*((Rew_last - V_last) - mean_s);
             
        mean_long = mean_s; mean_short=mean_f;
        exp1 = - explore*(sqrt(varf) - sqrt(vars));  % using kalman filter gaussian distributions.

      end
         
        if RT_last<RT_last2   && exp1<0 exp1=0; %-exp1; % reset if already explored in this direction last trial (see supp)
        elseif RT_last>RT_last2   &&exp1>0 exp1=0; %-exp1;
        end;



%% fast responses

    elseif (RT_last<= RT_avg)  % last resp was fast/short

        cnt_short = cnt_short +1; cnt_long =0; % for sutton exp bonus
 
        regress = +exp_alt; %  regress mean oscillation

      if strcmp(dist_type,'beta')
          
         if(Rew_last> V_last) 
             
              
            alph_short = alph_short +1;
        else
            b_short = b_short +1;
            lose_switch =exp_alt; % if was fast, slow down after neg PE
        end


        % mode and variances of beta distribution
        var_short = alph_short*b_short/(((alph_short+b_short)^2)*(alph_short+b_short+1));
        var_long = alph_long*b_long/(((alph_long+b_long)^2)*(alph_long+b_long+1));
        mode_long = (alph_long -1) / (alph_long + b_long -2);
        mode_short = (alph_short -1) / (alph_short + b_short -2);
        mean_long = (alph_long) / (alph_long + b_long);
        mean_short = (alph_short) / (alph_short + b_short);

       exp1 = + explore*(sqrt(var_long) - sqrt(var_short)); % explore
      
      elseif strcmp(dist_type,'Gauss') % kalman
        
        rewvar = (std(Reward))^2;
        alphaKf = varf / (varf +rewvar);
        varf = (1 - alphaKf)*varf;
        mean_f = mean_f + alphaKf*((Rew_last - V_last) - mean_f);
        
        mean_short = mean_f;mean_long = mean_s;
        
         exp1 = + explore*(sqrt(vars) - sqrt(varf));  % using kalman filter normal distributions.
          end
         
          
        if RT_last<RT_last2   && exp1<0 exp1=0; %-exp1;  % reset if already explored in this direction last trial (see methods in paper; 
														   % can also add -exp_alt*mom to rt equation below instead of resetting exploration)

        elseif RT_last>RT_last2   && exp1>0 exp1=0; %-exp1;
        end;

    end;


     

      sutton_exp = exp_alt*(sqrt(cnt_short)-sqrt(cnt_long)); % sutton exploration bonus

    

%% Main RT update equation
    RT_new = K+ lambda*RT_last - Go_new + NoGo_new  +exp1 + rho*(mean_long-mean_short) + nu*(bestRT-avg_RT)+ Noise*(rand-0.5) + 0*regress;


    
    rtvar = (std(Response))^2;
     
  
    RT_avg = RT_avg +alphaV*(RT_last-RT_avg); %update average RT locally...

    RTpred = [RTpred; RT_new]; % add new RT pred to vector
    V = [V; V_new];
    Go = [Go; Go_new];
    NoGo = [NoGo; NoGo_new];


    % store misc variables (default: PE and explore, but can change to alph and beta for plotting evolving beta dists)
    misc1a = Rew_last - V_last; % alph_long
    misc2a =  exp1_last; % b_long
    misc3a = Go_last;
    misc4a = NoGo_last;
    

   if RT_last > RT_avg 
      misc5a = 1; %for storing whether resps were slow..

   else 
       misc5a=0;

   end 
   
   if trial ==2
       misc1 =misc1a;
       misc2=misc2a;
       misc3 =misc3a;
       misc4=misc4a;
       misc5 =misc5a;
        
    else
        misc1=[misc1;  misc1a];
        misc2=[misc2; misc2a];
        misc3=[misc3;  misc3a];
        misc4=[misc4; misc4a];
        misc5=[misc5;  misc5a];
    
        
    end

end


   if Response(trial) > RT_avg 
       last_slow=1;

   else 
       last_slow=0;
   end
   
% after looping through trials add one last value for last trial to misc stored vectors

 
misc1 = [misc1; Reward(trial) - V_new]; %  store prediction errors 
misc2 = [misc2; exp1]; % store explore term
misc3 =[misc3; Go_new];
misc4 =[misc4; NoGo_new];
misc5 = [misc5; last_slow];%  for tracking whether responses were fast/slow to plot
%beta params..
 

Vstart=  V_new;  % set V_start for next block (carries over to set expectations).

Go =  0; % reset Go/NoGo values for next block (don't carry over)
NoGo = 0; 






