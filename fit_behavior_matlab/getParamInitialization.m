function [init_params, lower_limits, upper_limits] = getParamInitialization(model)
%model parameter initialization
if strcmp(model, 'noemo') || strcmp(model, 'noemo_scram')
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
    hdr = {'Subject','Session','lambda','explore','alphaG','alphaN','K','nu','ignore','rho','SSE'};
elseif strcmp(model, 'noemosticky') || strcmp(model, 'noemosticky_scram')
    %NOEMOSTICKY params 8 x 1 <numeric>
    %    ,1:  lambda           #weight for sticky choice (weight influencing prior RTs' effect on
    %                              current RT. This is a decaying function of RT history; see sticky_decay)
    %    ,2:  explore          #epsilon parameter: how much should RT be modulated by greater relative uncertainty
    %                              about fast vs. slow responses
    %    ,3:  alpha1           #learning rate for positive prediction errors (approach)
    %    ,4:  alpha2           #learning rate for negative prediction errors (avoid)
    %    ,5:  K                #baseline response speed (person mean RT?)
    %    ,6:  sticky_decay          #d: decay parameter influencing the degree to which prior RTs continue to affect current RTs
    %    ,7:  exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
    %    ,8:  meandiff         #rho parameter: weight for expected reward of fast versus slow
    init_params = [ 0.3 ; 2000 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
    lower_limits = [ 0 ; -100000 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
    upper_limits = [1 ; 100000 ; 5 ; 5 ; 5000 ; 1 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)
    hdr = {'Subject','Session','lambda','explore','alphaG','alphaN','K','sticky_decay','ignore','rho','SSE'};
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
    hdr = {'Subject','Session','lambda','explore_scram', 'explore_fear', 'explore_happy', 'alphaG','alphaN','K','nu','ignore','rho','SSE'};
elseif strcmp(model, 'emoexploresticky')
    %EMOEXPLORESTICKY: exploration parameter varies by emotion (scrambled, fear, happy)
    %In addition, epsilon allowed to be negative and sticky choice implementation to account
    %for prior RT effect on current RT.
    %Params 10 x 1 <numeric>
    %
    %   ,1:  lambda           #weight for sticky choice (weight influencing prior RTs' effect on
    %                              current RT. This is a decaying function of RT history; see sticky_decay)
    %   ,2:  explore_scram    #epsilon parameter for scrambled: how much should RT be modulated by greater relative uncertainty
    %   ,3:  explore_fear     #epsilon parameter for fearful: how much should RT be modulated by greater relative uncertainty
    %   ,4:  explore_happy    #epsilon parameter for happy: how much should RT be modulated by greater relative uncertainty
    %   ,5:  alpha1           #learning rate for positive prediction errors (approach)
    %   ,6:  alpha2           #learning rate for negative prediction errors (avoid)
    %   ,7:  K                #baseline response speed (person mean RT?)
    %   ,8:  sticky_decay     #d: decay parameter influencing the degree to which prior RTs continue to affect current RTs
    %   ,9:  exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
    %   ,10: meandiff         #rho parameter: weight for expected reward of fast versus slow
    init_params = [ 0.3 ; 2000 ; 2000 ; 2000 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
    lower_limits = [ 0 ; -100000 ; -100000 ; -100000 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
    upper_limits = [1 ; 100000 ; 100000 ; 100000 ; 5 ; 5 ; 5000 ; 1 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)
    hdr = {'Subject','Session','lambda','explore_scram', 'explore_fear', 'explore_happy', 'alphaG','alphaN','K','sticky_decay','ignore','rho','SSE'};
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
    hdr = {'Subject','Session','lambda','explore','alphaG_scram','alphaG_fear','alphaG_happy','alphaN_scram','alphaN_fear', 'alphaN_happy', 'K','nu','ignore','rho','SSE'};
elseif strcmp(model, 'emogonogosticky')
    %EMOGONOGOSTICKY: learning rates (go and no go) vary by emotion (scrambled, fear, happy)
    %sticky choice model where epsilon (explore) can be negative and past RTs influence
    %current RTs via a decayed function of prior RTs.
    %Params 12 x 1 <numeric>
    %   ,1:  lambda           #weight for sticky choice (weight influencing prior RTs' effect on
    %                              current RT. This is a decaying function of RT history; see sticky_decay)
    %   ,2:  explore          #epsilon parameter: how much should RT be modulated by greater relative uncertainty
    %   ,3:  alpha1_scram     #learning rate for scrambled positive prediction errors (approach)
    %   ,4:  alpha1_fear      #learning rate for fear positive prediction errors (approach)
    %   ,5:  alpha1_happy     #learning rate for happy positive prediction errors (approach)
    %   ,6:  alpha2_scram     #learning rate for scrambled negative prediction errors (avoid)
    %   ,7:  alpha2_fear      #learning rate for fear negative prediction errors (avoid)
    %   ,8:  alpha2_happy     #learning rate for happy negative prediction errors (avoid)
    %   ,9:  K                #baseline response speed (person mean RT?)
    %   ,10: sticky_decay     #d: decay parameter influencing the degree to which prior RTs continue to affect current RTs
    %   ,11: exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
    %   ,12: meandiff         #rho parameter: weight for expected reward of fast versus slow
    init_params = [ 0.3 ; 2000 ; 0.2 ; 0.2 ; 0.2 ; 0.2 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
    lower_limits = [ 0 ; -100000 ; 0.01 ; 0.01 ; 0.01 ; 0.01 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
    upper_limits = [1 ; 100000 ; 5 ; 5 ; 5 ; 5 ; 5 ; 5 ; 5000 ; 1 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)
    hdr = {'Subject','Session','lambda','explore','alphaG_scram','alphaG_fear','alphaG_happy','alphaN_scram','alphaN_fear', 'alphaN_happy', 'K','sticky_decay','ignore','rho','SSE'};
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
    hdr = {'Subject','Session','lambda','explore','alphaG', 'alphaN_scram', 'alphaN_fear', 'alphaN_happy', 'K','nu','ignore','rho','SSE'};
elseif strcmp(model, 'emonogosticky')
    %EMONOGOSTICKY: initial fit of emogonogo suggested that the go parameters (PPE) had little variation.
    %         Thus, allow just no go learning rate to vary by condition (scrambled, fear, happy)
    %Params 10 x 1 <numeric>
    %   ,1:  lambda           #weight for sticky choice (weight influencing prior RTs' effect on
    %                              current RT. This is a decaying function of RT history; see sticky_decay)
    %   ,2:  explore          #epsilon parameter: how much should RT be modulated by greater relative uncertainty
    %   ,3:  alpha1           #learning rate for positive prediction errors (approach)
    %   ,4:  alpha2_scram     #learning rate for scrambled negative prediction errors (avoid)
    %   ,5:  alpha2_fear      #learning rate for fear negative prediction errors (avoid)
    %   ,6:  alpha2_happy     #learning rate for happy negative prediction errors (avoid)
    %   ,7:  K                #baseline response speed (person mean RT?)
    %   ,8:  sticky_decay          #d: decay parameter influencing the degree to which prior RTs continue to affect current RTs
    %   ,9:  exp_alt          #alternative exponential models for RT swings (not sure of its use yet)
    %   ,10: meandiff         #rho parameter: weight for expected reward of fast versus slow
    init_params = [ 0.3 ; 2000 ; 0.2 ; 0.2 ; 0.2 ; 0.2 ; 1000 ; 0.1 ; 0.5 ; 300 ];
    lower_limits = [ 0 ; -100000 ; 0.01 ; 0.01 ; 0.01 ; 0.01 ; .1 ; 0 ; .1 ; 0 ];
    upper_limits = [1 ; 100000 ; 5 ; 5 ; 5 ; 5 ; 5000 ; 1 ; 5000 ; 10000 ]; % for rmsearch set min/max to 0 for unused params (otherwise spits out weird values that aren't used)
    hdr = {'Subject','Session','lambda','explore','alphaG', 'alphaN_scram', 'alphaN_fear', 'alphaN_happy', 'K','sticky_decay','ignore','rho','SSE'};
end

end