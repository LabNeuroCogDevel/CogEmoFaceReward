function trigger=defineTrigger()
    global opts
    %% set triggers (for MEG)
    % %%% divide codes into 3 sections
    % %%% 0-20 is for ITI and ISI
    % %%% remaing first half is for face onset (identifies emotionXreward)
    % %%% last half is for score onset (also tied to emotionXreward)
    %   trigger.ITI=10;
    %   trigger.ISI=15;
    %   trigger.done=255;
    %   numcodes=floor(255 - (255)/2)-20; % first section reserved for fixation intervals, last reserved for score
    %   triglen=ceil(numcodes/ ( length(unique(experiment{emotionC}))*length(unique(experiment{rewardC})) ) );
    %   i=1;
    %   for e=unique(experiment{emotionC})'
    %       for rew=unique(experiment{rewardC})
    %           fprintf('%s-%s\n',rew{1},e{1})
    %           trigger.(rew{1}).(e{1}).face  = 20+i*triglen;
    %           trigger.(rew{1}).(e{1}).score = 20+i*triglen + numcodes;
    %           i=i+1;
    %       end
    %   end

    % fixation periods 0-20
    trigger.ITI= 10;
    trigger.ISI= 15;
    trigger.done=255;
    %face 25-130
    trigger.CEV.fear.face  = 29;
    trigger.CEVR.fear.face = 38;
    trigger.DEV.fear.face  = 47;
    trigger.IEV.fear.face  = 56;
    trigger.CEV.happy.face = 65;
    trigger.CEV.scram.face = 101;
    trigger.CEVR.happy.face = 74;
    trigger.DEV.happy.face  = 83;
    trigger.IEV.happy.face  = 92;
    trigger.CEVR.scram.face = 110;
    trigger.DEV.scram.face  = 119;
    trigger.IEV.scram.face  = 128;
    % score 135 - 235 -- face+107
    trigger.CEV.fear.score  = 136;
    trigger.CEVR.fear.score = 145;
    trigger.DEV.fear.score  = 154;
    trigger.IEV.fear.score  = 163;
    trigger.CEV.happy.score = 172;
    trigger.CEVR.happy.score= 181;
    trigger.DEV.happy.score = 190;
    trigger.IEV.happy.score = 199;
    trigger.CEV.scram.score = 208;
    trigger.CEVR.scram.score= 217;
    trigger.DEV.scram.score = 226;
    trigger.IEV.scram.score = 235;
end
