% CogEmo Face Reward Task:
%
%
%  usage:
%   CogEmoFaceReward
%   % OR %
%   CogEmoFaceReward('usebox','com3')
%
%    http://arnold/dokuwiki/doku.php?id=howto:experiments:cogemofacereward
%
%  cd B:/bea_res/Personal/Will/CogEmoFaceReward
%  CogEmoFaceReward
%
%  Testing:
%  load subjects/test_tc.mat                                          % load everything the presentation saves
%  trialnum=597                                                       % set the trial number to be tested
%  subject.run_num=2                                                  % trial number > mid-way (300), trial==2
%  save('subjects/test_tc.mat','order','trialnum','subject','score'); % save new settings
%  CogEmoFaceReward
%     Enter the subject ID number: test
%     Is this a restart/want to load old file (y or n)? y
%
% TODO/DONE
%
%  [x] response box -- http://docs.psychtoolbox.org/CedrusResponseBox
%  [ ] time of score presentation -- NULL is actually a wait time? 0-12secs
%      most frequent is 2 in Frank code
%
%  [-] append total score to results file?
%  [x] between block message !?
%  [x] clear textures? -- preload textures would be better
%  [x] better/less intrusive indicator? -- larger border, otherwise fine
%  [x] reshape scamb faces
%  [x] count down
%  [x] 80% grey bg with central sphere
%  [x] increase fixation size by 25%
%  [x] darken dot by 25%
%
% 12/05
%  [x] counterbalance odd subjects by reversing order
%  [x] TR=1
%  [x] output includes emotion and face file
%  [x] had neutral, needed fear faces (R script)
%  [x] fear/happy faces to open mount (prev. closed b/c neutral
%                                      but using scram instead)
% 12/06
%  [x] remaining time /2
%  [x] session number drops in a start or half way pt
%  []  fix:if exit on next instruction screen, will redo previous on resume
%
% 12/10 
%  [x] add sound to no response
%
% 01/06
%  [ ] check new .csv file. have 12 instead of 18 runs
%
% 2013/03/26
%  [x] add conditional bonus message
%  [x] change finish screen to not crash? -- was it doing this before
%  [MH] fix trial number reporting (was hardcoded)  -- use experement{4}
%  [MH] block change done by mod trialsInBlock, was hard coded
%  [x] merge with other changes on github
%
% 2013/11/20
%  [WF] add response box
% 2013/12/03
%  [WF] use response box timing

function CogEmoFaceReward(varargin)
  %% CogEmoFaceReward
  % WF 2012-10-05
  % 
  % read from FaceITI.csv 
  %  get facenum,ITIs,emotion, and reward for each presentation
  %
  % 40 presentations per emotion_reward trial
  % 20 faces repeated twice for each
  % there are 3 emotions {fear,neut,happy} and three reward functions {inc,dec,const}
  % each emotion_reward trial is done twice
  % 40*2*(3*3) = 720 presentations
  %
  % each presentation can last upto 4 seconds
  % the subject can hit space at any time
  % reward is calcluated based on the time allowed to elapse and the current reward function
  %
  % score function from M. Frank
  % output emulates that of his timeconflict.m
  %
  % This used 'KbDemo' as template
  
  %screenResolution=[640 480]; %basic VGA
  %screenResolution=[1600 1200];
  %screenResolution=[1440 900]; %new eyelab room
  screenResolution=[1680 1050]; %mac laptop

  
  %% do we use a button box? get it from the options
  % CogEmoFaceReward('usebox','COM3')
  resppad=[];
  if(any(cellfun(@(x) any(strmatch(x,'usebox')),varargin)))
    fprintf('USING RESPOSE BOX\n')
    try; CedrusResponseBox('CloseAll'); end
    portargidx=find(cellfun(@(x) any(strmatch(x,'usebox')),varargin)) + 1;
    if(length(varargin)<portargidx || ~ any(regexpi(varargin{portargidx},'COM|dev')) )
      error('arguement after usebox should be either e.g. COM1 (win) or /dev/ttyUSB0 (linux)')
    end
    port=varargin{portargidx};
    try
     %% sometimes opening the the device seems to lead to
     %% an endless loop. Works when dev is *USB0
     resppad = CedrusResponseBox('Open',port );
     dev     = CedrusResponseBox('GetDeviceInfo', resppad);
     evt     = CedrusResponseBox('GetBaseTimer',resppad);
     boxMcomptime = evt.basetimer - evt.ptbreceivetime;
     fprintf('time difference box - comp: %f\n',boxMcomptime);
     % eprime likes to have pin one toggled, set this
     %CedrusResponseBox('SetOutputLineLevels',resppad, [);
    catch
     error('could not open response box!')
    end

  end
  
  %buyer beware: do not uncomment this for production use
  %Screen('Preference', 'SkipSyncTests', 1);
  
  % [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 255 255 255], [0 0 640 480] );
     % [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [0 0 1600 1200] );
     % [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [0 0 1440 900] );
     
  
  start       = 1;
  TR          = 1.0;
  %Set3 from Color Brewer
  %only provides 12 colors
%   blockColors = [141 211 199; ...
%       255 255 179; ...
%       190 186 218; ...
%       251 128 114; ...
%       128 177 211; ...
%       253 180 98; ...
%       179 222 105; ...
%       252 205 229; ...
%       217 217 217; ...
%       188 128 189; ...
%       204 235 197; ...
%       255 237 111];
  blockColors = round(255*hsv(24)); % a different color for each block of trials
  blockColors = blockColors(randperm(24),:); % randperm(24) should prob be replaced by a pre-made vector
  txtfid      = 0; %just so we know the file pntr's not a private nested function var
  
  receiptDuration  = 1.4; %show feedback for 1400ms
  timerDuration    = 4;
  
  % initialize total points earned
  % incremented as a function (inc,dec,const)
  score = 0;
  blockTotal = 0;
  
  
  %% set order of trials
  %  read in order of things
  % note, only one of these (Frank had 8)
  fid=fopen('FaceITI.csv');
  indexes={1,2,3,4,5,6};
  [ facenumC, ITIC, ISIC, blockC, emotionC, rewardC ] = indexes{:};
  experiment=textscan(fid,'%d,%d,%d,%d,%q','HeaderLines',1);
   % ugly unpack of " "," "
  for i=1:length(experiment{5})
      experiment{6}{i} = experiment{5}{i}(findstr(experiment{5}{i},',')+2:end);
      experiment{5}{i} = experiment{5}{i}(1:findstr(experiment{5}{i},',')-1    );
  end
  fclose(fid);


  %% start recording data
  % sets txtfid, subject.*, start, etc  

  totalSessions = 2; %broken into first and second halves
  halfwaypt=floor(length(experiment{blockC})/totalSessions); % 252
  
  % how long (trials) is a block
  [~,blockchangeidx] = unique(experiment{blockC});
  trialsPerBlock     = unique(diff(blockchangeidx)); % 42
  if(length(trialsPerBlock) > 1) 
      fprintf('Whoa!? different trial lengths? I dont know whats goign on!')
      trialsPerBlock = trialsPerBlock(1);
  end

  totalBlocks = length(experiment{blockC})/trialsPerBlock; % 12

  getSubjInfo

  % print the top of output file
  if start == 1
    fprintf(txtfid,'#Subj:\t%s\n', subject.subj_id);
    fprintf(txtfid,'#Run:\t%i\n',  subject.run_num); 
    fprintf(txtfid,'#Age:\t%i\n',  subject.age);
    fprintf(txtfid,'#Gender:\t%s\n',subject.gender);
    % allocate space
    subject.resptime.comp   =zeros(length(experiment{1}),1);
    subject.resptime.resppad=zeros(length(experiment{1}),1);
    subject.reward          =zeros(length(experiment{1}),1);
  end
  
  % always print date .. even though it'll mess up reading data if put in the middle
  fprintf(txtfid,'#%s\n',date);
  
  %% Counter balance 
  % by reversing order for odd subjects
  if mod(str2double(subject.subj_id),2)==1
      fprintf('NOTE: odd subject, order reversed of input csv!\n')
      for i=1:length(experiment)
          experiment{i}=experiment{i}(end:-1:1);
      end
  end
  
  %% debug timing -- get expected times
  % add the ITI,ISI, timer duration, and score presentation
  %expectedTime = sum(cell2mat(experiment([ITIC ISIC])),2)/10^3 + timerDuration + receiptDuration;


  %% launch presentation   
  try
  
     %% setup screen
     % Removes the blue screen flash and minimize extraneous warnings.
     % http://psychtoolbox.org/FaqWarningPrefs
     Screen('Preference', 'Verbosity', 2); % remove cli startup message 
     Screen('Preference', 'VisualDebugLevel', 3); % remove  visual logo
     %Screen('Preference', 'SuppressAllWarnings', 1);
  	
     % Find out how many screens and use smallset screen number.
     
     % Open a new window.
     [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [0 0 screenResolution] );
     % [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [] );
     
     %permit transparency
     Screen('BlendFunction', w, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
     
     % Set text display options. We skip on Linux.
     %if ~IsLinux
         Screen('TextFont', w, 'Arial');
         Screen('TextSize', w, 22);
     %end
  
     % Set colors.
     black = BlackIndex(w);
     %white = WhiteIndex(w);
     
     % Enable unified mode of KbName, so KbName accepts identical key names on
     % all operating systems:
     KbName('UnifyKeyNames');

     % Set keys.
     %spaceKey  = KbName('SPACE');
     escKey  = KbName('ESCAPE');
     %% TODO: how to get '^' for getting scanner start
     scannerStart = GetSecs;
  
     
    
     %% preload textures
     % makes assumption that images for every face of every facenumber
     % exists
     for emo=unique(experiment{emotionC})'
         for facenum=unique(experiment{facenumC})'
            stimfilename=strcat('faces/',emo{1},'_',num2str(facenum),'.png');
            [imdata colormap alpha]=imread(stimfilename);
            imdata(:, :, 4) = alpha(:, :); %add alpha information
            % make texture image out of image matrix 'imdata'
            facetex.(emo{1}){facenum} = Screen('MakeTexture', w, imdata);
         end
     end


     %% setup sound
     % http://docs.psychtoolbox.org/PsychPortAudio
     % http://wiki.stdout.org/matlabcookbook/Presenting%20auditory%20stimuli/Playing%20sounds/
     
     InitializePsychSound;
     [wavedata, sndFreq] = wavread('incorrect.wav');
     wavedata=wavedata';
     nrchannels = size(wavedata,1);
     % 2nd to last arg should be sndFreq, but portaudio returns error w/it
     pahandle= PsychPortAudio('Open', [], [], [], [], nrchannels);
     PsychPortAudio('FillBuffer',pahandle,wavedata);
     
     
     

     %% Instructions 
%         [ 'You will see a clock face.\n' ...
%           'Its arm will make a full turn over the course of 4 seconds.\n' ...
%           'Press the button to win points before the arm makes a full turn.\n' ...
%           'Try to win as many points as you can!\n\n' ...
%           'Press any key to read more instructions' ...
%         ]          
     Instructions = { ...
        [ 'For this game, you will see a dot moving around a picture.\n\n'...
          'The dot will make a full revolution over the course of ' num2str(timerDuration) ' seconds.\n\n' ...
          'Press any key to win points before the dot makes a full turn.\n\n' ...
          'Try to win as many points as you can!\n\n' ...
          'Press any key to continue' ...
        ], ...
        [ 'Sometimes you will win lots of points and sometimes you will win less.\n\n ' ...
          'The time at which you respond affects\n' ...
          'the number of points you win.\n\n' ...
          'If you don''t respond by the end of the turn,\n' ...
          'you will not win any points.\n\n' ...
          'Press any key to continue' ...
        ], ...
        [ 'When the color of the screen border changes,\n' ...
          'the game has changed. Try responding at different\n' ...
          'times in order to learn how to get the most points.\n\n' ...
          'Press any key to continue' ...
        ], ...
        [ 
          'Hint: Try to respond at different times\n' ... 
          'in order to learn how to get the most points.\n\n' ...
          'Note: The total length of the experiment does not change\n' ... 
          'and is not affected by when you respond.\n\n' ...
          'Press any key to begin' ...
        ]
      }; 
    
      % use boxes instead of prompts
        %Between run instructions
  
     InstructionsBetween = [ ...
         'Next, you will see a new set of pictures.\n' ...
         'Try responding at different times in order to learn\n' ...
         'how to win the most points with this new set.\n\n' ...
         'Press any key when you are ready' ];
     
     % is the first time loading?
     % we know this by where we are set to start (!=1 if loaded from mat)
     if start==1  
         % show long instructions for first time player
         for instnum = 1:length(Instructions)
             DrawFormattedText(w, Instructions{instnum},'center','center',black);
             Screen('Flip', w);
             waitForResponse;
             WaitSecs(.1);
             % clear response box so we push and release dont count make a button press count twice
             if(~isempty(evt))
               CedrusResponseBox('FlushEvents', resppad);
             end
         end

        % inialize the order of events only if we arn't resuming
        order=cell(length(experiment{facenumC}),1);
     
     % subjects know the drill. Give them breif instructions
     % order is already init. and loaded from mat, so don't work about it
     else
         DrawFormattedText(w, ['Welcome Back!\n\n' InstructionsBetween],'center','center',black);
         Screen('Flip', w);
         waitForResponse;
     end
     
     %% give subj a countdown and fixation
     for cdown = 3:-1:1
         DrawFormattedText(w, ['Beginning in\n\n' num2str(cdown)],'center','center',black);
         Screen('Flip',w);
         WaitSecs(1.0);
     end
     
     trialnum=start; % fixation calls drawRect which uses trialnum to get the block number
     fixation(1500);
     
     %% debug, timing
     StartOfRunTime=GetSecs();

  
   
     
     %% THE BIG LOOP -- for all remaining trials or to the halfwaypt
     for trialnum=start:length(experiment{facenumC})
        

        %% debug, start time keeping
        % start of time debuging global var
        checktime=GetSecs();
        startOfTrial=checktime;
        % seconds into the experiement from start of for loop
        timing.start=checktime-StartOfRunTime;
        
        %% face (4s) + ITI + score + ISI

        %dispRspTime=tic;
        % show face, record time to spacebar
        rspnstime = faceWithTimer;
        %dispRspTime=toc(dispRspTime)*10^3 % this time 
        % is shorter than rspnstime!!? how
        

        % add RT/2 to fixation time -- prev. added all of remainder
        %
        % math done in seconds then convereted to ms for fixation()
        dispRspTime=GetSecs() - checktime;
        fixation( (timerDuration - dispRspTime )*10^3/2);
        setTimeDiff('timer'); %build times (debug timing)
         
        % show first fixation
        fixation(experiment{ITIC}(trialnum));
        
        setTimeDiff('ITI'); %build times (debug timing)

        % show score
        scoreRxt(rspnstime,experiment{rewardC}{trialnum});
        
        setTimeDiff('receipt'); %build times (debug timing)
        
        % show second fixation
        fixation(experiment{ISIC}(trialnum));
        
        setTimeDiff('ISI'); %build times (debug timing)
        
        
        %% non critical things (debuging and saving)
        %nonPresTime=tic;
        
        %% write to data file
        emo=experiment{emotionC}{trialnum};
        face=experiment{facenumC}(trialnum);
        
        %set the output of the order structure
        trial = { experiment{rewardC}{trialnum} subject.run_num trialnum experiment{blockC}(trialnum) 0 t_start F_Mag inc F_Freq ev rspnstime emo };
        order(trialnum) = {trial};
        
        % print header
        if trialnum == 1
            fprintf(txtfid,'Func\tRun\tTrial\tBlock\tNull\ttrialStartTime\tMag\tScoreInc\tFreq\tEV\tRT\tEmotion\tImage\n');
        end
        
        fprintf(txtfid,'%s\t',order{trialnum}{1} );
        fprintf(txtfid, '%4i\t', order{trialnum}{2:end});
        fprintf(txtfid, '%s\t', emo, strcat(emo,'_',num2str(face),'.png') );
        fprintf(txtfid, '\n');
        
        % save to mat so crash can be reloaded
        save(filename,'order','trialnum','subject','score','blockTotal');
       
        % line like
        % CEVR       1      22       5       2    4.481533e+01   176       0       2.136929e-01    3.764108e+01    2.399020e+02
        % PHASE      RUN    TRIAL    BOXC   NULL   TRACT        FMAG   FMAGP(fmp)   FFREQ(ff=F_Freq)      EV              RT
        % | --------------- input file ---------|
        %%% tract = (trial start time    -  trstamp)/TR
        %%% null = old exprm field
        %%% we only have one run of the experiment
        
        
        %% debug, show time of this trial
        
        timing.end= GetSecs() - startOfTrial;
              
        expected.timer   = 4; 
        expected.ITI     = double(experiment{ITIC}(trialnum))/10^3;
        expected.receipt = receiptDuration;
        expected.ISI     = double(experiment{ISIC}(trialnum))/10^3;
        expected.end     = 0;
        expected.end     = sum(struct2array(expected));
        fprintf('%d: %s_%d.png\n%.2f in, expected, obs, diff\n',trialnum, experiment{emotionC}{trialnum},experiment{facenumC}(trialnum),timing.start);
        for f = {'timer' 'ITI' 'receipt' 'ISI' 'end' };
            f=f{1};
            fprintf('%s\t%.2f\t%.2f\t%.2f\n', f, expected.(f), timing.(f),timing.(f)-expected.(f));
        end
        
        
        
        
        %%%%%%%%%%%%%%%% halfwaypt break!
        if trialnum==halfwaypt 
            msgAndCloseEverything(['Great Job! Your score so far is ', num2str(score) ,' points\n\nLet''s take a break']);
            return
        end
        
                 
        %% instructions if new block
        % if i=halfwaypt, though mod 42==0, this is never seen
        % also, only display instructions between if this is not the last trial.
        if trialnum > (trialsPerBlock - 1) && mod(trialnum, trialsPerBlock) == 0 && trialnum < (trialsPerBlock*totalBlocks)

            Screen('TextSize', w, 22);
            %% give subj a 60 second break with countdown            
            for cdown = 60:-1:1
                % says e.g. 5 of 6 on first session
                % then     10 of 12 on the second
                DrawFormattedText(w, ...
                    [ '\n\nYou have ' num2str(score) ' points so far\n\n'...
                    'Completed Game: ' num2str(floor(trialnum/trialsPerBlock)) ' of ' num2str(totalBlocks*subject.run_num/totalSessions) ...
                    '\n\nNext game will begin in\n\n' num2str(cdown) ...
                    ],'center','center',black);               
                Screen('Flip',w);
                WaitSecs(1.0);
            end
            
            drawRect(trialnum+1);
            DrawFormattedText(w, InstructionsBetween,'center','center',black);
            blockTotal=0; %reset block score for new block
            Screen('Flip', w);
            waitForResponse;

        end
        
        % show all intervals + expected
%         disp([timing expectedTime(trialnum)]);
%         
%         % give a break down by expected
%         expected = double([ timerDuration*10^3 + experiment{ITIC}(trialnum) receiptDuration*10^3  experiment{ISIC}(trialnum)  ]);
%         expected = [ expected sum(expected) ]./10^3;
%         timing   = [timing(2) + timing(3)  timing(4:6)];
%         disp(expected - timing)
%         timing = []
        % and show the difference
         
        %otherstufftime=toc(nonPresTime) %.025 seconds
        

     end 

    % everyone should earn the bonus
    % but they should have at least 2000 pts
    earnedmsg='\n\nYou earned a $25 bonus !'; 
    if(score<2000); earnedmsg=''; end;


    msgAndCloseEverything(['Your final score is ', num2str(score) ,' points', earnedmsg, '\n\nThanks for playing!']);

    return

  catch
     Screen('CloseAll');
     CedrusResponseBox('CloseAll');
     psychrethrow(psychlasterror);
  end
  
  % close the screen
  sca

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           support functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % if we are using the response pad, print the drift
    function printdrift()
      if(~isempty(resppad))
       slope = CedrusResponseBox('GetBoxTimerSlope', resppad);
       fprintf('response box drift is: %f\n',slope)
       %TODO: check subject exists as struct, save subject mat
       subject.rsppadDrift= slope;
       % dev = CedrusResponseBox('GetDeviceInfo',resppad)
       % subject.rtdev = dev.roundtripstddev
      end
    end

    function msgAndCloseEverything(message)
       DrawFormattedText(w, [message '\n\n push any key but esc to quit'],...
           'center','center',black);
       fprintf('%s\n',message)
       Screen('Flip', w);
       waitForResponse;
       diary off;	%stop diary
       fclose('all');	%close data file
       Screen('Close')
       Screen('CloseAll');
       CedrusResponseBox('CloseAll');
       PsychPortAudio('Close');
       sca
    end

   %% print time since last check
   % updates global timing struct and checktime double
   function setTimeDiff(interval)
       timing.(interval) = (GetSecs() - checktime);
       checktime=GetSecs();
   end 



   %% block indicator
    function drawRect(varargin)
       t=i;
       % allow block to be specified, mostly for instruction display
       % b/c we want to show the color of the next block
       if nargin>0
           t=varargin{1};
       end
       if( t > length(experiment{blockC}) )
           rgbcolorIDX=1;
           fprintf('Something funny is happening!! -- we are at t=%i\n',t)
       else
           rgbcolorIDX=experiment{blockC}(t);
       end
       Screen('FrameRect', w, blockColors(rgbcolorIDX,:), [], 25);
    end



   %% Meat -- show the face and revolving dot (timer)
   function elapsedMS  = faceWithTimer
       
     % clear button pushes and (?) raw reset time (?)
     evt=[];

     % dot size and dist from center
     spotRadius         = 150;        % The radius of the spot from center.
     spotSize           = 10;         % The radius of the spot's fill.
     initialDotPosition = 3 * pi / 2; % The initial position. -- 12 o'clock
     
     % setup rectanges
     spotDiameter = spotSize * 2; % I guess I should've also multi by pi :)
     spotRect = [0 0 spotDiameter spotDiameter];
     centeredspotRect = CenterRect(spotRect, windowRect); % Center the spot.
     
     %% clear and time response pad if we are using it
     if(~isempty(resppad) ); 
       CedrusResponseBox('FlushEvents', resppad);
       % dont reset time, the reset will introduce unknown delay to timing
       %resetTime = CedrusResponseBox('ResetRTTimer', handle);
       % instead, use diff from built in timer
       evt = CedrusResponseBox('GetBaseTimer', resppad);
       cedrustime = evt.basetimer;
     end

     % Set up the timer.
     startTimeMS   = GetSecs()*10^3;
     durationMS  = timerDuration*10^3; % 4 seconds of looking at a face
     remainingMS = durationMS;
     
     %elapsedMS = 0;
     % Loop while there is time.
     while remainingMS > 0 
        elapsedMS = round((GetSecs()*10^3 - startTimeMS) );
        remainingMS = durationMS - elapsedMS;
        
        %Screen('DrawText', w, sprintf('%i ms remaining...',remainingMS), 20, 20, black);
        %Screen('DrawText', w, sprintf('%i ms elapsed...',elapsedMS), 20, 40, black);

        % what block is this (border color)
        drawRect;
        
        % white cirlce over trial area
        Screen('FillOval', w, [255 255 255], CenterRect([ 0 0 2*(spotRadius+spotSize)+10 2*(spotRadius+spotSize)+10 ],windowRect));
        
        % put the image up
        emo=experiment{emotionC}{i};
        facenum=experiment{facenumC}(i);
        Screen('DrawTexture', w,  facetex.(emo){facenum}  ); 
        
        % at 4 seconds, we do a full rotation
        theta=  initialDotPosition - (remainingMS/durationMS * 2 * pi) ;
        xOffset = spotRadius * cos(theta);
        yOffset = spotRadius * sin(theta);

        offsetCenteredspotRect = OffsetRect(centeredspotRect, xOffset, yOffset);
        Screen('FillOval', w, [0 191 95], offsetCenteredspotRect);

        
        %% super debug mode -- show EV for reponse times
%         for rt = 0:500:3500
%             [M, F] = getScore(rt,experiment{rewardC}{i});
%             
%             M_xOffset = (200) * cos(initialDotPosition - 2*pi * rt/durationMS);
%             M_yOffset = (200) * sin(initialDotPosition - 2*pi * rt/durationMS);
%             M_offRect = OffsetRect(centeredspotRect, M_xOffset, M_yOffset);
%             
%             F_xOffset = (300) * cos(initialDotPosition - 2*pi * rt/durationMS);
%             F_yOffset = (300) * sin(initialDotPosition - 2*pi * rt/durationMS);
%             F_offRect = OffsetRect(centeredspotRect, F_xOffset, F_yOffset);
%            
%             EV_xOffset = (400) * cos(initialDotPosition - 2*pi * rt/durationMS);
%             EV_yOffset = (400) * sin(initialDotPosition - 2*pi * rt/durationMS);
%             EV_offRect = OffsetRect(centeredspotRect, EV_xOffset, EV_yOffset);
%             
%             Screen('DrawText',w,num2str(M),  M_offRect(1),  M_offRect(2), [ 0 0 0]);
%             Screen('DrawText',w,num2str(F),  F_offRect(1),  F_offRect(2), [ 0 0 0]);
%             Screen('DrawText',w,num2str(M*F),EV_offRect(1), EV_offRect(2),[ 0 0 0]);
%         end
        
        % display screen
        Screen('Flip', w);
        
        %% stop clock based on keyboard or response pad
        % keyboard
        if(isempty(resppad))
          [ keyIsDown, seconds, keyCode ] = KbCheck;
          
          if keyIsDown
              if(keyCode(escKey)); 
                  msgAndCloseEverything(['Quit on trial ' num2str(i)]);
                  error('quit early (on %d)\n',i)
              end
              %if keyCode(spaceKey)
                  break
              %end
          end
        % Response box
        else
          evt = CedrusResponseBox('GetButtons', resppad);
          if(~isempty(evt))
           break
          end
        end

        
        % Wait 1 ms before checking the keyboard again to prevent
        % overload of the machine at elevated Priority():
        %WaitSecs(0.001);
     end
     
     elapsedMS = round(GetSecs()*10^3 - startTimeMS);
     % if 4s, give them no points? -- just a test for warning
     if elapsedMS >= (timerDuration) *10^3
     %    fprintf('warning: RT is %f\n', elapsedMS*10^8);
          % play the sound in pahandle once, start now, dont wait for
          % playback to start
          fprintf('playing sound\n');
          PsychPortAudio('Start', pahandle, 1, 0, 0);
    end
    %fprintf('\n Submitted @ %f\n',elapsedMS);
     

    subject.resptime.comp(trialnum)=elapsedMS;
    % append response pad timing to elapsedMS return value
    if(~isempty(evt))
     elapsedMS = (evt.rawtime - cedrustime) * 10^3;
     subject.resptime.resppad(trialnum)= elapsedMS ;
    end

    return;
   end



   %% Display a red cross for ITI (ms) time
   function fixation(waittime)
        % grab time here, so we can subtract the time it takes to draw
        % from the time we are actually waiting
        starttime=tic ;
        
        waittime=double(waittime)/10^3;
        %fprintf('waiting %.3f\n',waittime);
        
        oldFontSize=Screen(w,'TextSize', 40 );
        DrawFormattedText(w,'+','center','center',[ 255 0 0]);
        Screen(w,'TextSize', oldFontSize);
        drawRect;
        Screen('Flip', w);
        drawTime=toc(starttime);
        WaitSecs(waittime-drawTime);
   
        timediff=toc(starttime)-waittime;
        if abs(timediff) > .02; fprintf('!! WARNING, wait is off by > 20ms (%.4f)\n', timediff );end
        
        %disp([waittime, experiment{ITIC}(i), experiment{ISIC}(i), 4000 - rspnstime, rspnstime ])
   end
   


   %% wait for a response
    function seconds = waitForResponse
      while(1)
          % get keyboard
          [ keyIsDown, seconds, keyCode ] = KbCheck;

          % get respponse pad
          evt=[];
          if(~isempty(resppad))       
            evt = CedrusResponseBox('GetButtons', resppad);
          end
          
          if(keyIsDown && keyCode(escKey));
              msgAndCloseEverything(['Quit on trial ' num2str(i)]);
              error('quit early (on %d)\n',i)
           end
          
          if(any(keyCode) || ~isempty(evt) ); break; end %any() is redudant
          WaitSecs(.001);

      end
      Screen('Flip', w); % change the screen so we don't hold down space
      WaitSecs(.2);
    end


   %% score based on a response time and Rew Func (as string, eg. 'CEV')
   function scoreRxt(RT,func)
       startScoreTime=tic;
       %trial start time
       t_start = (GetSecs - scannerStart)/TR;
       
       [F_Mag F_Freq] = getScore(RT,func);

        %%% Compute Score
        %Add noise to magnitude
        a = -5;
        b = 5;
        r = a + (b-a).*rand(1);
        % noise is an integer from -5 to 5
        r = round(r) ;              
        F_Mag = F_Mag + r;
        ev = F_Mag*F_Freq;
        F_Mag = round(F_Mag);
        
        % random threshold
        rd=rand(1);
               
        % is freq above thresold and do we have a resonable RT
        if F_Freq > rd && RT~=0 && RT <= timerDuration*10^3
            score=score+F_Mag;
            blockTotal=blockTotal+F_Mag;
            inc=F_Mag;
        else
            %score=score;
            inc=0;
        end
        fprintf('%s: ev=%.2f; Mag=%.2f; Freq: %.2f; rand: %.2f; inc: %d; pts- block: %d; total: %d\n', ...
                experiment{rewardC}{i},ev, F_Mag,F_Freq,rd,inc,blockTotal,score);
       
        
        %%% Draw
        drawRect;
        %Screen('DrawText', w, sprintf('Your Score is: %d\nrecorded rxt: %d', score, rspnstime));
        %DrawFormattedText(w, sprintf('Total score is: %d\nincrease is: %d\nradnom vs Freq (ev): %f v %f (%f)\nrecorded rxt: %d', score,F_Mag,rd,F_Freq,ev, RT),'center','center',black);
        Screen('TextSize', w, 22);
        DrawFormattedText(w, sprintf('You won:  %d points\n\nTotal points this game: %d points', inc,blockTotal),'center','center',black);

        
        Screen('Flip', w);
        subject.reward(trialnum)=inc;
        WaitSecs(receiptDuration-toc(startScoreTime));
    
   end

   %% get who the subject is
   function getSubjInfo 
        % skip the questions if we provide var ourself
        subject.subj_id = input('Enter the subject ID number: ','s');
        

        filename = ['subjects/' subject.subj_id '_tc'];

        % is the subject new? should we resume from existing?
        % set t accordingly, maybe load subject structure 
        txtfile=[filename '.txt'];
        backup=[txtfile '.' num2str(GetSecs()) '.bak'];
        
        % we did something with this subject before?
        if exist(txtfile,'file') 
            
            % check that we have a mat file
            % if not, backup txt file and restart
            if ~ exist([filename '.mat'],'file')
                fprintf('%s.txt exists, but .mat does not!\n',filename)
                reload= 'n'; % move txt file to backup and start from scratch
            
            % we have the mat file
            % * is this resuming the previous run -- we were at the halfwaypt
            % * if it's not obviously resuming, do we want to continue where
            %   we left off?
            else
                localVar = load(filename);
                % sanity check
                if localVar.subject.subj_id ~= subject.subj_id
                    error('mat file data conflicts with name!: %d != %d',...
                        localVar.subject.subj_id, subject.subj_id);
                end
                
                % we have a mat, but did we stop when we should have?
                if  localVar.subject.run_num == 1 && ...
                   localVar.trialnum == halfwaypt;
                    
                    fprintf('incrementing run_num and assuming reload\n');
                    resume = 'y';
                    localVar.subject.run_num=2;
                    localVar.trialnum=halfwaypt+1; 
                    % need to increment trial here 
                    % b/c we exit before incrementing i earlier
                    % and we'll get stuck in a one trial loop otherwise
                
                % no where we expect, maybe psychtoolbox crashed
                % prompt if we want to restart
                else
                    fprintf('not auto resuming b/c run=%d and trail=%d\n\n',...
                        localVar.subject.run_num,localVar.trialnum)
                    resume = lower(input('Want to load previous session (y or n)? ','s'));
                end
       
                %
                % if we auto incremented run_num
                % or decided to resume
                %   clear subject, and load from mat file
                if strcmp(resume,'y')
                    
                    clear subject
                    start=localVar.trialnum;
                    subject=localVar.subject;

                    order=localVar.order;
                    score=localVar.score;
                
                % otherwise, move the existing txt file to a backup
                % and we'll fill in the subject info below
                else
                    fprintf('moving %s to %s, start from top\n', txtfile,backup)
                    movefile(txtfile,backup);
                end
                
            end
         end
        
        %% fill out the subject struct if any part of it is still empty
        for attribCell={'gender','age','run_num'}
            % make a normal string
            attrib = cell2mat(attribCell);

            % check if it's already filled out
            if  ~ismember( attrib,fields(subject) )  
              promptText=sprintf('Enter subject''s %s: ',attrib);
              subject.(attrib) = input(promptText,'s');
            else
               if ~ischar(subject.(attrib)); tmp=num2str(subject.(attrib)); else tmp=subject.(attrib); end 
              fprintf('using old %s (%s)\n', attrib, tmp);
            end
        end

        %% age should be a number
        if ischar(subject.age);     subject.age    =str2double(subject.age);    end
        if ischar(subject.run_num); subject.run_num=str2double(subject.run_num);end

        if start==1 && subject.run_num==2; 
            fprintf('WARNING: new subject, but run number 2 means start from the top\n')
            fprintf('there is no good way to do the first part again\n')
            start=halfwaypt+1;
        end

        %% set sex to a standard
        if ismember(lower(subject.gender),{'male';'dude';'guy';'m';'1'} )
            subject.gender = 'male';
        else
            subject.gender = 'female';
        end
        % print out determined sex, give user a chance to correct
        fprintf('Subject is %s\n', subject.gender);

        %% Initialize data storage and records
        % make directoires
        for dir={'subjects','logs'}
         if ~ exist(dir{1},'dir'); mkdir(dir{1}); end
        end
        
        % log all output of matlab
        diaryfile = ['logs/' subject.subj_id '_' num2str(GetSecs()) '_tcdiary'];
        diary(diaryfile);
        
        % log presentation,score, timing (see variable "order")
        txtfid=fopen(txtfile,'a'); % we'll append to this..maybe
        
        if txtfid == -1; error('couldn''t open text file for subject'); end


   end

    % The actual scoring function. Taken from Frank et al
    function [Mag Freq] = getScore(RT,scrfunc)
              % Values for Reward computation - constant for all phases
       k = 37;
       Shift = 700;
       rt_extended = 7000;
       DEV_factor = 10;
       DEV_factor2= 1;
       sin_factor = 0.25;

        % score response time based on one of 3 functions
        % given in master file (either inc,dec,or const)
        switch scrfunc
         case 'CEV'
             Mag = (k*rt_extended)/(rt_extended-(RT+Shift)); 
             Freq = 1-((RT+Shift)/rt_extended);
             
         case 'DEV'
            Mag = DEV_factor*log(DEV_factor2*(RT+Shift)); 
            CEV_x = 1-((RT+Shift)/rt_extended);
            IEV_x = CEV_x + (CEV_x*(sin_factor*sin((RT*pi)/5000)));
            Freq = (2*CEV_x)-IEV_x;
         case 'IEV'
            CEV_x = (k*rt_extended)/(rt_extended-(RT+Shift)); 
            DEV_x = DEV_factor*log(DEV_factor2*(RT+Shift));
            Mag = (2*CEV_x)-(DEV_x);
            CEV_x2 = 1-((RT+Shift)/rt_extended);
            Freq = CEV_x2 + (CEV_x2*(sin_factor*sin((RT*pi)/5000)));
         case 'CEVR'
            Mag = 1-((RT+Shift)/rt_extended);
            Mag = Mag*200;
            Freq = (k*rt_extended)/(rt_extended-(RT+Shift)) ;
            Freq = Freq/200;
         
         otherwise
          %score=score;
          Mag = 0;
          Freq = 0;
          warning(['!!!WHAT function did you mean by' scrfunc]);
        end

    end

end

