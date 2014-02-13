% CogEmo Face Reward Task:
%
%  usage: 
%   CogEmoFaceReward()
%   CogEmoFaceReward('DEBUG','MEG','NODAQ','screen',[800 1024])
%
%    1. will prompt for subject id, gender, runnum
%    2. show instructions in psychtoolbox screen 
%    3. ITI -> Face with 4s "clock" -> ISI -> Score -> ITI
%       will send trigger at ITI, ISI, Face, and Score if 'MEG' is given
%
%     ** esc key during clock will exit paradigm **
%
%  Requires: 
%     psychtoolbox
%     ltp trigger code sending requries DAQ toolbox (works on legacy 32bit matlabs only!)
%
%  Testing:
%    load subjects/test_tc.mat                                          % load everything the presentation saves
%    trialnum=597                                                       % set the trial number to be tested
%    subject.run_num=2                                                  % trial number > mid-way (300), trial==2
%    save('subjects/test_tc.mat','order','trialnum','subject','score'); % save new settings
%    CogEmoFaceReward
%       Enter the subject ID number: test
%       Is this a restart/want to load old file (y or n)? y
%
% TODO/DONE
%  [x] initialize parallel if we are using MEG/fMRI
%  [ ] response box -- http://docs.psychtoolbox.org/CMUBox
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
% 2013/09/25 -- adapt for MEG
%  [WF] send codes over LPT port to indicate phase of trial
%  [WF] add photodiode in DEBUG option to test timing
%  ---- these edits brought refactoring of settings via 'opts' struct
%       set via function call
%        * screen resolution
%        * debugging settings (photodiode, screen resolution, fprintfs
%        * MEG/send ltp trigger codes
%       see getopts()
%
% 2013/10/28
%  [WF] !!! ADD PROPRITY, use untiltime
%  [WF] use preset random order (specified in MEGorder/unused
%         move file to MEGorder/used when used)
%  [WF] remove counterbalence -- simple reverse of order could cause ugly
%       ITI+ISI mismatches for matching 
%  [WF] run number is yyyymmdd because there is only one run
%  [WF] remove quick response extra wait
% 2013/10/30 -- after pilot
%  [WF] Flash first get ready screen to help adjust photodiod
%  [WF] only alow space to exit task
%  [WF] change background on block end
%  [WF] Hide cursor and input -- keep keyboard (crashes otherwise)
%  {KH} reciept time to 850ms
%  {KH} no total in trial receipt, spaced out

function MEGCogEmoFaceReward(varargin)
  %% CogEmoFaceReward
  % WF 2012-10-05
  
  %% Settings:
  % get options: MEG, DEBUG, screen=[x y], 'mac laptop','VGA','eyelab'
  %    sets opts.trigger=1 if MEG
  getopts(varargin); % defines opts structure
  
  %% initialize trigger code sending on lpt port (skipped unless MEG)
  initTriggerSender()
  
  %% apply settings and setup starting info
  screenResolution=opts.screen;

  % if MEG, we want to display white,black,gray boxes for the photodiode
  %  what width should that box have?
  frameWidth=50;
  photodiodeWidth=frameWidth;
  
  % [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 255 255 255], [0 0 640 480] );
     % [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [0 0 1600 1200] );
     % [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [0 0 1440 900] );
     
  
  start       = 1;


  blockColors = round(255*hsv(24)); % a different color for each block of trials
  blockColors = blockColors(randperm(24),:); % randperm(24) should prob be replaced by a pre-made vector
  txtfid      = 0; %just so we know the file pntr's not a private nested function var

  receiptDuration  = .85; %850ms, match fMRI
  timerDuration    = 4;
  
  % initialize total points earned
  % incremented as a function (inc,dec,const)
  score = 0;
  blockTotal = 0;
  
  %% set order of trials 
  % experiment order file indexes
  indexes={1,2,3,4,5,6};
  [ facenumC, ITIC, ISIC, blockC, emotionC, rewardC ] = indexes{:};
  %  read in order of things
  experiment=getorderfile();
  
  %initialize timing
  timing(length(experiment{1})+1).start=Inf;

    
  
  
  %% defines triggers as a struct
  % trigger.(reward function).(emotion).(face or score)
  % plus trigger.(ITI,ISI,done)
  trigger=defineTrigger();

  %% start recording data
  % sets txtfid, subject.*, start, etc  

  totalSessions = opts.totalSessions; %broken into first and second halves
  halfwaypt=floor(length(experiment{blockC})/totalSessions); % 252
  
  % how long (trials) is a block
  [junk,blockchangeidx] = unique(experiment{blockC});
  trialsPerBlock     = unique(diff(blockchangeidx)); % 42
  if(length(trialsPerBlock) > 1) 
      fprintf('Whoa!? different trial lengths? I dont know whats goign on!')
      trialsPerBlock = trialsPerBlock(1);
  end

  totalBlocks = length(experiment{blockC})/trialsPerBlock; % 12

  % prompt for subject info
  % or ask to resume task
  getSubjInfo()


  
  % print the top of output file
  if start == 1
    fprintf(txtfid,'#Subj:\t%s\n', subject.subj_id);
    fprintf(txtfid,'#Run:\t%i\n',  subject.run_num); 
    fprintf(txtfid,'#Age:\t%i\n',  subject.age);
    fprintf(txtfid,'#Gender:\t%s\n',subject.gender);
    
  end
  
  % move order file into used if we've used it
  if (strfind(opts.TrialCSV,'unused') )
    % move the order file from unusused to used
    [rootdir, csvfilename, csvext]=fileparts(opts.TrialCSV);
    neworderfilename=[ 'MEGorder/used/' subject.subj_id '_' csvfilename csvext ];
    movefile(opts.TrialCSV,neworderfilename);  
    subject.orderCSV=neworderfilename;
    % for f in MEGorder/used/*csv; do cp $f MEGorder/unused/${f##*_}
  end
  
  % always print date .. even though it'll mess up reading data if put in the middle
  fprintf(txtfid,'#%s\n',date);
    
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
     backgroundColor=[204 204 204];
     [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),backgroundColor, [0 0 screenResolution] );
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
     white = WhiteIndex(w);
     
     
     % MEG photodiode settings
     FacePhotodiodeColor=white;
     ScorePhotodiodeColor=[204 204 204];
     emptyPhotodiodeColor=black;

     
     % Enable unified mode of KbName, so KbName accepts identical key names on
     % all operating systems:
     KbName('UnifyKeyNames');

     % Set keys.
     %spaceKey  = KbName('SPACE');
     escKey  = KbName('ESCAPE');

     %what keys are okay to press
     acceptableKeyPresses = [ KbName('space') KbName('2@') ];
  
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
     
     if(opts.sound)
      InitializePsychSound;
      [wavedata, sndFreq] = wavread('incorrect.wav');
      wavedata=wavedata';
      nrchannels = size(wavedata,1);
      % 2nd to last arg should be sndFreq, but portaudio returns error w/it
      pahandle= PsychPortAudio('Open', [], [], [], [], nrchannels);
      PsychPortAudio('FillBuffer',pahandle,wavedata);
     else
      fprintf('NOT USING SOUND\n');
     end
     
     
     %% Hide mouse and keyboard
     %ListenChar(2); % no keystrokes to matlab
     HideCursor;    % no cursor

     %% Instructions 
     Instructions = { ...
        [ 'For this game, you will see a dot moving around a picture.\n\n'...
          'The dot will make a full revolution over the course of ' num2str(timerDuration) ' seconds.\n\n' ...
          'Press right index finger to win points before the dot makes a full turn.\n\n' ...
          'Try to win as many points as you can!\n\n' ...
        ], ...
        [ 'Sometimes you will win lots of points and sometimes you will win less.\n\n ' ...
          'The time at which you respond affects\n' ...
          'the number of points you win.\n\n' ...
          'If you don''t respond by the end of the turn,\n' ...
          'you will not win any points.\n\n' ...
        ], ...
        [ 'When the color of the screen border changes,\n' ...
          'the game has changed. Try responding at different\n' ...
          'times in order to learn how to get the most points.\n\n' ...
        ], ...
        [ 
          'Hint: Try to respond at different times\n' ... 
          'in order to learn how to get the most points.\n\n' ...
          'Note: The total length of the experiment does not change\n' ... 
          'and is not affected by when you respond.\n\n' ...
        ]
      }; 
    
      % use boxes instead of prompts
        %Between run instructions
  
     InstructionsBetween = [ ...
         'Next, you will see a new set of pictures.\n' ...
         'Try responding at different times in order to learn\n' ...
         'how to win the most points with this new set.\n\n'];
     if(~opts.MEG)
         InstructionsBetween = [  InstructionsBetween 'Press any key when you are ready' ];
     end
     
     % set real time priority
     lowerPriority=Priority(MaxPriority(w));
     
     % is the first time loading?
     % we know this by where we are set to start (!=1 if loaded from mat)
     if start==1  
         % show long instructions for first time player
         for instnum = 1:length(Instructions)
             DrawFormattedText(w, Instructions{instnum},'center','center',black);
             Screen('Flip', w);
             waitForResponse('space');
         end
         getReady(0);

        % inialize the order of events only if we arn't resuming
        order=cell(length(experiment{facenumC}),1);
     
     % subjects know the drill. Give them breif instructions
     % order is already init. and loaded from mat, so don't work about it
     else
         DrawFormattedText(w, ['Welcome Back!\n\n' InstructionsBetween],'center','center',black);
         Screen('Flip', w);
         waitForResponse('space');
         getReady(0);
     end
     
     %% give subj a countdown and fixation
     if(~ opts.MEG)
         for cdown = 3:-1:1
             DrawFormattedText(w, ['Beginning in\n\n' num2str(cdown)],'center','center',black);
             Screen('Flip',w);
             WaitSecs(1.0);
         end
     end
     
     i=start; % fixation calls drawRect which uses i to get the block number
     fixation(1500);
     

     %% TODO: how/when to get '^' for getting scanner start
     scannerStart = GetSecs();
    
     %% debug, timing
     StartOfRunTime=scannerStart;

  
   
     
     %% THE BIG LOOP -- for all remaining trials or to the halfwaypt
     
     for i=start:length(experiment{facenumC})
        
        emo=experiment{emotionC}{i};
        rew=experiment{rewardC}{i};
        face=experiment{facenumC}(i);
        
        %% debug, start time keeping
        % start of time debuging global var
        startOfTrialTime=GetSecs();
        checktime=startOfTrialTime; % initialize checktime to start
        % seconds into the experiement from start of for loop
        timing(i).start=startOfTrialTime-StartOfRunTime;
        
        %% face (4s) + ITI + score + ISI

        % show face, record time to spacebar
        %dispRspTime=tic;
        
        sendTrigger(trigger.(rew).(emo).face)
        % face with timer will flip screen first
        rspnstime = faceWithTimer;
        %dispRspTime=toc(dispRspTime)*10^3 % this time 
        % is shorter than rspnstime!!? how
        

        % add RT/2 to fixation time -- prev. added all of remainder
        %
        % math done in seconds then convereted to ms for fixation()
        %% REMOVE this penetly for going quick
        %dispRspTime=GetSecs() - checktime;
        %fixation( (timerDuration - dispRspTime )*10^3/2, trigger.ITI);
        setTimeDiff('face'); %build times (debug timing)
         
        % show first fixation
        % N.B. to the subj, this is the same fixation cross that's already
        %      up
        % dispRspTime + 1/2
        fixation(experiment{ITIC}(i),trigger.ITI);
        
        setTimeDiff('ITI'); %build times (debug timing)

        % show score -- will send trigger and show box for diode before flip if needed
        % send trigger for receipt +4 if rewarded
        scoreRxt(rspnstime,rew,trigger.(rew).(emo).score);
        
        
        sendTrigger(trigger.(rew).(emo).score);
        
        setTimeDiff('receipt'); %build times (debug timing)
        
        % show second fixation
        fixation(experiment{ISIC}(i),trigger.ISI);
        
        setTimeDiff('ISI'); %build times (debug timing)
        
        
        %% non critical things (debuging and saving)
        %nonPresTime=tic;
        
        %% write to data file

        
        %set the output of the order structure
        trial = { experiment{rewardC}{i} subject.run_num i experiment{blockC}(i) 0 t_start F_Mag inc F_Freq ev rspnstime emo };
        order(i) = {trial};
        
        % print header
        if i == 1
            fprintf(txtfid,'Func\tRun\tTrial\tBlock\tNull\ttrialStartTime\tMag\tScoreInc\tFreq\tEV\tRT\tEmotion\tImage\n');
        end
        
        fprintf(txtfid,'%s\t',order{i}{1} );
        fprintf(txtfid, '%4i\t', order{i}{2:end});
        fprintf(txtfid, '%s\t', emo, strcat(emo,'_',num2str(face),'.png') );
        fprintf(txtfid, '\n');
        
        % save to mat so crash can be reloaded
        trialnum=i;
        save(filename,'order','trialnum','subject','score','blockTotal','timing');
       
        % line like
        % CEVR       1      22       5       2    4.481533e+01   176       0       2.136929e-01    3.764108e+01    2.399020e+02
        % PHASE      RUN    TRIAL    BOXC   NULL   TRACT        FMAG   FMAGP(fmp)   FFREQ(ff=F_Freq)      EV              RT
        % | --------------- input file ---------|
        %%% tract = (trial start time    -  trstamp)/TR
        %%% null = old exprm field
        %%% we only have one run of the experiment
        
        
        %% debug, show time of this trial
        
        setTimeDiff('end');
              
        timing(i).ITI.expected     = double(experiment{ITIC}(i))/10^3;
        timing(i).receipt.expected = receiptDuration;
        timing(i).ISI.expected     = double(experiment{ISIC}(i))/10^3;
        timing(i).facetrig         = trigger.(rew).(emo).face;
        timing(i).scoretrig        = trigger.(rew).(emo).score;
        if(opts.DEBUG)
            fprintf('%d: start@%.4f\t%s_%d.png\n%.2f\n',i,timing(i).start, experiment{emotionC}{i},experiment{facenumC}(i));
            for f = {'ITI' 'receipt' 'ISI' 'end'};
                f=f{1};
                fprintf('\t%.4f\t%s\n', timing(i).(f).onset, f);
            end
        end
        
        
        
        %%%%%%%%%%%%%%%% halfwaypt break! 
        % not for MEG
        if(~opts.MEG)
            if i==halfwaypt 
                msgAndCloseEverything(['Great Job! Your score so far is ', num2str(score) ,' points\n\nLet''s take a break']);
                sendTrigger(trigger.done)
                return
            end
        end
        
                 
        %% instructions if new block
        % if i=halfwaypt, though mod 42==0, this is never seen
        % also, only display instructions between if this is not the last trial.
        if i > (trialsPerBlock - 1) && mod(i, trialsPerBlock) == 0 && i < (trialsPerBlock*totalBlocks)

            Screen('TextSize', w, 22);
            %% give subj a 60 second break with countdown            

            % change screen color to alert operator
            Screen('FillRect',w,black,[0 0 screenResolution])
            
            DrawFormattedText(w, ...
                [ '\n\nYou have ' num2str(score) ' points so far\n\n'...
                'Completed Game: ' num2str(floor(i/trialsPerBlock)) ' of ' num2str(totalBlocks)
                ],'center','center',white);               
            Screen('Flip',w);
            Screen('FillRect',w,backgroundColor,[0 0 screenResolution])
            waitForResponse('space');
            
            drawRect(i+1);
            DrawFormattedText(w, InstructionsBetween,'center','center',black);
            blockTotal=0; %reset block score for new block
            Screen('Flip', w);
            waitForResponse('space');
            getReady();

        end
        
        % show all intervals + expected
%         disp([timing expectedTime(i)]);
%         
%         % give a break down by expected
%         expected = double([ timerDuration*10^3 + experiment{ITIC}(i) receiptDuration*10^3  experiment{ISIC}(i)  ]);
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
     ShowCursor;
     %ListenChar(0);
     Screen('CloseAll');
     Priority(0); % set priority to normal
     psychrethrow(psychlasterror);
     sendTrigger(trigger.done);
     
  end
  
  % set priority to normal
  Priority(0); 
  % close the screen
  sca
  % clear trigger
  sendTrigger(0);
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           support functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
    function msgAndCloseEverything(message)
       sendTrigger(trigger.done) % send end code
       Priority(0); % set priority to normal 
       % send last message
       DrawFormattedText(w, message,...
           'center','center',black);
       fprintf('%s\n',message)
       Screen('Flip', w);
       waitForResponse('space','escok');
       % close all files
       diary off;	      %stop diary
       fclose('all');	  %close data file
       Screen('Close');   % kill screen
       Screen('CloseAll');% all of them
       
       ShowCursor;    % Retrun cursor
       %ListenChar(0); % take keyboard input

       sca;           % be sure screen is gone
       
       
       % turn off sound
       if(opts.sound)
         PsychPortAudio('Close');
       end
       
       if(i<length(experiment))
        error('quit early (on %d)\n',i)
       end
       
       return
    end

   %% print time since last check
   % updates global timing struct and checktime double
   function setTimeDiff(interval)
       timing(i).(interval).onset = GetSecs()-startOfTrialTime;
       timing(i).(interval).diff  = timing(i).(interval).onset - checktime;
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
       Screen('FrameRect', w, blockColors(rgbcolorIDX,:), [], frameWidth);
       
       drawPhotodiodeBox(emptyPhotodiodeColor)
       % draw box for photodiode, opposite luminocity as when face is on
    end



   %% Meat -- show the face and revolving dot (timer)
   function [elapsedMS] = faceWithTimer
       
     % dot size and dist from center
     spotRadius         = 150;        % The radius of the spot from center.
     spotSize           = 10;         % The radius of the spot's fill.
     initialDotPosition = 3 * pi / 2; % The initial position. -- 12'o clock
     
     % setup rectanges
     spotDiameter = spotSize * 2; % I guess I should've also multi by pi :)
     spotRect = [0 0 spotDiameter spotDiameter];
     centeredspotRect = CenterRect(spotRect, windowRect); % Center the spot.
     
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
        % superDebug()
        
        % draw grey box for photodiode
        % this will help messure latence between lpt port and 
        % Photodiode
        drawPhotodiodeBox(FacePhotodiodeColor) 
        
        
        % display screen
        Screen('Flip', w);
        
        % check for escape's
        [ keyIsDown, seconds, keyCode ] = KbCheck;
        
        if keyIsDown
            if(keyCode(escKey)); 
                msgAndCloseEverything(['Quit on trial ' num2str(i)]);
                error('quit early (on %d)\n',i)
            elseif any(keyCode(acceptableKeyPresses))
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
          if(opts.sound)
           fprintf('playing sound\n');
           PsychPortAudio('Start', pahandle, 1, 0, 0);
          end
    end
     %fprintf('\n Submitted @ %f\n',elapsedMS);
     
     return;
   end



   %% Display a red cross for ITI (ms) time
   % vargargin is the code to send to the parallel port if one is open
   function fixation(waittime,varargin)
        % grab time here, so we can subtract the time it takes to draw
        % from the time we are actually waiting
        starttime=tic ;
        
        waittime=double(waittime)/10^3;
        %fprintf('waiting %.3f\n',waittime);
        
        oldFontSize=Screen(w,'TextSize', 40 );
        DrawFormattedText(w,'+','center','center',[ 255 0 0]);
        Screen(w,'TextSize', oldFontSize);
        drawRect;
        %% send code before flip
        % varargin is code to send
        if(~isempty(varargin))
            sendTrigger(varargin{1})
        end
        %% flip screan
        Screen('Flip', w);
        drawTime=toc(starttime);
        WaitSecs(waittime-drawTime);
   
        timediff=toc(starttime)-waittime;
        if abs(timediff) > .02; fprintf('!! WARNING, wait is off by > 20ms (%.4f)\n', timediff );end
        
        %disp([waittime, experiment{ITIC}(i), experiment{ISIC}(i), 4000 - rspnstime, rspnstime ])
   end
   
   %% get ready screen, so experementer knows they're going to start
   % first getRead flashes for the photodiode
   function getReady(varargin)
     Screen('TextSize', w, 22);
     if(isempty(varargin))
         drawRect(i+1)
         DrawFormattedText(w, 'Get Ready!' ,'center','center',black);
         Screen('Flip',w);
         waitForResponse('space');
     elseif(varargin{1}==0)
         getreadystart=GetSecs();
         while(1)
           [ keyIsDown, seconds, keyCode ] = KbCheck;
           if(keyCode(KbName('SPACE')) )
               break
           end
           drawRect(1);
           color=white;
           if(mod(round(2*(GetSecs()-getreadystart)),2))
               color=black;
           end
           drawPhotodiodeBox(color)
           DrawFormattedText(w, 'Get Ready!' ,'center','center',black);
           Screen('Flip',w);
           WaitSecs(.01);
         end
     end
     

   end

   %% wait for a response
    function seconds = waitForResponse(varargin)
      %% sometimes we only want a specfic set of keys
      if(~isempty(varargin))
           usekeys=KbName(varargin{1});
           % add escape to use keys if we say esc is okay
           if(length(varargin)>1 && strcmp(varargin{2},'escok'))
               usekeys=[usekeys escKey];
               WaitSecs(.2); % just so we don't esc all the way through
           end
      else
           usekeys=acceptableKeyPresses;

      end
      
      while(1)
          [ keyIsDown, seconds, keyCode ] = KbCheck;
          
          if(keyIsDown)
              if(any(keyCode(usekeys))) 
                  break;
              elseif(keyCode(escKey) ); 
                  msgAndCloseEverything(['Quit on trial ' num2str(i)]);
                  error('quit early (on %d)\n',i)
              %else, we don't care--keep looping
              end
          end
          
          WaitSecs(.001);
      end
      Screen('Flip', w); % change the screen so we don't hold down space
      WaitSecs(.2);
    end


   %% score based on a response time and Rew Func (as string, eg. 'CEV')
   function scoreRxt(RT,func,varargin)
       startScoreTime=tic;
       %trial start time
       t_start = (GetSecs - scannerStart);
       
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
        fprintf('%d:  %s\tev=%.2f; Mag=%.2f; Freq: %.2f; rand: %.2f; inc: %d; pts- block: %d; total: %d\n', ...
                i,experiment{rewardC}{i},ev, F_Mag,F_Freq,rd,inc,blockTotal,score);
       
        
        %%% Draw
        drawRect;
        %Screen('DrawText', w, sprintf('Your Score is: %d\nrecorded rxt: %d', score, rspnstime));
        %DrawFormattedText(w, sprintf('Total score is: %d\nincrease is: %d\nradnom vs Freq (ev): %f v %f (%f)\nrecorded rxt: %d', score,F_Mag,rd,F_Freq,ev, RT),'center','center',black);
        Screen('TextSize', w, 22);
        DrawFormattedText(w, sprintf('You \nwon\n%d\npoints', inc),'center','center',black);

        % varagin will be trigger, send if we have it
        % trigger is +4 if rewarded
        if(~isempty(varargin)), sendTrigger(varargin{1} + 4*inc>0), end
        
        
        % draw box for photodiode, opposite luminocity as when face is on
        drawPhotodiodeBox(ScorePhotodiodeColor) 
       
        % draw screen
        Screen('Flip', w);
        WaitSecs(receiptDuration-toc(startScoreTime));
    
   end

   %% get who the subject is
   function getSubjInfo 
        % skip the questions if we provide var ourself
        subject.subj_id = input('Enter the subject ID number: ','s');
        
        %run number is the date yyyymmdd
        c=clock();
        subject.run_num=c(1)*10000+c(2)*100+c(3);

        filename = ['subjects/MEG_' subject.subj_id '_tc'];

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
                
                resume = lower(input('Want to load previous session (y or n)? ','s'));
               
                %
                % if we auto incremented run_num
                % or decided to resume
                %   clear subject, and load from mat file
                if strcmp(resume,'y')
                    
                    %% from which block
                    
                    clear subject
                    disp(localVar), disp(localVar.subject);
                    
                    start=localVar.trialnum;
                    subject=localVar.subject;

                    order=localVar.order;
                    timing=localVar.timing;
                    score=localVar.score;
                    % set expereiment order with old file
                    opts.TrialCSV=localVar.subject.orderCSV;
                    experiment=getorderfile();

                    % restart from the last fully completed block
                    order_completed=order( cellfun(@(x) length(x)>1,order) );

                    [val,startpos] = unique(cell2mat(cellfun(@(x) x(4),order_completed)),'first');
                    [val,endpos]   = unique(cell2mat(cellfun(@(x) x(4),order_completed)),'last');
                    incompleteblock=val(find( endpos - startpos < trialsPerBlock-1) );
                    % there should only be one incomplete block
                    if(length(incompleteblock) > 1)
                       incompleteblock
                       error('too many incompleted blocks!')
                    elseif(length(incompleteblock == 1))
                      
                       % move start back
                       toremove=find(cellfun(@(x) x{4}==incompleteblock,order_completed));
                       start=localVar.trialnum - length(toremove) +1; % we will start on the first of the removed
                       fprintf('starting trial moved from %d to %d\n',localVar.trialnum, start)

                       %% start chopping the incomplete block
                       fprintf('chopping off incomplete block "%d"\n', incompleteblock);
                       fprintf('originally completed trials: %d\t', length(order_completed))

                       % replace all rows of the incomplete block
                       for i=1:length(toremove)
                         order{toremove(i)}= [];
                         timing(i).start=0;
                       end
                      

                       order_completed=order(cellfun(@(x) length(x)>1,order));
                       % reset score
                       score=sum(cellfun(@(x) x{8}, order(1:length(order_completed))));
                       fprintf('new length: %d\n', length(order_completed))
                       fprintf('have completed blocks:\n')
                       disp( unique(cell2mat(cellfun(@(x) x(4),order_completed)),'last') )

                       
                       
                    else
                      % don't need to do anything order is perfect!
                      % we killed right on a new block
                      fprintf('resuming right were we left off!\n')

                      if(mod( start-1, trialsPerBlock) ~= 0)
                       fprintf('.. but maybe we need to increase trial to start on (%d)\n', start);
                       start=start+1; % we never incremented the trial position when we exited
                      end
                    end
                    
                    % sanity check -- length should be a multiple of trialsPerBlock
                    if(mod( length(order_completed), trialsPerBlock) ~= 0)
                      error('trial length is off: mod(%d,%d) ~= 0',length(order_completed), trialsPerBlock)
                    end
                    if(mod( start-1, trialsPerBlock) ~= 0)
                      error('last completed trial is off: mod(%d,%d) ~= 0',start-1, trialsPerBlock)
                    end

                    fprintf('RESUMING ON PRESENTED BLOCK # %d\n', (start-1)/trialsPerBlock + 1); 

                
                % otherwise, move the existing txt file to a backup
                % and we'll fill in the subject info below
                else
                    fprintf('moving %s to %s, start from top\n', txtfile,backup)
                    movefile(txtfile,backup);
                end
                
            end
         end
        
        %% fill out the subject struct if any part of it is still empty
        for attribCell={'gender','age'}
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
        %if ischar(subject.run_num); subject.run_num=str2double(subject.run_num);end


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

    function trigger=defineTrigger()
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

  
    function initTriggerSender()
      
      % only initialize if we need to send triggers
      if(opts.trigger~=1), return,   end
      
      %% Parallel Port -- windows only, preinstalled driver
      % see windows-lpt/README.txt
      %addpath(genpath('windows-lpt')); % add outp function to write to port
      %opts.port=hex2dec('378');
      
      %% Parallel port, Windows + DAQ Legacy Interface only
      if(opts.USEDAQ==1)
          handle = digitalio('parallel','lpt1');
          addline(handle,0:7,'out');
          opts.port = daqgetfield(handle,'uddobject'); % speed up write by caching handle
          % http://psychtoolbox.org/faqttltrigger
      else 
          fprintf('initTriggerSender: disabled\n')
      end
      %% Serial Port - via matlab
      % % see instrhwinfo('serial')
      % % windows
      % opts.port = fopen(serial('COM1') )
      %  % OSX
      %  % serial('/dev/tty/KeySerial1')
      %  % Linux
      %  % serial('/dev/ttys0')
      
      %% Serial Port via psychtoolbox
      % % http://docs.psychtoolbox.org/IOPort
      % [ opts.port , error ] = IOPort('OpenSerialPort','COM1')
      % % for linux use /dev/ttyS0; OSX use  /dev/something
      
      % override any other codes
      sendTrigger(0)
    end

    function sendTrigger(trigger)
      % only send a trigger if we are set up to do so
      if(opts.trigger~=1), return,   end
      
      %% Parallel Port - windows
      %outp(opts.port,trigger)
      
      %% Parallel Port - Legacy
      if(opts.USEDAQ==1)
        putvalue(opts.port,0);      % weird addition prevention heres 
        putvalue(opts.port,trigger);
      end
      
      %% Serial Port -- any
      %fprintf(opts.port,'%d',trigger);
      
      %% Serial Port psychtoolbox
      % [nwritten, when, errmsg, prewritetime, postwritetime, lastchecktime] = IOPort31('Write32', opts.port, trigger,blocking=1);
      
      %% Testing
      if(opts.DEBUG==1)
        fprintf('%d: %d\t%.4f\n',i, trigger,  GetSecs() )
      end
    end
    
    %parse order file
    function experiment=getorderfile()
      fprintf('using %s\n', opts.TrialCSV);
      fid=fopen(opts.TrialCSV);
      %fid=fopen('FaceITI_MEG.csv');
      %indexes={1,2,3,4,5,6};
      %[ facenumC, ITIC, ISIC, blockC, emotionC, rewardC ] = indexes{:};
      experiment=textscan(fid,'%d,%d,%d,%d,%q','HeaderLines',1);
       % ugly unpack of " "," "
      for i=1:length(experiment{5})
          experiment{6}{i} = experiment{5}{i}(findstr(experiment{5}{i},',')+2:end);
          experiment{5}{i} = experiment{5}{i}(1:findstr(experiment{5}{i},',')-1    );
      end
      fclose(fid);
    end
    % get options: MEG, DEBUG, screen=[x y], 'mac laptop','VGA','eyelab'
    function getopts(o)
      
      %% MEG BY DEFAULT
      opts.ports=0; % handle for serial/lpt port
      opts.DEBUG=0;
      opts.test=0;
      useMEG();

      opts.screen=[1280 1024];
      
      
      %% PARSE REST
      i=1;
      while(i<=length(o))
          switch o{i}
              case {'TEST'}
                  opts.test=1;
                  % set order
              case {'DEBUG'}
                  opts.DEBUG=1;
                  opts.screen=[800 600];
              case {'screen'}
                  i=i+1;
                  if isa(o{i},'char')
                      
                    % CogEmoFaceReward('screen','mac laptop')
                    switch o{i}
                        case {'mac laptop'}
                            opts.screen=[1680 1050]; %mac laptop
                        case {'VGA'}
                            opts.screen=[640 480]; %basic VGA
                        case {'eyelab'}
                            opts.screen=[1440 900]; %new eyelab room
                        otherwise
                            fprintf('dont know what %s is\n',o{i});
                    end
                    
                  %CogEmoFaceReward('screen',[800 600])
                  else
                    opts.screen=o{i};    
                  end    
                  
                  
              case {'MEG'}
                  useMEG()

              case {'fMRI'}
                  opts.trigger=0;
                  opts.TrialCSV='FaceITI.csv';
                  opts.screen=[1680 1050];
                  opts.sound=1;

              case {'NODAQ'}
                  opts.USEDAQ=0;

              otherwise
                  fprintf('unknown option #%d\n',i)
          end
          
       i=i+1;    
      
      end
      
      function useMEG()
          opts.MEG=1;
          opts.trigger=1;
          opts.USEDAQ=1;
          opts.sound=0;
          opts.totalSessions=1;
          opts.TrailCSV=dir('MEGorder/unused/*csv');
          opts.TrialCSV=['MEGorder/unused/' opts.TrailCSV(1).name];
      end
      
      disp(opts)
    end

    function drawPhotodiodeBox(color)
       % draw box for photodiode, opposite luminocity as when face is on
       if(opts.MEG==1)
         
        diodeboxes= [0 0 photodiodeWidth  photodiodeWidth; ...
                     screenResolution(1)-photodiodeWidth 0 screenResolution(1)  photodiodeWidth; ...
                     0 screenResolution(2)-photodiodeWidth photodiodeWidth  screenResolution(2); ...
                     screenResolution(1)-photodiodeWidth screenResolution(2)-photodiodeWidth screenResolution(1)  screenResolution(2) ]';  
           
         % Put box on all corners
         Screen('FillRect',w,color,diodeboxes) 
         % or just draw one big one
         %photodiodeRect=[0 0 75 75];
         %Screen('FillRect',w,color,photodiodeRect) 
       end
    end


%% super debug mode -- show EV for reponse times
%    function superDebug()
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
%    end



end

