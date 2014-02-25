%%%%%
%
% MEGClockTask
% different from other clock tasks:
%  * use photodiode
%  * send triggers via command port (data aquisition toolbox)
%  * no delay after quick responses

function subject = MEGClockTask(sid,blk,varargin)
  clear -GLOBAL opts subject;
  global opts subject;
  subject.subj_id=sid;
  subject.run_num=blk;

  %% ClockTask
  
  %% Settings:
  % get options: MEG, DEBUG, screen=[x y], 'mac laptop','VGA','eyelab'
  %    sets opts.trigger=1 if MEG
  %    also set emofuncs and stimtimes
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
     
  
  start              = 1;
  subject.trialnum   = start;


  txtfid      = 0; %just so we know the file pntr's not a private nested function var

  receiptDuration  = .85; %850ms, match fMRI
  timerDuration    = 4;
  
  % initialize total points earned
  % incremented as a function (inc,dec,const)
  subject.score = 0;
  blockTotal = 0;
  
  %% set order of trials 
  % experiment order file indexes
  indexes={1,2,3,4,5,6};
  [ facenumC, ITIC, ISIC, blockC, emotionC, rewardC ] = indexes{:};
  %  read in order of things
  %experiment=getorderfile();


  
  totalnumblocks = length(opts.emofuncs);
  totalnumtrials = opts.trialsPerBlock * totalnumblocks;

  %initialize time keeping objects
  subject.timing(totalnumtrials+1).start=Inf;

  %% generate experiment
  % - do this as part of getting subject info 
  % subject.experiment=genTimingOrder(emofuncs,numfaces,opts.trialsPerBlock,stimtimes);
  

    
  
  
  %% defines triggers as a struct
  % trigger.(reward function).(emotion).(face or score)
  % plus trigger.(ITI,ISI,done)
  trigger=defineTrigger();


  totalSessions = opts.totalSessions; %broken into first and second halves
  halfwaypt=floor(totalnumtrials/totalSessions); % 252
  

  %% populate subject structure
  %     prompt for subject info or ask to resume task
  %     subject.*, start, etc  
  subject=getSubjInfo('MEG',subject, totalnumblocks,opts.trialsPerBlock,opts,blk);


  % log all output of matlab
  diaryfile = fullfile( 'logs', [num2str(subject.subj_id) '_' num2str(GetSecs()) '_tcdiary.log'] );
  diary(diaryfile);

  % save mat output to a textfile too
  fprintf('saving to %s\n', subject.txtfile);
  txtfid      =fopen(subject.txtfile,'a'); % append so we only have one text file but all blocks

  %% sanity checks
  % how long (trials) is a block
  [junk,blockchangeidx] = unique(subject.experiment{blockC});
  uniquetrialsperblock  = unique(diff(blockchangeidx)); % 42
  if(length(uniquetrialsperblock) > 1) 
      fprintf('Whoa!? different trial lengths? I dont know whats goign on!')
  end
  
  % does our calulation of trials per block match what was given
  if totalnumblocks ~= length(subject.experiment{blockC})/opts.trialsPerBlock 
      fprintf('Whoa!? total number of blocks had two different values?? I dont know whats goign on!')
  end


  
  %% print the top of output file if needed
  if start == 1
    fprintf(txtfid,'#Subj:\t%d\n', subject.subj_id);
    fprintf(txtfid,'#Run:\t%i\n',  subject.run_num); 
    fprintf(txtfid,'#Age:\t%i\n',  subject.age);
    fprintf(txtfid,'#Gender:\t%s\n',subject.gender);
    
  end
  
  % move order file into used if we've used it
  %if (strfind(opts.TrialCSV,'unused') )
  %  % move the order file from unusused to used
  %  [rootdir, csvfilename, csvext]=fileparts(opts.TrialCSV);
  %  neworderfilename=[ 'MEGorder/used/' subject.subj_id '_' csvfilename csvext ];
  %  movefile(opts.TrialCSV,neworderfilename);  
  %  subject.orderCSV=neworderfilename;
  %  % for f in MEGorder/used/*csv; do cp $f MEGorder/unused/${f##*_}
  %end
  
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
     for emo=unique(subject.experiment{emotionC})'
         for facenum=unique(subject.experiment{facenumC})'
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
     if sum(subject.blockTrial)==0
         % show long instructions for first time player
         for instnum = 1:length(Instructions)
             DrawFormattedText(w, Instructions{instnum},'center','center',black);
             Screen('Flip', w);
             waitForResponse('space');
         end
         getReady(0);

        % inialize the order of events only if we arn't resuming
        subject.order=cell(length(subject.experiment{facenumC}),1);
     
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
     
     subject.trialnum=start; % fixation calls drawRect which uses i to get the block number
     fixation(opts.fixation);
     

     %% TODO: how/when to get '^' for getting scanner start
     scannerStart = GetSecs();
    
     %% debug, timing
     StartOfRunTime=scannerStart;

  
     %% only work on a block
     % get start and end from run_num (block number)
     startTrial = (subject.run_num-1)*opts.trialsPerBlock + 1;
     endTrial   = subject.run_num*opts.trialsPerBlock;
     subject.blockTrial(subject.run_num) = 1; %track the trial number within block
   
     
     %% THE BIG LOOP -- for all remaining trials or to the halfwaypt
     
     for i = startTrial:endTrial
        subject.trialnum = i;
        
        emo=subject.experiment{emotionC}{subject.trialnum};
        rew=subject.experiment{rewardC}{subject.trialnum};
        face=subject.experiment{facenumC}(subject.trialnum);
        
        %% debug, start time keeping
        % start of time debuging global var
        startOfTrialTime=GetSecs();
        checktime=startOfTrialTime; % initialize checktime to start
        % seconds into the experiement from start of for loop
        subject.timing(subject.trialnum).start=startOfTrialTime-StartOfRunTime;
        
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
        fixation(subject.experiment{ITIC}(subject.trialnum),trigger.ITI);
        
        setTimeDiff('ITI'); %build times (debug timing)

        % show score -- will send trigger and show box for diode before flip if needed
        % send trigger for receipt +4 if rewarded
        scoreRxt(rspnstime,rew,trigger.(rew).(emo).score);
        
        
        sendTrigger(trigger.(rew).(emo).score);
        
        setTimeDiff('receipt'); %build times (debug timing)
        
        % show second fixation
        fixation(subject.experiment{ISIC}(subject.trialnum),trigger.ISI);
        
        setTimeDiff('ISI'); %build times (debug timing)
        
        
        %% non critical things (debuging and saving)
        %nonPresTime=tic;
        
        %% write to data file

        
        %set the output of the order structure
        trial = { subject.experiment{rewardC}{subject.trialnum} ...
                  subject.run_num ...
                  subject.trialnum ...
                  subject.experiment{blockC}(subject.trialnum)  ...
                  0 ...
                  t_start ...
                  F_Mag ...
                  inc ...
                  F_Freq ...
                  ev ...
                  rspnstime ...
                  emo };

        subject.order(subject.trialnum) = {trial};
        
        % print header
        if subject.trialnum == 1
            fprintf(txtfid,'Func\tRun\tTrial\tBlock\tNull\ttrialStartTime\tMag\tScoreInc\tFreq\tEV\tRT\tEmotion\tImage\n');
        end
        
        fprintf(txtfid,'%s\t',subject.order{subject.trialnum}{1} );
        fprintf(txtfid, '%4i\t', subject.order{subject.trialnum}{2:end});
        fprintf(txtfid, '%s\t', emo, strcat(emo,'_',num2str(face),'.png') );
        fprintf(txtfid, '\n');
        
        % save to mat so crash can be reloaded
        trialnum=subject.trialnum;
        save(subject.matfile,'trialnum','subject');
        subject.blockTrial(subject.run_num) = subject.blockTrial(subject.run_num) + 1;
       
        % line like
        % CEVR       1      22       5       2    4.481533e+01   176       0       2.136929e-01    3.764108e+01    2.399020e+02
        % PHASE      RUN    TRIAL    BOXC   NULL   TRACT        FMAG   FMAGP(fmp)   FFREQ(ff=F_Freq)      EV              RT
        % | --------------- input file ---------|
        %%% tract = (trial start time    -  trstamp)/TR
        %%% null = old exprm field
        %%% we only have one run of the experiment
        
        
        %% debug, show time of this trial
        
        setTimeDiff('end');
              
        subject.timing(subject.trialnum).ITI.expected     = double(subject.experiment{ITIC}(subject.trialnum))/10^3;
        subject.timing(subject.trialnum).receipt.expected = receiptDuration;
        subject.timing(subject.trialnum).ISI.expected     = double(subject.experiment{ISIC}(subject.trialnum))/10^3;
        subject.timing(subject.trialnum).facetrig         = trigger.(rew).(emo).face;
        subject.timing(subject.trialnum).scoretrig        = trigger.(rew).(emo).score;
        if(opts.DEBUG)
            fprintf('%d: start@%.4f\t%s_%d.png\n%.2f\n',subject.trialnum,subject.timing(subject.trialnum).start, subject.experiment{emotionC}{subject.trialnum},subject.experiment{facenumC}(subject.trialnum));
            for f = {'ITI' 'receipt' 'ISI' 'end'};
                f=f{1};
                fprintf('\t%.4f\t%s\n', subject.timing(subject.trialnum).(f).onset, f);
            end
        end
        
        
        
        
                 
        %% instructions if new block
        % if i=halfwaypt, though mod 42==0, this is never seen
        % also, only display instructions between if this is not the last trial.
        if subject.trialnum > (opts.trialsPerBlock - 1) && ...     % we've done a full block
           mod(subject.trialnum, opts.trialsPerBlock) == 0 && ...  % this is the trial number that is the size of the block
           subject.trialnum < (opts.trialsPerBlock*totalnumblocks) % this is not the very last block

            Screen('TextSize', w, 22);
            %% give subj a 60 second break with countdown            

            % change screen color to alert operator
            Screen('FillRect',w,black,[0 0 screenResolution])
            
            DrawFormattedText(w, ...
                [ '\n\nYou have ' num2str(subject.score) ' points so far\n\n'...
                'Completed Game: ' num2str(floor(subject.trialnum/opts.trialsPerBlock)) ' of ' num2str(totalnumblocks)
                ],'center','center',white);               
            Screen('Flip',w);
            Screen('FillRect',w,backgroundColor,[0 0 screenResolution])
            waitForResponse('space');

        end
        

     end 

    % everyone should earn the bonus
    % but they should have at least 2000 pts
    %
    if subject.trialnum >= (opts.trialsPerBlock*totalnumblocks) % this is it!
     earnedmsg='\n\nYou earned a $25 bonus !'; 
     if(subject.score<2000); earnedmsg=''; end;
     msgAndCloseEverything(['Your final score is ', num2str(subject.score) ,' points', earnedmsg, '\n\nThanks for playing!']);

    else
      closeEverything();
    end

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
       closeEverything()
    end
    function closeEverything()
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
       
       if(subject.trialnum<length(subject.experiment))
        error('quit early (on %d)\n',subject.trialnum)
       end
       
       return
    end

   %% print time since last check
   % updates global timing struct and checktime double
   function setTimeDiff(interval)
       subject.timing(subject.trialnum).(interval).onset = GetSecs()-startOfTrialTime;
       subject.timing(subject.trialnum).(interval).diff  = subject.timing(subject.trialnum).(interval).onset - checktime;
       checktime=GetSecs();
   end 



   %% block indicator
    function drawRect(varargin)
       t=subject.trialnum;
       % allow block to be specified, mostly for instruction display
       % b/c we want to show the color of the next block
       if nargin>0
           t=varargin{1};
       end
       if( t > length(subject.experiment{blockC}) )
           rgbcolorIDX=1;
           fprintf('Something funny is happening!! -- we are at t=%i\n',t)
       else
           rgbcolorIDX=subject.experiment{blockC}(t);
       end
       Screen('FrameRect', w, subject.blockColors(rgbcolorIDX,:), [], frameWidth);
       
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
     keyIsDown = 0;
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
        emo=subject.experiment{emotionC}{subject.trialnum};
        facenum=subject.experiment{facenumC}(subject.trialnum);
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
                msgAndCloseEverything(['Quit on trial ' num2str(subject.trialnum)]);
                error('quit early (on %d)\n',subject.trialnum)
            elseif any(keyCode(acceptableKeyPresses))
                break
            end
        end
        
        % Wait 1 ms before checking the keyboard again to prevent
        % overload of the machine at elevated Priority():
        %WaitSecs(0.001);
     end
     
     elapsedMS = round(GetSecs()*10^3 - startTimeMS);
     if ~keyIsDown
         elapsedMS = (timerDuration+1)*10^3; % make sure we know this trial is bogus
     end
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
        
        %disp([waittime, subject.experiment{ITIC}(i), subject.experiment{ISIC}(i), 4000 - rspnstime, rspnstime ])
   end
   
   %% get ready screen, so experementer knows they're going to start
   % first getRead flashes for the photodiode
   function getReady(varargin)
     Screen('TextSize', w, 22);
     if(isempty(varargin))
         drawRect(subject.trialnum+1)
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
                  msgAndCloseEverything(['Quit on trial ' num2str(subject.trialnum)]);
                  error('quit early (on %d)\n',subject.trialnum)
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
       
       % is this RT usable?
       % 200 just incase they did actually push the button 
       % 1second is added if no response
       usableRT = ~ (RT > timerDuration*10^3 + 200);
       
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
        if F_Freq > rd && RT~=0 && usableRT
            subject.score=subject.score+F_Mag;
            blockTotal=blockTotal+F_Mag;
            inc=F_Mag;
        else
            %score=score;
            inc=0;
        end
        fprintf('%d % 4s:: ev=% 3.2f; Mag=% 4.2f; Freq: %.2f; rand: %.2f; inc:% 4d; pts:: block:% 4d; total:% 6d\n', ...
                subject.trialnum,subject.experiment{rewardC}{subject.trialnum},ev, F_Mag,F_Freq,rd,inc,blockTotal,subject.score);
       
        
        %%% Draw
        drawRect;
        %Screen('DrawText', w, sprintf('Your Score is: %d\nrecorded rxt: %d', score, rspnstime));
        %DrawFormattedText(w, sprintf('Total score is: %d\nincrease is: %d\nradnom vs Freq (ev): %f v %f (%f)\nrecorded rxt: %d', score,F_Mag,rd,F_Freq,ev, RT),'center','center',black);
        Screen('TextSize', w, 22);
        
        % warn if there was no response!
        % otherwise just show how many points were gained
        if ~usableRT 
            DrawFormattedText(w, ['You won 0 points.\n\n\n' ...
                'Please respond before the ball goes all the way around.\n\n'],'center','center',black);
        else
            DrawFormattedText(w, sprintf('You \nwon\n%d\npoints', inc),'center','center',black);
        end
        % varagin will be trigger, send if we have it
        % trigger is +4 if rewarded
        if(~isempty(varargin)), sendTrigger(varargin{1} + 4*inc>0), end
        
        
        % draw box for photodiode, opposite luminocity as when face is on
        drawPhotodiodeBox(ScorePhotodiodeColor) 
       
        % draw screen
        Screen('Flip', w);
        WaitSecs(receiptDuration-toc(startScoreTime));
    
   end

    
    % get options: MEG, DEBUG, screen=[x y], 'mac laptop','VGA','eyelab'
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





end

