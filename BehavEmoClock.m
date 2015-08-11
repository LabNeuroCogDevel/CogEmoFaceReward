% CogEmo Face Reward Task:
%
%
%  usage:
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
%  BehavEmoClock
%     Enter the subject ID number: test
%     Is this a restart/want to load old file (y or n)? y
%


function BehavEmoClock(varargin)
%% BehavEmoClock
% adapted from CogEmoFaceReward (written by Will Foran 2012-10-05)
%
% Read FaceBehavOrder.csv
%  get facenum, emotion, and reward for each trial
%  following frank et al. 2009, use half of remaining trial length for ITI
%
% 50 presentations per run
% 25 faces repeated twice each
%
% Runs
%    scram    DEV
%    happy    IEV
%    scram   CEVR
%    happy    DEV
%    scram    IEV
%     fear    DEV
%    scram    CEV
%     fear    IEV
%
% each presentation can last up to 4 seconds
% the subject can hit space at any time
% reward is calculated based on the time allowed to elapse and the current reward function
%
% score function from M. Frank
% output emulates his timeconflict.m
%
% This used 'KbDemo' as template

%window pointer, slack, and subject structure are global across functions
global w slack subject resppad facenumC blockC emotionC rewardC ITIC experiment totalBlocks trialsPerBlock;

%% PROCESS ARGUMENTS GIVEN TO FUNCTION
% * are we using a response pad?
% * TODO: what resolution to use?, this should all be done in a for loop with class(varargin{i})
resppad=[];
% find all character arguments to the function
vanCharI    = varargin(cellfun(@(x) any(strmatch(class(x),'char')), varargin ));
% find all arguments that match usb or com (port for Cedrus)
vanCharIrpI = cellfun(@(x) ~isempty(regexpi(x,'usb|com')), vanCharI);

if(any(vanCharIrpI))
  %figure out port number
  port=vanCharI{vanCharIrpI==1};
  fprintf('USING RESPONSE BOX: %s\n', port)
  try CedrusResponseBox('CloseAll'); end
  try
   %% sometimes opening the the device seems to lead to
   %% an endless loop. Works when dev is *USB0
   resppad = CedrusResponseBox('Open',port );
   dev     = CedrusResponseBox('GetDeviceInfo', resppad);
   evt     = CedrusResponseBox('GetBaseTimer',resppad);
   % eprime likes to have pin one toggled, set this
   %CedrusResponseBox('SetOutputLineLevels',resppad, [);
  catch
   error('could not open response box!')
  end

end


%screenResolution=[640 480]; %basic VGA
%screenResolution=[1600 1200];
%screenResolution=[1440 900]; %new eyelab room
%screenResolution=[1680 1050]; %mac laptop
%screenResolution=[1024 768]; %mac laptop
screenResolution=[1920 1080]; %bellefield dell




textSize=22; %font size for intructions etc.

%buyer beware: do not uncomment this for production use
%Screen('Preference', 'SkipSyncTests', 1);

receiptDuration  = .9;  %show feedback for 900ms
postResponseISI = .05;  %50ms delay between response and feedback
postFeedbackITI = .10;  %100ms delay after feedback prior to next trial. Any ITI is added to this.
timerDuration    = 4.0; %time for revolution of clock

%N.B.: read in of design (blocks and trials) now internal to getSubjInfo to allow for customization based on session number.

%obtain subject and run information
%this populates:
% 1) subject.blockColors (colors of rectangles around stimuli for each block)
% 2) subject.runITIs (runs x trials matrix of ITIs)
% 3) subject.run_num (run to be executed)
% 4) runTotals (total points per run, clearing out old totals for re-run)
% 5) order (cell array of behavior)

[order, runTotals, filename] = getSubjInfo('BehavEmoClock');

%% Counter balance order of runs by reversing order for odd subjects
if mod(subject.subj_id, 2) == 1
    fprintf('NOTE: odd subject, order reversed of input csv!\n')
    %reverse by blocks
    blockIndices = reshape(1:length(experiment{blockC}), trialsPerBlock, totalBlocks);
    blockIndices = reshape(blockIndices(:,totalBlocks:-1:1), 1, []);
    experiment{facenumC} = experiment{facenumC}(blockIndices);
    experiment{emotionC} = experiment{emotionC}(blockIndices);
    experiment{rewardC} = experiment{rewardC}(blockIndices);
    %do not reverse block codes since this should always be ascending
end

%% Initialize data storage and records
% make directories
for dir={'subjects','logs'}
    if ~ exist(dir{1},'dir'); mkdir(dir{1}); end
end

% log all output of matlab
diaryfile = ['logs/BehavEmoClock_' num2str(subject.subj_id) '_' num2str(GetSecs()) '_tcdiary'];
diary(diaryfile);

% log presentation, score, timing (see variable "order")
txtfid=fopen([filename '.txt'],'a'); % append to existing log

if txtfid == -1; error('couldn''t open text file for subject'); end

% print the top of output file
if subject.run_num == 1
    fprintf(txtfid,'#Subj:\t%i\n', subject.subj_id);
    fprintf(txtfid,'#Run:\t%i\n',  subject.run_num);
    fprintf(txtfid,'#Age:\t%i\n',  subject.age);
    fprintf(txtfid,'#Gender:\t%s\n',subject.gender);
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
    [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [0 0 screenResolution] );
    FlipInterval = Screen('GetFlipInterval',w); %monitor refresh rate.
    slack = FlipInterval/2; %used for minimizing accumulation of lags due to vertical refresh
    
    % Set process priority to max to minimize lag or sharing process time with other processes.
    Priority(MaxPriority(w));
    
    %do not echo keystrokes to MATLAB
    %ListenChar(2); %leaving out for now because crashing at MRRC
    
    HideCursor;
    
    % Do dummy calls to GetSecs, WaitSecs, KbCheck to make sure
    % they are loaded and ready when we need them - without delays at the wrong moment.
    KbCheck;
    WaitSecs(0.1);
    GetSecs;
    
    %permit transparency
    Screen('BlendFunction', w, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
    % Set text display options. We skip on Linux.
    %if ~IsLinux
    Screen('TextFont', w, 'Arial');
    Screen('TextSize', w, textSize);
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
    caretKey = KbName('6^'); %used for scanner trigger
    equalsKey = KbName('=+'); %used for scanner trigger
    
    %% preload textures
    % makes assumption that images for every face of every facenumber exists
    for emo=unique(experiment{emotionC})'
        for facenum=unique(experiment{facenumC})'
            stimfilename=strcat('faces/',emo{1},'_',num2str(facenum),'.png');
            [imdata, ~, alpha]=imread(stimfilename);
            imdata(:, :, 4) = alpha(:, :); %add alpha information
            % make texture image out of image matrix 'imdata'
            facetex.(emo{1}){facenum} = Screen('MakeTexture', w, imdata);
        end
    end
        
    %% Instructions
    
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
    if subject.run_num==1
        % show long instructions for first time player
        for instnum = 1:length(Instructions)
            DrawFormattedText(w, Instructions{instnum},'center','center',black);
            Screen('Flip', w);
            waitForResponse;
            % clear events if we are using a control box, otherwise long presses flash through instructions
            if(~isempty(resppad)),CedrusResponseBox('FlushEvents', resppad); end
        end
    else
        DrawFormattedText(w, ['Welcome Back!\n\n' InstructionsBetween],'center','center',black);
        Screen('Flip', w);
        waitForResponse;
    end
    

    %flip to screen first frame of task
    [scannerStart, priorFlip] = Screen('Flip', w);
    
    fprintf('pulse flip: %.5f\n', priorFlip);

    %count down from 3 to 1, then a 1-second blank screen.
    for cdown = 1:3
        drawRect;
        DrawFormattedText(w, ['Beginning in\n\n' num2str(4.0 - cdown)],'center','center',black);
        priorFlip = Screen('Flip', w, scannerStart + (cdown - 1.0) - slack);
        %fprintf('cdown: %d, fix flip: %.5f\n', cdown, priorFlip);
        %WaitSecs(1.0);
    end
    
    %1 second of blank screen
    drawRect;
    fixation(1.0, 0, priorFlip + 1.0);
    
    pretrialEnd=GetSecs();
    pretrialLength=pretrialEnd - scannerStart;
    
    fprintf('pretrialLength was: %.5f\n', pretrialLength);
    
    %determine start and end trials based on block to be run
    startTrial = (subject.run_num-1)*trialsPerBlock + 1;
    endTrial = subject.run_num*trialsPerBlock;
    blockTrial = 1; %track the trial number within block
    
    %order of fields in order array
    orderfmt = { 'run', 'trial', 'rewFunc', 'emotion', 'magnitude', 'probability', 'score', 'ev', 'rt', 'clock_onset', ...
        'isi_onset', 'feedback_onset', 'iti_onset' 'iti_ideal' 'image' };

    %error('end here');
    %% THE BIG LOOP -- complete all trials for a run
    for i=startTrial:endTrial
        subject.trial_num=i;
        fprintf('start trial %d\n',subject.trial_num)
        %% debug, start time keeping
        % start of time debuging global var
        checktime=GetSecs();
        startOfTrial=checktime;
        % seconds into the experiment from start of for loop
        timing.start = checktime - scannerStart;
        
        %% face (4s) + ITI + score + ISI
        
        % show face, record time to spacebar
        [RTms, firstClockFlip, keyPressed] = faceWithTimer;
        setTimeDiff('clock'); %build times (debug timing)
        
        %based on flip time of final clock frame (at time of response), build expected timings below

        %show brief fixation after response
        %remove wait calls from functions such that they return immediately with flip time
        %then add appropriate time to when such that it waits at the next step.
        [isiFlip] = fixation(postResponseISI, 0, firstClockFlip + RTms/1000);
        setTimeDiff('ISI'); %build times (debug timing)
        
        % show score
        feedbackFlip = scoreRxt(RTms, experiment{rewardC}{subject.trial_num}, firstClockFlip + RTms/1000 + postResponseISI);
        setTimeDiff('receipt'); %build times (debug timing)
        
        %show fixation for min (100ms) plus scheduled ITI
        [ITIflip] = fixation(postFeedbackITI + (timerDuration - RTms/1000)/2, 1, firstClockFlip + RTms/1000 + receiptDuration + postResponseISI);
        
        setTimeDiff('ITI'); %build times (debug timing)
        
        timing.end= GetSecs() - startOfTrial;
        
        %% non critical things (debugging and saving)
        %nonPresTime=tic;
        
        %% write to data file
        emo=experiment{emotionC}{subject.trial_num};
        face=experiment{facenumC}(subject.trial_num);
        
        %set the output of the order structure
        trial = { subject.run_num subject.trial_num experiment{rewardC}{subject.trial_num} experiment{emotionC}{subject.trial_num} ...
            F_Mag F_Freq inc ev RTms (firstClockFlip - scannerStart) (isiFlip - scannerStart) ...
            (feedbackFlip - scannerStart) (ITIflip - scannerStart) ((timerDuration - RTms/1000)/2 + postFeedbackITI) strcat(emo,'_',num2str(face),'.png') };
        
        order(subject.trial_num) = {trial};
        
        % print header
        if subject.trial_num == 1
            fprintf(txtfid,'Run\tTrial\tFunc\tEmotion\tMag\tProb\tScore\tEV\tRT\tClock_Onset\tISI_Onset\tFeedback_Onset\tITI_Onset\tITI_Ideal\tImage\n');
        end
        
        fprintf(txtfid,'%d\t',order{subject.trial_num}{1:2} );
        fprintf(txtfid,'%s\t',order{subject.trial_num}{3:4} );
        fprintf(txtfid,'%4i\t',order{subject.trial_num}{5:14} );
        fprintf(txtfid, '%s', strcat(emo,'_',num2str(face),'.png') );
        fprintf(txtfid, '\n');
        
        % save to mat so crash can be reloaded
        trialnum=subject.trial_num; % this can be removed?
        save(filename,'order','orderfmt','trialnum','blockTrial','subject','runTotals');
        
        blockTrial = blockTrial + 1;
        
        %% debug, show time of this trial
        
        expected.clock   = timing.clock; %use the observed RT for expectation %timerDuration;
        expected.ITI     = double((timerDuration - RTms/1000)/2 + postFeedbackITI);
        expected.receipt = receiptDuration;
        expected.ISI     = double(postResponseISI);
        expected.end     = 0; 
        %expected.end     = sum(struct2array(expected)); %the sensible version, but somehow MRRC is missing struct2array
        expected.end     = sum(cellfun( @(x) x, struct2cell(expected))); %the ugly, but available, version.
        
        fprintf('\n%d: %s_%d.png\n%.2f in, expected, obs, diff\n',subject.trial_num, experiment{emotionC}{subject.trial_num},experiment{facenumC}(subject.trial_num),timing.start);
                
        for f = {'clock'  'ISI' 'receipt' 'ITI' 'end' };
            f=f{1};
            fprintf('%s\t%.4f\t%.4f\t%.2f\n', f, expected.(f), timing.(f), (timing.(f)-expected.(f))*1000);
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
    
    %%End of run, potentially with notification of bonus payment
    earnedmsg='';
    if subject.run_num == totalBlocks
        % everyone should earn the bonus
        % but they should have at least 2000 pts
        if(sum(runTotals) > 2000), earnedmsg='\n\nYou earned a $25 bonus !'; end
    end
    
    msgAndCloseEverything(['Your final score is ', num2str(sum(runTotals)) ,' points', earnedmsg, '\n\nThanks for playing!']);
    return
    
catch
    Screen('CloseAll');
    Priority(0); %reset to normal priority
    psychrethrow(psychlasterror);
    ListenChar(0);
    ShowCursor;
end

% close the screen
Priority(0); %reset to normal priority
ListenChar(0);
ShowCursor;
sca

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           support functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   function [seconds, VBLT] = scannerPulseSync
        while(1)
            [ keyIsDown, seconds, keyCode ] = KbCheck;
            
            if(keyIsDown && keyCode(escKey))
                msgAndCloseEverything(['Quit on trial ' num2str(subject.trial_num)]);
                error('quit early (on %d)\n',subject.trial_num)
            end
            
            if(keyIsDown && (keyCode(caretKey) || keyCode(equalsKey))), break; end
            WaitSecs(.0005);
        end
        % change the screen to prevent key code carrying forward
        % and obtain time stamp of pulse for use with timings
        [VBLT, SOnsetTime] = Screen('Flip', w);
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
        Priority(0);
        ListenChar(0);
        ShowCursor;
        sca
        if(~isempty(resppad)), CedrusResponseBox('Close', resppad); end
    end

%% print time since last check
% updates global timing struct and checktime double
    function setTimeDiff(interval)
        timing.(interval) = (GetSecs() - checktime);
        checktime=GetSecs();
    end

%% Meat -- show the face and revolving dot (timer)
    function [elapsedMS, firstVBLT, keyPressed] = faceWithTimer
        
        % make sure a key isn't being pressed before trial
        % prevents person from holding down button
        while KbCheck; end
        
        % dot size and dist from center
        spotRadius         = 150;        % The radius of the spot from center.
        spotSize           = 10;         % The radius of the spot's fill.
        initialDotPosition = 3 * pi / 2; % The initial position. -- 12'o clock
        
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
        firstFlip = 1;
        
        % Draw border color based on block. Only call once outside of animation loop
        drawRect;
        
        emo=experiment{emotionC}{subject.trial_num};
        facenum=experiment{facenumC}(subject.trial_num);
        
        clearmode=2; %don't clear frame buffer
        
        %elapsedMS = 0;
        
        %get timestamp of first flip
        %[VBLT, SOnsetTime] = Screen('Flip', w, 0, clearmode);
        keyPressed=0;
        
        %listen to 1-5 (right button glove)
        validKeys=[ KbName('1!') KbName('2@') KbName('3#')...
                    KbName('4$') KbName('5%') ];
        
        % Loop while there is time.
        while remainingMS > 0
            elapsedMS = round((GetSecs()*10^3 - startTimeMS) );
            remainingMS = durationMS - elapsedMS;
            
            %Screen('DrawText', w, sprintf('%i ms remaining...',remainingMS), 20, 20, black);
            %Screen('DrawText', w, sprintf('%i ms elapsed...',elapsedMS), 20, 40, black);
                        
            % white circle over trial area
            Screen('FillOval', w, [255 255 255], CenterRect([ 0 0 2*(spotRadius+spotSize)+10 2*(spotRadius+spotSize)+10 ], windowRect));
            
            % put the image up
            Screen('DrawTexture', w,  facetex.(emo){facenum}  );
            
            % at 4 seconds, we do a full rotation
            theta =  initialDotPosition - (remainingMS/durationMS * 2 * pi) ;
            xOffset = spotRadius * cos(theta);
            yOffset = spotRadius * sin(theta);
            
            offsetCenteredspotRect = OffsetRect(centeredspotRect, xOffset, yOffset);
            Screen('FillOval', w, [0 191 95], offsetCenteredspotRect);
            
            Screen('DrawingFinished', w); %tell PTB that we have finished with screen creation -- minimize timing delay
                        
            [ keyIsDown, keyTime, keyCode ] = KbCheck;
            evt=[];
            if(~isempty(resppad))
              evt = CedrusResponseBox('GetButtons', resppad);
            end

            if(keyCode(escKey));
                msgAndCloseEverything(['Quit on trial ' num2str(subject.trial_num)]);
                error('quit early (on %d)\n',subject.trial_num)
            end
               
            if any(keyCode(validKeys)) || ~isempty(evt)                
                %if keyCode(spaceKey)
                keyPressed=1; %person responded!
                break
            end

            
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
            [VBLT, SOnsetTime] = Screen('Flip', w, 0, clearmode);
            if firstFlip == 1
                firstOnset=SOnsetTime;
                firstVBLT=VBLT;
                firstFlip = 0;
                fprintf('first flip: %f\n',firstOnset)
            end
                        
            % Wait 0.5 ms before checking the keyboard again to prevent
            % overload of the machine at elevated Priority():
            WaitSecs(0.0005);
        end
        
        if keyPressed == 1
            elapsedMS = round((keyTime - firstOnset) * 10^3);
            subject.resptime.comp(subject.trial_num)= elapsedMS ;

            if ~isempty(resppad)
               elapsedMS = (evt.rawtime - cedrustime) * 10^3;
               subject.resptime.resppad(subject.trial_num)= elapsedMS ;
            end
        else
            elapsedMS = round((VBLT - firstOnset) * 10^3);
        end

        return;
    end

%% wait for a response
    function seconds = waitForResponse
        while(1)
            [ keyIsDown, seconds, keyCode ] = KbCheck;
            % get respponse pad
            evt=[];
            if(~isempty(resppad))       
              evt = CedrusResponseBox('GetButtons', resppad);
            end
            
            if(keyCode(escKey));
                msgAndCloseEverything(['Quit on trial ' num2str(subject.trial_num)]);
                error('quit early (on %d)\n',subject.trial_num)
            end
            
            if( any(keyCode) || ~isempty(evt) ); break; end 
            WaitSecs(.001);
        end
        Screen('Flip', w); % change the screen so we don't hold down space
        WaitSecs(.2);
    end


%% score based on a response time and Rew Func (as string, eg. 'CEV')
    function scoreflip = scoreRxt(RTms, func, reftime)
        % random threshold
        rd=rand(1);

        if (keyPressed==0)
            %no response
            inc = 0;
            ev = 0;
            F_Mag = 0;
            F_Freq = 0;
        else
            [F_Mag, F_Freq] = getScore(RTms,func);
            
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
                        
            % is freq above thresold and do we have a resonable RT
            if F_Freq > rd
                runTotals(subject.run_num) = runTotals(subject.run_num) + F_Mag;
                inc=F_Mag;
            else
                inc=0;
            end
        end
        
        subject.reward.value(subject.trial_num)=inc;
        subject.reward.ev(subject.trial_num)=ev;

        fprintf('%s: ev=%.2f; Mag=%.2f; Freq: %.2f; rand: %.2f; inc: %d; pts- block: %d; total: %d\n', ...
            experiment{rewardC}{subject.trial_num}, ev, F_Mag, F_Freq, rd, inc, runTotals(subject.run_num), sum(runTotals));
        
        %%% Draw
        drawRect;
        %Screen('DrawText', w, sprintf('Your Score is: %d\nrecorded rxt: %d', score, rspnstime));
        %DrawFormattedText(w, sprintf('Total score is: %d\nincrease is: %d\nradnom vs Freq (ev): %f v %f (%f)\nrecorded rxt: %d', score,F_Mag,rd,F_Freq,ev, RT),'center','center',black);
        Screen('TextSize', w, textSize);
        fprintf('RT is: %.2f\n', RTms);
        if keyPressed == 0
            %DrawFormattedText(w, sprintf(['You earned 0 points because you did not respond in time.\n\n' ...
            %    'Please respond before the ball goes all the way around.\n\n'...
            %    'Total points this game: %d points'], runTotals(subject.run_num)),'center','center',black);
            
            DrawFormattedText(w, ['You won 0 points.\n\n\n' ...
                'Please respond before the ball goes all the way around.\n\n'],'center','center',black);
        else
            %DrawFormattedText(w, sprintf('You won:  %d points\n\nTotal points this game: %d points', inc,runTotals(subject.run_num)),'center','center',black);
            DrawFormattedText(w, sprintf('You won\n\n%d\n\npoints', inc),'center','center',black);
        end
        
        Screen('DrawingFinished', w); %tell PTB that we have finished with screen creation -- minimize timing delay
        
        scoreflip = Screen('Flip', w, reftime); %onset of feedback
        
        drawRect;
        lastflip = Screen('Flip', w, reftime + receiptDuration - slack); %offset of feedback
        %WaitSecs(receiptDuration-toc(startScoreTime));
        
    end


end

