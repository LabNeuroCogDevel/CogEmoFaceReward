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
%  fMRIEmoClock
%     Enter the subject ID number: test
%     Is this a restart/want to load old file (y or n)? y
%

% TODO/DONE
function fMRIEmoClock
%% fMRIEmoClock
% adapted from CogEmoFaceReward (written by Will Foran 2012-10-05)
%
% Read FaceFMRIOrder.csv
%  get facenum, emotion, and reward for each trial
%  ITI distribution randomly sampled from 360 optimal runs located in fMRIOptITIs_284s_38pct.mat
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
% reward is calcluated based on the time allowed to elapse and the current reward function
%
% score function from M. Frank
% output emulates his timeconflict.m
%
% This used 'KbDemo' as template

%screenResolution=[640 480]; %basic VGA
%screenResolution=[1600 1200];
%screenResolution=[1440 900]; %new eyelab room
%screenResolution=[1680 1050]; %mac laptop
screenResolution=[1024 768]; %mac laptop

textSize=22; %font size for intructions etc.

%buyer beware: do not uncomment this for production use
%Screen('Preference', 'SkipSyncTests', 1);

% [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 255 255 255], [0 0 640 480] );
% [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [0 0 1600 1200] );
% [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 204 204 204], [0 0 1440 900] );

startRun       = 1; %default to running the first block
blockColors    = []; %empty so that subfunction getSubjInfo can set
txtfid      = 0; %just so we know the file pntr's not a private nested function var

receiptDuration  = .9;  %show feedback for 900ms
postResponseISI = .05;  %50ms delay between response and feedback
postFeedbackITI = .10;  %100ms delay after feedback prior to next trial. Any ITI is added to this.
timerDuration    = 4.0; %time for revolution of clock
preStartWait     = 8.0; %initial fixation

% initialize total points earned
% incremented as a function (inc,dec,const)
score = 0;

%% set order of trials

%%for fMRI, create a random (but optimal) timing distribution for each

%notes for trial structure:
% clock face displayed for maximum of 4 seconds
% median RT in behavioral data is 1753ms
% max median RT per subject was 2749ms

% Based on R calculations, we want to optimize the ITI sequence and distribution with an assumption
% of 2-second avg RTs and a target presentation percentage of 55%.
% thus


% read in order of blocks and trials
fid=fopen('FaceFMRIOrder.csv');
indexes={1,2,3,4,5};
[ facenumC, blockC, emotionC, rewardC, ITIC ] = indexes{:};
experiment=textscan(fid,'%d %d %s %s','HeaderLines',1,'Delimiter', ',');
fclose(fid);

%% start recording data
% sets txtfid, subject.*, start, etc

% how long (trials) is a block
[~,blockchangeidx] = unique(experiment{blockC});
trialsPerBlock     = unique(diff(blockchangeidx));
if(length(trialsPerBlock) > 1)
    fprintf('Whoa!? different trial lengths? I dont know what''s going on!\n')
    trialsPerBlock = trialsPerBlock(1);
end

totalBlocks = length(experiment{blockC})/trialsPerBlock;
runTotal=0; %reset block score for new block

%obtain subject and run information, or resume next run.
getSubjInfo

% print the top of output file
if startRun == 1
    fprintf(txtfid,'#Subj:\t%s\n', subject.subj_id);
    fprintf(txtfid,'#Run:\t%i\n',  subject.run_num);
    fprintf(txtfid,'#Age:\t%i\n',  subject.age);
    fprintf(txtfid,'#Gender:\t%s\n',subject.gender);
end

% always print date .. even though it'll mess up reading data if put in the middle
fprintf(txtfid,'#%s\n',date);

%% Counter balance
% by reversing order for odd subjects
if mod(str2double(subject.subj_id),2)==1
    fprintf('NOTE: odd subject, order reversed of input csv!\n')
    %reverse by blocks
    blockIndices = reshape(1:length(experiment{blockC}), trialsPerBlock, totalBlocks);
    blockIndices = reshape(blockIndices(:,totalBlocks:-1:1), 1, []);
    experiment{facenumC} = experiment{facenumC}(blockIndices);
    experiment{emotionC} = experiment{emotionC}(blockIndices);
    experiment{rewardC} = experiment{rewardC}(blockIndices);
    experiment{ITIC} = experiment{ITIC}(blockIndices);
    %do not reverse block codes since this should always be ascending
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
    FlipInterval = Screen('GetFlipInterval',w); %monitor refresh rate.
    slack = FlipInterval/2; %used for minimizing accumulation of lags due to vertical refresh
    
    % Set process priority to max to minimize lag or sharing process time with other processes.
    Priority(MaxPriority(w));
    
    %do not echo keystrokes to MATLAB
    %ListenChar(2);
    
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
            [imdata colormap alpha]=imread(stimfilename);
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
    if startRun==1
        % show long instructions for first time player
        for instnum = 1:length(Instructions)
            DrawFormattedText(w, Instructions{instnum},'center','center',black);
            Screen('Flip', w);
            waitForResponse;
        end
        
        % initialize the order of events only if we aren't resuming
        order=cell(length(experiment{facenumC}),1);
    else
        DrawFormattedText(w, ['Welcome Back!\n\n' InstructionsBetween],'center','center',black);
        Screen('Flip', w);
        waitForResponse;
    end
    
    %%%BEGIN TASK AFTER SYNC OBTAINED FROM SCANNER
    [scannerStart, priorFlip] = scannerPulseSync;
    
    fprintf('pulse flip: %.5f\n', priorFlip);

    %initial fixation of 8 seconds to allow for steady state magnetization.
    %count down from 3 to 1, then a 1-second blank screen.
    drawRect;
    priorFlip = fixation(preStartWait - 4.0, 1, scannerStart);
    
    fprintf('fix flip: %.5f\n', priorFlip);

    for cdown = 1:3
        drawRect;
        DrawFormattedText(w, ['Beginning in\n\n' num2str(4.0 - cdown)],'center','center',black);
        priorFlip = Screen('Flip', w, scannerStart + 4.0 + (cdown - 1.0) - slack);
        %fprintf('cdown: %d, fix flip: %.5f\n', cdown, priorFlip);
        %WaitSecs(1.0);
    end
    
    %1 second of blank screen
    drawRect;
    fixation(1.0, 0, scannerStart + 7.0);
    
    pretrialEnd=GetSecs();
    pretrialLength=pretrialEnd - scannerStart;
    
    fprintf('pretrialLength was: %.5f\n', pretrialLength);
    
    %determine start and end trials
    startTrial = (startRun-1)*trialsPerBlock + 1;
    endTrial = startRun*trialsPerBlock;
    blockTrial = 1; %track the trial number within block
    
    %order of fields in order array
    orderfmt = { 'run', 'trial', 'rewFunc', 'emotion', 'magnitude', 'probability', 'score', 'ev', 'rt', 'clock_onset', ...
        'isi_onset', 'feedback_onset', 'iti_onset' 'iti_ideal' 'image' };

    %error('end here');
    %% THE BIG LOOP -- complete all trials for a run
    for i=startTrial:endTrial
        
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
        feedbackFlip = scoreRxt(RTms, experiment{rewardC}{i}, firstClockFlip + RTms/1000 + postResponseISI);
        setTimeDiff('receipt'); %build times (debug timing)
        
        %show fixation for min (100ms) plus scheduled ITI
        [ITIflip] = fixation(postFeedbackITI + experiment{ITIC}(i), 1, firstClockFlip + RTms/1000 + receiptDuration + postResponseISI);
        
        setTimeDiff('ITI'); %build times (debug timing)
        
        timing.end= GetSecs() - startOfTrial;
        
        %% non critical things (debugging and saving)
        %nonPresTime=tic;
        
        %% write to data file
        emo=experiment{emotionC}{i};
        face=experiment{facenumC}(i);
        
        %set the output of the order structure
        trial = { subject.run_num i experiment{rewardC}{i} experiment{emotionC}{i} ...
            F_Mag F_Freq inc ev RTms (firstClockFlip - scannerStart) (isiFlip - scannerStart) ...
            (feedbackFlip - scannerStart) (ITIflip - scannerStart) experiment{ITIC}(i) strcat(emo,'_',num2str(face),'.png') };
        
        order(i) = {trial};
        
        % print header
        if i == 1
            fprintf(txtfid,'Run\tTrial\tFunc\tEmotion\tMag\tProb\tScore\tEV\tRT\tClock_Onset\tISI_Onset\tFeedback_Onset\tITI_Onset\tITI_Ideal\tImage\n');
        end
        
        fprintf(txtfid,'%d\t',order{i}{1:2} );
        fprintf(txtfid,'%s\t',order{i}{3:4} );
        fprintf(txtfid,'%4i\t',order{i}{5:14} );
        %fprintf(txtfid, '%4i\t', order{i}{2:end});
        fprintf(txtfid, '%s\t', strcat(emo,'_',num2str(face),'.png') );
        fprintf(txtfid, '\n');
        
        % save to mat so crash can be reloaded
        trialnum=i;
        save(filename,'order','trialnum','blockTrial','subject','score','runTotal');
        
        blockTrial = blockTrial + 1;
        
        % line like
        % CEVR       1      22       5       2    4.481533e+01   176       0       2.136929e-01    3.764108e+01    2.399020e+02
        % PHASE      RUN    TRIAL    BOXC   NULL   TRACT        FMAG   FMAGP(fmp)   FFREQ(ff=F_Freq)      EV              RT
        % | --------------- input file ---------|
        %%% tract = trial start time    -  trstamp
        %%% null = old exprm field
        %%% we only have one run of the experiment
        
        
        %% debug, show time of this trial
        
        expected.clock   = timing.clock; %use the observed RT for expectation %timerDuration;
        expected.ITI     = double(experiment{ITIC}(i) + postFeedbackITI);
        expected.receipt = receiptDuration;
        expected.ISI     = double(postResponseISI);
        expected.end     = 0; 
        expected.end     = sum(struct2array(expected));
        fprintf('\n%d: %s_%d.png\n%.2f in, expected, obs, diff\n',i, experiment{emotionC}{i},experiment{facenumC}(i),timing.start);
                
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
    
    % everyone should earn the bonus
    % but they should have at least 2000 pts
    earnedmsg='\n\nYou earned a $25 bonus !';
    if(score < 2000); earnedmsg=''; end;
    
    msgAndCloseEverything(['Your final score is ', num2str(score) ,' points', earnedmsg, '\n\nThanks for playing!']);
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
                msgAndCloseEverything(['Quit on trial ' num2str(i)]);
                error('quit early (on %d)\n',i)
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
    end

%% print time since last check
% updates global timing struct and checktime double
    function setTimeDiff(interval)
        timing.(interval) = (GetSecs() - checktime);
        checktime=GetSecs();
    end

%% block indicator
    function drawRect(varargin)
        t=subject.run_num;
        % allow block to be specified, mostly for instruction display
        % b/c we want to show the color of the next block
        if nargin>0
            t=varargin{1};
        end
                
        %fprintf('rect colors: %d %d %d\n', blockColors(rgbcolorIDX,:));
        %fprintf('t: %d, rgbidx: %d\n', t, rgbcolorIDX);
        Screen('FrameRect', w, blockColors(t,:), [], 50);
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
        
        % Set up the timer.
        startTimeMS   = GetSecs()*10^3;
        durationMS  = timerDuration*10^3; % 4 seconds of looking at a face
        remainingMS = durationMS;
        firstFlip = 1;
        
        % Draw border color based on block. Only call once outside of animation loop
        drawRect;
        
        emo=experiment{emotionC}{i};
        facenum=experiment{facenumC}(i);
        
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
            
            if keyIsDown
                if(keyCode(escKey));
                    msgAndCloseEverything(['Quit on trial ' num2str(i)]);
                    error('quit early (on %d)\n',i)
                end
                
                if any(keyCode(validKeys))               
                    %if keyCode(spaceKey)
                    keyPressed=1; %person responded!
                    break
                end
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
            end
                        
            % Wait 0.5 ms before checking the keyboard again to prevent
            % overload of the machine at elevated Priority():
            WaitSecs(0.0005);
        end
        
        if keyPressed == 1
            elapsedMS = round((keyTime - firstOnset) * 10^3);
        else
            elapsedMS = round((VBLT - firstOnset) * 10^3);
        end

        return;
    end


%% Display a red cross for ITI (ms) time
    function [VBLT_Onset] = fixation(waittime, drawfix, reftime)
        if nargin < 2
            drawfix=1;
        end
        if nargin < 3
            reftime=GetSecs();
        end
        
        % grab time here, so we can subtract the time it takes to draw
        % from the time we are actually waiting
        % starttime=tic ;
        
        %fprintf('waiting %.3f\n',waittime);
        
        oldFontSize=Screen(w,'TextSize', 40 );
        if drawfix, DrawFormattedText(w,'+','center','center',[ 255 0 0]); end
        Screen(w,'TextSize', oldFontSize);
        
        clearmode=0;
        drawRect;
        VBLT_Onset = Screen('Flip', w, reftime, clearmode); %display the fix ASAP
        
        drawRect;
        VBLT_Offset = Screen('Flip', w, reftime + waittime - slack, clearmode); %and display again after desired wait
        %this two-flip approach ensures that the fix is on screen for approximately the right length
        
        %drawTime=toc(starttime);
        %WaitSecs(waittime-drawTime);
        
        %timediff=toc(starttime)-waittime;
        %if abs(timediff) > .02; fprintf('!! WARNING, wait is off by > 20ms (%.4f)\n', timediff );end
        
        %disp([waittime, experiment{ITIC}(i), experiment{ISIC}(i), 4000 - rspnstime, rspnstime ])
    end



%% wait for a response
    function seconds = waitForResponse
        while(1)
            [ keyIsDown, seconds, keyCode ] = KbCheck;
            
            if(keyIsDown && keyCode(escKey));
                msgAndCloseEverything(['Quit on trial ' num2str(i)]);
                error('quit early (on %d)\n',i)
            end
            
            if(keyIsDown && any(keyCode)); break; end %any() is redudant
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
            [F_Mag F_Freq] = getScore(RTms,func);
            
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
                score=score+F_Mag;
                runTotal=runTotal+F_Mag;
                inc=F_Mag;
            else
                %score=score;
                inc=0;
            end
        end
        
        fprintf('%s: ev=%.2f; Mag=%.2f; Freq: %.2f; rand: %.2f; inc: %d; pts- block: %d; total: %d\n', ...
            experiment{rewardC}{i},ev, F_Mag,F_Freq,rd,inc,runTotal,score);
        
        %%% Draw
        drawRect;
        %Screen('DrawText', w, sprintf('Your Score is: %d\nrecorded rxt: %d', score, rspnstime));
        %DrawFormattedText(w, sprintf('Total score is: %d\nincrease is: %d\nradnom vs Freq (ev): %f v %f (%f)\nrecorded rxt: %d', score,F_Mag,rd,F_Freq,ev, RT),'center','center',black);
        Screen('TextSize', w, textSize);
        fprintf('RT is: %.2f\n', RTms);
        if keyPressed == 0
            %DrawFormattedText(w, sprintf(['You earned 0 points because you did not respond in time.\n\n' ...
            %    'Please respond before the ball goes all the way around.\n\n'...
            %    'Total points this game: %d points'], runTotal),'center','center',black);
            
            DrawFormattedText(w, ['You won 0 points.\n\n\n' ...
                'Please respond before the ball goes all the way around.\n\n'],'center','center',black);
        else
            %DrawFormattedText(w, sprintf('You won:  %d points\n\nTotal points this game: %d points', inc,runTotal),'center','center',black);
            DrawFormattedText(w, sprintf('You won\n\n%d\n\npoints', inc),'center','center',black);
        end
        
        Screen('DrawingFinished', w); %tell PTB that we have finished with screen creation -- minimize timing delay
        
        scoreflip = Screen('Flip', w, reftime); %onset of feedback
        
        drawRect;
        lastflip = Screen('Flip', w, reftime + receiptDuration - slack); %offset of feedback
        %WaitSecs(receiptDuration-toc(startScoreTime));
        
    end

%% get who the subject is
    function getSubjInfo
        
        %whether to resume with next run
        resume = 'n';
        
        % skip the questions if we provide var ourself
        subject.subj_id = input('Enter the subject ID number: ','s');
                
        filename = ['subjects/fMRIEmoClock_' subject.subj_id '_tc'];
        
        % is the subject new? should we resume from existing?
        % set t accordingly, maybe load subject structure
        txtfile=[filename '.txt'];
        backup=[txtfile '.' num2str(GetSecs()) '.bak'];
        
        % we did something with this subject before?
        if exist(txtfile,'file')
            
            % check that we have a matching mat file
            % if not, backup txt file and restart
            if ~ exist([filename '.mat'],'file')
                fprintf('%s.txt exists, but .mat does not!\n',filename)
                resume='n';
            else
                localVar = load(filename);
                
                % sanity check
                if localVar.subject.subj_id ~= subject.subj_id
                    error('mat file data conflicts with name!: %d != %d',...
                        localVar.subject.subj_id, subject.subj_id);
                end
                
                if localVar.blockTrial < trialsPerBlock
                    fprintf('It appears only %d trials were completed in run %d.\n', localVar.blockTrial, localVar.subject.run_num);
                    redoBlock = input(['Do you want to redo run ', num2str(localVar.subject.run_num), ' ? (y or n) '],'s');
                    
                    if strcmpi(redoBlock, 'y')
                        resume='y'; %may need to phase this out... doesn't make much sense
                    end
                else
                    resumeNext = input(['Continue with run ', num2str(localVar.subject.run_num + 1), ' ? (y or n) '], 's');
                    if (strcmpi(resumeNext, 'y'))
                        localVar.subject.run_num = localVar.subject.run_num + 1;
                        resume='y';
                    end
                end

                if ~strcmpi(resume, 'y')
                    chooseRun = input(['Specify the run to be completed (1 - ', num2str(totalBlocks), ') '], 's');
                    if str2double(chooseRun) > totalBlocks || str2double(chooseRun) < 1
                        error(['Must specify run 1 - ', num2str(totalBlocks)]);
                    end
                    resume='y';
                    localVar.subject.run_num = str2double(chooseRun);
                end

                if strcmpi(resume,'y')
                    
                    clear subject
                    subject=localVar.subject;
                    
                    order=localVar.order;
                    score=localVar.score;
                    
                    % otherwise, move the existing txt file to a backup
                    % and we'll fill in the subject info below
                else
                    fprintf('moving %s to %s, start from top\n', txtfile,backup)
                    movefile(txtfile,backup);
                end
                
                startRun = localVar.subject.run_num;
                
            end
        end
        
        %% fill out the subject struct if any part of it is still empty
        for attribCell={'gender','age', 'run_num'}
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
        if ischar(subject.age), subject.age=str2double(subject.age); end
        if ischar(subject.run_num), subject.run_num=str2double(subject.run_num); end
        
        %%for first run, need to sample the 8 orders from the mat file here
        %%only sample if we have not populated the ITIs before (i.e., don't resample for re-running run 1)
        if startRun==1 && ~ismember( 'runITI_indices', fields(subject))
            locV=load('fMRIOptITIs_284s_38pct.mat');
            subject.runITI_indices = randsample(size(locV.itimat,1), totalBlocks);
            subject.runITIs=locV.itimat(subject.runITI_indices, :);
            clear locV;
        end
        
        if startRun==1 && ~ismember('blockColors', fields(subject))
            %Set1 from Color Brewer
            %provides 8 colors
            blockColors = [228 26 28; ...
                55 126 184; ...
                77 175 74; ...
                152 78 163; ...
                255 127 0; ...
                255 255 51; ...
                166 86 40; ...
                247 129 191];
            
            blockColors = blockColors(randperm(8),:); %permute per subject
            subject.blockColors=blockColors;
            
            %Set3 from Color Brewer
            %only provides 12 colors
            % blockColors = [141 211 199; ...
            %     255 255 179; ...
            %     190 186 218; ...
            %     251 128 114; ...
            %     128 177 211; ...
            %     253 180 98; ...
            %     179 222 105; ...
            %     252 205 229; ...
            %     217 217 217; ...
            %     188 128 189; ...
            %     204 235 197; ...
            %     255 237 111];
            %blockColors = round(255*hsv(24)); % a different color for each block of trials
            %blockColors = blockColors(randperm(24),:); % randperm(24) should prob be replaced by a pre-made vector
        else
            blockColors=subject.blockColors;
        end
            
        
        %load ITI distribution for all runs.
        %NB: the .runITIs element is runs x trials in size (8 x 50)
        %here, we need to flatten it row-wise into a vector run*trials length
        experiment{ITIC} = reshape(subject.runITIs',[],1);

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
        diaryfile = ['logs/fMRIEmoClock_' subject.subj_id '_' num2str(GetSecs()) '_tcdiary'];
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

