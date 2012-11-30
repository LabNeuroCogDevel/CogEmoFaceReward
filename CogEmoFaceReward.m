%TODO:
%  better/less intrusive indicator?
%  reshape scamb faces
%  append total score to order?

function CogEmoFaceReward
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
  
  start       = 1;
  TR          = 1.5;
  blockColors = round(255*prism(24)); % a rotating color for each block
  txtfid      = 0; %just so we know it's not a private nested function var
  
  %% start recording data
  getSubjInfo
  
  if start == 1
    fprintf(txtfid,'#Subj:\t%s\n', subject.subj_id);
    fprintf(txtfid,'#Run:\t%i\n',  subject.run_num); 
    fprintf(txtfid,'#Age:\t%i\n',  subject.age);
    fprintf(txtfid,'#Gender\t%s\n',subject.gender);
  end
  
  % always print date .. even though it'll mess up reading data if put
  % in the middle
  fprintf(txtfid,'# %s\n',date);
  
  %% set order of trials
  %  read in order of things
  % note, only one of these (Frank had 8)
  fid=fopen('FaceITI.csv');
  indexes={1,2,3,4,5,6};
  [ facenumC, ITI1C, ITI2C, blockC, emotionC, rewardC ] = indexes{:};
  experiment=textscan(fid,'%d,%d,%d,%d,%q','HeaderLines',1);
   % ugly unpack of " "," "
  for i=1:length(experiment{5})
      experiment{6}{i} = experiment{5}{i}(  findstr(experiment{5}{i},',')+2:end);
      experiment{5}{i} = experiment{5}{i}(1:findstr(experiment{5}{i},',')-1    );
  end
  fclose(fid);

  % initialize total points earned
  % incremented as a function (inc,dec,const)
  score = 0;
     
  %% launch presentation   
  try
  
     %% setup screen
     % Removes the blue screen flash and minimize extraneous warnings.
  	 % Screen('Preference', 'VisualDebugLevel', 3);
     %Screen('Preference', 'SuppressAllWarnings', 1);
  	
     % Find out how many screens and use smallset screen number.
     
     % Open a new window.
     [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')),[ 255 255 255], [0 0 640 480] );
     
     % Set text display options. We skip on Linux.
     if ~IsLinux
         Screen('TextFont', w, 'Arial');
         Screen('TextSize', w, 18);
     end
  
     % Set colors.
     black = BlackIndex(w);
     white = WhiteIndex(w);
     
     % Enable unified mode of KbName, so KbName accepts identical key names on
     % all operating systems:
     KbName('UnifyKeyNames');

     % Set keys.
     spaceKey  = KbName('SPACE');
     escKey  = KbName('ESCAPE');
     %% TODO: how to get '^' for getting scanner start
     scannerStart = GetSecs;
  
     
    
     
     


     %% Instructions 
          
     Instructions = { ...
        [ 'You will see a clock face.\n' ...
          'Its arm will make a full turn over the course of 5 seconds.\n' ...
          'Press the button to win points before the arm makes a full turn.\n' ...
          'Try to win as many points as you can!\n\n' ...
          'Press any key to read more instructions' ...
        ]
        [ 'Sometimes you will win lots of points and sometimes you will win less.\n ' ...
          'The time at which you respond affects in some way\n' ...
          'the number of points that you can win.\n' ...
          'If you dont respond by the end of the clock cycle,\n' ...
          'you will not win any points.\n\n' ...
          'Press any key to read more instructions' ...
        ]
        [ 
          'Hint: Try to respond at different times along\n' ... 
          'the clock cycle in order to learn how to make the most points.\n' ...
          'Note: The length of the experiment is constant\n' ... 
          'and is not affected by when you respond.\n\n' ...
          'Press any key when you are ready' ...
        ]
      }; 
    
      % use boxes instead of prompts
        %Between run instructions
        %betweenInstructions = [
        %    'Next, you will see a new clock face.\n' ...
        %    'Try responding at different times in order to learn\n' ...
        %    'how to make the most points with this clock face.\n\n' ...
        %    'Press any key when you are ready' ...
        %    ];
        
     for i = 1:length(Instructions)
               
         DrawFormattedText(w, Instructions{i},'center','center',black);
         Screen('Flip', w);
         waitForResponse;
     end

       

  




     %% iterate through trials
     % inialize the order of events only if we didn't load it from a crash
     % we know this by where we are set to start
     if start == 1; order=cell(length(experiment{1}),1); end
     
     for i=start:length(experiment{facenumC})
        %load image
        % read stimulus image into matlab matrix 'imdata':
        %stimfilename=strcat('stims/',char(objname(trial))); % assume stims are in subfolder "stims"
        stimfilename=strcat('faces/',experiment{emotionC}{i},'_',num2str(experiment{facenumC}(i)),'.png');
        imdata=imread(stimfilename);
        % make texture image out of image matrix 'imdata'
        tex=Screen('MakeTexture', w, imdata);

        %DrawRectFrame(w,[0 0 480 640], black); %if mod/
        % show face, record time to spacebar
        rspnstime = faceWithTimer;   
        

        % TOCHANGE?: add RT/2 to fixation time? instead of all remainder
        
        % show first fixation
        % this fixation lasts the remainder of the trial and then ITI1
        fixation(experiment{ITI1C}(i)+ (4000 - rspnstime) );

        % show score
        scoreRxt(rspnstime,experiment{rewardC}{i});
       
        % show second fixation
        fixation(experiment{ITI2C}(i));
        
        
        trial = {experiment{rewardC}(i) subject.run_num i experiment{blockC}(i) 0 t_start F_Mag inc F_Freq ev rspnstime };
        order(i) = {trial};
        
        % print header
        if i == 1
            fprintf(txtfid,'Func\tRun\tTrial\tBlock\tNull\ttrialStartTime\tMag\tScoreInc\tFreq\tEV\tRT\n');
        end
               
        fprintf(txtfid,'%s\t',order{i}{1}{1} );
        fprintf(txtfid, '%4i\t', order{i}{2:end});
        fprintf(txtfid, '\n');
        
        % save to mat so crash can be reloaded
        trialnum=i;
        save(filename,'order','trialnum','subject','score');
        
        %% write to data file
        % line like
        % CEVR       1      22       5       2    4.481533e+01   176       0       2.136929e-01    3.764108e+01    2.399020e+02
        % PHASE      RUN    TRIAL    BOXC   NULL   TRACT        FMAG   FMAGP(fmp)   FFREQ(ff=F_Freq)      EV              RT
        % | --------------- input file ---------|
        %%% tract = (trial start time    -  trstamp)/TR
        %%% null = old exprm field
        %%% we only have one run of the exeriment

     end

 
    
    diary off;	%stop diary
    fclose('all');	%close data file

     Screen('CloseAll');
     return

  catch
     Screen('CloseAll');
     psychrethrow(psychlasterror);
  end
  
  % close the screen
  sca



   %% Meat
   function [elapsedMS] = faceWithTimer
   
     % Control a screen spot with the keyboard.
     
     % Here are the parameters for this demo.
     spotRadius         = 100;        % The radius of the spot.
     spotSize           = 10;         % The radius of the spot.
     initialDotPosition = 3 * pi / 2; % The initial position. -- 12'o clock
     
     % Use the parameters.
     spotDiameter = spotSize * 2;
     spotRect = [0 0 spotDiameter spotDiameter];
     centeredspotRect = CenterRect(spotRect, windowRect); % Center the spot.
     
     % Set up the timer.
     startTime   = now;
     durationMS  = 4000; % 4 seconds of looking at a face
     remainingMS = durationMS;
     
     % Loop while there is time.
     while remainingMS > 0
        elapsedMS = round((now - startTime) * 10 ^ 8);
        remainingMS = durationMS - elapsedMS;
        
        %Screen('DrawText', w, sprintf('%i ms remaining...',remainingMS), 20, 20, black);

        Screen('FrameRect', w, blockColors(experiment{blockC}(i),:), [], 5);
        % put the image up
        Screen('DrawTexture', w, tex); 
        
        % at 4 seconds, we do a full rotation
        theta=  initialDotPosition - (remainingMS/durationMS * 2 * pi) ;
        xOffset = spotRadius * cos(theta);
        yOffset = spotRadius * sin(theta);

        offsetCenteredspotRect = OffsetRect(centeredspotRect, xOffset, yOffset);
        Screen('FillOval', w, [0 255 127], offsetCenteredspotRect);

        % display screen
        Screen('Flip', w);
        
        [ keyIsDown, seconds, keyCode ] = KbCheck;
        
        if keyIsDown
            if(keyCode(escKey)); Screen('CloseAll'); end
            %if keyCode(spaceKey)
                break
            %end
        end
        
        % Wait 1 ms before checking the keyboard again to prevent
        % overload of the machine at elevated Priority():
        WaitSecs(0.001);
     end
     
     elapsedMS = round((now - startTime) * 10^8);
     % if 4s, give them no points?
     
     %fprintf('\n Done @ %f\n',elapsedMS);
     
     return;
   end

   %% Display a red cross for ITI (ms) time
   function fixation(ITI)
        %Screen('DrawText', w, '+');
        DrawFormattedText(w,'+','center','center',[ 255 0 0]);
        Screen('Flip', w);
        WaitSecs(double(ITI/10^3));
        %disp([ITI, experiment{ITI1C}(i), experiment{ITI2C}(i), 4000 - rspnstime, rspnstime ])
   end
   
   %% wait for a response
    function seconds = waitForResponse
      while(1)
          [ keyIsDown, seconds, keyCode ] = KbCheck;
          
          if(keyIsDown && keyCode(escKey)); 
              Screen('CloseAll');
              sca;
              break;
          end
          
          if(keyIsDown && any(keyCode)); break; end %any() is redudant
          WaitSecs(.001);
      end
      Screen('Flip', w); % change the screen so we don't hold down space
      WaitSecs(.1);
    end

   %% score based on a response time and Rew Func (as string, eg. 'CEV')
   function scoreRxt(RT,scrfunc)
       %trial start time
       t_start = (GetSecs - scannerStart)/TR;
       
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
             F_Mag = (k*rt_extended)/(rt_extended-(RT+Shift)); 
             F_Freq = 1-((RT+Shift)/rt_extended);
             
         case 'DEV'
            F_Mag = DEV_factor*log(DEV_factor2*(RT+Shift)); 
            CEV_x = 1-((RT+Shift)/rt_extended);
            IEV_x = CEV_x + (CEV_x*(sin_factor*sin((RT*pi)/5000)));
            F_Freq = (2*CEV_x)-IEV_x;
         case 'IEV'
            CEV_x = (k*rt_extended)/(rt_extended-(RT+Shift)); 
            DEV_x = DEV_factor*log(DEV_factor2*(RT+Shift));
            F_Mag = (2*CEV_x)-(DEV_x);
            CEV_x2 = 1-((RT+Shift)/rt_extended);
            F_Freq = CEV_x2 + (CEV_x2*(sin_factor*sin((RT*pi)/5000)));
         case 'CEVR'
            F_Mag = 1-((RT+Shift)/rt_extended);
            F_Mag = F_Mag*200;
            F_Freq = (k*rt_extended)/(rt_extended-(RT+Shift)) ;
            F_Freq = F_Freq/200;
         
         otherwise
          %score=score;
          F_Mag = 0;
          F_Freq = 0;
          warning(['!!!WHAT function did you mean by' scrfunc]);
        end

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
        if F_Freq > rd && RT~=0 && RT <= 40000
            score=score+F_Mag;
            inc=F_Mag;
        else
            %score=score;
            inc=0;
        end
        
       
        
        %%% Draw
        
        %Screen('DrawText', w, sprintf('Your Score is: %d\nrecorded rxt: %d', score, rspnstime));
        %DrawFormattedText(w, sprintf('Total score is: %d\nincrease is: %d\nradnom vs Freq (ev): %f v %f (%f)\nrecorded rxt: %d', score,F_Mag,rd,F_Freq,ev, RT),'center','center',black);
        DrawFormattedText(w, sprintf('You earned:  %d\nTotal score: %d', inc,score),'center','center',black);

        Screen('Flip', w);
        WaitSecs(1.5);
    
   end

   %% get who the subject is
   function getSubjInfo 
        % skip the questions if we provide var ourself
        subject.subj_id = input('Enter the subject ID number: ','s');
        

        filename = ['subjects/' subject.subj_id '_tc'];

        % is the subject new? is it a restart or resume of an existing?
        % set t accordingly, maybe load subject structure 
        txtfile=[filename '.txt'];
        if exist(txtfile,'file') 
            restart = input('Is this a restart/want to load old file (y or n)? ','s');
            if lower(restart) == 'y' 
                %subj_id = subject.subj_id;
                if exist([filename '.mat'],'file')
                    clear subject
                    localVar = load(filename);
                    start=localVar.trialnum;
                    subject=localVar.subject;
                    subject
                    order=localVar.order;
                    score=localVar.score;
                end
            else
                % dont append new stuff to an old file, move the old one
                movefile(txtfile,[txtfile '.' num2str(now) '.bak']);  
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
              fprintf('using old %s', attrib); fprintf(' (%s)\n', subject.(attrib));
            end
        end

        %% age should be a number
        if ischar(subject.age); subject.age    =str2double(subject.age);    end
        if ischar(subject.age); subject.run_num=str2double(subject.run_num);end

        %% set sex to a standard
        if ismember(lower(subject.gender),{'male';'dude';'guy';'m';'1'} )
            subject.gender = 'male';
        else
            subject.gender = 'female';
        end
        % print out determined sex, give user a chance to correct
        fprintf('Subject is %s\n', subject.gender);

        %% Initialize data storage and records
        diaryfile = [subject.subj_id 'tcdiary'];
        diary(diaryfile);
        
        txtfid=fopen(txtfile,'a'); % we'll append to this..maybe


   end


end

