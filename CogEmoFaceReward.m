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
  % This used 'KbDemo' as template
  
  try
  
     % Removes the blue screen flash and minimize extraneous warnings.
  	  Screen('Preference', 'VisualDebugLevel', 3);
     Screen('Preference', 'SuppressAllWarnings', 1);
  	
     % Find out how many screens and use smallset screen number.
     
     % Open a new window.
     [ w, windowRect ] = Screen('OpenWindow', max(Screen('Screens')));
     
     % Set text display options. We skip on Linux.
     if ~IsLinux
         Screen('TextFont', w, 'Arial');
         Screen('TextSize', w, 18);
     end
  
     % Set colors.
     black = BlackIndex(w);
     
     % Enable unified mode of KbName, so KbName accepts identical key names on
     % all operating systems:
     KbName('UnifyKeyNames');

     % Set keys.
     spaceKey  = KbName('SPACE');
     %% TODO: how to get '^' for getting scanner start
     
  
     
     
     %  read in order of things
     fid=fopen('FaceITI.csv')
     experiment=textscan(fid,'%d,%d,%d,%q','HeaderLines',1)
     for i=1:length(experiment{4})
        experiment{5}{i} = experiment{4}{i}(  findstr(experiment{4}{i},',')+2:end);
        experiment{4}{i} = experiment{4}{i}(1:findstr(experiment{4}{i},',')-1    );
     end
     fclose(fid)
     
     % initialize total points earned
     % incremented as a function (inc,dec,const)
     score = 0;
     
     


     %% Setup screen -- want to wait for "^" (TODO) use SPACE instead
     DrawFormattedText(w, 'space means youre ready','center','center');
     Screen('Flip', w);
     while(1)
      [ keyIsDown, seconds, keyCode ] = KbCheck;
      if(keyIsDown & keyCode(spaceKey)); break; end
      WaitSecs(.001)
     end




     % display face and move a dot around a cirlce in 4seconds
     for i=1:3 %length(experiment{1})
        %load image
        % read stimulus image into matlab matrix 'imdata':
        %stimfilename=strcat('stims/',char(objname(trial))); % assume stims are in subfolder "stims"
        stimfilename=strcat('faces/',experiment{4}{i},'_',num2str(experiment{1}(i)),'.png')
        imdata=imread(stimfilename);
        % make texture image out of image matrix 'imdata'
        tex=Screen('MakeTexture', w, imdata);


        % show face, record time to spacebar
        rspnstime = faceWithTimer;

        % show first fixation
        fixation(experiment{2}(i))

        % show score
        scoreRxt(rspnstime,experiment{5}{i})
       
        % show second fixation
        fixation(experiment{3}(i))

         
     end



     
     Screen('CloseAll');
     return

  catch
     Screen('CloseAll');
     psychrethrow(psychlasterror);
  end




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
        
        Screen('DrawText', w, sprintf('%i ms remaining...',remainingMS), 20, 20, black);
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
            if keyCode(spaceKey)
                break
            end
        end
        
        % Wait 1 ms before checking the keyboard again to prevent
        % overload of the machine at elevated Priority():
        WaitSecs(0.001);
     end
     
     elapsedMS = round((now - startTime) * 10^8);
     % if 4s, give them no points?
     
     fprintf('\n Done @ %f\n',elapsedMS);
     
     return;
   end

   function fixation(ITI)
        %Screen('DrawText', w, '+');
        DrawFormattedText(w,'+','center','center');
        Screen('Flip', w);
        WaitSecs(double(ITI/10^3));
   end
    
   function scoreRxt(rspnstime,scrfunc)
        % score response time based on one of 3 functions
        % given in master file (either inc,dec,or const)
        switch scrfunc
         case 'constant'
          score=score+2000;
         case 'decrease'
          score=score-rspnstime;
         case 'increase'
          score=score+rspnstime;
         otherwise
          score=score;
          fprintf('!!!WHAT function did you mean by %s\n',scrfunc);
        end

        %Screen('DrawText', w, sprintf('Your Score is: %d\nrecorded rxt: %d', score, rspnstime));
        DrawFormattedText(w, sprintf('Your Score is: %d\nrecorded rxt: %d', score, rspnstime),'center','center');
        Screen('Flip', w);
        WaitSecs(1.5);
    
   end
end

