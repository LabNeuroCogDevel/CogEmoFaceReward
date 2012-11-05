% time conflict
% script written by N. Long 2.09.09
% Modified from B. Doll, M. Frank script previously used on EPRIME
% Modifications for fMRI testing

PHASE = 1;      %Phase of experiment
CEV = 1;        %Constant Expected Value
DEV = 2;        %Increasing Expected Value
IEV = 3;        %Decreasing Expected Value
CEVR = 4;       %Constant Expected Value, reversed
RUN = 2;        %Run of the experiment
TRIAL= 3;       %Trial number
BOXC = 4;       %Box color
RED = 1;
ORANGE = 2;
YELLOW = 3;
GREEN = 4;
BLUE = 5;
PURPLE= 6;
PINK = 7;
BROWN = 8;
NULL = 5;       %Null interval

TRACT_COL = 6;  %Actual TR, appended
FMAGA_COL = 7;  %F Mag, actual calculated, appended
FMAGP_COL = 8; %F Mag, presented to subj, appended
FFREQ_COL = 9; %F freq, appended
EV_COL = 10;     %Expected value, appended
RT_COL = 11;     %RT, appended

%Durations
CLOCK_DUR = .625;	%Each clock is presented on screen for 625ms
CLOCK9_DUR = .1;    % Final/First clock face is presented for 100ms since Subjs can't respond at this point
FIXED_NULL = .1;    % Enforced null (not used in No Response trials)
REWARD_DUR = .9;     %Reward message is presented on screen for 1s
TRIAL_TIME = 4;     %Subjs actually given 4s total
WAIT_TIME = 6;      %Length between trigger start and first trial
TR = 2;             %how long each volume takes, 2s

% Values for Reward computation - constant for all phases
k = 37;
Shift = 700;
rt_extended = 7000;
DEV_factor = 10;
DEV_factor2= 1;
sin_factor = 0.25;

% WF dont need to declare subject structure
% Building as we go allows checks

% skip the questions if we provide var ourself
if(~exist('subject','var') || ~ismember('subj_id',fields(subject)))
 subject.subj_id = input('Enter the subject ID number: ','s');
end

filename = [subject.subj_id '_tc'];

% is the subject new? is it a restart or resume of an existing?
% set t accordingly, maybe load subject structure 
if(exist(filename,'file'))
    restart = input('Is this a restart (y or n)? ','s');
    if lower(restart) == 'y' 
        %subj_id = subject.subj_id;
        clear subject
        load(filename);
    else 
        t=1;
    end
else
    t=1;
end


% fill out the subject struct if any part of it is still empty
for attribCell={'gender','age','cb_num'}
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

% set sex to a standard
if ismember(lower(subject.gender),{'male';'dude';'guy';'m'} )
    subject.gender = 'male';
else
    subject.gender = 'female';
end
% print out determined sex, give user a chance to correct
fprintf('Subject is %s\n', subject.gender);


% read in order of runs from file (specified on start)
orderFile=['order' num2str(subject.cb_num) '.txt'];
if(~exist(orderFile,'file')); error('give me a good order number!'); end
order = load(orderFile);
order = [order zeros(size(order,1),2)];

whichScreen = 0;

ORDER_LENGTH=length(order);

%Initialize data storage and records
diaryfile = [subject.subj_id 'tcdiary'];
diary(diaryfile);

%% Start output file
fid = fopen([subject.subj_id '_tc.txt'],'a');

if t == 1
    fprintf(fid,'%s\n',subject.subj_id);
    fprintf(fid,'%s	','CB#:');
    fprintf(fid,'%4i\n',subject.cb_num);
    fprintf(fid,'%s	','Age:');
    fprintf(fid,'%4i\n',subject.age);
    fprintf(fid,'%s	','Gender:');
    fprintf(fid,'%s\n',subject.gender);
end

fprintf(fid,'%s\n',date);

% Print header
chron = clock();
timestring=[mat2str(chron(4)) ':' mat2str(chron(5)) ':' mat2str(round(chron(6)))];
header = { ...
   timestring, 'PHASE','RUN','TRIAL','BKGD','NULL','TRACT', ...
   'FMAGA','FMAGP','FFREQ','EV','RT' ...
  };
for hdridx = 1:length(header)
    fprintf(fid,'%s ',header{hdridx});
end
fprintf('\n');

%% Initialize Screen
% debugging: screen(,,[],[0 0 640 480],32,2)
% otherwise:          [],[],32,2)
window = Screen('OpenWindow',whichScreen,[],[0 0 640 480],32,2);
white=WhiteIndex(window); % pixel value for white
black=BlackIndex(window); % pixel value for black
Screen('FillRect',window,black);
Screen('Flip',window);

%Define some various display parameters

Screen('TextSize',window,30);
Screen('TextFont',window,'Helvetica');
Screen('TextStyle',window,0);

rect = Screen('Rect',window);
middle_x = rect(:,3)/2;
middle_y = rect(:,4)/2-10;


% Instructions
instructions1 = [ 
  'You will see a clock face.\n' ...
  'Its arm will make a full turn over the course of 5 seconds.\n' ...
  'Press the button to win points before the arm makes a full turn.\n' ...
  'Try to win as many points as you can!\n\n' ...
  'Press any key to read more instructions' ...
];

instructions2 =  [ 
  'Sometimes you will win lots of points and sometimes you will win less.\n ' ...
  'The time at which you respond affects in some way\n' ...
  'the number of points that you can win.\n' ...
  'If you dont respond by the end of the clock cycle,\n' ...
  'you will not win any points.\n\n' ...
  'Press any key to read more instructions' ...
];

instructions3 = [ 
  'Hint: Try to respond at different times along\n' ... 
  'the clock cycle in order to learn how to make the most points.\n' ...
  'Note: The length of the experiment is constant\n' ... 
  'and is not affected by when you respond.\n\n' ...
  'Press any key when you are ready' ...
];

%Between run instructions

betweenInstructions = [
  'Next, you will see a new clock face.\n' ...
  'Try responding at different times in order to learn\n' ...
  'how to make the most points with this clock face.\n\n' ...
  'Press any key when you are ready' ...
 ];

%wait message
wait_message = 'Please remain as still as possible.';

%ready message
ready_txt    = 'Get ready';

  
%zero message
zero_msg = '0';
%win message1
win_msg1 = 'You win';
%win message2
win_msg2 = 'points';

%load colored boxes for practice and test trials
clocks=zeros(9);

% WF
% changed indexer from clock to clockIdx
% clock is also a function used by this script
for clockIdx=1:9
    tmp_clock=imread(['stims/' mat2str(clockIdx) '.jpg'],'jpg');
    clocks(clockIdx)=Screen('MakeTexture',window,tmp_clock);
end


boxes = zeros(8);
for box=1:8
    tmp_box=imread(['stims/box/' mat2str(box) '.jpg'],'jpg'); 
    boxes(box)=Screen('MakeTexture',window,tmp_box);
end


%load fixation images 

%load green fixation image -- never used!?
%green_fix = Screen('MakeTexture',window,imread('stims/green_fix.jpg','jpg'));

%load red fixation image
red_fix = Screen('MakeTexture',window,imread('stims/red_fix.jpg','jpg'));

%load wait fixation image
tmp_fix = imread('stims/wait_fix.jpg','jpg');
wait_fix = Screen('MakeTexture',window,tmp_fix);

% imgsize used to compute center
imgsize = size(tmp_fix);
imgsize = imgsize(1:2);

%load red fixation image
blank_clock = Screen('MakeTexture',window,imread('stims/blank.jpg','jpg'));


% %load instructions image
% tmp_inst = imread('instructions.jpg','jpg');
% instsize = size(tmp_inst);
% instsize = instsize(1:2);
% instructions = Screen('MakeTexture',window,tmp_inst);



%set the center % needed case change
center=CenterRect([0 0 imgsize(2) imgsize(1)],rect);

%% Setup Trial
% Restart Trials
if t > 1
    t=t+2;
    
    DrawFormattedText(window,bewteenInstructions , 'center', 'center',[256 256 256]);
    Screen('Flip',window);
    waitForAnyInput();
    
    % RESPONSE BOX CODES FOR FMRI WERE HERE
    
    %Display wait message
    Screen('TextSize',window,38);
    DrawFormattedText(window, wait_message, 'center', 'center',[256 256 256]);
    Screen('Flip',window);
    waitForAnyInput()
    
    
    %Display ready message
    DrawFormattedText(window, ready_txt, 'center', 'center',[256 256 256]);
    Screen('Flip',window);
    waitForAnyInput()
    
    trstamp=GetSecs; %stamps time when scanner starts script
    inst_stamp=trstamp; %stamps time, accounts for all time between ready message and end of instructions
    
    %     err=mriTrigger_hack(TTL, V, device);
    %     inst_stamp=GetSecs;
    %     trstamp=inst_stamp-(order(t,TRID)*2);

    Screen('FillRect',window,black);
    Screen('DrawTexture',window,red_fix);
    Screen('Flip',window);

    while (GetSecs-wait_stamp) <= WAIT_TIME,end

end  %end of if loop for restart trials

%offical start of trials

while t <= ORDER_LENGTH
    if t==1 %Initial instruction screen
        % PAGE 1 INSTRUCTIONS
        DrawFormattedText(window, instructions1, 'center', 'center',[256 256 256]);
        Screen('Flip',window);
        waitForAnyInput();
     
        %PAGE 2 INSTRUCTIONS
        DrawFormattedText(window, instructions2, 'center', 'center',[256 256 256]);
        Screen('Flip',window);
        waitForAnyInput();
         
         %PAGE 3 INSTRUCTIONS
        DrawFormattedText(window, instructions3, 'center', 'center',[256 256 256]);
        Screen('Flip',window);
        waitForAnyInput();
        wait_stamp=GetSecs; 
    end
    
    if t==1 || order(t,RUN)~=order(t-1,RUN) %at each new run, show these messages
       
        % only give the between message when order changes and it's not the
        % first trial
        if t > 1 
            DrawFormattedText(window,betweenInstructions, 'center', 'center',[256 256 256]);
            Screen('Flip',window);
            %DrawFormattedText(window, instructions3, 'center', 'center',[256 256 256]);
            waitForAnyInput();
        end
        
        %this is so that the any held key press doesn't overlap from the last message
        wait_stamp=GetSecs;
        while (GetSecs-wait_stamp)<.1;end 
        
        % wait
        DrawFormattedText(window, wait_message, 'center', 'center',[256 256 256]);
        Screen('Flip',window);
        waitForAnyInput();

        % ready
        DrawFormattedText(window, ready_txt, 'center', 'center',[256 256 256]);
        Screen('Flip',window);

        waitForAnyInput();
        wait_stamp=GetSecs;
        trstamp=GetSecs; %stamps time when scanner starts script
        rt_stamp=trstamp; %stamps time, accounts for all time between ready message and end of instructions

        %err=mriTrigger_hack(TTL, V, device);
        %inst_stamp=GetSecs;
        %trstamp=inst_stamp;

        Screen('FillRect',window,black);
        Screen('DrawTexture',window,wait_fix);
        Screen('Flip',window);

        while (GetSecs-wait_stamp) <= WAIT_TIME,end                             %current time minus how long it took to load red fixation and the trial stims

    end

    % Start trials
    trial_clockarm
    % workspace now has
    %   tract   (trial start time    -  trstamp)/TR
    %   RT      reaction time
    %   resp    eg. 'space', 'NR'==no response;

    %Reward

    if order(t,PHASE) == 1
        %COMPUTE CEV
        % magnitude increases while frequency decreases.
        F_Mag = (k*rt_extended)/(rt_extended-(RT+Shift)); 
        F_Freq = 1-((RT+Shift)/rt_extended);

    elseif order(t,PHASE) == 2
        % COMPUTE DEV
        %magnitude increases while frequency decreases.
        F_Mag = DEV_factor*log(DEV_factor2*(RT+Shift)); 
        CEV_x = 1-((RT+Shift)/rt_extended);
        IEV_x = CEV_x + (CEV_x*(sin_factor*sin((RT*pi)/5000)));
        F_Freq = (2*CEV_x)-IEV_x;

   elseif order(t,PHASE) == 3
        %COMPUTE IEV
        % magnitude increases while frequency decreases.
        CEV_x = (k*rt_extended)/(rt_extended-(RT+Shift)); 
        DEV_x = DEV_factor*log(DEV_factor2*(RT+Shift));
        F_Mag = (2*CEV_x)-(DEV_x);
        CEV_x2 = 1-((RT+Shift)/rt_extended);
        F_Freq = CEV_x2 + (CEV_x2*(sin_factor*sin((RT*pi)/5000)));

    elseif order(t,PHASE) == 4
        %CEVR ?
        %magnitude decreases while frequency increases.
        F_Mag = 1-((RT+Shift)/rt_extended);
        F_Mag = F_Mag*200;
        F_Freq = (k*rt_extended)/(rt_extended-(RT+Shift)) ;
        F_Freq = F_Freq/200;
       
    end
    
    ff = F_Freq;
    %Add noise to magnitude
    a = -5;
    b = 5;
    r = a + (b-a).*rand(1);
    % noise is an integer from -5 to 5
    r = round(r) ;              
    F_Mag = F_Mag + r;
    ev = F_Mag*F_Freq;
    F_Mag = round(F_Mag);
    fma = F_Mag;
    F_Mag = mat2str(F_Mag); % f mag is now a string that can be displayed to subj

    if F_Freq > rand(1) && RT~=0 
     Screen('DrawTexture',window,boxes(order(t,BOXC)));
        Screen('DrawTexture',window,blank_clock); 
        Screen('DrawText',window,win_msg1,middle_x-150,middle_y,black);
        Screen('DrawText',window,F_Mag,middle_x,middle_y,black);
        Screen('DrawText',window,win_msg2,middle_x+75,middle_y,black);
        fmp=fma;
        [junk,junk2,temp_stamp]=Screen('Flip',window);
        while(GetSecs-temp_stamp)<=(REWARD_DUR);end
    else
       Screen('DrawTexture',window,boxes(order(t,BOXC)));
        Screen('DrawTexture',window,blank_clock); 
        Screen('DrawText',window,win_msg1,middle_x-150,middle_y,black);
        Screen('DrawText',window,zero_msg,middle_x,middle_y,black);
        Screen('DrawText',window,win_msg2,middle_x+75,middle_y,black);
        fmp=0;
        [junk,junk2,temp_stamp]=Screen('Flip',window);
        while(GetSecs-temp_stamp)<=(REWARD_DUR);end
    end
    null_stamp=GetSecs;    
% add=0;
% subtract=0;
%     % Time remaining from allotted 4 seconds
%     if GetSecs-pres_stamp <=(TRIAL_TIME)
%         add=1;
%         value=4-(GetSecs-pres_stamp);
%     elseif (GetSecs-pres_stamp) > TRIAL_TIME
%         subtract =1;
%         value= (GetSecs-pres_stamp)-4;
%     end
%     while (GetSecs-pres_stamp) <= (TRIAL_TIME)
%         Screen('DrawTexture',window,boxes(order(t,BOXC)));
%         Screen('DrawTexture',window,blank_clock); 
%         Screen('DrawTexture',window,red_fix);          
%         null_stamp=GetSecs;
%         Screen('Flip',window);
%     end

    %Null interval - red fixation
    Screen('DrawTexture',window,boxes(order(t,BOXC)));
    Screen('DrawTexture',window,blank_clock);  
    Screen('DrawTexture',window,red_fix);
    Screen('Flip',window);

    %data file
    %PHASE
    if order(t,PHASE)==CEV
        fprintf(fid,'%s	','CEV');
    elseif order(t,PHASE)==IEV
        fprintf(fid,'%s	','IEV');
    elseif order(t,PHASE)==DEV
        fprintf(fid,'%s	','DEV');
    else
        fprintf(fid,'%s	','CEVR');
    end

    %RUN
    fprintf(fid,'%4i	',order(t,RUN));

    %TRIAL_NUM
    fprintf(fid,'%4i	',order(t,TRIAL));

    %BOX Color
    fprintf(fid,'%4i	',order(t,BOXC));

    %NULL
    fprintf(fid,'%4i	',order(t,NULL));

%     %TRID
%     fprintf(fid,'%4i	',order(t,TRID));

    %TR actual
    fprintf(fid,'%4i	',tract);
    order(t,TRACT_COL)=tract;
    
    %F MAG ACTUAL 
    fprintf(fid,'%4i	',fma);
    order(t,FMAGA_COL)=fma;
    
    %F MAG PRESENTED 
    fprintf(fid,'%4i	',fmp);
    order(t,FMAGP_COL)=fmp;
    
    % F Freq    
    fprintf(fid,'%4i	',ff);
    order(t,FFREQ_COL)=ff;

    %Expected Value
    fprintf(fid,'%4i	',ev);
    order(t,EV_COL)=ev;

    %RT
    fprintf(fid,'%4i\n',RT);
    order(t,RT_COL)=RT;

    %Save subject_id.mat file
    save(filename,'order','t','subject');

    while (GetSecs-null_stamp)<= (order(t,NULL));end
    
%     % Fixed null only implemented in trials where there is a response
%     if RT==0
%         while (GetSecs-null_stamp)<= (((order(t,NULL))-var));end    %Var accounts for extra time during trial
%     elseif add==1
%         while (GetSecs-null_stamp)<= (((order(t,NULL))+value-var));end    %Var accounts for extra time during trial
%     elseif subtract==1
%         while (GetSecs-null_stamp)<= (((order(t,NULL))-value-var));end        
%     end

    t = t+1;

    
end %end trials

%% done
%Close out
diary off;	%stop diary
fclose('all');	%close data file
ShowCursor;
Screen('CloseAll');