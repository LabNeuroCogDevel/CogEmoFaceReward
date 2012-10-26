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

%Input subject info
subject = struct(...
    'subj_id',[],...
    'age',[],...
    'gender',[],...
    'cb_num',[]); %end subject struct

subject.subj_id = input('Enter the subject ID number: ','s');

filename = [subject.subj_id '_tc'];

restart = input('Is this a restart (y or n)? ','s');

if restart == 'y' || restart == 'Y'
    subj_id = subject.subj_id;
    clear subject
    load(filename);
else
    subject.age = input('Enter subject age: ');
    sex = input('Enter subject gender (m or f): ','s');

    if sex == 'm' || sex =='M'
        subject.gender = 'male';
    else
        subject.gender = 'female';
    end

    subject.cb_num = input('Enter CB#: ');

    order = load(['order' mat2str(subject.cb_num) '.txt']);
    order = [order zeros(size(order,1),2)];
    t = 1;
end

whichScreen = 0;

ORDER_LENGTH=length(order);

%Initialize data storage and records
diaryfile = [subject.subj_id 'tcdiary'];
diary(diaryfile);

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
chron = clock;
time = [mat2str(chron(4)) ':' mat2str(chron(5)) ':' mat2str(round(chron(6)))];
fprintf(fid,'%s\n',time);
fprintf(fid,'%s	','PHASE');
fprintf(fid,'%s	','RUN');
fprintf(fid,'%s	','TRIAL');
fprintf(fid,'%s	','BKGD');
fprintf(fid,'%s	','NULL');
fprintf(fid,'%s	','TRACT');
fprintf(fid,'%s	','FMAGA');
fprintf(fid,'%s	','FMAGP');
fprintf(fid,'%s	','FFREQ');
fprintf(fid,'%s	','EV');
fprintf(fid,'%s\n','RT');

%Initialize Screen
window = Screen('OpenWindow',whichScreen,[],[],32,2);
white=WhiteIndex(window); % pixel value for white
black=BlackIndex(window); % pixel value for black
Screen('FillRect',window,black);
Screen('Flip',window);

%Define some various display parameters
rect = Screen('Rect',window);
Screen('TextSize',window,30);
Screen('TextFont',window,'Helvetica');
Screen('TextStyle',window,0);
middle_x = rect(:,3)/2;
middle_y = rect(:,4)/2-10;

%wait message
wait_message = 'Please remain as still as possible.';
wait_message_bounds = Screen('TextBounds',window,wait_message);

%ready message
ready_txt = 'Get ready';
ready_txt_bounds=Screen('TextBounds',window,ready_txt);

% Instruct pg 1
in1p1 = 'You will see a clock face.';
in1p1_bounds = Screen('TextBounds',window,in1p1);
in2p1 = 'Its arm will make a full turn over the course of 5 seconds.';
in2p1_bounds = Screen('TextBounds',window,in2p1);
in3p1= 'Press the button to win points before the arm makes a full turn.';
in3p1_bounds = Screen('TextBounds',window,in3p1);
in4p1 = 'Try to win as many points as you can!';
in4p1_bounds = Screen('TextBounds',window,in4p1);
in5p1 = 'Press any key to read more instructions';
in5p1_bounds = Screen('TextBounds',window,in5p1);

%Instruct pg2
in1p2 = 'Sometimes you will win lots of points and sometimes you will win less. ';
in1p2_bounds = Screen('TextBounds',window,in1p2);
in2p2 = 'The time at which you respond affects in some way';
in2p2_bounds = Screen('TextBounds',window,in2p2);
in3p2 = 'the number of points that you can win.';
in3p2_bounds = Screen('TextBounds',window,in3p2);
in4p2= 'If you dont respond by the end of the clock cycle,';
in4p2_bounds = Screen('TextBounds',window,in4p2);
in5p2= 'you will not win any points. ';
in5p2_bounds = Screen('TextBounds',window,in5p2);
in6p2 = 'Press any key to read more instructions';
in6p2_bounds = Screen('TextBounds',window,in6p2);

%Instruct pg3
in1p3 = 'Hint: Try to respond at different times along'; 
in1p3_bounds = Screen('TextBounds',window,in1p3);
in2p3 ='the clock cycle in order to learn how to make the most points.';
in2p3_bounds = Screen('TextBounds',window,in2p3);
in3p3 = 'Note: The length of the experiment is constant'; 
in3p3_bounds = Screen('TextBounds',window,in3p3);
in4p3 ='and is not affected by when you respond.';
in4p3_bounds = Screen('TextBounds',window,in4p3);
in5p3 = 'Press any key when you are ready';
in5p3_bounds = Screen('TextBounds',window,in5p3);

%Between run instructions
in1br = 'Next, you will see a new clock face.';
in1br_bounds = Screen('TextBounds',window,in1br);
in2br = 'Try responding at different times in order to learn';
in2br_bounds = Screen('TextBounds',window,in2br);
in3br = 'how to make the most points with this clock face.';
in3br_bounds = Screen('TextBounds',window,in3br);
in4br= 'Press any key when you are ready';
in4br_bounds = Screen('TextBounds',window,in4br);
  
%zero message
zero_msg = '0';
zero_msg_bounds=Screen('TextBounds',window,zero_msg);

%win message1
win_msg1 = 'You win';
win_msg1_bounds=Screen('TextBounds',window,win_msg1);
%win message2
win_msg2 = 'points';
win_msg2_bounds=Screen('TextBounds',window,win_msg2);

%load colored boxes for practice and test trials
cd stims
clocks=zeros(9);

for clock=1:9
    tmp_clock=imread([mat2str(clock) '.jpg'],'jpg');
    clocksize = size(tmp_clock);
    clocksize = clocksize(1:2);
    clocks(clock)=Screen('MakeTexture',window,tmp_clock);
end

cd box
boxes = zeros(8);
for box=1:8
    tmp_box=imread([mat2str(box) '.jpg'],'jpg'); 
    boxsize = size(tmp_box);
    boxsize = boxsize(1:2);
    boxes(box)=Screen('MakeTexture',window,tmp_box);
end
cd ..

%load fixation images 

%load green fixation image
tmp_fix = imread('green_fix.jpg','jpg');
imgsize = size(tmp_fix);
imgsize = imgsize(1:2);
green_fix = Screen('MakeTexture',window,tmp_fix);

%load red fixation image
tmp_fix = imread('red_fix.jpg','jpg');
imgsize = size(tmp_fix);
imgsize = imgsize(1:2);
red_fix = Screen('MakeTexture',window,tmp_fix);

%load wait fixation image
tmp_fix = imread('wait_fix.jpg','jpg');
imgsize = size(tmp_fix);
imgsize = imgsize(1:2);
wait_fix = Screen('MakeTexture',window,tmp_fix);


%load red fixation image
tmp_clock = imread('blank.jpg','jpg');
imgsize = size(tmp_fix);
imgsize = imgsize(1:2);
blank_clock = Screen('MakeTexture',window,tmp_clock);

% %load instructions image
% tmp_inst = imread('instructions.jpg','jpg');
% instsize = size(tmp_inst);
% instsize = instsize(1:2);
% instructions = Screen('MakeTexture',window,tmp_inst);

cd ..

%set the center
center=centerrect([0 0 imgsize(2) imgsize(1)],rect);

%Restart Trials
if t > 1
    t=t+2;
    Screen('DrawText',window,in1br,middle_x-(in1br_bounds(3)/2),middle_y-150,white);
    Screen('DrawText',window,in2br,middle_x-(in2br_bounds(3)/2),middle_y-75,white);
    Screen('DrawText',window,in3br,middle_x-(in3br_bounds(3)/2),middle_y-25,white); 
    Screen('DrawText',window,in4br,middle_x-(in4br_bounds(3)/2),middle_y+75,white);
    Screen('Flip',window);
   
        while 1
            keyCode=[];
            [keyIsDown,secs,keyCode] = KbCheck;
            if keyCode(39) || keyCode(98)
                wait_stamp=GetSecs;
                break;
            end
        end
         % RESPONSE BOX CODES FOR FMRI
    %         button_time = GetSecs;
    %         buttons=[Gamepad('GetButton', 1, 1) Gamepad('GetButton', 1, 2) Gamepad('GetButton', 1, 3) Gamepad('GetButton', 1, 4)];
    %         if any(buttons)
    %             if length(find(buttons))==1
    %                 resp=mat2str(find(buttons));
    %             else
    %                 resp='2R';
    %             end
    %             RT = (button_time-rt_stamp)*1000
    %         end
    
    %Display wait message
    Screen('TextSize',window,38);
    Screen('DrawText',window,wait_message,middle_x-(wait_message_bounds(3)/2),middle_y,white);
    Screen('Flip',window);
    while 1
        keyCode=[];
        [keyIsDown,secs,keyCode] = KbCheck;
        if keyCode(39) || keyCode(98)
            break;
        end
    end

    wait_stamp=GetSecs;
    while (GetSecs-wait_stamp)<.1;end %this is so that the 0 press doesn't overlap from the last message

    %Display ready message
    Screen('DrawText',window,ready_txt,middle_x-(ready_txt_bounds(3)/2),middle_y,white);
    Screen('Flip',window);

    while 1
        keyCode=[];
        [keyIsDown,secs,keyCode] = KbCheck;
        if keyCode(39) || keyCode(98)
            trstamp=GetSecs; %stamps time when scanner starts script
            inst_stamp=trstamp; %stamps time, accounts for all time between ready message and end of instructions
            break;
        end
    end

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
    Screen('DrawText',window,in1p1,middle_x-(in1p1_bounds(3)/2),middle_y-150,white);
    Screen('DrawText',window,in2p1,middle_x-(in2p1_bounds(3)/2),middle_y-75,white);
    Screen('DrawText',window,in3p1,middle_x-(in3p1_bounds(3)/2),middle_y,white);
    Screen('DrawText',window,in4p1,middle_x-(in4p1_bounds(3)/2),middle_y+75,white);
    Screen('DrawText',window,in5p1,middle_x-(in5p1_bounds(3)/2),middle_y+150,white);
    Screen('Flip',window);
   
        while 1
            keyCode=[];
            [keyIsDown,secs,keyCode] = KbCheck;
            if keyCode(39) || keyCode(98)
                wait_stamp=GetSecs;
                break;
            end
        end
         % RESPONSE BOX CODES FOR FMRI
    %         button_time = GetSecs;
    %         buttons=[Gamepad('GetButton', 1, 1) Gamepad('GetButton', 1, 2) Gamepad('GetButton', 1, 3) Gamepad('GetButton', 1, 4)];
    %         if any(buttons)
    %             if length(find(buttons))==1
    %                 resp=mat2str(find(buttons));
    %             else
    %                 resp='2R';
    %             end
    %             RT = (button_time-rt_stamp)*1000
    %         end
    while (GetSecs-wait_stamp)<.1;end
    %PAGE 2 INSTRUCTIONS
    Screen('DrawText',window,in1p2,middle_x-(in1p2_bounds(3)/2),middle_y-150,white);
    Screen('DrawText',window,in2p2,middle_x-(in2p2_bounds(3)/2),middle_y-75,white);
    Screen('DrawText',window,in3p2,middle_x-(in3p2_bounds(3)/2),middle_y-25,white);
    Screen('DrawText',window,in4p2,middle_x-(in4p2_bounds(3)/2),middle_y+75,white);
    Screen('DrawText',window,in5p2,middle_x-(in5p2_bounds(3)/2),middle_y+150,white);
    Screen('DrawText',window,in6p2,middle_x-(in6p2_bounds(3)/2),middle_y+225,white);
    Screen('Flip',window);
   
        while 1
            keyCode=[];
            [keyIsDown,secs,keyCode] = KbCheck;
            if keyCode(39) || keyCode(98)
                wait_stamp=GetSecs;
                break;
            end
        end
         % RESPONSE BOX CODES FOR FMRI
    %         button_time = GetSecs;
    %         buttons=[Gamepad('GetButton', 1, 1) Gamepad('GetButton', 1, 2) Gamepad('GetButton', 1, 3) Gamepad('GetButton', 1, 4)];
    %         if any(buttons)
    %             if length(find(buttons))==1
    %                 resp=mat2str(find(buttons));
    %             else
    %                 resp='2R';
    %             end
    %             RT = (button_time-rt_stamp)*1000
    %         end
    while (GetSecs-wait_stamp)<.1;end
     %PAGE 3 INSTRUCTIONS
    Screen('DrawText',window,in1p3,middle_x-(in1p3_bounds(3)/2),middle_y-150,white);
    Screen('DrawText',window,in2p3,middle_x-(in2p3_bounds(3)/2),middle_y-100,white);
    Screen('DrawText',window,in3p3,middle_x-(in3p3_bounds(3)/2),middle_y,white); 
    Screen('DrawText',window,in4p3,middle_x-(in4p3_bounds(3)/2),middle_y+50,white);
    Screen('DrawText',window,in5p3,middle_x-(in5p3_bounds(3)/2),middle_y+150,white); 
    Screen('Flip',window);
   
        while 1
            keyCode=[];
            [keyIsDown,secs,keyCode] = KbCheck;
            if keyCode(39) || keyCode(98)
                wait_stamp=GetSecs;
                break;
            end
        end
         % RESPONSE BOX CODES FOR FMRI
    %         button_time = GetSecs;
    %         buttons=[Gamepad('GetButton', 1, 1) Gamepad('GetButton', 1, 2) Gamepad('GetButton', 1, 3) Gamepad('GetButton', 1, 4)];
    %         if any(buttons)
    %             if length(find(buttons))==1
    %                 resp=mat2str(find(buttons));
    %             else
    %                 resp='2R';
    %             end
    %             RT = (button_time-rt_stamp)*1000
    %         end
    elseif t~=1 && order(t,RUN)~=order(t-1,RUN)
    Screen('DrawText',window,in1br,middle_x-(in1p3_bounds(3)/2),middle_y-150,white);
    Screen('DrawText',window,in2br,middle_x-(in2p3_bounds(3)/2),middle_y-100,white);
    Screen('DrawText',window,in3br,middle_x-(in3p3_bounds(3)/2),middle_y,white); 
    Screen('DrawText',window,in4br,middle_x-(in4p3_bounds(3)/2),middle_y+75,white);
    Screen('Flip',window);
   
        while 1
            keyCode=[];
            [keyIsDown,secs,keyCode] = KbCheck;
            if keyCode(39) || keyCode(98)
                wait_stamp=GetSecs;
                break;
            end
        end
         % RESPONSE BOX CODES FOR FMRI
    %         button_time = GetSecs;
    %         buttons=[Gamepad('GetButton', 1, 1) Gamepad('GetButton', 1, 2) Gamepad('GetButton', 1, 3) Gamepad('GetButton', 1, 4)];
    %         if any(buttons)
    %             if length(find(buttons))==1
    %                 resp=mat2str(find(buttons));
    %             else
    %                 resp='2R';
    %             end
    %             RT = (button_time-rt_stamp)*1000
    %         end
    end
    if t==1 || order(t,RUN)~=order(t-1,RUN) %at each new run, show these messages
    

     wait_stamp=GetSecs;
        while (GetSecs-wait_stamp)<.1;end %this is so that the 0 press doesn't overlap from the last message
   
        Screen('DrawText',window,wait_message,middle_x-(wait_message_bounds(3)/2),middle_y,white);
        Screen('Flip',window);
        while 1
            keyCode=[];
            [keyIsDown,secs,keyCode] = KbCheck;
            if keyCode(39) || keyCode(98)
                break;
            end
        end

        wait_stamp=GetSecs;
        while (GetSecs-wait_stamp)<.1;end %this is so that the 0 press doesn't overlap from the last message
        %display ready message
        Screen('DrawText',window,ready_txt,middle_x-(ready_txt_bounds(3)/2),middle_y,white);
        Screen('Flip',window);

        while 1
            keyCode=[];
            [keyIsDown,secs,keyCode] = KbCheck;
            if keyCode(39) || keyCode(98)
                wait_stamp=GetSecs;
                trstamp=GetSecs; %stamps time when scanner starts script
                rt_stamp=trstamp; %stamps time, accounts for all time between ready message and end of instructions
                break;
            end
        end

        %err=mriTrigger_hack(TTL, V, device);
        %inst_stamp=GetSecs;
        %trstamp=inst_stamp;

        Screen('FillRect',window,black);
        Screen('DrawTexture',window,wait_fix);
        Screen('Flip',window);

        while (GetSecs-wait_stamp) <= WAIT_TIME,end                             %current time minus how long it took to load red fixation and the trial stims

    end

    %Start trials
   
    RT = 0; %currently there is no reaction time
    a=1;    %each new trial will begin with the first clock face
    pres_stamp=GetSecs;     %Time stamp signalling beginning of the for loop
    tract=(pres_stamp-trstamp)/TR; %current TR = time trial starts minus when the scanner started
    %cumulative=(order(t,TRID)*TR); % total time in s that should have passed
    %var = ((tract*TR)-cumulative); % total time that has passed minus what should have. this is added onto the null so that each trial stays on TR
%     if var<0
%         var=0;
%     end    
    
    for a=1:8 % Loop through all clock faces until there is an RT
        Screen('DrawTexture',window,boxes(order(t,BOXC)));
        Screen('DrawTexture',window,clocks(a));
        [junk,junk2,rt_stamp]=Screen('Flip',window);
        while ~RT && (GetSecs-rt_stamp) <= (CLOCK_DUR) %While no RT and while time is less than clock dur, wait for a keypress for each clock face
            [keyIsDown,secs,keyCode] = KbCheck;
            if keyIsDown && ~RT
                if ~(keyCode(39) || keyCode(98))
                    RT = (secs - pres_stamp)*1000;    %RT = time elapsed between beginning of for loop and time of keypress
                    if keyCode(17) % the "n" key
                        keyCode = 89;
                    end
                    resp = KbName(keyCode);
                end
            end
        end
        if RT   %If there is an RT, break out of the for loop
            break;
        end
    end
    
    clock_stamp=GetSecs;
    if ~RT 
        while (GetSecs-clock_stamp) < CLOCK9_DUR
    Screen('DrawTexture',window,boxes(order(t,BOXC)));
    Screen('DrawTexture',window,clocks(9));
    Screen('Flip',window);
        end
    end
    
    %for no response
    if ~RT
        RT = 0;
        resp = 'NR';
        ev = 0;
        fm=0;
        ff=0;
    end

    % RESPONSE BOX CODES FOR FMRI
    %         button_time = GetSecs;
    %         buttons=[Gamepad('GetButton', 1, 1) Gamepad('GetButton', 1, 2) Gamepad('GetButton', 1, 3) Gamepad('GetButton', 1, 4)];
    %         if any(buttons)
    %             if length(find(buttons))==1
    %                 resp=mat2str(find(buttons));
    %             else
    %                 resp='2R';
    %             end
    %             RT = (button_time-rt_stamp)*1000
    %         end


    %Reward
    %COMPUTE CEV
    if order(t,PHASE) == 1
        F_Mag = (k*rt_extended)/(rt_extended-(RT+Shift)); % magnitude increases while frequency decreases.
        F_Freq = 1-((RT+Shift)/rt_extended);
        ff = F_Freq;

        %Add noise to magnitude
        a = -5;
        b = 5;
        r = a + (b-a).*rand(1);
        round(r) ;              % noise is an integer from -5 to 5
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
    elseif order(t,PHASE) == 2
        % COMPUTE DEV
        F_Mag = DEV_factor*log(DEV_factor2*(RT+Shift)); %magnitude increases while frequency decreases.
        CEV_x = 1-((RT+Shift)/rt_extended);
        IEV_x = CEV_x + (CEV_x*(sin_factor*sin((RT*pi)/5000)));
        F_Freq = (2*CEV_x)-IEV_x;
        ff = F_Freq;
        %Add noise to magnitude
        a = -5;
        b = 5;
        r = a + (b-a).*rand(1);
        round(r) ;              % noise is an integer from -5 to 5
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

    elseif order(t,PHASE) == 3
        %COMPUTE IEV
        CEV_x = (k*rt_extended)/(rt_extended-(RT+Shift)); % magnitude increases while frequency decreases.
        DEV_x = DEV_factor*log(DEV_factor2*(RT+Shift));
        F_Mag = (2*CEV_x)-(DEV_x);
        CEV_x2 = 1-((RT+Shift)/rt_extended);
        F_Freq = CEV_x2 + (CEV_x2*(sin_factor*sin((RT*pi)/5000)));
        ff = F_Freq;

        %Add noise to magnitude
        a = -5;
        b = 5;
        r = a + (b-a).*rand(1);
        round(r) ;              % noise is an integer from -5 to 5
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
    elseif order(t,PHASE) == 4
        F_Mag = 1-((RT+Shift)/rt_extended); %magnitude decreases while frequency increases.
        F_Mag = F_Mag*200;
        F_Freq = (k*rt_extended)/(rt_extended-(RT+Shift)) ;
        F_Freq = F_Freq/200;
        ff = F_Freq;
        
        %Add noise to magnitude
        a = -5;
        b = 5;
        r = a + (b-a).*rand(1);
        round(r) ;              % noise is an integer from -5 to 5
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
    end    
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

%Close out
diary off;	%stop diary
fclose('all');	%close data file
ShowCursor;
Screen('CloseAll');