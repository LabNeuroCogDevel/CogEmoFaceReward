% in function so add path doesn't mess up other things
function test = TestEM()
    % TestEM: run fake people - 20131029 - WF
    %  this function co-ops WaitSecs, input, and KbCheck
    %  to automate testing
    %  * expected input is provided through
    %     * initInput (fed to input)
    %       - cell array with strings for each input request
    %     * KBResponse (fed to KbCheck)
    %       - Nx2 matrix [ time_from_last_kbcheck, keycode]
    %  * experiment can be sped up with speedincrease var
    
    %% setup
    addpath(genpath('private_testing'))
    global  LastKBCheck KBcounter KBResponse inputCounter initInput speedincrease;
    speedincrease=4;
    trialsPerBlock=63;
    
    % are we using the right functions?
    %which KbCheck
    %which input

    %% a MEG run
    % 1. start but exit after 2 trials
    % 2. start again, dont resume, quit after 2 trials
    % 3. start again, resume (should be on block one), quit on block 2
    % 4. resume at the start of the second block
    subjmat='subjects/MEG_1_tc.mat';
    if(exist(subjmat,'file'))
        delete(subjmat);
    end
    
    %% 1. fresh start, 2 trials
    %Enter the subject ID number: 1
    %Enter subject's gender: 2
    %Enter subject's age: 12
    inputCounter=1;  KBcounter=1;
    initInput={'1','Female', '12'};
    KBResponse=[...
     0 KbName('SPACE'); ... initial instructions
     0 KbName('SPACE'); ... win/loose
     0 KbName('SPACE'); ... color boxes
     0 KbName('SPACE'); ... hint
     0 KbName('SPACE'); ... get ready
     5/speedincrease KbName('SPACE');... ITI + first cirlce 
     5/speedincrease KbName('SPACE');... ISI+score+ITI+circle
     5/speedincrease KbName('SPACE');... ISI+score+ITI+circle
     0 KbName('ESCAPE');... WANT TO BE DONE
     0 KbName('SPACE');  ... and exit
     ];
    try
      MEGCogEmoFaceReward('DEBUG','NODAQ')
    catch 
       fprintf('Presentation died -- early quit I hope\n');
    end
    
    % check saved output against expectation
    taskout=load(subjmat);
    test.one_subjnum    = taskout.subject.subj_id == 1;
    test.one_gender     = taskout.subject.gender == 'female';
    test.one_age        = taskout.subject.age == 12;
    test.one_trial      = taskout.trialnum == 3;
    test.one_trialsaved =~ isempty(taskout.order{3}) & isempty(taskout.order{4});
    
    %% 2. try again, 1/2 fresh
    % Enter the subject ID number: 1
    % Want to load previous session (y or n)? n
    % Enter subject's gender: Male
    % Enter subject's age: 12
    inputCounter=1;  KBcounter=1;
    initInput={'1','n', 'Male', '16'};
    KBResponse=[...
     0 KbName('SPACE'); ... instructions1
     0 KbName('SPACE'); ... win loose
     0 KbName('SPACE'); ... color 
     0 KbName('SPACE'); ... hint
     0 KbName('SPACE'); ... get ready
     repmat([4/speedincrease KbName('SPACE')],trialsPerBlock-1,1); ... ITI + first cirlce + ISI + score
     0 KbName('ESCAPE');... WANT TO BE DONE
     0 KbName('SPACE');  ... and exit
     ];
 
    try MEGCogEmoFaceReward('DEBUG','NODAQ'), catch,fprintf('Presentation died -- early quit I hope\n'), end
    
    
    % check saved output against expectation
    taskout=load(subjmat)
    test.two_subjnum    = taskout.subject.subj_id == 1;
    test.two_gender     = strcmp(taskout.subject.gender,'male');
    test.two_age        = taskout.subject.age == 16;
    test.two_trial      = taskout.trialnum == trialsPerBlock-1;
    test.two_trialsaved =~ isempty(taskout.order{trialsPerBlock-1}) & isempty(taskout.order{trialsPerBlock});
    
    
    %% 3. resume, but should be on the first block still
    inputCounter=1;  KBcounter=1;
    initInput={'1','y'}; % resume but never past block 1
    KBResponse=[...
     0 KbName('SPACE'); ... instructions1
     0 KbName('SPACE'); ... win loose
     0 KbName('SPACE'); ... color 
     0 KbName('SPACE'); ... hint
     0 KbName('SPACE'); ... get ready
     repmat([4/speedincrease KbName('SPACE')],trialsPerBlock+2,1); ... ITI + first cirlce + ISI + score
     0 KbName('ESCAPE');... WANT TO BE DONE
     0 KbName('SPACE');  ... and exit
     ];
    try MEGCogEmoFaceReward('DEBUG','NODAQ'), catch,fprintf('Presentation died -- early quit I hope\n'), end
    taskout=load(subjmat)
    test.four_trial      = taskout.trialnum == trialsPerBlock+2;
    test.four_trialsaved =~ isempty(taskout.order{trialsPerBlock+2}) & isempty(taskout.order{trialsPerBlock+2});

    
    %% 4. resume, but on second block now (but dont finish)
    inputCounter=1;  KBcounter=1;
    initInput={'1','y'}; % resume but never past block 1
    KBResponse=[...
     0 KbName('SPACE'); ... reminder
     0 KbName('SPACE'); ... get ready
     repmat([4/speedincrease KbName('SPACE')],trialsPerBlock-1,1); ... ITI + first cirlce + ISI + score
     0 KbName('ESCAPE');... WANT TO BE DONE
     0 KbName('SPACE'); ... and exit
     ];
    try MEGCogEmoFaceReward('DEBUG','NODAQ'), catch,fprintf('Presentation died -- early quit I hope\n'), end
    
    taskout=load(subjmat)
    test.four_trial      = taskout.trialnum == 2*trialsPerBlock-1;
    test.four_trialsaved =~ isempty(taskout.order{2*trialsPerBlock-1}) & isempty(taskout.order{2*trialsPerBlock});
    
    
    %% 5. resume again on the second block
    inputCounter=1;  KBcounter=1;
    initInput={'1','y'}; % resume but never past block 1
    KBResponse=[...
     0 KbName('SPACE'); ... reminder
     0 KbName('SPACE'); ... get ready
     repmat([4/speedincrease KbName('SPACE')],trialsPerBlock+1,1); ... ITI + first cirlce + ISI + score
     0 KbName('ESCAPE');... WANT TO BE DONE
     0 KbName('SPACE');  ... and exit
     ];
    try MEGCogEmoFaceReward('DEBUG','NODAQ'), catch,fprintf('Presentation died -- early quit I hope\n'), end
    
    taskout=load(subjmat)
    test.five_trial      = taskout.trialnum == 2*trialsPerBlock+1;
    
    
    
    
    rmpath('private_testing');
end