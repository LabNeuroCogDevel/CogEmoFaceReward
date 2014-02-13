% in function so add path doesn't mess up other things
% TODO: check score!
% remove next game "begin in"
function test = TestfMRI()
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
    addpath(genpath('private_testing'));
    KbName('UnifyKeyNames');
    global  LastKBCheck KBcounter KBResponse inputCounter initInput speedincrease;
    speedincrease=8;
    % % task info % %
    testSubjNum='1'; % string, not num
    subjmat=['subjects/fMRIEmoClock_' testSubjNum '_tc.mat'];
    % num trials per block
    trialsPerBlock=50;
    blocksPerRun=1;
    numRuns=8;
    % how to run it
    function success=runParadigm()
        try 
            fMRIEmoClock
            success=1;
        catch
            fprintf('Presentation died -- early quit I hope\n')
            success=0;
        end
    end
    % go for a block: 
    %                 waittime  keypress    num repeats
    blockresponses= repmat([2 KbName('2@')],trialsPerBlock,1);

    %% a fMRI run
    % 0. complete run to test everything out
    % 1. start but exit after 2 trials
    % 2. start again, dont resume, quit after 2 trials
    % 3. start again, resume (should be on block one), quit on block 2
    % 4. resume at the start of the second block
    
    % remove test subj if file exists
    if(exist(subjmat,'file')),  delete(subjmat),  end

    %% 0. run through
    inputCounter=1;  KBcounter=1;
    initInput={testSubjNum,'female', '12', '1'};
    KBResponse=[...
     0 KbName('SPACE'); ... initial instructions
     0 KbName('SPACE'); ... win/loose
     0 KbName('SPACE'); ... color boxes
     0 KbName('SPACE'); ... hint
     0 KbName('SPACE'); ... hint
     0 KbName('6^');    ... fixation cross
     blockresponses;
     2 KbName('SPACE'); ... final score
     ];
    test.zero_completed = runParadigm();
    taskout=load(subjmat);

    test.zero_subjnum    = strcmp(taskout.subject.subj_id,testSubjNum);
    test.zero_gender     = strcmp(taskout.subject.gender, initInput{2} );
    test.zero_age        = taskout.subject.age == initInput{3};
    test.zero_trial      = taskout.trialnum == trialsPerBlock;
    test.zero_trialsaved = ~isempty(taskout.order{end});
    test.zero_score      = taskout.score>20;
    test.zero_counterUsed = KBcounter == size(KBResponse,1);

    % 0.2 do this for the remaining runs
    KBResponse=[...
     0 KbName('SPACE'); ... reminder
     0 KbName('6^');    ... fixation cross
     blockresponses;
     2 KbName('SPACE'); ... final score
     ];
    for i=2:numRuns
      % use same subject, resume, at block i
      initInput={testSubjNum,'y', num2str(i) }; inputCounter=1;KBcounter=1;
      test.zero_rest(i).complete = runParadigm();
      taskout=load(subjmat);
      test.zero_rest(i).score    = taskout.score;
      test.zero_rest(i).trial    = taskout.trialnum;
    end
    
    %% 1. fresh start, 2 trials
    %Enter the subject ID number: 1
    %Enter subject's gender: 2
    %Enter subject's age: 12
    if(exist(subjmat,'file')),  delete(subjmat),  end
    inputCounter=1;  KBcounter=1;
    initInput={testSubjNum,'female', '12'};
    KBResponse=[...
     0 KbName('SPACE'); ... initial instructions
     0 KbName('SPACE'); ... win/loose
     0 KbName('SPACE'); ... color boxes
     0 KbName('SPACE'); ... hint
     0 KbName('SPACE'); ... get ready
     5/speedincrease KbName('2@');... ITI + first cirlce 
     5/speedincrease KbName('2@');... ISI+score+ITI+circle
     5/speedincrease KbName('2@');... ISI+score+ITI+circle
     0 KbName('ESCAPE');... WANT TO BE DONE
     0 KbName('SPACE');  ... and exit
     ];
    
    runParadigm();
    % check saved output against expectation
    taskout=load(subjmat);
    test.one_subjnum    = strcmp(taskout.subject.subj_id,testSubjNum);
    test.one_gender     = strcmp(taskout.subject.gender,'female');
    test.one_age        = taskout.subject.age == 12;
    test.one_trial      = taskout.trialnum == 3;
    test.one_trialsaved = ~isempty(taskout.order{3}) & isempty(taskout.order{4});

    % make sure score is reseting
    oldscore=taskout.score;
    
    %% 2. try again, 1/2 fresh
    % Enter the subject ID number: 1
    % Want to load previous session (y or n)? n
    % Enter subject's gender: Male
    % Enter subject's age: 12
    inputCounter=1;  KBcounter=1;
    initInput={testSubjNum,'n', 'Male', '16'};
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
 
    runParadigm();
    
    % check saved output against expectation
    taskout=load(subjmat)
    test.two_subjnum    = strcmp(taskout.subject.subj_id,testSubjNum);
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
    runParadigm();
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
    runParadigm();
    
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
    runParadigm();
    
    taskout=load(subjmat)
    test.five_trial      = taskout.trialnum == 2*trialsPerBlock;
    
    
    
    
    rmpath('private_testing');
end
