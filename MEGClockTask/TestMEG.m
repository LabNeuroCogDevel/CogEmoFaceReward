% TestMEG paradigm
%
%     test = TestMEG; res=run(test)


classdef TestMEG < matlab.unittest.TestCase

    properties
     orgpath
     sid
     trialsPerBlock
     speedincrease
     trialsPerRun
     subjmat
    end

     methods(TestMethodSetup)
       function pathandglobal(tc)
            % overloading the path causes some warnings
            % we dont want to see it everytime
            % --- but this means if the path isn't there, we wont see the warning
            tc.orgpath = path;
            warning('off','all');
            addpath(genpath('../private_testing'));
            warning('on','all');

            KbName('UnifyKeyNames');
            global  LastKBCheck KBcounter KBResponse inputCounter initInput speedincrease;
            % test info
            tc.sid=99999;
            tc.trialsPerBlock=63; tc.trialsPerRun=8;
            tc.speedincrease=8;
            % matfile
            c=clock();
            tc.subjmat=['subjects/MEG_' num2str(tc.sid) '_' num2str(c(1)*10000+c(2)*100+c(3)) '_tc.mat'];

            % delete the subjmat if it exists
            if(exist(tc.subjmat,'file')),  delete(tc.subjmat),  end

            fprintf('setup\n');
       end
     end

     methods(TestMethodTeardown)
       function removepath(testCase)
            % rmpath('../private_testing');
            path= testCase.orgpath;
            rmpath('../private_testing/') % just incase it was still there
            fprintf('teardown\n');
       end
     end


    methods (Test)


        %%%%%%%%%% ACTUAL TASK  %%%%%%%%
        function testActual(tc)

            % path and overloading things
            global  LastKBCheck KBcounter KBResponse inputCounter initInput speedincrease;


            % starting question
            % (1) age, (2) m or f
            inputCounter=1;  KBcounter=1;
            initInput={'99', 'm'};
            % screen (need space):
            % (1):winpoints, (2):response, (3):rewardfunc=borderinfo, (4)hint, (5)getready 

            spcrsp = KbName('SPACE');
            blockresponses=[ ...
                ... ITI + first cirlce + ISI + score
                repmat([4/tc.speedincrease KbName('2@')],tc.trialsPerBlock,1); ...
                8 spcrsp; ... "score and n/total progress"
                2 spcrsp; ... Final score (on last trial) -- not eaten otherwise
                ];

            KBResponse=[...
             0 spcrsp;      ... initial instructions
             0 spcrsp;      ... win/loose
             0 spcrsp;      ... color boxes
             0 spcrsp;      ... hint
             2 spcrsp;      ... get ready
             blockresponses;
             ];
            


            % num trials per block
            [success subject]=runParadigm(tc.sid,1);
            tc.verifyEqual(success,1);
            % check subject
            tc.verifyEqual(subject.subj_id,tc.sid)
            % we will want to check experiment stays the same for each run, so grab it here
            experiment=subject.experiment;

            orderlens = length(find(cellfun(@(x) length(x), subject.order)));
            tc.verifyEqual(orderlens,tc.trialsPerBlock)
            
            

            %% do it again
            for(i=2:tc.trialsPerRun)
               % next blox all llike
               KBResponse=[...
                0 spcrsp;      ... initial instructions
                2 spcrsp;      ... get ready
                blockresponses;
                ... %just so we dont get stuck, use some escapse
                2 KbName('ESCAPE'); 
                2 KbName('ESCAPE');
                ];
               % do not redo block 1
               initInput={'n'};
               inputCounter=1;  KBcounter=1;

               [success subject]=runParadigm(tc.sid,1);
               tc.verifyEqual(success,1);
               % make sure the experiment is the same each time
               tc.verifyEqual(subject.experiment,experiment);
            end



            % support running functions
            function [success subject]=runParadigm(sid,block)
                try 
                    fprintf('MEGClockTask(%d,%d,''DEBUG'',''NODAQ'')\n',tc.sid,block);
                    subject=MEGClockTask(tc.sid,block,'DEBUG','NODAQ');
                    success=1;
                catch
                    fprintf('Presentation died -- early quit I hope\n')


                    % load subject mat
                    if(exist(tc.subjmat,'file'))
                     load(tc.subjmat)
                    else 
                     subject=0;
                    end
                    success=0;
                end
            end
        end

        %%%%%%%%%% Test asking where to start %%%%%%%%%%%%%%%%%%%%
        %% should start just fine
        function testFirst(testCase)
            desired = chooseRun(1, [0 0 0 0], 10);
            testCase.verifyEqual(desired,1);

            % check for two arguments
            [desired, trial] = chooseRun(1, [0 0 0 0], 10);
            testCase.verifyEqual([desired trial],[1 1]);
        end

        %% should continue to 2 becuase 1 is full
        function testContinue(testCase)
            % we need input globals to feed to the overloaded read functions
            global inputCounter initInput;
            
            % respond to prompt: do not want to redo run 1
            inputCounter=1; initInput={'n'};

            desired = chooseRun(1, [10 0 0 0], 10);
            testCase.verifyEqual(desired,2);

            % test with two outputs
            inputCounter=1;
            [desired, trial] = chooseRun(1, [10 0 0 0], 10);
            testCase.verifyEqual([desired trial],[2 1]);
            
            % test that we continue far into the run
            inputCounter=1;
            desired = chooseRun(1, [10 10 10 0], 10);
            testCase.verifyEqual(desired,4);

        end

        %% test end of triallist
        % expect return 0 if all trials are done
        function testEnd(testCase)
            % we need input globals to feed to the overloaded read functions
            global inputCounter initInput;
            
            % respond to prompt: do not want to redo run 1
            inputCounter=1; initInput={'n'};

            tottrials= 10;
            blocklist= [10 10 10 10];
            for b=1:length(blocklist)
              inputCounter=1;
              desired = chooseRun(b, blocklist, tottrials);
              testCase.verifyEqual(desired,0);
            end

        end

 
        %% redo block
        function testRedo(testCase)
            % we need input globals to feed to the overloaded read functions
            global inputCounter initInput;
            
            % respond to prompt: do not want to redo run 1
            inputCounter=1; initInput={'y'};

            tottrials= 10;
            blocklist= [10 10 10 10];
            for b=1:length(blocklist)
              inputCounter=1;
              desired = chooseRun(b, blocklist, tottrials);
              testCase.verifyEqual(desired,b);
            end

            inputCounter=1;
            blocklist = [ 5 0 0 0];
            desired = chooseRun(1, blocklist, tottrials);
            testCase.verifyEqual(desired,1);

            inputCounter=1;
            blocklist = [ 5 5 0 0];
            desired = chooseRun(1, blocklist, tottrials);
            testCase.verifyEqual(desired,1);

            inputCounter=1;
            blocklist = [ 5 0 5 0];
            desired = chooseRun(3, blocklist, tottrials);
            testCase.verifyEqual(desired,3);

            inputCounter=1; initInput={'n','4'};
            blocklist = [ 5 0 5 0];
            desired = chooseRun(3, blocklist, tottrials);
            testCase.verifyEqual(desired,4);
        end

        %% test weirdness
        function testCompleteAfterIncomplete(testCase)
            
            % we need input globals to feed to the overloaded read functions
            global inputCounter initInput;
            inputCounter=1; initInput={'y'};

            blocklist= [5 10 0 0];
            tottrials= 10;
            % we should reset 1
            desired = chooseRun(1,blocklist,tottrials);
            testCase.verifyEqual(desired,1);

            % somewhat hidden option to continue
            inputCounter=1; initInput={'continue'};
            [desired trial] = chooseRun(1,blocklist,tottrials);
            testCase.verifyEqual([desired trial],[1 5]);
            
            % go to a new block
            %  -- respond n to run block 1 again, respond 3 to which block to run
            inputCounter=1; initInput={'n','3'};
            desired = chooseRun(1, blocklist,tottrials);
            testCase.verifyEqual(desired,3);


        end

        %%%% test reseting a block 
        function testOrderRest(testCase)
           
           % first 5 elements of order
           % score is
           % 24
           % 14 -> 38
           % 13 -> 51
           % 20 -> 71
           % 19 -> 90
           order=reshape({reshape({reshape('IEV',[1  3]) reshape([1.000000 ],[1  1]) reshape([1.000000 ],[1  1]) reshape([1.000000 ],[1  1]) reshape([0.000000 ],[1  1]) reshape([1.440079 ],[1  1]) reshape([24.000000 ],[1  1]) reshape([24.000000 ],[1  1]) reshape([0.858221 ],[1  1]) reshape([20.767556 ],[1  1]) reshape([1139.000000 ],[1  1]) reshape('fear',[1  4]) },[1  12]) reshape({reshape('IEV',[1  3]) reshape([1.000000 ],[1  1]) reshape([2.000000 ],[1  1]) reshape([1.000000 ],[1  1]) reshape([0.000000 ],[1  1]) reshape([4.373275 ],[1  1]) reshape([14.000000 ],[1  1]) reshape([14.000000 ],[1  1]) reshape([0.893257 ],[1  1]) reshape([12.729213 ],[1  1]) reshape([477.000000 ],[1  1]) reshape('fear',[1  4]) },[1  12]) reshape({reshape('IEV',[1  3]) reshape([1.000000 ],[1  1]) reshape([3.000000 ],[1  1]) reshape([1.000000 ],[1  1]) reshape([0.000000 ],[1  1]) reshape([7.323204 ],[1  1]) reshape([13.000000 ],[1  1]) reshape([13.000000 ],[1  1]) reshape([0.896496 ],[1  1]) reshape([12.065009 ],[1  1]) reshape([343.000000 ],[1  1]) reshape('fear',[1  4]) },[1  12]) reshape({reshape('IEV',[1  3]) reshape([1.000000 ],[1  1]) reshape([4.000000 ],[1  1]) reshape([1.000000 ],[1  1]) reshape([0.000000 ],[1  1]) reshape([10.123119 ],[1  1]) reshape([20.000000 ],[1  1]) reshape([20.000000 ],[1  1]) reshape([0.894189 ],[1  1]) reshape([17.907985 ],[1  1]) reshape([443.000000 ],[1  1]) reshape('fear',[1  4]) },[1  12]) reshape({reshape('IEV',[1  3]) reshape([1.000000 ],[1  1]) reshape([5.000000 ],[1  1]) reshape([1.000000 ],[1  1]) reshape([0.000000 ],[1  1]) reshape([13.123091 ],[1  1]) reshape([19.000000 ],[1  1]) reshape([19.000000 ],[1  1]) reshape([0.892791 ],[1  1]) reshape([17.284815 ],[1  1]) reshape([493.000000 ],[1  1]) reshape('fear',[1  4]) },[1  12]) },[5  1]);


           % remove the first 3
           [order, score] = resetOrder(order,1,3);
           % score (90) looses first 3 (tot:51)
           testCase.verifyEqual(score,90-51);
           % order should only have total(5) - triallength(3) trilas
           orderlength = length( find( cellfun(@(x) length(x), order) == 12) );

           testCase.verifyEqual(orderlength,5-3);
        end



    end
    
end
