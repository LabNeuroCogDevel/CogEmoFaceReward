%% helper function to get subject info and reload after failed run
function [order, runTotals, filename] = getSubjInfo(taskname)

global subject facenumC blockC emotionC rewardC ITIC experiment totalBlocks trialsPerBlock;

if nargin < 1
    taskname='fMRIEmoClock';
end

if nargin < 2
    trialsPerBlock = 50; %assume 50
end

subject=[]; %clear out any cached subject information

%determine subject number
%if .mat file exists for this subject, then likely a reload and continue
subject.subj_id = NaN;
while isnan(subject.subj_id)
    idInput = str2double(input('Enter the subject ID number: ','s')); %force to be numeric
    if ~isnan(idInput)
        subject.subj_id = idInput;
    else
        fprintf('\n  Subject id must be a number\n\n');
    end
end

%determine session number (for repeated behavioral visits)
subject.session = NaN;
while isnan(subject.session)
    idInput = str2double(input('Enter the session number: ','s')); %force to be numeric
    if ~isnan(idInput)
        subject.session = idInput;
    else
        fprintf('\n  Session must be a number\n\n');
    end
end

if strcmpi(taskname, 'BehavEmoClock')
    if subject.session == 1
        csvfile='FaceBehavOrder.csv';
    elseif subject.session == 2
        csvfile='FaceBehavOrder_Followup.csv';
    elseif subject.session == 3
        csvfile='FaceBehavOrder_Followup.csv';
    else
        error(['unable to identify design file for session ' subject.session]);
    end

    fid=fopen(csvfile);
    indexes={1,2,3,4};
    [ facenumC, blockC, emotionC, rewardC ] = indexes{:};
    experiment=textscan(fid,'%d %d %s %s','HeaderLines',1,'Delimiter', ',');
    fclose(fid);
elseif strcmpi(taskname, 'fMRIEmoClock')
    csvfile='FaceFMRIOrder.csv'; %not checking for session at this time
    
    fid=fopen(csvfile);
    indexes={1,2,3,4,5};
    [ facenumC, blockC, emotionC, rewardC, ITIC ] = indexes{:};
    experiment=textscan(fid,'%d %d %s %s','HeaderLines',1,'Delimiter', ',');
    fclose(fid);
else
    %MEG currently uses forked copies of utility scripts, so don't check
    error(['Unable to determine what to do for task' taskname]);
end

fprintf('Reading design from %s\n', csvfile);

% how long (trials) is a block
[~,blockchangeidx] = unique(experiment{blockC});
trialsPerBlock     = unique(diff(blockchangeidx));
if(length(trialsPerBlock) > 1)
    error('Whoa?! Different block lengths? I dont know what''s going on!\n')
end

totalBlocks = length(experiment{blockC})/trialsPerBlock;

% initialize the order of events
order=cell(trialsPerBlock*totalBlocks,1);

%initialize run totals
runTotals = zeros(totalBlocks, 1);

%whether to prompt user for run to execute
askRun=false;


filename = ['subjects/' taskname '_' num2str(subject.subj_id) '_' num2str(subject.session) '_tc'];

% is the subject new? should we resume from existing?
% set t accordingly, maybe load subject structure
txtfile=[filename '.txt'];
backup=[txtfile '.' num2str(GetSecs()) '.bak'];

% we did something with this subject before?
if exist(txtfile,'file')
    % check that we have a matching mat file
    % if not, backup txt file and restart
    if ~ exist([filename '.mat'],'file')
        fprintf('%s exists, but .mat does not!\n', txtfile)
        fprintf('moving %s to %s, start from top\n', txtfile, backup)
        movefile(txtfile, backup);
    else
        localVar = load(filename);
        
        % sanity check
        if localVar.subject.subj_id ~= subject.subj_id
            error('mat file data conflicts with name!: %d != %d',...
                localVar.subject.subj_id, subject.subj_id);
        end
        
        %load previous information: place below above check to preserve user input
        subject=localVar.subject;
        order=localVar.order;
        runTotals=localVar.runTotals;
        
        if localVar.blockTrial < trialsPerBlock
            fprintf('It appears only %d trials were completed in run %d.\n', localVar.blockTrial, subject.run_num);
            redoRun=[];
            while isempty(redoRun)
                redoRun = input(['Do you want to redo run ', num2str(subject.run_num), ' ? (y or n) '],'s');
                if ~(strcmpi(redoRun, 'y') || strcmpi(redoRun, 'n'))
                    redoRun=[];
                end
            end
            
            if strcmpi(redoRun, 'n')
                askRun=true;
            end
        else
            continueRun=[];
            while isempty(continueRun)
                continueRun = input(['Continue with run ', num2str(subject.run_num + 1), ' ? (y or n) '], 's');
                if ~(strcmpi(continueRun, 'y') || strcmpi(continueRun, 'n'))
                    continueRun=[];
                end
            end
            
            if (strcmpi(continueRun, 'y'))
                subject.run_num = subject.run_num + 1;
            else
                askRun=true;
            end
            
        end
        
        if askRun
            chooseRun = input(['Specify the run to be completed (1 - ', num2str(totalBlocks), ') '], 's');
            if str2double(chooseRun) > totalBlocks || str2double(chooseRun) < 1
                error(['Must specify run 1 - ', num2str(totalBlocks)]);
            end
            
            subject.run_num = str2double(chooseRun);
        end
        
        %for the run about to be completed, clear out any prior responses and run totals
        runTotals(subject.run_num) = 0; %reset total points in this run
        for l = ((subject.run_num-1)*trialsPerBlock+1):(subject.run_num*trialsPerBlock)
            order{l} = [];
        end
        
    end
end

if ~ismember('run_num', fields(subject)), subject.run_num = 1; end %if new participant, assume run1 start and don't prompt

%% fill out the subject struct if any part of it is still empty

if ~ismember('age', fields(subject)) || isnan(subject.age)
    subject.age = NaN;
    while isnan(subject.age)
        ageInput = str2double(input('Enter the subject''s age: ','s')); %force to be numeric
        if ~isnan(ageInput)
            subject.age = ageInput;
        else
            fprintf('\n  Subject age must be a number\n\n');
        end
    end
else
    fprintf('using old age: %d\n', subject.age);
end

if ~ismember('gender', fields(subject))
    subject.gender=[];
    while isempty(subject.gender)
        subject.gender = input(['Enter subject''s gender (m or f): '], 's');
        if ~(strcmpi(subject.gender, 'm') || strcmpi(subject.gender, 'f'))
            subject.gender=[];
        end
    end
else
    fprintf('using old gender: %s\n', subject.gender);
end

%% Customizations for fMRI version of task
if strcmpi(taskname, 'fMRIEmoClock')
    % For first run of fMRI task, need to sample the 8 orders from the mat file here.
    % Only sample if we have not populated the ITIs before (i.e., don't resample for re-running run 1)
    if subject.run_num==1 && ~ismember('runITI_indices', fields(subject))
        locV=load('fMRIOptITIs_284s_38pct.mat');
        subject.runITI_indices = randsample(size(locV.itimat,1), totalBlocks);
        subject.runITIs=locV.itimat(subject.runITI_indices, :);
        clear locV;
    end
    
    if subject.run_num==1 && ~ismember('blockColors', fields(subject))
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
    end
    
elseif strcmpi(taskname, 'BehavEmoClock')
    %use randomized colors
    blockColors = round(240*hsv(totalBlocks)); % a different color for each block of trials
    blockColors = blockColors(randperm(totalBlocks),:);
    subject.blockColors=blockColors;
end


%% set sex to a standard
if ismember(lower(subject.gender),{'male';'dude';'guy';'m';'1'} )
    subject.gender = 'male';
else
    subject.gender = 'female';
end

% print out determined sex, give user a chance to correct
fprintf('Subject is %s\n', subject.gender);


end
