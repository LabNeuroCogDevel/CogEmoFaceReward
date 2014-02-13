%% helper function to get subject info and reload after failed run
function subject=getSubjInfo(taskname,subject,totalBlocks,trialsPerBlock,opts,blk)
  % opts need for genTimingOrder: opts.emofuncs,opts.numfaces,opts.trialsPerBlock,opts.stimtimes
  % which puts experiment into the subject structure
  
  % initialize the order of events
  % this will be filled with "experiment" and behavioral actions/results (e.g. RT, ev, score)
  subject.order=cell(trialsPerBlock*totalBlocks,1);
  
  
  %whether to prompt user for run to execute
  askRun=false;
  
  %determine subject number
  %if .mat file exists for this subject, then likely a reload and continue
  %subject.subj_id = NaN;
  while ~exist('subject','var') || ~ismember('subj_id', fields(subject)) ||  isnan(subject.subj_id)
      idInput = str2double(input('Enter the subject ID number: ','s')); %force to be numeric
      if ~isnan(idInput)
          subject.subj_id = idInput;
      else
          fprintf('\n  Subject id must be a number\n\n');
      end
  end
  
  %rundate
  c=clock();
  subject.run_date=c(1)*10000+c(2)*100+c(3);
  fprintf('rundate: %d\n',subject.run_date);
  
  filename = ['subjects/' taskname '_' num2str(subject.subj_id) '_' num2str(subject.run_date) '_tc'];
  subject.filename=filename; % export where we are saving things
  subject.matfile=[filename '.mat']; % export where we are saving things
  
  
  % is the subject new? should we resume from existing?
  % set t accordingly, maybe load subject structure
  subject.txtfile=[filename '.txt'];
  backup=[subject.txtfile '.' num2str(GetSecs()) '.bak'];
  
  % we did something with this subject before?
  if exist(subject.txtfile,'file') || exist(subject.matfile,'file')
      % check that we have a matching mat file
      % if not, backup txt file and restart
      if ~ exist(subject.matfile,'file')
          fprintf('%s exists, but (%s) does not!\n', subject.txtfile,subject.matfile)
          fprintf('moving %s to %s, start from top\n', subject.txtfile, backup)
          movefile(subject.txtfile, backup);
      else
          localVar = load(subject.matfile);
          
          % sanity check
          if localVar.subject.subj_id ~= subject.subj_id
              error('mat file data conflicts with name!: %d != %d',...
                  localVar.subject.subj_id, subject.subj_id);
          end
          
          %load previous information: place below above check to preserve user input
          subject=localVar.subject;
          % subject has order, experiment, i, score
          
          
      end
  end
  

  %% fill out the subject struct if any part of it is still empty
  
  % generate experiment (block (emotion+reward) and face number order
  if ~ismember('experiment',fields(subject)), 
    subject.experiment=genTimingOrder(opts.emofuncs,opts.numfaces,opts.trialsPerBlock,opts.stimtimes);
  end

  %if new participant, assume run1 start and totally empt block Trials
  if ~ismember('run_num', fields(subject))
    subject.run_num = 1;
  end 
  if ~ismember('blockTrial', fields(subject))
    subject.blockTrial=zeros(1,totalBlocks);
  end
  
  
  % subjects age
  if ~ismember('age', fields(subject))
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
  
  % gender is 'm' or 'f'
  if ~ismember('gender', fields(subject))
      subject.gender=[];
      while isempty(subject.gender)
          subject.gender = input(['Enter subject''s gender (m or f): '], 's');
          if ~(strcmpi(subject.gender, 'm') || strcmpi(subject.gender, 'f'))
              subject.gender=[];
          end
      end
  end

  % make sure sex is what we want
  if ismember(lower(subject.gender),{'male';'dude';'guy';'m';'1'} )
      subject.gender = 'male';
  else
      subject.gender = 'female';
  end
  
  % print out determined sex
  fprintf('Subject is %s\n', subject.gender);
  


  %% deal with run_number
  subject.run_num = chooseRun(blk,subject.blockTrial,trialsPerBlock);
  if(subject.run_num==0)
    error('no good block, not running')
  end

  % reset block list and score -- remove trials on the block we are going to try
  [subject.order, subject.score] = resetOrder(subject.order,subject.run_num,trialsPerBlock)
  

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
      
  %elseif strcmpi(taskname, 'BehavEmoClock')
  
  %all other tasks
  else
     %use randomized colors for blocks
     if ~ismember('blockColors', fields(subject)) || length(subject.blockColors) ~= totalBlocks
      blockColors = round(240*hsv(totalBlocks)); % a different color for each block of trials
      blockColors = blockColors(randperm(totalBlocks),:);
      subject.blockColors=blockColors;
     end
  end
  
  
  
 subject  
end
