% emofuncs is a 2d cell of emoface and function
% numfaces is the number of images that exist per emotion type
% numtrials are the number of trials to be in each block
% times is a structure with times.(ITI,ISI).(min,max,mean) that will be generated
%   e.g.
%   emofuncs = { {'happy', 'CEV'}, {'fear','CEVR'} };
%   emofuncs=cell(8,1); i=0; for f={'happy','fear','scram'}; for r={'DEV','IEV'};i=i+1; emofuncs{i}={f{1},r{1} }; end; end; emofuncs{i+1}={'scram','CEV'}; emofuncs{i+2}={'scram','CEVR'}
%   numfaces = 21;
%   numtrials= 63; % trials per block
%   times.ITI.min=300; times.ITI.max=300; times.ITI.mean=300;
%   times.ISI.min=1000;times.ISI.max=1500;times.ISI.mean=1250;

% experiment is a cell like
% facenum ITI ISI block emotion reward
function experiment = genTimingOrder(emofuncs,numfaces,numtrials,times)
  numblocks = length(emofuncs);
  totTrial  = numblocks*numtrials;
  %experiment = repmat(cell(numtrials*length(emofuncs)),6,1);
  experiment = { [1:totTrial]',[1:totTrial]',[1:totTrial]',[1:totTrial]',{},{} };

  %% faces -- faces repeat in blocks of number of faces
  % for each block, get some random faces. try to show each face the same amount 
  for blockidx = 1:numblocks
     
     if(numtrials < numfaces) 
       % we have more faces than we need
       numFullFaceRepeats = 1; 
     else
       % we need to reuse faces
       numFullFaceRepeats = floor(numtrials/numfaces);
     end

     faces2d = ones(numfaces,numFullFaceRepeats);
     faceslist= zeros(numtrials,1);
     for fr = 1:numFullFaceRepeats
      a=randsample(1:numfaces,numfaces);
      faces2d(:,fr)=a';
     end

     evenfacenum = numFullFaceRepeats*numfaces;
     faceslist(1:evenfacenum) = reshape(faces2d,1,prod(size(faces2d)));

     %% capture any leftover faces
     facenumdiff = numtrials - evenfacenum;
     if( facenumdiff > 0 )
       faceslist((end-facenumdiff):end) = randsample(1:numfaces,facenumdiff)
     end

     fidx=col2idx('facenum');
     b=(blockidx-1)*numtrials;
     experiment{fidx}((1+b):(b+numtrials)) = faceslist; 
  end


  %% timing
  % for each ITI and ISI
  for II={'ITI','ISI'}
    II=II{1};
    fprintf('generating %s timing...\n',II);
    pop=times.(II).min:50:times.(II).max;
    pop=repmat(pop,1,2); % just incase we only have one value in pop
    for bn=1:numblocks
       %% generate random samples in specified range with desired mean
       x=Inf;
       while( mean(x) ~= times.(II).mean ) 
        x=randsample(pop, numtrials,1);
        
        % make sure we didn't cheat
        if(length(find(x==times.(II).mean))>.3*numtrials && ~all(pop==times.(II).mean))
          %fprintf('  skipping weak randomization\n'); 
          x=Inf;
        end
       end

       %% add to the ITI or ISI column in the rows for that block
       b=(bn-1)*numtrials;
       IIidx=col2idx(II);
       range=(1+b):(b+numtrials);
       experiment{IIidx}(range) = x; 
    end      
  end


  %% emotion(face) and reward (function)
  fprintf('generating non-repeat face emotion + reward function order...\n');
  norepeats=0;
  while(~norepeats)
     % randomize order
     emofuncs = emofuncs( randsample(1:length(emofuncs),length(emofuncs) ) );

     % extract emotion
     emotions = cellfun(@(x) x{1},emofuncs,'UniformOutput',false);
     % extract reward functions
     rewards  = cellfun(@(x) x{2},emofuncs,'UniformOutput',false);

     % make sure we do not have any face emotion or reward function repeating
     norepeats=1;
     for checkrep={rewards,emotions}
        checkrep=checkrep{:};     % remove from cell of cell to just cell
        if(~norepeats),break,end; % skip emotions if rewards failed

        % there has to be a nice vectorized bsx or cell fun for this
        for i=2:length(checkrep)
          if(strmatch(checkrep{i-1},checkrep{i}) )
           norepeats=0;
           break
          end
        end

     end

  end

  % print blocks
  for i=1:length(emotions)
   fprintf('\t%d % 5s % 4s\n',i, emotions{i}, rewards{i})
  end
  fprintf('\n\n\n');

  % add reward and emotion to experiment -- cell is shaped like it would be were it read off a CSV
  experiment{col2idx('reward')}  = reshape(repmat(rewards',numtrials,1),numtrials*numblocks,1);
  experiment{col2idx('emotion')} = reshape(repmat(emotions',numtrials,1),numtrials*numblocks,1);

  
  %% block numbers
  % assign each trial a block (repeated for number of trials)
  experiment{col2idx('block')} = reshape(repmat(1:numblocks,numtrials,1),1,numtrials*numblocks) ;


  

  %% suppot funtion to label columns 
  % translates column name into index
  function idx=col2idx(name)
    colnames={'facenum','ITI','ISI','block','emotion','reward'};
    idx=find(cellfun(@(x) any(strmatch(x,name)),colnames));
  end

end
