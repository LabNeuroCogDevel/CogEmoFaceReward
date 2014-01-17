function getopts(varargin)
  varargin=varargin{:};
  global opts 
  %% MEG BY DEFAULT
  opts.DEBUG=0;
  opts.test=0;
  useMEG();

  %% Default scren size (MEG center)
  opts.screen=[1280 1024];
  
  %% PARSE REST
  i=1;
  while(i<=length(varargin))
      switch varargin{i}
          case {'TEST'}
              opts.test=1;
              % set order
          case {'DEBUG'}
              opts.DEBUG=1;
              opts.screen=[800 600];
          case {'screen'}
              i=i+1;
              if isa(varargin{i},'char')
                  
                % CogEmoFaceReward('screen','mac laptop')
                switch varargin{i}
                    case {'mac laptop'}
                        opts.screen=[1680 1050]; %mac laptop
                    case {'VGA'}
                        opts.screen=[640 480]; %basic VGA
                    case {'eyelab'}
                        opts.screen=[1440 900]; %new eyelab room
                    otherwise
                        fprintf('dont know what %s is\n',varargin{i});
                end
                
              %CogEmoFaceReward('screen',[800 600])
              else
                opts.screen=varargin{i};    
              end    
              
              
          case {'MEG'}
              useMEG()

          case {'fMRI'}
              opts.trigger=0;
              %opts.TrialCSV='FaceITI.csv';
              opts.screen=[1680 1050];
              opts.sound=1;

          case {'NODAQ'}
              opts.USEDAQ=0;

          otherwise
              fprintf('unknown option #%d\n',i)
      end
      
   i=i+1;    
  
  end
  
  function useMEG()
      opts.MEG=1;
      opts.trigger=1;
      opts.USEDAQ=1;
      opts.sound=0;
      opts.totalSessions=1;
      %% before we were loding order from a list -- now we generate it
      %opts.TrailCSV=dir('MEGorder/unused/*csv');
      %opts.TrialCSV=['MEGorder/unused/' opts.TrailCSV(1).name];
      
     %% define paradigm experiment
     % face emotions - reward function pairs, these pairs define the number of blocks
     emofuncs=cell(8,1); 
     bnum=0; for f={'happy','fear','scram'}; for r={'DEV','IEV'};bnum=bnum+1; 
       emofuncs{bnum}={f{1},r{1} }; 
     end; end; 
     emofuncs{bnum+1}={'scram','CEV'}; emofuncs{bnum+2}={'scram','CEVR'};
     % timing distribution
     stimtimes.ITI.min=300; stimtimes.ITI.max=300; stimtimes.ITI.mean=300;
     stimtimes.ISI.min=1000;stimtimes.ISI.max=1500;stimtimes.ISI.mean=1250;

     opts.emofuncs       = emofuncs;
     opts.stimtimes      = stimtimes;
     
     % number of faces per emotion
     opts.numfaces       = 21;

     % number of trials in each block
     opts.trialsPerBlock = 63;

     % fixation in milliseconds
     opts.fixation       = 1500;

  end
  
  disp(opts)
end

