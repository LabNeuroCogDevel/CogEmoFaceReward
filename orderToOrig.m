%%%
% restructure presentation mat file to original task structure
% to be used in computation models
%
%  * replace string {D,C,I}EV.. with index (e.g. DEV=2)
%  * order: cell converted back to double matrix 
%  * trialnum known again as t, subject.num_run as subject.cb_num
%  * saves mat files to new subdirectory "orgfmt/"
%  * new column 12 is emotion
%
%  emotion of the face displayed is new, column 12
%  happy=1
%   fear=2
%  scram=3
%
% USAGE EXAMPLE:
%
%  m=dir('subjects/100*tc.mat'); 
%  for i=1:length(m); orderToOrig(['subjects/' m(i).name]); end
%  ls subjects/orgfmt/
%
%%%
function orderToOrig(filename)
 
   %% check inputs
   if ~exist(filename,'file'); fprintf('file: %s DNE\n',filename);return; end

   load(filename)
   %% check load is as expected
   if ~exist('order','var');fprinf('order DNE, something went wrong with loading\n');return; end
   
   %% replace reward function as a string with reward function as a number
   %            1    2      3    4
   rewards= {'CEV','DEV','IEV','CEVR'};
   faces=
   
   for i=1:length(order); 
       if isempty(order{i}); break; end; 
       order{i}{1}=find(strcmp(order{i}{1},rewards));
   end
   
   %% replace emotion/face with number
   % newer format has emotion in the order cell (12 columns), older does
   % not (11 cols)
   if(size(order{1},2) >=12)
     faces={'happy','fear','scram'};
     for i=1:length(order); 
         if isempty(order{i}); break; end; 
         order{i}{12}=find(strcmp(order{i}{12},faces));
     end
   end
   
   % truncate order when the rows are empty
   if i~=length(order); order=order(1:(i-1)); end 
   
   % convert from cell to matrix
   % -there must be an easy way to do this with cell2mat, but i dont see it
   neworder=zeros(length(order),length(order{1}) );
   for i=1:length(order)
       neworder(i,:)=cellfun(@(x) double(x), order{i});
   end
   
   % get file name
   [name.path,name.name,name.ext]=fileparts(filename); 
   
   %% set emotion for old mat format
   % should never hit this code
   if( size(order{1},2) < 12 )
       % get emotion
       txtoutputinfn = [name.path '/' name.name '.txt'];
       if(~ exist(txtoutputinfn,'file')); fprintf('oh no txt file DNE\n'); return; end
       f=fopen(txtoutputinfn);

       % should check this exists
       while(~feof(f))
          txtoutputin=textscan(f,'%s\t%d\t%d\t%d\t%d\t%f\t%f\t%d\t%f\t%f\t%f\t%s\t%s', 'CommentStyle','#');
       end
       fclose(f);

       % go through each line and assign a number
       emo=zeros(size(neworder,1),1);
       faces={'happy','fear','scram'};
       j=0;
       for i=1:length(txtoutputin{12})
          if isempty(txtoutputin{12}{i})
            continue 
          end
          j=j+1;
          emo(j)=find(strcmp(txtoutputin{12}(i),faces));
       end
       % would check j = expected length, but will get an error below
   end
   
   %% save new mat to orgfmt
   
   % make a new name to save the modifications
   name.path=[name.path '/orgfmt/'];
   % make sure the save path exists
   if ~exist(name.path,'dir'); mkdir(name.path); end
   newname=struct2array(name);

   % warn about overwriting 
   if exist(newname,'file'); fprintf('WARNING: overwriting %s\n',newname);end
   
   % set other variable names to what is expected
   order          = [neworder, emo];
   t              = trialnum;
   subject.cb_num = subject.run_num;
   subject        = rmfield(subject,'run_num');
   
   % save new version
   save(newname, 'order','subject','t')
   
end
