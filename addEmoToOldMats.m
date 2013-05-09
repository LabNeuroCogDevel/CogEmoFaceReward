%%%
%  CogEmoFaceReward.m was not originanlly written to record emotion
%   .. this appends emotion to the cell matrix using txt files
% 
%  * add new column: emotion
%
%
% USAGE EXAMPLE:
%
%  m=dir('subjects/*tc.mat'); 
%  for i=1:length(m); addEmoToOldMats(['subjects/' m(i).name]); end
%  ls subjects/withEmo/
%
%%%
function addEmoToOldMats(filename)
 
   % check inputs
   if ~exist(filename,'file'); fprintf('file: %s DNE\n',filename);return; end

   load(filename);
   % check load is as expected
   if ~exist('order','var');fprinf('order DNE, something went wrong with loading\n');return; end

   % check if already has faces
   if ( any(strcmp(order{1}(end) ,{'happy','fear','scram'}))>0 ); fprintf('%s order already has emotion, exiting!\n',filename);return; end
   
   % get file name
   [name.path,name.name,name.ext]=fileparts(filename); 

   % get emotion
   txtoutputinfn = [name.path '/' name.name '.txt'];
   if(~ exist(txtoutputinfn,'file')); fprintf('oh no txt file DNE\n'); return; end
   f=fopen(txtoutputinfn);
   
   while(~feof(f));
      txtoutputin=textscan(f,'%s\t%d\t%d\t%d\t%d\t%f\t%f\t%d\t%f\t%f\t%f\t%s\t%s', 'CommentStyle','#');
   end
   fclose(f);

   % go through each line and assign a number
   emo=txtoutputin{12};
   if(size(emo,1)<10 ); fprintf('[%s] too few emotions (%d)!\n',txtoutputinfn,size(emo,1)); return; end

   mv = name;
   name.path=[name.path '/withEmo/'];
   mv.path=[mv.path '/withoutEmo/'];

   % make sure the save path
   %       and the move path
   %  exist
   for dir={name.path,mv.path}
    dir=dir{1};
    if ~exist(dir,'dir'); mkdir(dir); end
   end
   newname = struct2array(name);
   mvname  = struct2array(mv);

   % warn about overwriting 
   if exist(mvname,'file'); fprintf('EXITING: [%s] already have %s\n',filename,mvname);return; end
   
   % add emo to the order cell
   % * skip first emotion (blank dud from textscan?) 
   % * order is likely to be much larget than actual data
   a=order; 
   fprintf('order reports %d rows, we have %d emotions\n',size(order,1),size(emo,1))
   for i=1:size(order,1)
       if(i+1>size(emo,1)); break;end
       a{i}(end+1)=  emo(i+1) ;
   end
   order          = a;
   
   %old mats inadvertently stored reward function in a cell
   for i=1:size(order,1); order{i}{1} = order{i}{1}{1}; end
   
   % save new version
   % - save new to withEmo
   % - mv old to withoutEmo
   % - mv new to subjects/ (where withoutemo was)
   save(newname, 'order','score','subject','trialnum')
   movefile(filename, mvname)
   movefile(newname,filename)
   
end
