%
% need to catch instances that insure timing
% but fall back to the real screen flip to do work
% 
% example, catch
%   Screen('Flip', w, reftime + waittime - slack, clearmode); %and display again after desired wait
function varargout = Screen(varargin)
 
 % find where the Screen mex file directory is
 % go there (to put the mex at top of scope)
 % return the current directory
 cwd=cd(fileparts(subsref(which('Screen','-all'),struct('type','{}','subs',{{2}}))));


 %fprintf('want %d outs, screen call: ',nargout); disp(varargin)
 varargout=cell(1,nargout);

 % if we are trying to flip, only take the first two inputs
 % DO NOT CARE about timing -- we want to speed things up
 if(strcmp(varargin{1},'Flip') )
  n=2;
 else
  n=nargin;
 end

 if(nargout)
   [ varargout{:} ] = Screen(varargin{1:n}) ;
 else
   Screen(varargin{1:n}) ;
   varargout={};
 end
 
 % return directory to where we should be
 cd(cwd);
 
 
end
