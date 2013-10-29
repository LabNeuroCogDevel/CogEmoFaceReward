function [ waitedtime ] = WaitSecs( waittime )
global speedincrease;
% default to no acceleration if speedincrease is not defined
if(isempty(speedincrease))
 speedincrease=1;
end
%% GO to psychtoolbox directory and run WaitSecs from there
 %cwd=cd('/home/foranw/MATLAB/Psychtoolbox/Psychtoolbox/PsychBasic/');
 % the real waitsecs should be second in the which list
 % fileparts is the same as dirname here
 cwd=cd(fileparts(subsref(which('WaitSecs','-all'),struct('type','{}','subs',{{2}}))));
 waitedtime=WaitSecs(waittime/speedincrease);
 % return directory to where we should be
 cd(cwd);

 % diff between getsecs and waitsecs is ~ .005 secs
 %f=@(x) abs(GetSecs-WaitSecs(x))-x/2;
 %a=arrayfun(f,repmat(2,1,60)); [mean(a) std(a)] % 0.0046    0.0011
end

