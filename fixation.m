%% Display a red cross for ITI (ms) time
function [VBLT_Onset] = fixation(waittime, drawfix, reftime)
global w slack;

if nargin < 2
    drawfix=1;
end
if nargin < 3
    reftime=GetSecs();
end

%fprintf('waiting %.3f\n',waittime);

oldFontSize=Screen(w,'TextSize', 40 );
if drawfix, DrawFormattedText(w,'+','center','center',[ 255 0 0]); end
Screen(w,'TextSize', oldFontSize);

clearmode=0;
drawRect;
VBLT_Onset = Screen('Flip', w, reftime, clearmode); %display the fix ASAP

drawRect;
VBLT_Offset = Screen('Flip', w, reftime + waittime - slack, clearmode); %and display again after desired wait
%this two-flip approach ensures that the fix is on screen for approximately the right length

end