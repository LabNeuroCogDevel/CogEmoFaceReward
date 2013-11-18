%% block indicator
function drawRect(t)
global w subject;
% allow block to be specified, mostly for instruction display
if (nargin < 1)
    t=subject.run_num;
end


%fprintf('rect colors: %d %d %d\n', subject.blockColors(rgbcolorIDX,:));
%fprintf('t: %d, rgbidx: %d\n', t, rgbcolorIDX);
Screen('FrameRect', w, subject.blockColors(t,:), [], 50);
end
