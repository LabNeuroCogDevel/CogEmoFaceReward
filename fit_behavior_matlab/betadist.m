function [y] = betadist(alph,b)

i=0;
for x=.01:.01:1
    i=i+1;
    y(i) = x.^(alph-1).*(1-x).^(b-1)./beta(alph,b); % generate probability distribution of prediction errors for this choice
end