% this file compute log likelihood estimate (LLE).
% input is matrix. (in my case, it will be Prob_A and Prob_B)
function f=SE(x, dim)

f=sum(x^2,dim);
