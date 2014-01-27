function [eta_f,eta_s,b_f,b_s,mean_f,mean_s,sigma_f,sigma_s,delta_f,delta_s] = Reconstruct_betaParams(Resp,PE)

global FitTrls;

eta_f(1) = 1; b_f(1) =1; % reconstruct eta and beta for fast and slow resps
eta_s(1) = 1; b_s(1) =1;


mean_s(1) = 0.5; mean_f(1) = 0.5;
sigma_s(1) = sqrt(eta_s(1)*b_s(1)/(((eta_s(1)+b_s(1))^2)*(eta_s(1)+b_s(1)+1)));
sigma_f(1) = sqrt(eta_f(1)*b_f(1)/(((eta_f(1)+b_f(1))^2)*(eta_f(1)+b_f(1)+1)));

for(i=2:FitTrls-1)
    eta_s(i) = eta_s(i-1); eta_f(i) = eta_f(i-1); b_s(i) = b_s(i-1); b_f(i) = b_f(i-1);
    delta_f(i) =0;delta_s(i) =0;
    if PE(i) > 0 % positive PE
        if Resp(i) >0
            eta_s(i) =eta_s(i) +1;
            delta_s(i) = PE(i);
        else
            eta_f(i) =eta_f(i) +1;
            delta_f(i) = PE(i);
        end
    else
        if Resp(i) >0
            b_s(i) =b_s(i) +1;
            delta_s(i) = PE(i);
        else
            b_f(i) =b_f(i) +1;
            delta_f(i) = PE(i);
        end
    end
    mean_s(i) = eta_s(i) / (eta_s(i) + b_s(i));
    mean_f(i) = eta_f(i) / (eta_f(i) + b_f(i));
    sigma_s(i) = sqrt(eta_s(i)*b_s(i)/(((eta_s(i)+b_s(i))^2)*(eta_s(i)+b_s(i)+1)));
    sigma_f(i) = sqrt(eta_f(i)*b_f(i)/(((eta_f(i)+b_f(i))^2)*(eta_f(i)+b_f(i)+1)));
    
end