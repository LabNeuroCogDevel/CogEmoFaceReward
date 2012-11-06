function Reward = ComputeReward(Type, Response); 
Reward = 0; 

switch (Type),  
    case 12, 
        if (Response == 1), 
            if (rand < 0.8) 
            Reward = 1; 
            else Reward = 0; 
            end
        else
            if (rand < 0.2) 
            Reward = 1; 
            else Reward = 0; 
            end
        end
    case 34, 
        if (Response == 1), 
            if (rand < 0.7) 
            Reward = 1; 
            else Reward = 0; 
            end
        else
           if (rand < 0.3) 
            Reward = 1; 
            else Reward = 0; 
           end
        end
    case 56,        
        if (Response == 1), 
            if (rand < 0.6) 
            Reward = 1; 
            else Reward = 0; 
            end
        else
            if (rand < 0.4) 
            Reward = 1; 
            else Reward = 0; 
            end
        end
end
