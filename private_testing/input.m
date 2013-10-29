function output = input( varargin )
% overlaod input function
    global inputCounter initInput;

    if(isempty(inputCounter))
         fprintf('initializing input autos\n');
         inputCounter=1;
         if(isempty(initInput))
           error('need global initInput as cell array of inputs')
         end
    end    
    
     fprintf('(Q#%d: "%s") ',inputCounter,varargin{1});
     output=initInput{inputCounter};
     fprintf('answered %s\n',output);
     inputCounter=inputCounter+1;
end

