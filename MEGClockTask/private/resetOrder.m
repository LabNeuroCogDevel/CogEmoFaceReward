% reset order    
%  for the run about to be completed, clear out any prior responses and run totals
%  reset total score
function [order, score] = resetOrder(order,run_num,trialsPerBlock)
  
  %runTotals(subject.run_num) = 0; %reset total points in this run
  for l = ((run_num-1)*trialsPerBlock+1):(run_num*trialsPerBlock)
      order{l} = [];
  end

  % score is the 8th entry in each cell (inside the contianing cell)
  % all trials that have been completed (and stay completed) should have 12 entries
  existingtrials=find(cellfun(@(x) length(x),order)==12);
  % score is the sum of all the score increases (column 8)
  score=sum( cellfun(@(x) x{8},order(existingtrials))  );
end
