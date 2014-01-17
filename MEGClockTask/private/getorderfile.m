%% OLD -- DO NOT USE
%parse order file
function experiment=getorderfile()
  global opts;
  fprintf('using %s\n', opts.TrialCSV);
  fid=fopen(opts.TrialCSV);
  %fid=fopen('FaceITI_MEG.csv');
  %indexes={1,2,3,4,5,6};
  %[ facenumC, ITIC, ISIC, blockC, emotionC, rewardC ] = indexes{:};
  experiment=textscan(fid,'%d,%d,%d,%d,%q','HeaderLines',1);
   % ugly unpack of " "," "
  for i=1:length(experiment{5})
      experiment{6}{i} = experiment{5}{i}(findstr(experiment{5}{i},',')+2:end);
      experiment{5}{i} = experiment{5}{i}(1:findstr(experiment{5}{i},',')-1    );
  end
  fclose(fid);
end
