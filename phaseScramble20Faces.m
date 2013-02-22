for facenum=1:20
   %% load face
   neuface  = ['faces/neutral_' num2str(facenum) '.png'];
   scramface  = ['faces/scram_' num2str(facenum) '.png'];
   phaseScrambleImage(neuface, scramface);
end
