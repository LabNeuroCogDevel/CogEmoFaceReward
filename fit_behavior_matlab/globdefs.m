%tdef defines the column structure for the trial data used during fitting
global tdef;
global trialsPerBlock;
global trialLength;
global rewFuncNames;
global emoNames;

tdef.subj = 1;     %subject id
tdef.cb = 2;       %counterbalance order
tdef.sess = 3;     %session number
tdef.block = 4;    %block number
tdef.trial = 5;    %trial number
tdef.rt = 6;       %reaction time
tdef.rewFunc = 7;  %contingency (CEV, CEVR, IEV, DEV)
tdef.score = 8;    %points obtained
tdef.emo = 9;      %emotion of face
tdef.comt = 10;    %comt genotype (not used)
tdef.d2 = 11;      %drd2 genotype (not used)
tdef.d32 = 12;     %darpp32 genotype (not used)

trialsPerBlock = 42;
trialLength = 4000;

rewFuncNames = { 'CEV' 'CEVR' 'DEV' 'IEV' };
emoNames = { 'happy' 'fear' 'scram' };