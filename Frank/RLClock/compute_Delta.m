function delta = compute_Delta(Q_Trn, sess_trn)
% 
global TrlType_Colmn Resp_Colmn FeedBck_Colmn; 

trn_AB = sess_trn(find(sess_trn(:,TrlType_Colmn) == 12), :);
% pick out the CD trials
trn_CD = sess_trn(find(sess_trn(:,TrlType_Colmn) == 34), :);
% pick out the EF trials
trn_EF = sess_trn(find(sess_trn(:,TrlType_Colmn) == 56), :);

% construct reward and response vectors
Reward_AB= trn_AB(:,FeedBck_Colmn);
Reward_CD= trn_CD(:,FeedBck_Colmn);
Reward_EF= trn_EF(:,FeedBck_Colmn);
SubjResp_AB = trn_AB(:, Resp_Colmn);
SubjResp_CD = trn_CD(:, Resp_Colmn);
SubjResp_EF = trn_EF(:, Resp_Colmn);

Q_chosen(find(SubjResp_AB == 1),1) = Q_Trn(find(SubjResp_AB == 1),1);
Q_chosen(find(SubjResp_AB == 0),1) = Q_Trn(find(SubjResp_AB == 0),2);

Q_chosen(find(SubjResp_CD == 1),2) = Q_Trn(find(SubjResp_CD == 1),3);
Q_chosen(find(SubjResp_CD == 0),2) = Q_Trn(find(SubjResp_CD == 0),4);

Q_chosen(find(SubjResp_EF == 1),3) = Q_Trn(find(SubjResp_EF == 1),5);
Q_chosen(find(SubjResp_EF == 0),3) = Q_Trn(find(SubjResp_EF == 0),6);

delta = [Reward_AB Reward_CD Reward_EF] - Q_chosen; 