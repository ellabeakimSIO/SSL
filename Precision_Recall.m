%Precision_Recall of manual picks

%% first step-- drop detedit ID file of PR_01 into command window so that you have zid
%sort zid second column so all 1s at top and 2s at bottom-- index for just
%the ones
labels_PR_01=zID(1:2564,2); %check to make sure worked
times_chorus=zID(1:2564,1);
times_noise=zID(2564:7397,1);

%convert to date
chorus_dates=datestr(times_chorus)
noise_dates=datestr(times_noise)

%keep it all together unsorted
PR_01=zID
dates=PR_01(:,1)
datenew=datestr(PR_01(:,1))
labels=PR_01(:,2)
%PR_01_labels=[datenew labels]

%save dates as csv
csvwrite('PR_01_dates.csv',datenew)
csvwrite('PR_01.csv',PR_01)
