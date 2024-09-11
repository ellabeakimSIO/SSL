clear all
%DIEL PLOTTING CODE WITH LOGGED PRESENCE
%THIS PLOT CODE MAKES DIEL SQUIGGLE PLOTS WITH NIGHTTIME AND LUNAR DATA
%originally from Annebelle
%edited by Ella_added workarounds to get nighttime data when
%tethys is down. lunar part of code isn't fully functional yet but almost
%there.


%% Annebelle's part of code to upload data-- didn't use
%upload the data
%i just dragged in the ID1 files and sorted through the zID file and
%disregarded annebelle's chunk of code here

%This piece of code will let you pick a logger file. Make sure the times in
%the excel file are in number format with 8 decimals (change in Excel before you open it
%here).
%[infile,inpath]=uigetfile('.xls','Select a xls file with manual picks');
%if isequal(infile,0)
   % disp('Cancelled button pushed');
    %return
%end

%read in start and end to pnum
%[pnum, txt] = xlsread(strcat(inpath,'\',infile),'Detections');

%% ELLA didn't use this
%if detections are in date format use this to convert to matlab times
%date=txt(2:end,5:6)
%date=dbISO8601toSerialDate(date)
%pmatnum=date

% aside-- drop matrix here called T010203 that has all the chorus times as matlab times. save as matrix
%pnum=T010203chorus

%% Ella UPLOAD AND SORT DETECTIONS  
% sorted zID so from low to high (column 2)and grab the dates (column 1)for those values (when its chorus=1)
%grab times and save as pmatnum
%note zID is particular file that essentially are just detection times (but
%only start times so i had to add an end time)

%site T start times
%T_01
timesT_01=zID(1:360,1); 
pnum1=timesT_01
%T_02
timesT_02=zID(1:38,1);
pnum2=timesT_02
%T_03
timesT_03=zID(1:2408,1);
pnum3=timesT_03
%combine all 3
pnum=combine(pnum1,pnum2,pnum3)

%% get end time through adding 20 minutes

%add column that has 20 minutes added to the first column
%%new way to figure out what malab value is for 20 minutes
blah1 = dbISO8601toSerialDate('2016-09-27T20:00:00Z');
blah2 = dbISO8601toSerialDate('2016-09-27T20:20:00Z');
diffnew=blah2-blah1 %diff new is 20 minutes in matlab value

%create new column that is times+20 (diff=20 min)
pnum20=pnum+diffnew

%make a new matrix that combines original times with times+20
pnumnew=[pnum pnum20]

%% Annebelle code-- didn't use because tethys down
%convert to Matlab times-- already in matlab time so didn't use this
%pmatnum = ones(size(pnum)).*datenum('30-Dec-1899')+pnum; %not sure if i should do this or just make previous line pmatnum
%

%call the server for all the info of the recordings (tethys)
q=dbInit('Server','breach.ucsd.edu','Port',9779);
dbSpeciesFmt('Input','Abbrev','SIO.SWAL.v1');

%specify the project and site
%[deployments, ~] = dbDeploymentInfo(q, 'Project',project,'Site','EI');

% you can pull lat and long and start and end times of disks
%For manual input, lat and long in decimals.
% indx = 1; % which deployment you want to look at
% long = deployments(indx).DeploymentDetails.Longitude;
% lat = deployments(indx).DeploymentDetails.Latitude;

%% location and effort times

%Location of site T (Input lat/long information here) 
long = 360-117.55855;
lat = 32.88691;

%Input start and end times of the deployments (don't need multiple start
%and ends can just do start 1 and end 1 if only 1 start and end)
startEffort1 = dbISO8601toSerialDate('2016-09-27T20:00:00Z');
startEffort2 = dbISO8601toSerialDate('2017-03-02T00:00:00Z');
endEffort1 = dbISO8601toSerialDate('2016-12-13T023:59:59Z');
endEffort2 = dbISO8601toSerialDate('2018-01-17T23:59:59Z');

%startEffort1 = dbISO8601toSerialDate('2016-09-27T20:00:00Z');
%endEffort1 = dbISO8601toSerialDate('2018-01-17T23:59:59Z');

%% Off Effort Shading
%If you are plotting more than one deployment at once, input start and end
%of no effort.
noeffort = dbISO8601toSerialDate({'2016-12-14T00:00:01Z','2017-07-06T00:00:01Z'});
noeffortend = dbISO8601toSerialDate({'2017-03-02T00:00:00Z','2017-07-07T00:00:00Z'});
noefforttimes = horzcat(noeffort',noeffortend');
%% SKIP-- this is if you have smaller sampling range than effort 
 
%If you want to focus on a smaller sampling range than the effort.
fixStart1 = startEffort1;
fixStart2 = startEffort2;
fixEnd1 = endEffort1;
fixEnd2 = endEffort2;
%%  ELLA: uploading my own sunrise and sunset times
%make sure have sunrise.m file in current folder (download this from
%online)

%sunrise(lat,long,0,0,'2016-09-27') %practicing how sunrise script works

%save the days of the year that looking for
days2016=datenum(2016,1,271:365) %start sept 27th 2016 (leap year)
days2017=datenum(2017,1,1:365) %all 2017
days2018=datenum(2018,1,1:17)% up to jan 17 2018
%save sunrise and sunset times for each year
%2016
[srise,sset]=sunrise(lat,long,0,0,days2016)
sunrise2016=srise'
sunset2016=sset'
%2017
[srise,sset]=sunrise(lat,long,0,0,days2017)
sunrise2017=srise'
sunset2017=sset'
%2018
[srise,sset]=sunrise(lat,long,0,0,days2018)
sunrise2018=srise'
sunset2018=sset'
%combine 3 years 
sunriseall=combine(sunrise2016,sunrise2017,sunrise2018)
sunsetall=combine(sunset2016,sunset2017,sunset2018)
%combine sunrise and sunset into 1 matrix
sunrisesunset=[sunriseall sunsetall] %day   
sunsetsurnrise=[sunsetall sunriseall] %night


%% lunar illumination data from suncalc in r (getmoonillumination and getmoontimes)

%metadata:
%got illumination and lunar times and downloaded as csv files. uploading
%them here (dropping in data)
%moon illu saved that one variable which is the fraction of lunar
%"fraction" : illuminated fraction of the moon; varies from 0.0 (new moon) to 1.0 (full moon)
%illumination as a matrix (just one column)
%moontimes saves a table

%uploading data (dropped it into command window and organized below)
moonillu=moonillu
rise=moontimes1(2:end,"rise")
rise=table2array(rise)
set=moontimes1(2:end,"set")
set=table2array(set)
% save moonrise and set to matlab numbers
moonrisenum=datenum(rise)
moonsetnum=datenum(set)
%combine moon rise and moon set into 1 matrix
moon=[moonsetnum moonrisenum]

%seeing if issues are coming up with the NANs by saving smaller matrix in
%which its a smaller section without the NANs
moonsmall=moon(1:30,1:2) 
moonsmall([16],:) = [];
%still having weird issues
%i think its because i need to shift one of the arrays down one so that its
%sunset of day 1 and then sunrise of day 2 because its giving an error
%message that things are out of order. this would also likely make it so
%the nighttime shading on the plots is nighttime shading rather than
%daytime shading

%didn't quite get this to work because not using lunar illumination for
%this paper-- but instead of doing moon times-- should use lunar
%iilumination so just color night-time by illumination intensity

%if want to use in the future use the VisLunarIllumination function rather
%than visPresence-- which takes the input as one date vector and one
%illumination vector (which in my case would be moon fraction) 

%%%%%%%%%%%%%%% plot %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% if tethys is working just use this for to get night and lunar data
night = dbDiel(q,lat, long,  startEffort1, endEffort); %Make sure that you check which dates are in startEffort and endEffort, adjust to startEffort2 or endEffort1 if necessary.
illu = dbGetLunarIllumination(q,lat,long,startEffort1,endEffort1,30); %Lunar cycle, usually plot in orange.

nightTime = dbSerialDateToISO8601(night); %tethys version
lunarTime = dbSerialDateToISO8601(illu(:,1));

%% PLOTTING
% jerry rigged version without tethys for plotting
figure('Position',[0,0,500,800])
%lunarH = visLunarIllumination(illu); %include this if tethys is working
night=sunrisesunset %added this in for jerryrigged version
nightH = visPresence(night, 'Color', 'yellow', ...
        'LineStyle', 'none', 'Transparency', .2);
%moonH = visPresence(moon, 'Color', 'blue', ...
        %'LineStyle', 'none', 'Transparency', .3);
%%
%Only run this if you have information on times without effort.
noeffortH = visPresence(sort(noefforttimes), ...
        'Color',[0 0.4 0.8],...
        'Resolution_m', 8,'DateTickInterval',92,...
        'LineStyle', 'none','Transparency',.1,...
        'DateRange', [startEffort1, endEffort2]); 
%%    plot chorus/detections
chorus = visPresence(sort(pnumnew),...
    'Color',[0.4940 0.1840 0.5560],...
    'Label','slow_click',...
    'DateTickInterval',30,...
    'DateRange', [startEffort1, endEffort2]); % 'DateRange', [fixStart, fixEnd]


set(gca, 'YDir', 'reverse');  %make axis go in the right direction
title('Chorus Presence')
legendH = legend([noeffortH(1),chorus(1)], {'No Effort','Fish Chorus'},'Location','northeast'); 

%%
