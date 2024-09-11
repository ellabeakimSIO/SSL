%%%created by MAZ on 4/17/2020 to plot locations of HARP sites

%% LOAD YOUR SITES
%%%%%you can use data stored in Pac_latLongs if your site is in there.
%%%%%Otherwise, you'll have to enter the latLongs of your site on your
%%%%%own...

%if using the preset ones: load Pac_latLongs.mat

%training/testing sites
SDT_BF_01=[32.51718, -117.36448];
SDT_DP_01=[32.51494, -117.27172];
SDT_HP_01=[32.45597, -117.39302];
SDT_WQ_01=[32.46301, -117.47905];
SOCAL_P_35=[32.53598,-117.22714];
SOCAL_A_15=[33.15027,-118.14991];
SOCAL_T_03=[32.53199,-117.33496];

%novel sites ADD THESE AND RUN
SDT_PR_01=[32.54885,-117.29772];
SDT_SL_01=[32.47916,-117.34489];
SDT_SW_01=[32.42502,-117.45800];
SDT_SZ_01=[32.49691,-117.30947];
SDT_GR_01=[32.49220,-117.41800];
SOCAL_5_A=[33.15140,-118.15130];
LJ_P_40=[32.53053,-117.23954];
LJ_P_21=[32.53493,-117.24016];
SOCAL_G_18=[32.55605,-118.37254];
SOCAL_T_01=[32.53914,-117.36586];
SOCAL_T_02=[32.53212,-117.33362];
MB02_02=[36.6495,-121.9084];

%% get means

%change this to w/e sites you care about to calculate mean lat/lon
%locations- see get_latLon below 

%morgans stuff
%Kona = get_latLon(HAWAIIK_latLongs,'Kona');
%Kauai = get_latLon(KAU_latLongs,'Kauai');
%PHR = get_latLon(PHR_latLongs,'PHR');

BF=get_latLon(SDT_BF_01,'BF');
DP=get_latLon(SDT_DP_01,'DP');
HP=get_latLon(SDT_HP_01,'HP');
WQ=get_latLon(SDT_WQ_01,'WQ');
P=get_latLon(SOCAL_P_35,'P');
A=get_latLon(SOCAL_A_15,'A');
T=get_latLon(SOCAL_T_03,'T');
PR=get_latLon(SDT_PR_01,'PR');
SL=get_latLon(SDT_SL_01,'SL');
SW=get_latLon(SDT_SW_01,'SW');
SZ=get_latLon(SDT_SZ_01,'SZ');
GR=get_latLon(SDT_GR_01,'GR');
G=get_latLon(SOCAL_G_18,'G');
MB=get_latLon(MB02_02,'MB');


%% conversions to table
%whatever your sites were above, concatenate them and make them into a
%table
%fullSites = [Kona;Kauai;PHR];
fullSites=[BF;DP;HP;WQ;P;A;T;PR;SL;SW;SZ;GR;G;MB] %without MB
fullSiteMat = cell2mat(fullSites(:,2:3));

fullSiteTab = array2table(fullSiteMat);
fullSiteTab.Properties.VariableNames = {'Latitude', 'Longitude'};
%you'll need to edit these labels based on your site names 
fullSiteTab.Labels = {'BF';'DP';'HP';'WQ';'P';'A';'T';'PR';'SL';'SW';'SZ';'GR';'G';'MB'};

%% plotting
lats = fullSiteTab.Latitude;
longs = fullSiteTab.Longitude;
labs = fullSiteTab.Labels;
%below is the color of your markers, edit if needed
C = [1 0 0];
%size of markers 
markSize = 80;


%within plotting: changing the text line will change alignment, etc of your
%site labels 
figure
gm = geoscatter(fullSiteTab.Latitude,fullSiteTab.Longitude,markSize,'.','MarkerEdgeColor',C);
for iSite = 1:3
    text(lats(iSite),longs(iSite)-1,labs(iSite),'HorizontalAlignment','right','FontSize',12);
end

%use geolimits to edit the size of your plot as desired
%geolimits([32.3 36.7], [-122 -117])
geolimits([32.3 33.2], [-119 -117])

%use geobasemap to change the basemap underneath your sites- you can google
%options for this 
geobasemap landcover


%save your map to wherever you want 
print('H:\HI_sitemap','-dpng')



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculate average location from given latLongs. inputs are your latitudes
% and longitudes, and then the string you want saved as your site for
% plotting use
function meanLoc = get_latLon(latLons,siteName)

latLonFull = meanm(latLons(:,1),latLons(:,2));
siteNameCell = {siteName};
meanLoc = [siteNameCell, num2cell(latLonFull)];
end


