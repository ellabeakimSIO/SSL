%Ella Bathymetry Plot, based on original code from Simone, doctored by
%Shelby

clear all
close all

%mapdir
mapdir = 'G:\SDT\Figures for Paper\sitemap';
cd(mapdir)

% read in ELLAs SITES:
%training/testing sites
SDT_BF_01=[-117.60744 32.86201];
SDT_DP_01=[-117.45340 32.85779];
SDT_HP_01=[-117.65492 32.76066];
SDT_WQ_01=[-117.79841 32.77195];
%SOCAL_P_35=[-117.22714 32.53598];
%SOCAL_A_15=[-118.14991 33.15027]; %incorrect because in degree minute
%second--> converted to decimal degrees
SOCAL_A_15=[-118.50861111 33.2575]; %correct A converted to decimal degrees
SOCAL_T_03=[-117.55855 32.88691];
%novel sites 
SDT_PR_01=[-117.49675 32.91445];
SDT_SL_01=[-117.57522 32.79884];
SDT_SW_01=[-117.76342 32.70853];
SDT_SZ_01=[-117.51585 32.82809];
SDT_GR_01=[-117.69670 32.82052];
%SOCAL_5_A=[-118.15130 33.15140];
LJ_P_40=[-117.39923 32.88421];
%LJ_P_21=[-117.24016 32.53493];
%SOCAL_G_18=[-118.37254 32.55605]; %incorrect need to convert
SOCAL_G_18=[-118.68722222 33.08472222]; %converted
%SOCAL_T_01=[-117.36586 32.53914];
%SOCAL_T_02=[-117.33362 32.53212];
%MB02_02=[-121.9084 36.6495];

% setup for colormap
mindepth = -4000; %depth in meters
maxheight = 3000; %height in meters
step = 10; %step size in meters
depthstr = [num2str(mindepth),'/',num2str(maxheight),'/',num2str(step)];

%read in gridded data
G = gmt ('read', '-Tg gebco_2021_n33.6_s32.3_w-118.8_e-117.0.nc'); %file needs to be in current directory
% Evaluate an asymmetrical color palette with hinge at sealevel 
C = gmt('makecpt', ['-Cgeo -T',depthstr]);

%change colormap such that land is all grey
gr = [0.5 0.5 0.5];
idx = abs(mindepth)/step;
C.colormap(idx+1:end,:) = repmat(gr,length(C.colormap(idx+1:end,1)),1);
C.cpt(idx+1:end,:) = repmat(gr,length(C.colormap(idx+1:end,1)),2);

%% to create a png for matlab display
clear map
% Make the GMT plot 
map = gmt('psbasemap', '-B30m30m/30ma30m -JM -R-118.8/-117/32.3/33.6 -K -X1 -Y1'); %changed these values to make bounds correct
map = gmt('grdimage', '-R -JM -V -X0 -Y0 -O -C -K',G,C);
map = gmt('grdcontour', '-R -C500 -L-4000/0 -Q50 -JM -V -X0 -Y0 -O -K',G);
%map = gmt('psscale', '-D9i/1i/5c/0.5c -B1000/:m: -C -O -X -Y -V',C);


%% plot circles and lines

%training neural net colors: colored as yellow G255/255/0
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SDT_BF_01);
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SDT_DP_01);
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SDT_HP_01);
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SDT_WQ_01);
%novel sites: colored as red: G255/0/0      
        
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_GR_01);
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_PR_01);
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_SL_01);
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_SW_01);
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_SZ_01);
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SOCAL_G_18);
%combo sites (training and novel): colored as orange G242/164/28      
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G242/164/28 -A -X0 -Y0 -K'],LJ_P_40);  
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G242/164/28 -A -X0 -Y0 -K'],SOCAL_A_15);

%long term site-- site T-- magenta G128/0/128       
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G128/0/128 -A -X0 -Y0 -K'],SOCAL_T_03);  



map = gmt('psscale', '-D9i/1i/5c/0.5c -B1000/:m: -C -O -X -Y -V',C);


% Convert plot to a PNG and display in MATLAB as Figure
I = gmt('psconvert -TG -P -E300 -A', map); %transparent PNG

figure()
h = imshow (I.image);


% save file
file = ['G:\SDT\Figures for Paper\sitemap\map_socalfish_','.png'];
saveas(h,file,'png')




%map = gmt('psbasemap', '-B30m30m/30ma30m -JM -R-119/-116/32/34 -K -X1 -Y1');
%map = gmt('grdimage', '-R -JM -V -X0 -Y0 -O -C -K',G,C);
%map = gmt('grdcontour', '-R -C500 -L-4000/0 -Q50 -JM -V -X0 -Y0 -O -K',G);
%map = gmt('psscale', '-D9i/1i/5c/0.5c -B1000/:m: -C -O -X -Y -V',C);

% Convert plot to a PNG and display in MATLAB as Figure
%I = gmt('psconvert -TG -P -E300 -A', map); %transparent PNG
%figure()
%h = imshow (I.image); 
%xlabel('Longitude');
%ylabel('Latitude');
%set(gcf, 'Position',  [100, 100, 1500, 500])

%save image as EPS for Illustrator
file = 'G:\Shared drives\Shelby Masters Data\maps\map_SOCAL_T_SB_2.eps';
saveas(h,file,'epsc')

