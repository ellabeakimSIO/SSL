%Ella Bathymetry Plot, based on original code from Simone, doctored by
%Shelby

clear all
close all

%mapdir
mapdir = 'N:\SDT\Figures for Paper\2_site map';
cd(mapdir)

% read in ELLAs SITES:
MB02_02=[-121.9084 36.6495];
SOCAL_T_03=[-117.55855 32.88691];

% setup for colormap
mindepth = -8000; %depth in meters
maxheight = 3000; %height in meters
step = 10; %step size in meters
depthstr = [num2str(mindepth),'/',num2str(maxheight),'/',num2str(step)];

%read in gridded data
G = gmt ('read', '-Tg gebco_2022_n70.0_s25.0_w-167.0_e-97.0.nc'); %file needs to be in current directory
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
map = gmt('psbasemap', '-B30m30m/30ma30m -JM -R-167.0/-97/25/70 -K -X1 -Y1'); %changed these values to make bounds correct
map = gmt('grdimage', '-R -JM -V -X0 -Y0 -O -C -K',G,C);
map = gmt('grdcontour', '-R -C500 -L-4000/0 -Q50 -JM -V -X0 -Y0 -O -K',G);
%map = gmt('psscale', '-D9i/1i/5c/0.5c -B1000/:m: -C -O -X -Y -V',C);


%% plot circles and lines

%training neural net colors: colored as yellow G255/255/0
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
           % ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SDT_BF_01);
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
           % ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SDT_DP_01);
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
            %' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SDT_HP_01);
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
           % ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SDT_WQ_01);
%novel sites: colored as red: G255/0/0      
        
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
        %    ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_GR_01);
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
           % ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_PR_01);
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
           % ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_SL_01);
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
           % ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_SW_01);
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
            %' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SDT_SZ_01);
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
            %' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],SOCAL_G_18);
%combo sites (training and novel): colored as orange G242/164/28      
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
           % ' -O -L -W0.5p -G242/164/28 -A -X0 -Y0 -K'],LJ_P_40);  
%map = gmt('psxy',['-R -JM -N -Sc0.3',...
           % ' -O -L -W0.5p -G242/164/28 -A -X0 -Y0 -K'],SOCAL_A_15);

%so cal and MB site in yellow stars   
map = gmt('psxy',['-R -JM -N -Sa0.3',...
            ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],SOCAL_T_03);  
map = gmt('psxy',['-R -JM -N -Sa0.3',...
            ' -O -L -W0.5p -G255/255/0 -A -X0 -Y0 -K'],MB02_02);

%%
%scale things
map = gmt('psscale', '-D9i/1i/5c/0.5c -B1000/:m: -C -O -X -Y -V',C);


% Convert plot to a PNG and display in MATLAB as Figure
I = gmt('psconvert -TG -P -E300 -A', map); %transparent PNG

figure()
h = imshow (I.image);




%% STOP HERE
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

