%Ella Bathymetry Plot, based on original code from Simone, doctored by
%Shelby

clear all
close all

%mapdir
mapdir = 'G:\SDT\Figures for Paper\sitemap\monterey bay';
cd(mapdir)

% read in ELLAs SITES:
MB02_02=[-121.9084 36.6495];



% setup for colormap
mindepth = -4000; %depth in meters
maxheight = 3000; %height in meters
step = 10; %step size in meters
depthstr = [num2str(mindepth),'/',num2str(maxheight),'/',num2str(step)];

%read in gridded data
G = gmt ('read', '-Tg gebco_2021_n37.3_s36.3_w-122.6_e-121.7.nc'); %file needs to be in current directory
%i read the newest gebco one in but can play around with this just make
%sure to change base map lat and longs later
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
map = gmt('psbasemap', '-B30m30m/30ma30m -JM -R-122.6/-121.7/36.3/37.3 -K -X1 -Y1'); %changed these values to make bounds correct
map = gmt('grdimage', '-R -JM -V -X0 -Y0 -O -C -K',G,C);
map = gmt('grdcontour', '-R -C500 -L-4000/0 -Q50 -JM -V -X0 -Y0 -O -K',G);
%map = gmt('psscale', '-D9i/1i/5c/0.5c -B1000/:m: -C -O -X -Y -V',C);


%% plot circles and lines


%novel sites: colored as red: G255/0/0      
        
map = gmt('psxy',['-R -JM -N -Sc0.3',...
            ' -O -L -W0.5p -G255/0/0 -A -X0 -Y0 -K'],MB02_02);

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

