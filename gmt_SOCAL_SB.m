%Shelby Bathymetry Plot, based on original code from Simone

clear all
close all

%mapdir
mapdir = 'O:\Shared drives\Shelby Masters Data\maps';
cd(mapdir)

% setup for colormap
mindepth = -4000; %depth in meters
maxheight = 3000; %height in meters
step = 10; %step size in meters
depthstr = [num2str(mindepth),'/',num2str(maxheight),'/',num2str(step)];

%read in gridded data
G = gmt ('read', '-Tg gebco_2021_n34.0_s32.0_w-120.0_e-116.0.nc'); %file needs to be in current directory
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
map = gmt('psbasemap', '-B30m30m/30ma30m -JM -R-119/-116/32/34 -K -X1 -Y1');
map = gmt('grdimage', '-R -JM -V -X0 -Y0 -O -C -K',G,C);
map = gmt('grdcontour', '-R -C500 -L-4000/0 -Q50 -JM -V -X0 -Y0 -O -K',G);
map = gmt('psscale', '-D9i/1i/5c/0.5c -B1000/:m: -C -O -X -Y -V',C);

% Convert plot to a PNG and display in MATLAB as Figure
I = gmt('psconvert -TG -P -E300 -A', map); %transparent PNG
figure()
h = imshow (I.image);
map = gmt('psbasemap', '-B30m30m/30ma30m -JM -R-119/-116/32/34 -K -X1 -Y1');
map = gmt('grdimage', '-R -JM -V -X0 -Y0 -O -C -K',G,C);
map = gmt('grdcontour', '-R -C500 -L-4000/0 -Q50 -JM -V -X0 -Y0 -O -K',G);
map = gmt('psscale', '-D9i/1i/5c/0.5c -B1000/:m: -C -O -X -Y -V',C);

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

