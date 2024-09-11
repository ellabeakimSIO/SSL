%concatenate plots

%select folder that just has the types that you want concatenated

inDir = 'E:\SDT\SDT Clustering\Trial 4_Normalized 800 Hz\BF\LR\80p\type4';
fList = dir(fullfile(inDir,'*type*.mat'));
catAllSpec = [];
catAllICI = [];
catAllWave = [];
newDim = [];

for iF = 1:size(fList,1)
    inFile = fullfile(fList(iF).folder,fList(iF).name);
    load(inFile)
    [~,typeName,~] = fileparts(inFile);
    allNames{iF} = typeName;
    previousDim = newDim;
    newDim = size(thisType.Tfinal{1,1},2);
    if ~isempty(previousDim)&& newDim~=previousDim
        error('Spectra have different sizes')
    end
    nSpecSet(iF,1) = size(thisType.Tfinal{1,1},1);
    catAllSpec = [catAllSpec;vertcat(thisType.Tfinal{:,1})];
    catAllICI = [catAllICI;vertcat(thisType.Tfinal{:,2})];
    catAllWave = [catAllWave;vertcat(thisType.Tfinal{:,10})];
    
end
%% create plot here

f = 60:.5:800;
figure(400);clf;colormap(jet)
%subplot(3,1,1)
imagesc(1:size(catAllSpec,1),f,catAllSpec')
set(gca,'ydir','normal')
colorbar
hold on
for iL = 1:size(fList)
    nDiv(iL) = sum(nSpecSet(1:iL));
    plot([nDiv(iL),nDiv(iL)],[min(f),max(f)],'k','LineWidth',2,'linestyle','--')
   % thisName = strrep(allNames{iL},'_','\_');
   % text(nDiv(iL)-nSpecSet(iL),max(f)+5,0,thisName,'FontSize',7,'Rotation',30)
end
hold off
ylabel('Frequency (Hz)')
xlabel('Bin Number')
title('Low Rank "Noise"')



%% DIDN'T USE THIS 
subplot(3,1,2)
imagesc(1:size(catAllICI,1),0:.01:1,catAllICI')
set(gca,'ydir','normal')
colorbar
hold on
for iL = 1:size(fList)
    plot([nDiv(iL),nDiv(iL)],[0,1],'k','LineWidth',2,'linestyle','--')
end
hold off
ylabel('Seconds')

subplot(3,1,3)
imagesc(1:size(catAllWave,1),1:200,catAllWave')
set(gca,'ydir','normal')
colorbar
hold on
for iL = 1:size(fList)
    plot([nDiv(iL),nDiv(iL)],[1,200],'k','LineWidth',2,'linestyle','--')
end
hold off
xlabel('Bins')
ylabel('Samples')