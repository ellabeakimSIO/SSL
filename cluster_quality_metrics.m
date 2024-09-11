%measuring cluster quality

close all
clear all

%load data
clearvars
p1 = load('E:\SDT\Cluster Quality\OM_BF_01_60to800_90p_types_all.mat');
p2 = load('E:\SDT\Cluster Quality\LR_BF_01_60to800_80p_types_all.mat');

%cluster 1
clust1 = zeros(size(p1.specNorm,1),1);
for iC = 1:length(p1.nodeSet)
    clust1(p1.nodeSet{iC}) = iC;
end
notClustered1 = find(clust1==0);
clust1(notClustered1,:) = [];
spec1 = p1.specNorm;
spec1(notClustered1,:) = [];
%calinksi harabsz
eva1 = evalclusters(spec1,clust1,'CalinskiHarabasz');
%silhoutte
evasil1=evalclusters(spec1,clust1,'silhouette')

%cluster2
clust2 = zeros(size(p2.specNorm,1),1);
for iC = 1:length(p2.nodeSet)
    clust2(p2.nodeSet{iC}) = iC;
end
notClustered2 = find(clust2==0);
clust2(notClustered2,:) = [];
spec2 = p2.specNorm;
spec2(notClustered2,:) = [];
%calinksi harabasz
eva2 = evalclusters(spec2,clust2,'CalinskiHarabasz');
%silhouette
evasil2=evalclusters(spec2,clust2,'silhouette')

%plotting silhouttes
figure(1)
subplot(1,2,1)
silhouette(spec1,clust1,'Euclidean')
title('Original Matrix Clusters')
subplot(1,2,2)
silhouette(spec2,clust2,'Euclidean')
title('Low Rank Matrix Clusters')
%do we do k means clsuter? if so euclidean is no good


%plotting silhouttes different distance measures (looks a little diff)
%correlation
figure(2)
subplot(1,2,1)
silhouette(spec1,clust1,'correlation')
title('Original Matrix Clusters')
subplot(1,2,2)
silhouette(spec2,clust2,'correlation')
title('Low Rank Matrix Clusters')

%jaccard and hamming were no good

%correlation
figure(3)
subplot(1,2,1)
silhouette(spec1,clust1,'cityblock')
title('Original Matrix Clusters')
subplot(1,2,2)
silhouette(spec2,clust2,'cityblock')
title('Low Rank Matrix Clusters')

%default- squared euclidean
figure(4)
subplot(1,2,1)
silhouette(spec1,clust1)
title('Original Matrix Clusters')
subplot(1,2,2)
silhouette(spec2,clust2)
title('Low Rank Matrix Clusters')
%%
s1=silhoutte(spec1,clust1,'sqEuclidean')

