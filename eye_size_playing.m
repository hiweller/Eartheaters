%% MESSIN ABOOT

k = 3;
m = 5;
datedir = dir('*-16');
trialdir = dir([datedir(k).name, '/Intervals/*Intervals']);
jpgdir = dir([datedir(k).name, '/Intervals/', trialdir(m).name, '/*.jpg']);
pixel.areas = [];


for i = 24:length(jpgdir)
    imagename = sprintf('%s%s%s%s%s', datedir(k).name, '/Intervals/', trialdir(m).name, '/', jpgdir(i).name);
    pixel.areas(i) = eye_size(imagename, 40); hold on;
    pause
end




%% trying to threshold good ;-;

k = 3;
m = 2;
datedir = dir('*-16');
trialdir = dir([datedir(k).name, '/Intervals/*Intervals']);
jpgdir = dir([datedir(k).name, '/Intervals/', trialdir(m).name, '/*.jpg']);
pixel.areas = [];
imagename = sprintf('%s%s%s%s%s', datedir(k).name, '/Intervals/', trialdir(m).name, '/', jpgdir(1).name);

image = imread(imagename);
greyimage = rgb2gray(image);
imdim = size(greyimage);

% set threshold; if above then turn white
threshold = 50;
for i = 1:imdim(1)
    for j = 1:imdim(2)
        if greyimage(i, j) > threshold
            greyimage(i, j) = 0;
        else
            greyimage(i, j) = 1;
        end
    end
end

greyimage = logical(greyimage);
% BW2 = bwmorph(greyimage,'remove');
alternate = bwmorph(greyimage, 'majority', Inf);
alternate = bwmorph(alternate, 'clean', Inf);
alternate = bwmorph(alternate, 'fill', Inf);
alternate = bwareafilt(alternate, [1000 7000]);


stats = regionprops('table', alternate, 'Centroid','Extent', 'Area', 'PixelList');
imshow(image); hold on; plot(stats.Centroid(:,1), stats.Centroid(:,2), 'c*'); hold off
imshow(alternate);
variances = [];
means = [];
for i = 1:height(stats)
    centroid = stats.Centroid(i,:);
    pixels = stats.PixelList{i};
    centroid_dist = [];
    for j = 1:length(pixels)
        centroid_dist(j) = get_distance(centroid, pixels(j,:));
    end
    figure; hist(centroid_dist);
    variances(i) = var(centroid_dist);
    means(i) = mean(centroid_dist);
%     pause
end



extenty = cat(1, stats.Extent);
areas = cat(1, stats.Area);
value = 0.75;

diffo = abs(value-extenty);
[idx idx] = min(diffo);
closest = areas(idx);

imshow(image); hold on; plot(stats.Centroid(idx,1), stats.Centroid(idx,2), 'c*'); hold off

%% color segmentation?
k = 3;
m = 2;
datedir = dir('*-16');
trialdir = dir([datedir(k).name, '/Intervals/*Intervals']);
jpgdir = dir([datedir(k).name, '/Intervals/', trialdir(m).name, '/*.jpg']);
pixel.areas = [];
imagename = sprintf('%s%s%s%s%s', datedir(k).name, '/Intervals/', trialdir(m).name, '/', jpgdir(1).name);

image = imread(imagename);
greyimage = image(:,:,1);
imdim = size(greyimage);

for i = 1:3
    figure; imshow(image(:,:,i));
end

cform = makecform('srgb2lab');
lab_he = applycform(image,cform);
ab = double(lab_he(:,:,2:3));
nrows = size(ab,1);
ncols = size(ab,2);
ab = reshape(ab,nrows*ncols,2);

nColors = 3;
% repeat the clustering 3 times to avoid local minima
[cluster_idx, cluster_center] = kmeans(ab,nColors,'distance','sqEuclidean', ...
                                      'Replicates',3);
pixel_labels = reshape(cluster_idx,nrows,ncols);
imshow(pixel_labels,[]), title('image labeled by cluster index');


%% taking reds
mkdir('testreds');
dir1 = dir('02-26-16/Intervals/*Intervals');

for i = 1:length(dir1)
    dir2 = dir(['02-26-16/Intervals/', dir1(i).name, '/*.jpg']);
    mkdir(['testreds/', dir1(i).name]);
    for j = 1:length(dir2)
        img = imread(dir2(j).name);
        savename = sprintf('%s%s%s%s', 'testreds/', dir1(i).name, '/', dir2(j).name);
        imwrite(img(:,:,1), savename);
    end
end
