function[closest] = eye_size(imagename, threshold)
image = imread(imagename);
greyimage = image(:,:,1);
imdim = size(greyimage);

% set threshold; if above then turn white
% threshold = 30;
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
alternate = bwareafilt(alternate, [1500 7000]);


stats = regionprops('table', alternate, 'Centroid','Extent', 'Area', 'PixelList');
extenty = cat(1, stats.Extent);
areas = cat(1, stats.Area);
value = 0.74;

diffo = abs(value-extenty);
[idx idx] = min(diffo);
closest = areas(idx);
% imshow(alternate); hold on; plot(stats.Centroid(idx,1), stats.Centroid(idx,2), 'b*'); hold off
imshow(image); hold on; plot(stats.Centroid(idx,1), stats.Centroid(idx,2), 'c*'); hold off

end