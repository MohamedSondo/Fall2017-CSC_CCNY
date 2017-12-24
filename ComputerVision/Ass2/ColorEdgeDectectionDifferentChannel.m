clc;    % Clear the command window.
close all;  % Close all figures (except those of imtool.)
imtool close all;  % Close all imtool figures.
workspace;  % Make sure the workspace panel is showing.
fontSize = 14;
% Read in a standard MATLAB color demo image.
folder = fullfile(matlabroot, '\toolbox\images\imdemos');
baseFileName = 'IDPicture.bmp';
fullFileName = fullfile(folder, baseFileName);
% Get the full filename, with path prepended.
fullFileName = fullfile(folder, baseFileName);
if ~exist(fullFileName, 'file')
	% Didn't find it there.  Check the search path for it.
	fullFileName = baseFileName; % No path this time.
	if ~exist(fullFileName, 'file')
		% Still didn't find it.  Alert user.
		errorMessage = sprintf('Error: %s does not exist.', fullFileName);
		uiwait(warndlg(errorMessage));
		return;
	end
end
rgbImage = imread(fullFileName);
% Get the dimensions of the image.  numberOfColorBands should be = 3.
[rows columns numberOfColorBands] = size(rgbImage);
% Display the original color image.
subplot(2, 4, 1);
imshow(rgbImage, []);
title('Original Color Image', 'FontSize', fontSize);
% Enlarge figure to full screen.
set(gcf, 'units','normalized','outerposition',[0 0 1 1]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract the individual red, green, and blue color channels.
redChannel = rgbImage(:, :, 1);
greenChannel = rgbImage(:, :, 2);
blueChannel = rgbImage(:, :, 3);
% Display them
subplot(2,4,2);
imshow(redChannel);
title('Red Channel', 'FontSize', fontSize);
subplot(2,4,3);
imshow(greenChannel);
title('Green Channel', 'FontSize', fontSize);
subplot(2,4,4);
imshow(blueChannel);
title('Blue Channel', 'FontSize', fontSize);
% Compute Sobel Filters and threshold them
threshRed = 1;
threshGreen = 3;
threshBlue = 4;
[sobelRed threshRed] = edge(redChannel,'sobel');
[sobelGreen threshGreen] = edge(greenChannel,'sobel');
[sobelBlue threshBlue] = edge(blueChannel,'sobel');
% Display them
subplot(2,4,6);
imshow(sobelRed, []);
title('Sobel Filter on Red Channel', 'FontSize', fontSize);
subplot(2,4,7);
imshow(sobelGreen, []);
title('Sobel Filter on Green Channel', 'FontSize', fontSize);
subplot(2,4,8);
imshow(sobelBlue, []);
title('Sobel Filter on Blue Channel', 'FontSize', fontSize);