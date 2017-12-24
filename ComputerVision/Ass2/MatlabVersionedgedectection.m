
 i = imread('peppers.png');
I = rgb2gray(i);
BW1 = edge(I,'Prewitt');
% BW2= edge(I,'sobel');
% % BW3= edge(I,'roberts');
% subplot (2,2,1);
% imshow(I);
% title('original');
% subplot(2,2,2); 
% imshow(BW1);
% title('1X2 Operator');
% subplot(2,2,3);
% imshow(BW2);
% title('Sobel');
% subplot(2,2,4);
% imshow(BW3); 
% title('Roberts'); 
% 
% grayImage= rgb2gray(rgbImage);
size(I);
BW = edge(I,'sobel',0.006);
% filteredImage = conv2(double(I), [-1 1], 'same');
subtractedImage = imsubtract(BW,BW1);
imshow(subtractedImage);

title('original');
drawnow();
