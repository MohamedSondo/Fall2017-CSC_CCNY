clear all; close all; clc;
rgb_img = imread('Photo_062011_002.jpg');
I = .2989*rgb_img(:,:,1)...
 +.5870*rgb_img(:,:,2)...
 +.1140*rgb_img(:,:,3);
%display grayscaled image in figure 2 with gray(256) colormap
figure; colormap(gray(256)); image(I);
axis image;
[x,y] = ginput;
xInt = uint16(round(x))';
yInt = uint16(round(y))';
hold 'on'; plot(xInt,yInt,'+r');
hold 'off';
