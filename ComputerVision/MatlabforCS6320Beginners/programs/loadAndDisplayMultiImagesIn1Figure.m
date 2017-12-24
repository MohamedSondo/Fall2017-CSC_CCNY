clear all; close all; clc;
%load an image and display it in first row, 1st column, figure 1
im1 = imread('Photo_062011_002.jpg');
subplot(2,2,1);image(im1);
%fit plot box tightly around the image data
axis image;
%Change image to grayscale
I = .2989*im1(:,:,1)...
 +.5870*im1(:,:,2)...
 +.1140*im1(:,:,3);
%display grayscaled image in 1st row, 2nd column, figure 1
subplot(2,2,2); colormap(gray(256)); image(I);
axis image;
%load another image and display it in second row, 1st column figure 1
im2 = imread('IMG_1766.jpg');
subplot(2,2,3); image(im2);
axis image;
%load 3rd color image and display it in second row, 2nd column figure 1
im3 = imread('IMG_1768.jpg');
subplot(2,2,4); image(im3);
axis image;
