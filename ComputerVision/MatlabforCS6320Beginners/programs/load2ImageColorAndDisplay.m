clear all; close all; clc;
%load an image and display it in figure 1
im1 = imread('Photo_062011_002.jpg');
image(im1);
%fit plot box tightly around the image data
axis image;
%load another image and display it in figure 2
im2 = imread('IMG_1766.jpg');
figure; image(im2);
axis image;
