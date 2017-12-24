clear all; close all; clc;
rgb_img = imread('Photo_062011_002.jpg');
I = .2989*rgb_img(:,:,1)...
 +.5870*rgb_img(:,:,2)...
 +.1140*rgb_img(:,:,3);
%display grayscaled image in figure 2 with gray(256) colormap
fig = figure; colormap(gray(256)); image(I);
axis image;
%get data object
dcm_obj = datacursormode(fig);
%enable cursormode
set(dcm_obj,'DisplayStyle','datatip',...
    'SnapToDataVertex','on','Enable','on')
%prompt user
disp('Click on the image to display a data tip, then press Return.')
pause % Wait while the user does this.
%save cursor position and display in Command Window
c_info = getCursorInfo(dcm_obj);
pos = c_info.Position
