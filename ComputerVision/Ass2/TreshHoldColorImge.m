% %5
% 
% I = imread('peppers.png');
% RGB_x =zeros(250,250,3);
% RGB_y =zeros(250,250,3);
% Gx = [1, 0, -1; 2, 0, -2; 1, 0, -1];
% Gy = [-1, -2, -1; 0, 0 ,0; 1, 2, 1];
% for i = 1 : 3
%     RGB_x(:,:,i)= abs(double(conv2(I(:,:,i),Gx, 'same')/4));
%     RGB_y(:,:,i)= abs(double(conv2(I(:,:,i),Gy, 'same')/4));
% end
% comb_RGB = comb_XY(RGB_x,RGB_y);
% figure();
% together = [RGB_x,RGB_y,comb_RGB];
% image(together),title('Sobel operator to each of the RGB color bands of a color image');
% %thresholding RGB
% COLOR_THRESHOLDING = uint8(zeros(250,250,3));
% T_holding = zeros(3,1);
% for i = 1:3
%     T_holding(i) = keep_pixel(comb_RGB(:,:,i), 0.20);
%     COLOR_THRESHOLDING(:,:,i) = Thresholdingcolor(comb_RGB(:,:,i), T_holding(i), 250, 250, 1);
% end
% figure();
% image(COLOR_THRESHOLDING), title('Thresholding of color image');
% 
% 
% 
% 
% function  [output] = comb_XY(Gx,Gy)
% G = uint8(sqrt(double(Gx.^2 + Gy.^2)));
% output = G;
% end