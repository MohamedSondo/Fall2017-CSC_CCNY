I = imread('peppers.png');
% I2 = imread('IDPicture.bmp');
%  
 
[ROWS COLS CHANNELS] = size(I);
[R C CHANNELS] = size(I2);
 
MAP = double(zeros(256, CHANNELS));
for i = 1 : 256
    for band = 1:CHANNELS
        MAP(i,band) = (i-1)/255;
    end 
end
 
 
 
intensity= 0.299*I(:,:,1)+ 0.587*I(:,:,2) + 0.114*I(:,:,3);
% intensity2=0.299*I2(:,:,1)+ 0.587*I2(:,:,2) + 0.114*I2(:,:,3);
%


 
%Q3
map_one = uint8(zeros(250, 250));
map_sobel = uint8(zeros(250,250));
keep_one = ROWS *COLS * .85;
keep_sobel = ROWS *COLS * .75;
counter_one=zeros(256,1);
counter_sobel=zeros(256,1);
 
 
for i=1:250
    for j=1:250
    counter_one(combine_one(i,j)+1) = counter_one(combine_one(i,j)+1)+1;
 
    end
end
 
for i=1:250
    for j=1:250
    counter_sobel(round(sobel_XY(i,j))+1) = counter_sobel(round(sobel_XY(i,j))+1)+1;
 
    end
end
 
sum=0;
sum2=0;
for  i=1:256
    if sum >= keep_one 
        one_threhold = i-1;
        break;
    else
        sum = sum +counter_one(i);
    end
end
 
for  i=1:256
    if sum2 >= keep_sobel 
        sobel_threhold = i-1;
        break;
    else
        sum2 = sum2 +counter_sobel(i);
    end
end
        
    
 
for i = 1:250
    for j = 1:250
        if combine_one(i,j) <= one_threhold
            map_one(i,j) = 0;
        else
            map_one(i, j) = 255;
        end
        if sobel_XY(i,j) <= sobel_threhold
            map_sobel(i,j) = 0;
        else
            map_sobel(i, j) = 255;
        end
    end
end
 
edge_maps = [map_one, map_sobel];
No14 = figure;
image(edge_maps),title(' 1X2 Edge map VS sobel operation');
colormap(MAP);
 
%local, adaptive thresholds
p = zeros(125,125,4);
p(:,:,1) = sobel_XY(1:125, 1:125);
p(:,:,2) = sobel_XY(126:250, 1:125);
p(:,:,3) = sobel_XY(1:125, 126:250);
p(:,:,4) = sobel_XY(126:250, 126:250);
p_threshold=zeros(4,1);
local = zeros(125,125,4);
combine4 = zeros(250,250);
 
for i = 1:4
    p_threshold(i) = keep_pixel(p(:,:,i), 0.20);
end
for i = 1:125
    for j = 1:125
        for k = 1:4
            if p(i,j,k) <= p_threshold(k)
                local(i,j, k) = 0;
            else
                local(i, j, k) = 255;
            end
        end
    end
end
combine4(1:125, 1:125) = local(:,:,1);
combine4(126:250, 1:125) = local(:,:,2);
combine4(1:125, 126:250) = local(:,:,3);
combine4(126:250, 126:250) = local(:,:,4);
figure();
image(combine4),title('SKetch Of Peppers With 4');
colormap(MAP);
 
 
function output = keep_pixel(image,pixle)
    counter = zeros(256,1);
    r = length(image(:,1));
    c = length(image(1, :));
    for i=1 : r
        for j=1:c
            counter(round(image(i,j))+1)= counter(round(image(i,j))+1) + 1;
        end
    end% counter of image
    counter_so_far = 0;
    result_value =0;
    pixle_size = r*c*(1-pixle);
    
      for i=1:256
        if counter_so_far >= pixle_size 
            result_value = i-1;
            break;
        else
            counter_so_far = counter_so_far + counter(i);
        end
    end
    output = result_value;
end
