im_left = imread('pic410.bmp');
im_right = imread('pic430.bmp');

height = size(im_left, 1);
width = size(im_left, 2);

disparity_max = 16;

ad_costs = zeros(height, width,disparity_max);

for disparity = 1:disparity_max
    for row = 1:height
        for col = 1:width
            %Left to right matching
            col_disp = col - disparity;
            if col_disp < 1
                ad_costs(row, col, disparity) = 0;
            else
                %Average RGB
                left_pixel = (im_left(row, col, 1) + im_left(row, col, 2) + im_left(row, col, 3))/3;
                right_pixel = (im_right(row, col_disp, 1) + im_right(row, col_disp, 2) + im_right(row, col_disp, 3))/3;

                %Subtract averages
                ad_costs(row, col, disparity) = abs(left_pixel - right_pixel);
            end
        end
    end
end

min_costs = zeros(height, width);
imshow(min_cost, []);

for disparity = 1:disparity_max
    for row = 1:height
        for col = 1:width
            %The minimum disparity is chosen
            min_costs(row, col) = min(ad_costs(row, col, :));
        end
    end
end