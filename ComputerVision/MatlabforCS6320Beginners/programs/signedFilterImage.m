%Function: signedFilterImage
%parameters:    I = 2D array containing image data
%               maskSize = size of the Square mask applied
%               mask = Actual mask array of size maskSize x maskSize.
%               Account for the normalisation considering the division
%               factor.
%returns:   signedFilter = 2D array containing filtered image data. Please
%                           note that no border padding is done; hence the
%                           border pixels are left unfiltered
function [signedFilter] = signedFilterImage(I, maskSize, mask)

Idouble = double(I);
maskdouble = double(mask);

tempImage = double(I);

for i = ((maskSize-1)/2)+1 : (size(I,1)-((maskSize-1)/2))%row of image pixel
    for j = ((maskSize-1)/2)+1 : (size(I,2)-((maskSize-1)/2))%col of image pixel
        subImage = Idouble(i-((maskSize-1)/2) : i+((maskSize-1)/2), j-((maskSize-1)/2) : j+((maskSize-1)/2));
        %subImage is same size as mask; centered around current pixel        
        filterResult = subImage .* maskdouble;
        %accumulate sum of products of subImage and mask
        pixelValue = double(0);%accumulator
        for p = 1:size(filterResult,1)%row of mask and subImage
            for q = 1:size(filterResult,2)%col of mask and subImage
                pixelValue = pixelValue + filterResult(p,q);
            end
        end
        %tempImage contains only non-negative values
        tempImage(i,j) = abs(pixelValue);
    end
end

signedFilter = uint8(tempImage);%return filtered image

end