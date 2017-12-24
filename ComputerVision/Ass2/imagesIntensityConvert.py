#####################################
#
#This Block of code Convert the image to intensity image.
#
# import numpy as np
# import matplotlib.pyplot as plt
# import matplotlib.image as mpimg
#
# def rgb2gray(rgb):
#     return np.dot(rgb[...,:3], [0.299, 0.587, 0.114])
#
# img = mpimg.imread('peppers.png')
# gray = rgb2gray(img)
# plt.imshow(gray, cmap = plt.get_cmap('gray'))
# plt.show()

# from PIL import Image
# img = Image.open('peppers.png').convert('LA')
# img.save('greyscalePeppers.png')


import cv2
import numpy as np
from matplotlib import pyplot as plt

img = cv2.imread('home.jpg',0)
plt.hist(img.ravel(),256,[0,256]); plt.show()