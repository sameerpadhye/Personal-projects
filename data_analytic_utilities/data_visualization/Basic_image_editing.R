#Basic image editing in R

#https://cran.r-project.org/web/packages/magick/vignettes/intro.html - magick package use to edit images

if(!require(magick))install.packages('magick')


#Path for the image file (assuming the data file is stored in the working directory)

image_path<-paste0(getwd(),"/test_image.jpg")


#Importing the image

test_image <- image_read(image_path, density = 25)


#Details of the image

image_info(test_image)


#Resizing the image

#Defining the parameters. It can be done by number of ways. Here percent based modification is shown

size_geometry<-geometry_size_percent(width=25,height = 20)

#Re-sizing

image_resize(test_image,
             size_geometry)


#Cropping the image

#cropping paramaters

crop_geometry<-geometry_size_pixels(width=300,
                                    height=300,
                                    preserve_aspect = TRUE)

#Cropping

image_chop(test_image,
           crop_geometry)

#OR

image_chop(test_image,
           "100x50+50") # the numbers represent height x width + starting point in pixels from the left


# Image Rotation 

image_rotate(test_image, #image
             90) # angle


# Horizontal flipping

image_flop(test_image)


# Editing image 

#the brightness and contrast of the image

image_modulate(test_image, 
               brightness = 80, ## Image editing parameters
               saturation = 20, 
               hue = 20)

# Blur manipulation

image_blur(test_image,
           radius=10, # radius of pixels
           signma=5)

# Noise manipulation

image_noise(test_image,
            noisetype = 'gaussian')

#Convert to grayscale

image_convert(test_image,
              type = 'Grayscale')

