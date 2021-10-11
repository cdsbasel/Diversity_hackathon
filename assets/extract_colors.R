library(tidyverse) 
library(magick)
library(scales)
library(imager)

get_colorPal <- function(im, n=8, cs="RGB"){
  #print(cs) 
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>% 
    magick2cimg() %>%  
    RGBtoHSV() %>% 
    as.data.frame(wide="c") %>%  
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) 
}

files=list.files(path = ".", pattern = "*jpg")
colors=data.frame(file=files,hex=NA)

for (file in 1:length(files))
{
  im <- image_read(files[file])
  colorspace=get_colorPal(im)
  
  colors[file,"hex"]=colorspace[1,"hex"]
}

write.csv(colors,"colors.csv")
