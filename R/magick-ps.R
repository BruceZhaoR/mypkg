library(magick)

ma <- image_read("data/ma.jpg")

plot(1:100,type = "n")
axis(2,seq(0,100,5))
axis(3,seq(0,100,5))
# 将图片换算成0:100,方便定位
rasterImage(ma,0,0,100,100)

# Paint the bg white
image_info(ma)
#1024*0.15=153
#1417*0.15 = 212
#从左上角往旁边的距离
image_fill(ma, "white", point = "+153+212", fuzz = 30)

ma1 <- image_fill(ma, "white", point = "+153+212", fuzz = 20)
image_write(ma1,"data/ma1.png")

