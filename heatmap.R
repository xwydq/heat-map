###############################
## 需求：
# 动态展现随时间变化公司业务（会员）分布的热力图
# 1；累积热力图（会员越来越多）
# 2；按照某个时间段推进的热力图（按月新增会员的分布情况）

options(stringsAsFactors=FALSE, scipen=10, digits=4)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(REmap)
library(animation)
library(png)

setwd("~/R_test/remap")

## 删除临时文件中的html
# remap包将生成的html文件保存在临时目录下
# 目前没有找到修改保存路径的方法
tdir = tempdir()

htmlfile = list.files(tdir)

for (i in htmlfile) {
  if(grepl("html", i)){
    system(paste0("rm ", file.path(tdir, i)))
  }
}


## 数据获取
# 包含会员经纬度以及加入时间
userdat <- read.csv("userdat.csv")
head(userdat)
# TIME_IN COUNTRY       CITY     ADDRESS_CLEAN   LAT   LNG
# 1 2015-05-11 13:19:11    美国 加利福尼亚 美国%20加利福尼亚 40.68 122.3
# 2 2015-05-11 14:48:50    中国     深圳市     中国%20深圳市 22.55 114.0
# 3 2015-05-11 14:59:59    中国     深圳市     中国%20深圳市 22.55 114.0
# 4 2015-05-11 16:22:41    中国     苏州市     中国%20苏州市 31.32 120.6
# 5 2015-05-11 17:19:52    中国     南京市     中国%20南京市 32.06 118.8
# 6 2015-05-11 17:40:49    中国     上海市     中国%20上海市 31.25 121.5


# 国内数据整理-国外数据点太少
chinaUser <- userdat %>%
  filter(COUNTRY == "中国") %>%
  arrange(TIME_IN) %>%
  mutate(rn=1:n())

## 按照历史时间线生成html的地图文件（依次增加数据）
for (i in seq(10, 690, 10)) {
  chinaCityNum <- chinaUser %>%
    filter(rn < i) %>%
    group_by(COUNTRY, CITY, LNG, LAT) %>%
    summarise(num=n()) %>%
    as.data.frame()
  
  ## 热力图
  heatmap = chinaCityNum %>%
    ungroup() %>%
    select(LNG, LAT, num)
  names(heatmap) <- c("lon", "lat", "prob")
  out=remapH(heatmap,minAlpha = 0.4, title = "国内分布热力图")
  # 不能默认取消这个弹窗
  plot(out)
}


## 将生成的js文件和html文件的热力图复制到指定位置方便后续操作
htmlfile = list.files(tdir)
system(paste0("cp -r ", file.path(tdir, "js"), " ", "/home/xuwy/R_test/remap/html"))

for (i in htmlfile) {
  if(grepl("html", i)){
    system(paste0("cp ", file.path(tdir, i), " ", "/home/xuwy/R_test/remap/html"))
  }
}


## html文件转化为png图片-phantomjs
# 生成html文件与png文件名的对照表
htmlfile = list.files("~/R_test/remap/html")
htmlfile = htmlfile[grepl("html", htmlfile)]

pngfiledf = data.frame(htmlfile = htmlfile)
pngfiledf = pngfiledf %>%
  arrange(htmlfile) %>%
  mutate(rn=sprintf("%03d", 1:n())) %>%
  mutate(pngfile=paste0("Rplot_", rn, ".png"))

# 使用phantomjs将html转化为png
for (i in 1:nrow(pngfiledf)) {
  htmlnm = pngfiledf$htmlfile[i]
  htmlpath = file.path(getwd(), "html", htmlnm)
  
  pngnm = pngfiledf$pngfile[i]
  pngpath = file.path(getwd(), "png", pngnm)
  
  picout = paste0("var page = require('webpage').create();\n
                  page.viewportSize = { width: 1280, height: 800 };\n
                  page.open('", htmlpath, "', function() {\n
                  window.setTimeout(function () {\n
                  page.render('", pngpath, "');\n
                  phantom.exit();\n
                  }, 100);\n
                  });\n")
  cat(picout, file="render.js")
  
  system("/usr/local/src/phantomjs-2.1.1-linux-x86_64/bin/phantomjs --ignore-ssl-errors=true render.js")
}


## 制作gif
# 使用linux命令convert直接将png转化为gif
system("convert -delay 20 ~/R_test/remap/png/*.png ~/R_test/remap/png/sim.gif")

# 使用animation包进行转化
# ani.options(convert = 'C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe')
im.convert(files = file.path("png", pngfiledf$pngfile), 
           output = "hmap.gif")

