# 根据经纬度获取省份城市
get_city <- function(lng, lat){
  library(RCurl)
  library(jsonlite)
  library(stringr)
  url_addr <- paste0("http://api.map.baidu.com/geocoder/v2/?ak=dZaI6FTMf9GBjAKIBwLcOmRS&callback=renderReverse&location=",
                     lng, ",", lat,
                     "&output=json")
  
  url_json <- getURL(url_addr)
  url_json <- str_extract(url_json, "\\{.*\\}")
  url_json <- fromJSON(url_json)
  
  if(url_json$status == 0){
    province = url_json$result$addressComponent$province
    city = url_json$result$addressComponent$city
  }else{
    province = NA
    city = NA
  }
  
  return(data.frame(province = province,
                    city = city))
}


get_city_rep <- function(lng, lat, reptime=3){
  if(exists("lng_lat")){
    rm(lng_lat)
  }
  
  i_t <- 1
  while(TRUE){
    tryCatch({
      lng_lat <- get_city(lng, lat)
      # test_success <- 1
    }, warning = function(w) {
      print("warning produced")
    }, error = function(er) {
      print("Error produced")
    })
    
    if(exists("lng_lat")){
      break
    }else{
      i_t <- 1 + i_t
      if(i_t == reptime){
        lng_lat <- data.frame(province = NA,
                              city = NA)
        break
      }
    }
  }
  
  return(lng_lat)
}



# 地址获取经纬度1：有可能报错或者取不到数据
get_loc <- function(addr){
  library(RCurl)
  library(jsonlite)
  url_addr <- paste0("http://api.map.baidu.com/geocoder/v2/?address=",
                     addr,
                     "&output=json&ak=dZaI6FTMf9GBjAKIBwLcOmRS&callback=showLocation")
  
  url_json <- getURL(url_addr)
  url_json <- str_extract(url_json, "\\{.*\\}")
  url_json <- fromJSON(url_json)
  
  if(url_json$status == 0){
    lng = url_json$result$location$lng
    lat = url_json$result$location$lat
    lvl = url_json$result$level
  }else{
    lng = NA
    lat = NA
    lvl = "NO RESULT"
  }
  
  return(data.frame(lng = lng,
                    lat = lat))
}

# 地址获取经纬度2：如果取不到尝试多次获取，无果返回NA(处理报错)
get_loc_rep <- function(addr, reptime=3){
  if(exists("lng_lat")){
    rm(lng_lat)
  }
  
  i_t <- 1
  while(TRUE){
    tryCatch({
      lng_lat <- get_loc(addr)
      # test_success <- 1
    }, warning = function(w) {
      print("warning produced")
    }, error = function(er) {
      print("Error produced")
    })
    
    if(exists("lng_lat")){
      break
    }else{
      i_t <- 1 + i_t
      if(i_t == reptime){
        lng_lat <- data.frame(lng = NA,
                              lat = NA)
        break
      }
    }
  }
  
  return(lng_lat)
}


#############
# 度分秒转换
deg2dec <- 
  function(h,m,s){
    if(h < 0)
    {m = - m
    s = -s
    }
    res = h + m/60 + s/3600
    return(res)
  }

## 巴黎
# L1 = deg2dec(-2,20,14)
# phi1 = deg2dec(48, 50, 11)

## 根据两点经纬度计算最近距离
# 第一种低精度方案，
# 由于地球可以的扁率较小，可以近似看作一个球体，利用球面三角学公式可以快速的求出两点之间的距离。写成计算机程序，如下所示:
geodist <- 
  function(L1, phi1, L2, phi2)
  {
    Ri = 6371
    d = ( Ri * acos(sin(phi1*(pi/180))*sin(phi2*(pi/180)) + cos(phi1*(pi/180))*cos(phi2*(pi/180))*cos((L1 - L2)*(pi/180))))
    res <- round(d, 3)
    return(res)
  }

# 第二种 高精度方案，高精度的方案的误差低于+_50m.
# 采用较为精密的公式，考虑到地球是一个椭球,a是地球的长轴，f是扁率，用如下公式计算
hageodist <- 
  function(L1, phi1, L2, phi2){
    a = 6378.14
    f = 1/298.257
    F = (phi1+phi2)/2
    G = (phi1 - phi2)/2
    ramda <- (L1 - L2)/2
    
    S = (sin(G*pi/180)^2)*(cos(ramda*pi/180)^2) + (cos(F*pi/180)^2)*(sin(ramda*pi/180)^2)
    C= (cos(G*pi/180)^2)*(cos(ramda*pi/180)^2) + (sin(F*pi/180)^2)*(sin(ramda*pi/180)^2)
    
    omega = atan(sqrt(S/C))
    R = sqrt(S*C)/omega
    D = 2*omega*a
    
    H1 = (3*R-1)/(2*C)
    H2 = (3*R+1)/(2*S)
    
    res = D*(1 + f*H1*(sin(F*pi/180)^2)*(cos(G*pi/180)^2) - f*H2*(cos(F*pi/180)^2)*(sin(G*pi/180)^2))
    return(round(res,3))
  }


# 按照千位使用逗号分隔并保存一位小数
formatX <- function(x, deci=1){
  rest <- NULL
  
  for(x_i in x){
    if(is.na(x_i) | is.infinite(x_i)){
      res <- "-"
    }else{
      digit <- nchar(round(x_i))
      
      if(digit > 3){
        res <- format(x_i, digits=digit + deci, big.mark=",")
      }else{
        res <- as.character(round(x_i, deci))
      }
    }
    
    rest <- c(rest, res)
  }
  
  return(rest)
}

# 数值型转化为百分比
numtop <- function(num, p=1){
  num <- ifelse((is.na(num) | is.infinite(num)), 0, num)
  num1 <- num * 100
  formt <- paste0("%.", p, "f")
  percent <- sprintf(formt, num1)
  percent <- paste0(percent, "%")
  
  percent <- ifelse(num == 0, "-", percent)
  percent
}


nrom01 <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
