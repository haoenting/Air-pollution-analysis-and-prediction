Import <- function(){
  setwd(file.path("C:/Users/tingh/Desktop/mid project")) 
  #更改工作目錄至專案中
  Data <- read.csv("pollution.csv", fileEncoding = 'UTF-8-BOM')
  #取得檔案內容，以中文編碼方式開啟
  return(Data) #回傳檔案內容
}

Statistics <- function(data){
  print(head(data)) #觀察前六個資料
  print(tail(data)) #觀察後六個資料
  print(summary(data)) #
  
  library(Hmisc) #呼叫
  #print(describe(data))
}

Histogram <- function(data){
  #觀察各地區的空汙含量
  boxplot(data[,11]~data[,1], data, xlab = "SiteName", ylab = 'PM2.5')
  boxplot(data[,12]~data[,1], data, xlab = "SiteName", ylab = 'PM10')
  
  #觀察各時間的空汙含量
  boxplot(data[,11]~data[,13], data, xlab = "Time", ylab = 'PM2.5')
  boxplot(data[,12]~data[,13], data, xlab = "Time", ylab = 'PM10')
  
  #觀察不同時間下，PM2.5和PM10之間的關係
  cor.all <- by(data[,c(11,12)],INDICES = data$DataCreationDate,cor)
  print(cor.all)
  
}

Normal_test <- function(data){
  par(mfrow = c(2,2))
  
  total <- data$SO2 + data$CO + data$O3+
    data$NO + data$NO2 + data$NO2 + data$NOx
  
  qqnorm(total);qqline(total,col='red')
  print(shapiro.test(total))
  
  qqnorm(data$PM2.5);
  qqline(data$PM2.5,col="red")
  print(shapiro.test(data$PM2.5))
  
  qqnorm(data$PM10);
  qqline(data$PM10,col="red")
  print(shapiro.test(data$PM10))
}

regression_pm2.5 <- function(data){
  library(ggplot2) # 使用ggplot2套件
  
  # 建立模型
  total_pollution <- data$SO2 + data$CO + data$O3+
                      data$NO + data$NO2 + data$NO2 + data$NOx
  pm2.5 <- data$PM2.5
  LM2.5 <- lm(pm2.5~total_pollution, data=data)
  #summary(LM2.5)
  shapiro.test(LM2.5$residual[0:150])
  durbinWasonTest
  
  # 分布&預測圖
  #ggplot(data, aes(x=pm2.5, y=total_pollution)) +
 #   geom_point(shape=5, size=2) + geom_smooth(method=lm) + 
 #   labs(x="PM2.5", y="Total_pollution")
  
  # 取得方程式參數
  #new <- data.frame(total_pollution = 48)
  #result <- predict(LM2.5, newdata=new)
  #print(result)
}

regression_pm10 <- function(data){
  library(ggplot2) # 使用ggplot2套件
  
  # 建立模型
  total_pollution <- data$SO2 + data$CO + data$O3+
                       data$NO + data$NO2 + data$NO2 + data$NOx
  pm10 <- data$PM10
  LM10 <- lm(pm10~total_pollution, data=data)
  
  # 分布&預測圖
  ggplot(data, aes(x=pm10, y=total_pollution)) +
    geom_point(shape=5, size=2) + geom_smooth(method=lm) + 
    labs(x="PM10", y="Total_pollution") 
  
  summary(LM10) # 取得方程式參數
  #new <- data.frame(total_pollution = 48)
  #result <- predict(LM10, newdata=new)
  #print(result)
}

regression_all <- function(data){
  library(ggplot2) # 使用ggplot2套
  
  # 建立模型
  total_pollution <- data$SO2 + data$CO + data$O3+
                       data$NO + data$NO2 + data$NO2 + data$NOx
  pm2.5 <- data$PM2.5
  pm10 <- data$PM10
  LM <- lm(pm2.5+pm10 ~ total_pollution,  data= data)
  
  # 分布&預測圖
  ggplot(data, aes(x=pm2.5+pm10, y=total_pollution)) + 
    geom_point(shape = 5, size = 2) + geom_smooth(method = lm) +
   labs(x = "pm2.5+pm10", y = "Total_pollution")
  
  summary(LM) # 取得方程式參數
  
  new <- data.frame(total_pollution = 48)
  result <- predict(LM, newdata=new)
  print(result)
}

data <- Import()
#Statistics(data)
#Histogram(data)
#Normal_test(data)
#regression_pm2.5(data)
#regression_pm10(data)
#regression_all(data)