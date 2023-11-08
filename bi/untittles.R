library(lubridate)

data = data.frame(time = ymd(20100101)+((1:100)*5),
                  temp = sin(1:100),
                  chlor = ((1:100)+30)*0.1,
                  mag = rep(1, 100))


write.csv(data,"time.csv")



data = data.frame(
  first = sample(c('redi',"bliu","geen"), size = 100, replace = TRUE),
  second = sample(c('redi',"bliu","geen"), size = 100, replace = TRUE),
  third = sample(c('redi',"bliu","geen"), size = 100, replace = TRUE),
  forth = sample(c('redi',"bliu","geen"), size = 100, replace = TRUE)
)


write.csv(data,"cat.csv")

data = data.frame(
 asn = sample(110,100) 
)


write.csv(data,"rand.csv")

data.frame(fog = )