start_time <- Sys.time()

set.seed(1234567890)
library(geosphere)

stations <- read.csv("TDDE01/lab3/stations.csv")
temps <- read.csv("TDDE01/lab3/temps50k.csv")
st <- merge(stations,temps,by="station_number")

# parameters
h_distance <- 200*1000
h_date <- 20
h_time <- 4

position <- c(14.86, 58.4274) # vadstena 
#position <- c(20.1534, 63.4942) # ume?? 

my_date <- "2013-01-14"
#my_date <- "2013-07-14"

times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "00:00:00")
temp <- vector(length=length(times))

st <- subset(st, as.Date(date) < as.Date(my_date))

# ---- compares positions -----

station.positions <- cbind(st[,5], st[,4])
diff.dist = apply(station.positions, 1, function(x) distHaversine(position, x))

# ---- compares dates -----

convert_to_days = function(date) {
  tokens = as.numeric(strsplit(date, split="-")[[1]])
  days_in_month = 30.4 #avg days in a month
  return(as.integer((tokens[2]-1)*days_in_month + tokens[3]))
}

compare_days = function(x,y) {
  diff.forward = abs(x-y)
  diff.backward = abs((365-x)-y)
  return(min(c(diff.forward, diff.backward)))
}

days = sapply(st["date"], as.character)
days = sapply(days, convert_to_days)
day = convert_to_days(my_date)
diff.date = sapply(days, function(x) compare_days(x, day))

# ---- compares times -----

convert_to_hours = function(timestamp) {
  tokens = as.numeric(strsplit(timestamp, split=":")[[1]])
  return(tokens[1])
}

compare_hours = function(x,y) {
  return(abs(x-y))
}

time = sapply(st["time"], as.character)
time = sapply(time, convert_to_hours)

# ---- compares applies kernels, time above -----

smoothed.date = sapply(diff.date/h_date, function(x) exp(-x*x))
smoothed.dist = sapply(diff.dist/h_distance, function(x) exp(-x*x))

diff.time = matrix(nrow=dim(st)[1], ncol=length(times))
smoothed.time = matrix(nrow=dim(st)[1], ncol=length(times))

for(i in 1:length(times)) {
  comp_time = convert_to_hours(times[i])
  diff.time[,i] = sapply(time, function(x) compare_hours(x,comp_time))
  smoothed.time[,i] = sapply(diff.time[,i]/h_time, function(x) exp(-x*x))
  
  kernels = smoothed.time[,i]*smoothed.date*smoothed.dist
  temp[i] = sum(kernels*st["air_temperature"])/sum(kernels)
}

plot(temp, type="o", xaxt="n", xlab="Time of day", ylab="Temperature")
axis(1, at=1:length(temp), labels=times)

end_time <- Sys.time()

print(end_time - start_time)