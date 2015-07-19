data1 <- activity[1,]

for (i in levels(as.factor(activity$interval))) {
    data <- activity[as.factor(activity$interval)==i,]
    library(plyr)
    fill.missing <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    dat2 <- sapply(data, function(x){
        if(is.numeric(x)){
            fill.missing(x)
        } else {
            x
        }
    }
    )
    
    data2 <- data.frame(dat2)  
    data1 <- rbind(data1,data2)
    
}
activity_new <- data1[2:17569,]
activity_new <-activity_new[with(activity_new, order(date)),]
