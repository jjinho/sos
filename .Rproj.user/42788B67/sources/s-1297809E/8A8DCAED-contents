# Helper Functions
# Converts from "Y" and "N" factors to "1" and "0"
# This is required for some functions that expect "1" and "0"
library(plyr)
zero_ones <- function(var) {
    if(is.factor(var)) {
        var <- revalue(var, c("Y"="1", "N"="0"))
    }
    return(as.factor(var))
}
