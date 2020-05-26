#' A Moving Average Filter Function
#'
#' This function computes the centered moving average for filtering.
#' @param alpha a number that denotes the one-sided window length of the filter.
#' @param data a numeric vector to apply the filter
#' @param windowed a logical that defaults to FALSE. If true, then it will apply correction for bias caused by seasonal behaviors.
#' @return A numeric vector containing the filtered data
#' @examples
#' ma(2, mydata)
#' ma(6, mydata, T)

ma <- function(alpha, data, windowed=F) {
    sum = 0
    filtered_data = rep(0, length(data))
    if(windowed == T){
        for(i in (alpha+1):(length(filtered_data)-alpha)) {
            for(j in -alpha:alpha) {
                sum = sum + data[i-j]
                }
        filtered_data[i] <- (1/(2*alpha))*sum -
            (1/(4 * alpha))*( data[i-alpha] + data[i+alpha] )
        sum = 0
    }
    } else {
        for(i in (alpha+1):(length(filtered_data)-alpha)) {
            for(j in -alpha:alpha) {
                sum = sum + data[i-j]
                }
        filtered_data[i] <- (1/(2*alpha + 1))*sum
        sum = 0
        }
    }
    return(filtered_data)
}
