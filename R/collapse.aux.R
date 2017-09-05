
d_to_m <- function(x){
    
    serie <- BETS.get(code = x, data.frame = TRUE)
    serie$mes <- lubridate::month(serie$date)
    serie$ano <- lubridate::year(serie$date)

    serie$markup_anual <- (serie$value/100 + 1)^252 # 21*12
    serie$markup_mensal <- (serie$value/100 + 1)^21 # 21*12
    
    
    agg <- aggregate(cbind(markup_anual, markup_mensal) ~ serie$mes + serie$ano, FUN = mean, data = serie)
    agg$tj_mensal <- (agg$markup_mensal - 1) * 100
    agg$tj_anual <- (agg$markup_anual - 1) * 100
    return(agg)
}


#





gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
    if(any(x < 0, na.rm = TRUE)){
        return(NaN)
    }
    if(zero.propagate){
        if(any(x == 0, na.rm = TRUE)){
            return(0)
        }
        exp(mean(log(x), na.rm = na.rm))
    } else {
        exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    }
}


markup <- function(x){x/100 + 1}