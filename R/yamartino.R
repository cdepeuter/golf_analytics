#' Calculate wind mean and variance using yamartino method
#'
#' This function finds and saves historical weather info for tournaments
#' @param obs wind direction in degrees
#' @return mean wind direction according to yamartino
#' @export
#' @examples
#' yamartino(safeway$lastWindSpeed)

yamartino <- function(obs){
    if(length(obs) == 0 ){
        return(NA_integer_)
    }
    
    # to radians
    obs <- obs * pi / 180
    s_a <- mean(sin(obs), na.rm = TRUE)
    c_a <- mean(cos(obs), na.rm = TRUE)

    avg_dir <- atan2(s_a, c_a)
    
   
    eps <- sqrt(1-(s_a**2 + c_a **2))
    std <- asin(eps) * (1+(2/sqrt(3) - 1) * eps**3)
    
    
    # to degrees
    avg_dir <- avg_dir * 180 / pi
    std <- std * 180/pi
    if(avg_dir < 0){
        avg_dir <- avg_dir + 360
    }
    
    return(avg_dir)
}

#' Calculate wind variance using yamartino method
#'
#' This function finds and saves historical weather info for tournaments
#' @param obs wind direction in degrees
#' @return std wind direction according to yamartino
#' @export
#' @examples
#' yamartino(safeway$lastWindSpeed)

yamartino_std <- function(obs){
    if(length(obs) == 0 ){
        return(NA_integer_)
    }
    
    # to radians
    obs <- obs * pi / 180
    s_a <- mean(sin(obs), na.rm = TRUE)
    c_a <- mean(cos(obs), na.rm = TRUE)
    
    eps <- sqrt(1-(s_a**2 + c_a **2))
    std <- asin(eps) * (1+(2/sqrt(3) - 1) * eps**3)
    
    
    # to degrees
    std <- std * 180/pi

    
    return(std)
}

