#' Plot forecast result.
#'
#' Make plot to show the result of flow forecast as ggplot2 style.
#'
#' Visualize the result of forecast is helpful to check the efficiency
#' of forecast. This function use ggplot2 to make the plot more elegant.
#'
#' @section Two Components:
#' \enumerate{
#'     \item the real value of traffic flow, marked as blue point;
#'     \item the forecast value of traffic flow, marked as red line;
#' }
#'
#' @seealso \code{\link[ggplot2]{ggplot2-package}}
#'
#' @param rv real value of traffic flow, a row from a dataframe;
#' @param pv forecast value, can be the return value of flow_knn function,
#'           a row from a dataframe;
#'
#' @export
flow_forecastplot <- function(rv,pv){
    df <- rbind(rv,pv)
    df <- t(df)
    df <- as.data.frame(df)
    names(df) <- c("real","forecast")

    forecastplot <- ggplot2::ggplot(df,aes(1:288,df$"real"))+
      ggplot2::geom_point(colour="steelblue")+
      ggplot2::geom_line(aes(1:288,df$"forecast"),colour="red",size=1)+
      ggplot2::xlab("timestamp")+ggplot2::ylab("traffic flow")+
      ggplot2::scale_x_continuous(breaks = seq(0,288,24))+
      ggplot2::scale_y_continuous(breaks = seq(0,120,20))

    return(forecastplot)
}
