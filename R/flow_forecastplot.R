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
    df[3] <- 1:288
    names(df) <- c("real","forecast","timestamp")
    df <- reshape2::melt(df,id.vars="timestamp")

    forecastplot <- ggplot2::ggplot(df,ggplot2::aes(x=timestamp,y=value,group=variable,color=variable))+
      geom_point()+geom_line()+
      scale_color_manual(values=c("steelblue","red"))+
      xlab("Timestamp")+ylab("Traffic volume")+labs(color="Legend")+
      scale_x_continuous(breaks = seq(0,288,24))+
      scale_y_continuous(breaks = seq(0,120,20))+
      theme_bw()

    return(forecastplot)
}
