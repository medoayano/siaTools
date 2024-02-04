#' @title Performing Steel-Dwass test
#' @description \code{SDtest} set the slack information to system
#' @importFrom NSM3 pSDCFlig
#' @param dt data set with ID on column 1 and stable isotope ratios after on column 2
#' @param save_results if you want to save results as .txt file, put TRUE here
#' @export
#' @examples
#' # mydata <- read.csv("All.csv", header = T)
#' # SDtest(mydata, TRUE)

SDtest <- function(dt, save_results){

  lis <- list()

  Group <- dt[,1]
  Group_list <- unique(Group)

  for (j in 2:ncol(dt)) {

    dt[,j] <- round(dt[,j], digits = 1)

    for (i in 1:length(Group_list)) {

      Group_i <- Group_list[i]
      dt_iso <- dt[dt[,1] == Group_i,]
      lis[[i]] <- dt_iso[,j]

    }

    results <- pSDCFlig(lis, method="Asymptotic")
    file.name <- sprintf("%s.txt", colnames(dt)[j])

    if(save_results == TRUE){
      sink(file.name, append = TRUE)
      print(results)
      sink()
    } else if (save_results == FALSE) {
      print(colnames(dt_iso)[j])
      print(results)
      print(lis)
    }

  }
}
