#' @title Performing Steel-Dwass test
#' @description \code{WCtest} set the slack information to system
#' @importFrom exactRankTests wilcox.exact
#' @param dt data set with ID (column 1) and stable isotope ratios (column 2~n)
#' @param save_results if you want to save results as .txt file, put TRUE here
#' @export
#' @examples
#' # mydata <- read.csv("All.csv", header = T)
#' # Wilctest(mydata, TRUE)

WCtest <- function(dt, save_results){

  lis <- list()

  Group <- dt[,1]
  Group_list <- unique(Group)

  if(length(Group_list) > 2) {
    stop(paste("***Error: Wilcoxon Rank Test can only run with 2 groups"))
  }

  for (j in 2:ncol(dt)) {

    dt[,j] <- round(dt[,j], digits = 1)

    for (i in 1:length(Group_list)) {

      Group_i <- Group_list[i]
      dt_iso <- dt[dt[,1] == Group_i,]
      lis[[i]] <- dt_iso[,j]

    }

    results <- wilcox.exact(lis, paired = F)
    file.name <- sprintf("wilcox.exact_%s.txt", colnames(dt)[j])

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
