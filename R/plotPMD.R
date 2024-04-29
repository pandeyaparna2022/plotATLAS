#' Generic method to plot PMD for objects with class "RGInfo"
#'
#' @param obj A RGInfo object
#' @param \dots Further arguments that can be passed for plotting
#'
#' @author Aparna Pandey
plotPMD <- function(obj,...) {
  UseMethod("plotPMD",obj)
}


################################################################################
#' Function to plot Postmortem Damage that takes object with class "RGInfo" and additional parameters as input
#'
#' @param info data in JSON format with "RGInfo" class.
#' @param side 5(5' to 3') or 3 (3' to 5') or both.
#' @param rg_no how many read groups to plot. Default is all read Groups in the data.
#' @param legend bool : To print legend or not.
#' @param xlim x-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param ylim y-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param multiPannel bool: to plot two plots (one for each mate) on one page or not.
#' @param xlab x-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param ylab y-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param main Main heading of the for the plot. Default is Null and will be generated based on data labels if no input is provided.
#' @param ... additional parameters for plotting (optional).
#'
#' @author Aparna Pandey
plotPMD.RGInfo <- function(info,
                           side = "both",
                           rg_no=length(info),
                           legend = TRUE,
                           xlim = NULL,
                           ylim = NULL,
                           multiPannel = TRUE,
                           xlab = NULL,
                           ylab = NULL,
                           main = NULL,
                           ...){
  par(mfrow=c(1,1))
  length_info =length(info)
  for(rg in 1:length_info) {
    if(!("pmd" %in% names(info[[rg]]))) {
      warning("Data does not have PMD data.")
      return(invisible())
    }
  }

  if (!(side %in% c(3, 5, "both"))) {
    warning("Unknown side '", side, "'! Use either '3', '5', or 'both'.")
    return(invisible())
  }

  if(length_info > rg_no) {
    warning("Data is available for ",length(info)," ReadGroups. PMD will be plotted for
            only the first ", rg_no, " Readgroups." )
    # Print the message
    cat("If you want to plot specific read groups, please specify them in a vector
    while creating the RGInfo class object. For example:")
    cat('\nrg_info <- c("ERR8666961", "ERR8684188", "ERR8692891", "ERR8609272")')

    cat('\ninfo <- RGInfo("test.json", rg_info)')
    cat ('\n if the total number of the specified ReadGroups are more than 20,
         legend will not be printed: plotPMD(info,rg_no=20) \n ')

    info <- info[1:rg_no]
  }

  if(length_info < rg_no) {
    rg_no = length(info)
  }

  # ylim shared for both CT5 and GA3
  # Plot read_position vs probability for each dataset
  # Check if the ylim variable is null
  if(is.null(ylim)) {
    # If side is equal to 'both', execute the following block of code
    if(side == 'both') {
      # Calculate the maximum value of the probability distribution
      # function (pmd) across all datasets in the info list and assign
      # it to ylim
      ylim <- c(0,max(sapply(info, function(x) max(unlist(x$pmd)))))
      if(multiPannel) {
        par(mfrow=c(1,2));
      }
      # Plot the data for side "CT5" with the calculated ylim
      .plot_one_side(info, "CT5", ylim,legend,xlab,ylab,main)
      # Plot the data for side "GA3" with the calculated ylim
      .plot_one_side(info, "GA3", ylim,legend,xlab,ylab,main)
      # If side is not 'both', execute the following block of code

    }
    else {
      # Determine the specific side name based on the value of side
      # (5 maps to "CT5", other values map to "GA3")
      side <- ifelse(side == 5, "CT5", "GA3")
      # Set the multiPanel variable to FALSE
      multiPannel <- FALSE
      # Calculate the maximum value of the pmd for the specified side and assign
      # it to ylim
      ylim <- c(0,max(sapply(info, function(x) max(x$pmd[[side]]))))
      # Plot the data for the specified side with the calculated ylim
      if(multiPannel){
        par(mfrow=c(1,2));
      }
      .plot_one_side(info, side, ylim,legend,xlab,ylab,main)
    }
  }
}


################################################################################
#' To plot data for one mate. can be mate1 or mate2
#'
#' @param info data of class "RGInfo".
#' @param side 3' to 5'("GA3") or 5' to 3'("CT3").
#' @param ylim y-axis limits of a plot.
#' @param legend bool: To print the legend or not.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param main Main heading of the for the plot.
#' @param ... additional parameters for plotting (optional).
#'
#' @return plot of the Post Mortem Damage for the specified side.
#'
#' @importFrom graphics lines par
#'
#' @author Aparna Pandey
.plot_one_side<- function(info,side,ylim=ylim,legend = legend, xlab = xlab, ylab = ylab, main = main, ...

){
  # Determine xlim (max length)
  xlim <- c(0,max(sapply(info, function(x) length(x$pmd[[side]]))))
  # decide on xlab, ylab and main
  if(is.null(xlab)){
    xlab <- paste0("Position from ", substr(side, 3,3), "' end");
  }
  if(is.null(ylab)){
    ylab <- bquote(.(substr(side,1,1)) %->% .(substr(side,2,2))~Probability);
  }
  if(is.null(main)){
    main <- bquote(bold(.(substr(side,1,1)) %->% .(substr(side,2,2))~PMD~at~.(substr(side, 3,3))*"'"~ends));
  }
  position="topright"
  if(side=="GA3"){
    xlim <- rev(xlim) # flip x-axis
    position="topleft";
  }

  plot(0, type = "n",
       xlim = xlim,
       ylim = ylim,
       xlab = xlab,
       ylab = ylab,
       main = main)
  dataset_names=list()
  for(rg in 1:length(info)){
    # extract read group names for legend
    dataset_names[rg] <- names(info)[rg]

    if("pmd" %in% names(info[[rg]])){

      lines(1:length(info[[rg]]$pmd[[side]]), info[[rg]]$pmd[[side]], col = rg, lty = rg)# ,pch = rg)
    }
  }
  # Plot the legend only if the no of read groups is less than 5
  if(is.logical(legend) && legend && length(dataset_names)<20){
    legend(position, legend = dataset_names, col = 1:length(info),  lty = 1:length(info),cex = 0.5)#,pch = 1:length(info))
  }
}


################################################################################
#' Plot Post Mortem Damage
#'
#' @param path path to the data file.
#' @param side 5(5' to 3') or 3 (3' to 5') or both.
#' @param rg_no how many read groups to plot. Default is all read Groups in the data.
#' @param legend bool : To print legend or not.
#' @param xlim x-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param ylim y-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param multiPannel bool: to plot two plots (one for each mate) on one page or not.
#' @param xlab x-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param ylab y-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param main Main heading of the for the plot. Default is Null and will be generated based on data labels if no input is provided.
#' @param readGroups read Groups to plot. By default data for all read Groups are plotted.
#' @param ... additional parameters for plotting (optional).
#'
#' @return plots depicting postmortem damage.
#' @export
#'
#' @note readGroups need to be provided as 'readGroups = c(some vector)' or 'readGroups = some_variable'. See examples below.
#'
#' @examples
#' path <- list_example("test.json")
#' readGroups_1 <- c("ERR8666961", "ERR8684188")
#' plot_PMD(path,readGroups=readGroups_1)
#' plot_PMD(path)
#' plot_PMD(path,3,readGroups=readGroups_1)
#' plot_PMD(path,3)
#' plot_PMD(path,5,readGroups=readGroups_1)
#' plot_PMD(path,5)
#'
#'@author Aparna Pandey
plot_PMD <-function(path, side = "both",
                    rg_no=length(info),
                    legend = TRUE,
                    xlim = NULL,
                    ylim = NULL,
                    multiPannel = TRUE,
                    xlab = NULL,
                    ylab = NULL,
                    main = NULL,
                    readGroups = c(),
                    ...){
  info <-CreateClassRGInfo(path,readGroups)
  pdf("./pmd.pdf")
  plotPMD(info,side,rg_no,legend,xlim,ylim,multiPannel,xlab,ylab,main,...)
  dev.off()
}


################################################################################
