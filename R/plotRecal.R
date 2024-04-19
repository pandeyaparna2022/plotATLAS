#' Extract data for different sequencing error covariates to plot
#'
#' @param info data of class "RGInfo"
#' @param mate mates of a paired-end sequencing read. Mate1 - 5' end of the
#' fragment , Mate2 - 2' end of the fragment.
#' @param covariate Sequencing Error Covariate
#' @param ... Additional Parameters
#'
#' @return Data extracted for the specified covariate for the specified mate
#'
#'
#' @author Aparna Pandey
.extractRecalData <- function(info, mate, covariate, ...){
  # Assess if the input covariated is supported/present in Data
  if(!covariate %in% names(info[[1]]$recal) || covariate == 'intercept' ){
    warning(paste0("Covariate ", covariate, "  is of an unsupported type!"))
    return (NULL);
  }

  # validate arguments - whether its single end or paired end
  if(mate !=1 && mate != 2){
    warning("Unknown mate ", mate, "! Only values 1 and 2 are supported.");
    return(NULL)
  } else {
  # Assign different functions to the FUN parameter dependingon the covariate
      if (covariate =='rho'){
        FUN = .extractRho
      } else {
        FUN = .extractSeqErrorCovariates
      }
  # prepare data to plot for each read group
      data <- list()
      for(r in 1:length(info)){
      # single end or paired end?
        if(mate == 1 && "Mate1" %in% names(info[[r]]$recal)){
          data[[r]] <- FUN(info[[r]]$recal$Mate1, covariate);

        } else if(mate == 1 && !"Mate1" %in% names(info[[r]]$recal)){
          data[[r]] <- FUN(info[[r]]$recal, covariate);

        } else if(mate == 2 && "Mate2" %in% names(info[[r]]$recal)){
          data[[r]] <- FUN(info[[r]]$recal$Mate2, covariate);

        } else if(mate == 2 && !("Mate2" %in% names(info[[r]]$recal))){
          data[[r]] <- list(x = NA, y = NA);

        } else {
          data[[r]] <- NA;
        }
      }
    }

  # add mate attribute to data structure
  attributes(data)$mate <- mate;
  return(data);
}


################################################################################
#' Extract the data for specified covariate
#'
#' @param info Data
#' @param covariate Sequencing Error Covariate
#'
#' @return Data for the specified covariate and the type of data (empiric vs polynomial)
#'
#' @author Aparna Pandey
.extractSeqErrorCovariates <- function(info, covariate){
  # called by function .extractRecalData.
  # Assess if the input covatiate exists in the data
  if(!covariate %in% names(info)){
    return (NULL);
  }

  # Assess if the data is empiric or polynomial
  if("empiric" %in% names(info[[covariate]])){
    return(list(type = "empiric",
                x = sapply(info[[covariate]]$empiric, "[", 1),
                y = sapply(info[[covariate]]$empiric, "[", 2))
    );
  } else if ("polynomial" %in% names(info[[covariate]])) {
    return(list(type = "polynomial",
                x = length(info[[covariate]]$polynomial),
                y = info[[covariate]]$polynomial))
  }
  else {
    # If the covariate is neither empiric nor polynomial issue a warning and return NULL
    warning(paste0("Covariate '", covariate, "' is of an unsupported type!"));
    return (NULL);
  }
}


################################################################################
#' Extract the data for 'rho' covariate
#'
#' @param info Data
#' @param covariate Sequencing Error Covariate 'rho'
#'
#' @return Data for rho covariate and the data type i.e. rho
#'
#'@author Aparna Pandey
.extractRho <- function(info,covariate){
  # called by function .extractRecalData.
  # extract specific elements from the info$rho object and stores them in the
  # data
  data = list()
  data$type = covariate
  data$y <- unlist(info$'rho')[c(2:5,7:10,12:15)];

  return(data);
}


################################################################################
#' Generic method to plot Sequencing Error covariates for objects with class "RGInfo"
#'
#' @param obj Data of class "RGInfo"
#' @param ... additional parameters
#'
#' @author Aparna Pandey
plotRecal <- function(obj, ...) {
  UseMethod("plotRecal",obj)
}


################################################################################
#' Plot covariate data from one of the mates in the readgroup. Can be mate1 or mate2.
#'
#' @param data Sequencing error data for Specified covariate. Nested list containing
#' x and y values for different readgroups for a single mate e.g., x = data[\[1]]$x, y= data[\[1]]$y
#' @param legend bool: To print the legend or not.
#' @param xlim x-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param ylim y-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param inset to indicate margins for legend.
#' @param xlab x-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param ylab y-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param main Main heading of the for the plot. Default is Null and will be generated based on data labels if no input is provided.
#' @param ... additional parameters for plotting (optional).
#'
#' @return Plot for specified Sequencing Error covariate.
#'
#'
#'@author Aparna Pandey
.plotCovariateOneMate<- function(data,
                                 legend,
                                 xlim=xlim,
                                 ylim=ylim,
                                 inset=inset,
                                 xlab = NULL,
                                 ylab = NULL,
                                 main = NULL,
                                          ...){
  # Called by plotRecalfunction
  # set labels
  if(is.null(xlab)){
    xlab <-  sub("([a-z])", "\\U\\1", gsub("([A-Z])", " \\1", attributes(data)$covariate), perl = TRUE);
  }
  if(is.null(ylab)){
    ylab <- "Coefficient";
  }
  if(is.null(main)){
    main <- paste0("Coefficients for ",xlab, " Mate ", attributes(data)$mate);
  }
  # set values for xlim
  if(is.null(xlim)){
    # extract x values from the data
    x_lim = list()
    for(rg in 1:length(data)){
      x_lim[[rg]] = data[[1]]$x
    }
    x_lim <- unlist(x_lim)
    if (all(is.na(x_lim))) {
      xlim <- c(0, 10)
    }
    else {
      xlim <-range(x_lim, na.rm = TRUE)
    }
  }

# open an empty plot
  plot(0, type='n',
       ylim = ylim,
       xlim = xlim,
       xlab = xlab,
       ylab = ylab,
       main = main,
   ...);
  # extract names of the dataset for legend and plot the x,y values
  dataset_names=list()
  for(rg in 1:length(data)){
    if(!all(is.na(data[rg]))){
      dataset_names[rg] <- names(data)[rg]
      x <-unlist(data[rg][[1]]$x)
      y <- unlist(data[rg][[1]]$y);
      lines(x, y, col = rg, lty = rg );
    }
  }

  # if legend is true and no of data sets are less than 20 plot the legend
  if (is.logical(legend) &&legend && length(dataset_names)<20) {
  legend("topright", legend = unlist(dataset_names), inset=inset, col = 1:length(data), lty = 1:length(data), cex = 0.5,xpd = TRUE)
  }
}


####################################################################################
#' To plot data for rho one mate. can be mate1 or mate2
#'
#' @param data sequencing error data for rho
#' @param legend bool: To print the legend or not.
#' @param xlim x-axis limits of a plot. Default is Null and will be set to c(1,12).
#' @param ylim y-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param inset to indicate margins for legend.
#' @param xlab x-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param ylab y-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param main Main heading of the for the plot. Default is Null and will be generated based on data labels if no input is provided.
#' @param ... additional parameters for plotting (optional).
#'
#' @return Plot for rho parameter for all base transitions
#'
#' @importFrom graphics abline axis
#'
#' @author Aparna Pandey
.plotRhoOneMate<- function(data,
                           legend,
                           xlim=xlim,
                           ylim=ylim,
                           inset,
                           xlab = NULL,
                           ylab = NULL,
                           main = NULL,
                           ...){
  # Called by plotRecalfunction
  # set labels
  if(is.null(xlab)){
    xlab <- "";
  }
  if(is.null(ylab)){
    ylab <- "Probability";
  }
  if(is.null(main)){
    main <- paste0("Relative error probabilities Rho, Mate ", attributes(data)$mate);
  }
 # open plot
  plot(0, type='n',
       ylim = ylim,
       xlim = c(1,12),
       xlab = xlab,
       ylab = ylab,
       main = main,
       xaxt = 'n')#,
  # ...);
  abline(h = 1/3, col = 'gray', lty = 2);
  abline(v = 3.5, col = 'black', lty = 1);
  abline(v = 6.5, col = 'black', lty = 1);
  abline(v = 9.5, col = 'black', lty = 1);


  # add x-axis
  bases <- c("A", "C", "G", "T");
  from <- rep(bases, each = 3);
  to <- c(bases[-1], bases[-2], bases[-3], bases[-4]);
  labels <- parse(text = paste0(from, "%->%", to));
  axis(1, at = 1:12, labels = labels, las = 2)
  dataset_names=list()


  for(rg in 1:length(data)){
    if(!all(is.na(data[rg]))){
      dataset_names[rg] <- names(data)[rg]

      y <- unname(unlist(data[[rg]]$y));
      x <- 1:length(y);
      lines(x, y, col = rg, lty = rg ,pch = rg);
    }
  }
  if (is.logical(legend) && legend && length(dataset_names)<20) {

    legend("topright", legend = dataset_names, inset = inset, col = 1:length(data), lty = 1:length(data), cex = 0.5,xpd = TRUE)
  }
}

#####################################################################################

#' Function to plot Sequencing Error Covariates that takes object with class "RGInfo" and additional parameters as input
#'
#' @param info Data in JSON format with "RGInfo" class
#' @param covariate Sequencing Error Covariate.
#' @param mate mate information to assess if sequencing was single or paired-ed. By default
#' data is parsed to access information for both mates.
#' @param legend bool : To print legend or not.
#' @param xlim x-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param ylim y-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param xlab x-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param ylab y-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param main Main heading of the for the plot. Default is Null and will be generated based on data labels if no input is provided.
#' @param multiPannel bool: to plot two plots (one for each mate) on one page or not.
#' @param ... additional parameters for plotting (optional).
#'
#' @author Aparna Pandey
plotRecal.RGInfo <- function(info,
                                 covariate,
                                 mate=c(1,2),
                                 legend = TRUE,
                                 xlim = NULL,
                                 ylim = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 main = NULL,
                                 multiPannel = TRUE,
                                 ...){
  # parse data for each mate
  data <- list();
  for(m in mate){
    data[[m]] <- .extractRecalData(info = info, mate = m, covariate = covariate);
  }
  # If no data is found issue a warning and return NULL
  if (length(data)==0){
    warning(paste0("For ",covariate,' Nothing to plot'))
    return(NULL)
  }
  # Assess if the list of data have null values , if so remove them
  # This can happen if user only wants to assess mate 2 and bypass mate1 i.e. when mate =2
  for (l in length(data):1) {
    if (is.null(data[[l]])) {
      data <- data[-l]
    }
    # Attach readgroup information to data. Used for plotting purposes.
    names(data[[l]]) <-names(info)
  }
  # Assess if the list of data have null values for any the data$type, if so remove the data
  # This can happen if the some read groups in the data lack mate 2 i.e. when mate =2
  # but some of the read groups lack mate 2
  for (i in 1:length(data)) {
    attribute <-  attributes(data[[i]])$mate
    for (j in length(data[[i]]):1) {
      if (is.null(data[[i]][[j]]$type)) {
        data[[i]] <- data[[i]][-j]
      }
    }
    # Add attributes for each read group, will be used later for plotting
    attributes(data[[i]])$mate = attribute
    attributes(data[[i]])$covariate <- covariate;
  }

  # check if there is data for at least some read groups if not exit with a warning
  if(all(unlist(lapply(data, is.na)))){
    warning("No data to plot!")
    return(NULL)
  }
  # set parameters for plotting
  if (length(mate) < 2){
    inset = c(-0.15,0)
    multiPannel= FALSE
  }
  if(multiPannel){
    par(mfrow=c(1,2));
    inset <- c(-0.4, 0);
  }
  else {
    par(mfrow=c(1,1))
    inset = c(-0.15,0)
  }

  if (legend){
    par(mar = c(5.1, 4.1, 4.1, 6))
  }

  # set defaults if not provided based on data type
  if (data[[1]][[1]]$type == "empiric"){
    if (is.null(ylim)) {
      y_lim <- unlist(lapply(data, function(x) unlist(lapply(x, function(y) unlist(y$y)))))
      ylim <- range(y_lim, na.rm = TRUE)
    }
  }
  else if (data[[1]][[1]]$type == "polynomial"){
    # if the data type is polynomial calculate the y values based on the
    # empiric function and reset x and y values in the data based on the
    # calculation
    # Note: here range is x is from 1:100, change this according to your
    # necessity
    if (is.null(xlim)) {
      x_values <- seq(1, 100)
      xlim <- range(x_values, na.rm = TRUE)
    }
    else {
      x_values <- seq(xlim[1],xlim[2])
    }
    y_values = list()
    for(m in 1:length(mate)){
      y_values[[m]] <- list()
      for (i in 1:length(data[[m]])){
        data[[m]][[i]]$x  <- x_values
        y = data[[m]][[i]]$y
        y_values[[m]][[i]]= sapply(x_values, function(i) sum(i^sapply(y, as.numeric)))
        data[[m]][[i]]$y = y_values[[m]][[i]]
      }
    }
    if(all(unlist(lapply(y_values, is.na)))){
      print("No data to plot!")
      return(NULL)
    }
    else {
      ylim <- c(min(unlist(y_values), na.rm = TRUE),max(unlist(y_values), na.rm = TRUE))
    }
  }
  else if (data[[1]][[1]]$type  == "rho"){
    if(is.null(ylim)){
      y_lim <- unlist(lapply(data, function(x) unlist(lapply(x, function(y) unlist(y$y)))))
      ylim <- range(y_lim, na.rm = TRUE)
    }
  }

  # Plot the covariate data: Call different functions depending on the covariate type
  if (length(mate) == 2){
    for (mate_idx in 1:2) {
      data_mate <- data[[mate_idx]]
      if (covariate == 'rho'){
        .plotRhoOneMate(data_mate, legend, xlim, ylim, inset)
      }
      else {
        .plotCovariateOneMate(data_mate, legend, xlim, ylim, inset)
      }
    }
  }
  else {
    data = data[[1]]
    if (covariate == 'rho'){
      .plotRhoOneMate(data,legend,xlim,ylim,inset)
    }
    else {
      .plotCovariateOneMate(data, legend, xlim, ylim, inset)
    }
  }
}


################################################################################
#' Function to plot Sequencing Error Covariates.
#'
#' @param path path to the data file.
#' @param covariate Sequencing Error Covariate of interest
#' @param mate mate information to assess if sequencing was single or paired-ed. By default
#' data is parsed to access information for both mates.
#' @param legend bool : To print legend or not.
#' @param xlim x-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param ylim y-axis limits of a plot. Default is Null and will be calculate based on the data if no input is provided.
#' @param xlab x-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param ylab y-axis label. Default is Null and will be generated based on data labels if no input is provided.
#' @param main Main heading of the for the plot. Default is Null and will be generated based on data labels if no input is provided.
#' @param multiPannel bool: to plot two plots (one for each mate) on one page or not.
#' @param readGroups read Groups to plot. By default data for all read Groups are plotted.
#' @param ... additional parameters for plotting (optional).
#'
#' @return Plots depicting data for the specified Sequencing error covariate.
#' @export
#'
#' @examples
#' path <- list_example("test.json")
#' readGroups_1 <- c("ERR8666961", "ERR8684188")
#' Covariates = list('quality','position','context','fragmentLength','mappingQuality','rho')
#' for (i in Covariates){
#' plot_SeqError_covariate(path,i,1);
#' plot_SeqError_covariate(path,i,2);
#' plot_SeqError_covariate(path,i);
#' }
#'
#' @author Aparna Pandey
plot_SeqError_covariate <-function(path,
                                   covariate,
                                   mate=c(1,2),
                                   legend = TRUE,
                                   xlim = NULL,
                                   ylim = NULL,
                                   xlab = NULL,
                                   ylab = NULL,
                                   main = NULL,
                                   multiPannel = TRUE,
                                   readGroups = c(),...){
  info <-CreateClassRGInfo(path,readGroups)
  plotRecal(info, covariate, mate, legend, xlim, ylim, xlab, ylab, main, multiPannel, ...)
}


################################################################################

