
#' Generic function plot data with "BAMdiagnostics" class
#'
#' @param data Data to plot that has "BAMDiagnostics" class.
#' @param ... additional parameters to pass on to the function.
#'
plotBAMdiagnostic <- function(data, ...){
  UseMethod("plotBAMdiagnostic", data);
}

################################################################################

#' Function to extract unique part from the name of each dataframe from a list of
#' dataframes
#' @param list_of_dataframes A list of dataframes with data to be plotted
#'
#' @return Names unique to each sample.
#'
.getFileNames <- function(list_of_dataframes) {
  # Create an empty vector to store file names
  file_names=c()
  # Iterate over each data frame in the list
  for (df in 1:length(list_of_dataframes)) {
    # Get the names of the data frame and append them to the file_names vector
    file_names <-c(file_names,names(list_of_dataframes[df]))
  }
  # Extract the unique part of the file names by removing everything after the
  # last underscore i.e. get the Sample name
  extracted_names <- sub("_[^_]*$", "", file_names)
  # Get the unique names from the extracted_names vector
  unique_names <- unique(extracted_names)
  # Return the unique sample names
  return(unique_names)
}


################################################################################
#' This function merges and stores the same type of data from a list of multiple dataframes
#'
#' @param dataframe_list List of dataframes containing different type of data for different
#' Samples
#'
#' @return A dataframe that contains data for allReadGroups from all samples for
#' the same data type
.merge_and_store_data <- function(dataframe_list) {
  #  checks if the length of the dataframe_list object is greater than 0 and if there
  # are any file names in dataframe_list that match the pattern given within the quation marks.

  # If the condition is true, it creates a new data frame called new_df with
  # three columns: readGroup, 'characteristic of interest', and count. The columns are initialized
  # with character(), numeric(), and numeric() respectively. The stringsAsFactors
  # argument is set to FALSE.

  # Called by .plotBAMdiagnosticMultiple function


  if (length(dataframe_list) > 0 && any(grepl("fragmentLengthHistogram\\.txt$", names(dataframe_list)))) {
    new_df <- data.frame(readGroup = character(), fragmentLength = numeric(), count = numeric(), stringsAsFactors = FALSE)

  }else if (length(dataframe_list) > 0 && any(grepl("mappingQualityHistogram\\.txt$", names(dataframe_list)))) {
    new_df <- data.frame(readGroup = character(), mappingQuality = numeric(), count = numeric(), stringsAsFactors = FALSE)
  }else if (length(dataframe_list) > 0 && any(grepl("alignedLengthHistogram\\.txt$", names(dataframe_list)))) {
    new_df <- data.frame(readGroup = character(), alignedLength = numeric(), count = numeric(), stringsAsFactors = FALSE)
  }else if (length(dataframe_list) > 0 && any(grepl("readLengthHistogram\\.txt$", names(dataframe_list)))) {
    new_df <- data.frame(readGroup = character(), readLength = numeric(), count = numeric(), stringsAsFactors = FALSE)
  }else if (length(dataframe_list) > 0 && any(grepl("softClippedLengthHistogram\\.txt$", names(dataframe_list)))) {
    new_df <- data.frame(readGroup = character(), softClippedLength = numeric(), count = numeric(), stringsAsFactors = FALSE)
  }
  for (i in 1:length(dataframe_list)){
    # Get the subset dataframe at index i
    subset_df <- dataframe_list[[i]]
    # Filter the subset dataframe to include only rows where the first column is
    # "allReadGroups"
    subset_df <- subset_df[subset_df[1]== "allReadGroups", ]
    # Extract the column name from the file path
    col_name <- sub(".*/([^/]*)$", "\\1", names(dataframe_list[i]))
    # Extract the desired part of the column name using regex
    col_name <- sub("^(.*?)_[^_]*\\.txt", "\\1", col_name)
    # Modify the values in the first column of the subset data frame
    subset_df[, 1] <- ifelse(subset_df[, 1] == "allReadGroups", paste0("allReadGroups_", col_name), subset_df[, 1])
    # Append the subset dataframe to the new data frame
    new_df <- rbind(new_df, subset_df)
  }

  return(new_df)
}


################################################################################
#' Plot data for a single Sample
#'
#' @param data Single data set to be plotted
#' @param title Title for each plot
#' @param log_scale y-axis scale is log10. At the moment this is the only option.
#' @param ... Additional parameters (optional)
#'
#' @import ggplot2
#' @importFrom rlang sym
#'
#' @return Plot for the Input data
.plotBAMdiagnosticSingle <-  function(data,title,log_scale =TRUE,...){
  # TODO: add option for no log_transformation

  read_groups <- unique(data[, 1])
  column2Name <- colnames(data)[2]

  # If there is only one sample plot the value for individual ReadGroups
  if (length(read_groups) < 3 && "allReadGroups" %in% read_groups){
    plot_1 = ggplot2::ggplot(subset(data, readGroup == "allReadGroups"), aes(x = !!sym(column2Name), y = count,fill = readGroup)) +
      geom_bar(stat = "identity", position = "dodge",alpha = 0.3) +
      geom_density(stat="identity", alpha = 0.02,aes(color = readGroup), linewidth = 0.2)   +
      labs(title = paste(stringr::str_to_title(column2Name) ,title ), x = stringr::str_to_title(column2Name), y = "log10 Count") +
      theme_minimal() +
      scale_y_log10() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.title = element_blank())
    plot(plot_1)

  } else {

    # If there are multiple samples plot the data for allReadGroups
    plot_1 = ggplot2::ggplot(data, aes(x = !!sym(column2Name) , y = count, fill = readGroup)) +
      geom_bar(stat = "identity", position = "dodge",alpha = 0.3) +
      geom_density(stat="identity", alpha = 0.02, aes(color = readGroup), linewidth = 0.2)   +
      labs(title = paste(stringr::str_to_title(column2Name) ,title), x = stringr::str_to_title(column2Name), y = "log10 Count") +
      theme_minimal() +
      scale_y_continuous(trans='log10') +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.title = element_blank())
    plot(plot_1)
  }
  return(plot_1)
}


################################################################################
#' Plot data for multiple samples
#'
#' @param data Data to be plotted
#' @param title Title for the plot
#' @param directory_name Path to the output directory.
#'
#' @importFrom grDevices dev.off pdf
#'
#' @return Plots with data for allReadGroups for all the samples
.plotBAMdiagnosticMultiple <- function(data,title,directory_name) {

  if (!dir.exists(directory_name)) {
    dir.create(directory_name)
  }
  file_names <- names(data)
  # Create an empty_list
  plot_list <- list()
  # Create a plot for each type of data from multiple samples
  fragmentLengthHistogram <- data[grep(paste0("fragmentLengthHistogram.txt", "$"), file_names)]
  alignedLengthHistogram<- data[grep(paste0("alignedLengthHistogram.txt", "$"), file_names)]
  mappingQualityHistogram<- data[grep(paste0("mappingQualityHistogram.txt", "$"), file_names)]
  readLengthHistogram<- data[grep(paste0("readLengthHistogram.txt", "$"), file_names)]
  softClippedLengthHistogram<- data[grep(paste0("softClippedLengthHistogram.txt", "$"), file_names)]

  dataframe_lists <- list()

  if (length(fragmentLengthHistogram) >= 1) {
    dataframe_lists <- append(dataframe_lists, list(fragmentLengthHistogram))
  }
  if (length(alignedLengthHistogram) >= 1) {
    dataframe_lists <- append(dataframe_lists, list(alignedLengthHistogram))
  }
  if (length(mappingQualityHistogram) >= 1) {
    dataframe_lists <- append(dataframe_lists, list(mappingQualityHistogram))
  }
  if (length(readLengthHistogram) >= 1) {
    dataframe_lists <- append(dataframe_lists, list(readLengthHistogram))
  }
  if (length(softClippedLengthHistogram) >= 1) {
    dataframe_lists <- append(dataframe_lists, list(softClippedLengthHistogram))
  }
  plots = list()

  for (i in dataframe_lists){

    data = .merge_and_store_data(i)
    #.plotBAMdiagnosticSingle(data,title)
    plot = .plotBAMdiagnosticSingle(data,title)
    #plot(plot)
    plot_list <-  append(plot_list, list(plot))
  }


  # Open a PDF file
  #pdf(paste0(directory_name,"/bamdiagnostic_plot_multiple.pdf"))
  #for (plot in plot_list){
  #  plot(plot)
  #}
  # Close PDF device
  #dev.off()
  return(plot_list)
}


################################################################################
#' Function takes data objects with "BAMdiagnostic" class and plots the data
#'
#' @param data Data to be plotted.
#' @param directory_name Path to the output directory.
#' @param ... Additional parameters (optional).
#' @importFrom grDevices dev.off pdf
#' @importFrom utils read.table
#'
#'
#' @return Plots of the input data.
plotBAMdiagnostic.BAMdiagnostic <-function(data,directory_name=NULL,...){
  Unique_filenames = .getFileNames(data)
  if (is.null(directory_name)){
    directory_name <- unique(dirname(Unique_filenames))
    directory_name <- min(directory_name)

    directory_name <- paste0(directory_name,"/plots")
  }
  if (!dir.exists(directory_name)) {
    dir.create(directory_name)
  }

  if (length(Unique_filenames)==1){
    # Open PDF device
    #pdf(paste0(directory_name,"/bamdiagnostic_plot_single.pdf"))
    title <- "Count by Read Group"
    for (file in names(data)){
      data = read.table(file,header = TRUE)
      plot <- .plotBAMdiagnosticSingle(data,title)
      #plot <- .plotBAMdiagnosticSingle(data,title)
      #file_name <- tools::file_path_sans_ext(basename(file))  # Extract file name without extension
      #plot_file_path <- paste0(directory_name,"/",file_name, "_plot.png")

      #ggsave(plot_file_path, plot, device = "png")

    }
    #return(plot)
    #dev.off()  # Close pdf device
  }
  else{
    title <- "Count by Sample"
    #.plotBAMdiagnosticMultiple(data,title,directory_name)
    plot <- .plotBAMdiagnosticMultiple(data,title,directory_name)
    #plot <-.plotBAMdiagnosticMultiple(data,title,directory_name)

    #return(plot)
  }
  return(plot)
}
###################################################################

#' Function to plot BAMDiagnostic histograms
#'
#' @param input_path Path to the folder with all BAM diagnostic *_histogram.txt files
#' @param recursive bool: To plot data from subfolders within the input folder path or not.
#' @param output_directory_name : Path to an output directory. Default is NULL and plots will
#' only be displayed not saved.
#'
#' @param ... Additional parameters. Optional.
#'
#' @return pdf and png images of the histograms
#' @export
#'
#' @examples
#' input_path <- list_example_folder()
#' plot_BAMdiagnostic(input_path, recursive = TRUE, output_directory_name = getwd())
#' plot_BAMdiagnostic(input_path, recursive = TRUE)
plot_BAMdiagnostic <- function(input_path,recursive = TRUE, output_directory_name=NULL,...){
  data <-suppressWarnings(BAMdiagnostic(input_path,recursive))

  plots = plotBAMdiagnostic(data = data,directory_name = output_directory_name, ...)

  if(!is.null(output_directory_name)){
  pdf(paste0(output_directory_name,"/BAMdiagnostic.pdf"))
  for (plot in plots){
    print(plot)

  }

  dev.off()
  }
  }

