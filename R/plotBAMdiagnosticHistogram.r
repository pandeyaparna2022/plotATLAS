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
#' @examples
#' input_folder <- list_example_folder()
#' data <- BAMdiagnostic(input_folder)
#' Unique_filenames = .getFileNames(data)
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
#' @return Plot for the Input data
.plotBAMdiagnosticSingle <-  function(data,title,log_scale =TRUE,...){
  # TODO: add option for no log_transformation

  read_groups <- unique(data[, 1])
  column2Name <- colnames(data)[2]

  # If there is only one sample plot the value for individual ReadGroups
  if (length(read_groups) < 3){
    plot=ggplot(subset(data, readGroup == "allReadGroups"), aes(x = !!sym(column2Name), y = count,fill = readGroup)) +
      geom_bar(stat = "identity", position = "dodge",alpha = 0.3) +
      geom_density(stat="identity", alpha = 0.02,aes(color = readGroup), linewidth = 0.2)   +
      labs(title = paste(str_to_title(column2Name) ,title ), x = str_to_title(column2Name), y = "log10 Count") +
      theme_minimal() +
      scale_y_log10() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.title = element_blank())

  }
  else{

    # If there are multiple samples plot the data for allReadGroups
    plot=ggplot(data, aes(x = !!sym(column2Name) , y = count, fill = readGroup)) +
      geom_bar(stat = "identity", position = "dodge",alpha = 0.3) +
      geom_density(stat="identity", alpha = 0.02,aes(color = readGroup), linewidth = 0.2)   +
      labs(title = paste(str_to_title(column2Name) ,title), x = str_to_title(column2Name), y = "log10 Count") +
      theme_minimal() +
      scale_y_continuous(trans='log10') +

      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.title = element_blank())

  }
  return(plot)
}


################################################################################
#' Plot data for multiple samples
#'
#' @param data Data to be plotted
#' @param title Title for the plot
#'
#' @return Plots with data for allReadGroups for all the samples
.plotBAMdiagnosticMultiple <- function(data,title) {

  file_names <- names(data)
  directory_name <- unique(dirname(file_names))
  directory_name <- min(directory_name)
  # Creates a directory named plots within the directory where the input files are
  # to store the plots
  directory_name <- paste0(directory_name,"/plots")

  if (!dir.exists(directory_name)) {
    dir.create(directory_name)
  }

  # Create a plot for each type of data from multiple samples
  fragmentLengthHistogram <- data[grep(paste0("fragmentLengthHistogram.txt", "$"), file_names)]
  alignedLengthHistogram<- data[grep(paste0("alignedLengthHistogram.txt", "$"), file_names)]
  mappingQualityHistogram<- data[grep(paste0("mappingQualityHistogram.txt", "$"), file_names)]
  readLengthHistogram<- data[grep(paste0("readLengthHistogram.txt", "$"), file_names)]
  softClippedLengthHistogram<- data[grep(paste0("softClippedLengthHistogram.txt", "$"), file_names)]

  pdf(paste0(directory_name,"/bamdiagnostic_plot_multiple.pdf")) # Open a PDF file
  data = .merge_and_store_data(fragmentLengthHistogram)
  plot= .plotBAMdiagnosticSingle(data,title)
  plot(plot)
  plot_file_path <- paste0(directory_name,"/fragmentLengthHistogram", "_plot.png")  # Define plot file path
  ggsave(plot_file_path, plot, device = "png")

  data = .merge_and_store_data(alignedLengthHistogram)
  plot= .plotBAMdiagnosticSingle(data,title)
  plot(plot)
  plot_file_path <- paste0(directory_name,"/alignedLengthHistogram", "_plot.png")  # Define plot file path
  ggsave(plot_file_path, plot, device = "png")

  data = .merge_and_store_data(mappingQualityHistogram)
  plot = .plotBAMdiagnosticSingle(data,title)
  plot(plot)
  plot_file_path <- paste0(directory_name,"/mappingQualityHistogram", "_plot.png")  # Define plot file path
  ggsave(plot_file_path, plot, device = "png")

  data = .merge_and_store_data(readLengthHistogram)
  plot = .plotBAMdiagnosticSingle(data,title)
  plot(plot)
  plot_file_path <- paste0(directory_name,"/readLengthHistogram", "_plot.png")  # Define plot file path
  ggsave(plot_file_path, plot, device = "png")

  data = .merge_and_store_data(softClippedLengthHistogram)
  plot = .plotBAMdiagnosticSingle(data,title)
  plot(plot)
  plot_file_path <- paste0(directory_name,"/softClippedLengthHistogram", "_plot.png")  # Define plot file path
  ggsave(plot_file_path, plot, device = "png")
  dev.off()  # Close PDF device
}


################################################################################
#' Function takes data objects with "BAMdiagnostic" class and plots the data
#'
#' @param data Data to be plotted
#' @param ... Additional parameters (optional).
#'
#' @return Plots of the input data.
#' @export
plotBAMdiagnostic.BAMdiagnostic <-function(data,...){
  Unique_filenames = .getFileNames(data)
  directory_name <- unique(dirname(Unique_filenames))
  directory_name <- min(directory_name)

  directory_name <- paste0(directory_name,"/plots")

  if (!dir.exists(directory_name)) {
    dir.create(directory_name)
  }

  if (length(Unique_filenames)==1){
    # Open PDF device
    pdf(paste0(directory_name,"/bamdiagnostic_plot_single.pdf"))
    title <- "Count by Read Group"

    for (file in names(data)){
      data = read.table(file,header = TRUE)
      plot <- .plotBAMdiagnosticSingle(data,title)
      plot(plot)

      file_name <- tools::file_path_sans_ext(basename(file))  # Extract file name without extension
      plot_file_path <- paste0(directory_name,"/",file_name, "_plot.png")

      ggsave(plot_file_path, plot, device = "png")

    }
    dev.off()  # Close pdf device
  }
  else{
    title <- "Count by Sample"

    plot <-.plotBAMdiagnosticMultiple(data,title)

  }
}
