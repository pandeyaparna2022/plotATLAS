#' A constructor to load data in JSON format and  assign it "RGInfo" class
#'
#' @param RGInfoFile The path to the JSON file containing the data.
#' @param readGroups A character vector specifying the read groups to plot from the JSON data.
#'
#' @return The JSON data loaded from the file, filtered based on the specified read groups (if any), and assigned the class "RGInfo".
#'
#' @export
#'
#' @usage CreateClassRGInfo(RGInfoFile,readGroups)
#'
#' @examples
#' RGInfoFile <- list_example("test.json")
#' readGroups_1 <- c("ERR8666961", "ERR8684188")
#' data_1 <- CreateClassRGInfo(RGInfoFile,readGroups_1)
#' data_2 <- CreateClassRGInfo(RGInfoFile)
#'
#' @author Aparna Pandey

CreateClassRGInfo <- function(RGInfoFile , readGroups = c()){
  # Check if the file has a ".json" extension
  if (!endsWith(tolower(RGInfoFile), ".json")) {
    message("Skipping file ", RGInfoFile, " because it is not in JSON format")
    return(NULL)
  }

  # open json file
  js <- rjson::fromJSON(file = RGInfoFile);

  # To limit to specified read groups
  # Filter the JSON data to only include keys specified in the readGroups vector

  if(length(readGroups) > 0){
    js <- js[names(js) %in% readGroups]
  }
  # assign the class "RGInfo" to the variable json data stored in js variable
  class(js) <- "RGInfo";
  # add attributes to data structure
  # add attribute named "file" to the js object and
  # assigns it the value of the variable RGInfoFile  i.e. original input
  attributes(js)$file <- RGInfoFile;

  return(js);
}


################################################################################
#' A constructor to load all the *Histogram.txt files in the input folder and
#' assign the data "BAMdiagnostic" class
#'
#' @param folder_path Folder containing output of ATLAS "BAMDiagnostics" task.
#' @param recursive bool: To ckeck in subfolders to finds that fit the criteria.
#' Default is TRUE.
#'
#' @return A filtered dataframe with all the data for all the *Histogram.txt files
#' in the  input folder that match certain criteria such as file format, fragment
#' length of reads <=1500 and column names within the files
#'
#' @export
#'
#' @examples
#' input_folder <- list_example_folder()
#' data <- BAMdiagnostic(input_folder)
BAMdiagnostic <- function(folder_path=getwd(),recursive = TRUE) {
  # List the only '.txt' files in the folder
  filenames <- list.files(path = folder_path, full.names
                          = TRUE, recursive = recursive,
                          pattern = "\\.txt")
  # used to avoid listing sub folders, not required if pattern is
  # specified
  filenames <- filenames[!file.info(filenames)$isdir]

  # Check if filenames is empty.
  if (length(filenames) == 0) {
    warning("There are no files in the directory that fit the required format.")
    return(NULL)
  }
  # Initialize an empty vector to store the names of the files that meet certain
  # conditions
  filtered_names <- c()
  # Iterate over each element of filenames
  for (file_path in filenames) {
    # Check if the file has a ".txt" extension and matches the required pattern
    if (tools::file_ext(file_path) == "txt" && grepl("Histogram.txt$", file_path)) {
      temp_data <- read.table(file_path, header = TRUE)
      # Check if the file has 3 columns and specific column names
      if (ncol(temp_data) == 3 && all(c("readGroup", "count") %in% colnames(temp_data))) {
        filtered_names <-c(filtered_names,file_path)
      }
      else {
        warning(paste("Invalid file format for", file_path,": Class will not be modified"))
      }
    }
  }
  # Initialize a list to store the data frames
  data_frames <- list()
  for (i in seq_along(filtered_names)) {
    file_path <- filtered_names[i]
    data_frames[[i]] <- read.table(file_path, header = TRUE)
    # Add "BAMdiagnostic" class to each data frame
    class(data_frames[[i]]) <- c("BAMdiagnostic", class(data_frames[[i]]))
    # Assign the file name as the name of the data frame
    names(data_frames)[i] <- file_path
  }

  # remove dataframes that have no data
  filtered_dataframes <- data_frames[sapply(data_frames, function(df) {
    class(df)[1] == "BAMdiagnostic" & nrow(df) > 0
  })]
  # remove  fragmentLengthHistogram dataframes here the fragment lengths exceed 1500
  filtered_dataframes <- filtered_dataframes[sapply(names(filtered_dataframes), function(name) {
    fragmentLengthHistogram <- grepl("fragmentLengthHistogram.txt$", name)

    # exclude fragmentLengthHistogram data where the fragment lengths exceed 1500
    excluded_names <-c()
    if (fragmentLengthHistogram) {
      max_value <- max(filtered_dataframes[[name]][, 2])
      if (max_value > 1500) {
        excluded_names <- c(excluded_names, name)  # Append the name to the excluded_names vector
      return(FALSE)  # Exclude the dataframe from the list
      }
     }
    return(TRUE)  # Include the dataframe in the list
    }
  )
  ]
  # Create a vector containing the names of the filtered dataframes
  filtered_dataframes_names <- sapply(filtered_dataframes, function(df) deparse(substitute(df)))

  # Create a vector containing the names of the excluded dataframes
  removed_dataframes <- names(data_frames)[!sapply(names(data_frames), function(element) element %in% names(filtered_dataframes))]

  if ((length(removed_dataframes)) > 0) {
    print(paste("The following ",length(removed_dataframes)," file/s are excluded because either they are empty or do",
                "not align with the required format.", collapse = " "))
  }
  print(removed_dataframes)

  # Add the class "BAMdiagnostic" to the filtered dataframes
  class(filtered_dataframes) <- c("BAMdiagnostic", class(filtered_dataframes))

  # add attribute named "files" to the filtered_dataframes object and
  # assigns it the value of the variable filtered_dataframes  i.e. the filtered data
  attributes(filtered_dataframes)$files <- filtered_dataframes

  return(filtered_dataframes)
}


################################################################################
#' List example files
#'
#' This function lists available example files, found in the `inst/extdata`
#'  directory.
#'
#' @param path The name of an example file. If not provided, the function lists
#'  the available example files.
#'
#' @returns A character vector.
#'
#' @examples
#' list_example() # Lists example files contained in the package
#' list_example("test.json") # Lists the path of the specified file
#' @export
#'
#' @source This function was adapted from [readxl::readxl_example()] and
#'  originally created by *Hadley Wickham & Jennifer Bryan*.
list_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "plotATLAS"))
  } else {
    system.file("extdata", path, package = "plotATLAS", mustWork = TRUE)
  }
}


#' Title
#'
#' @param path The path containing output of ATLAS "BAMDiagnostics" task. If not
#' provided, the function returns the path to the folder containming available
#' example files.
#'
#' @return Path containing example files.
#' @export
#'
#' @examples
#' list_example_folder()
list_example_folder <- function(path = NULL) {
  if (is.null(path)) {
    return(normalizePath(system.file("extdata", package = "plotATLAS"), mustWork = TRUE))
  } else {
    return(normalizePath(system.file("extdata", path, package = "plotATLAS"), mustWork = TRUE))
  }
}

