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
    dir(system.file("extdata", package = "plotJSON"))
  } else {
    system.file("extdata", path, package = "plotJSON", mustWork = TRUE)
  }
}
