#---------------------------------------------------
# class to visualize sequencing error parameters
#---------------------------------------------------
RGInfo <- function(RGInfoFile, readGroups = c()){
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
