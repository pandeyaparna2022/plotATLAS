## code to prepare `RGInfo` dataset goes here
RGInfoFile <- list_example("test.json")
readGroups = c("ERR8666961", "ERR8684188")
exampleData  <- CreateClassRGInfo(RGInfoFile = RGInfoFile, readGroups= readGroups )
usethis::use_data(exampleData , overwrite = TRUE)
