
# Path, there data files are"
path = "~/git/coursera-data-science/r-programming/week2-programming/specdata"
date_format = "%Y-%m-%d"
# data reader function
read_single_file = function(path, single_id) {
  # Reading id's of existing data files.
  files_ids = gsub(".csv", "", dir(path))
  files_ids_int = as.integer(files_ids)
  if (single_id %in% files_ids_int)
  {
    data = read.csv(paste(path, "/", files_ids[match(single_id, files_ids_int)], ".csv", sep=""), header=TRUE)
    data$Date = as.Date(data$Date, date_format)
    data 
  }
  else
  {
    FALSE
  }
}
read_multiple_files = function(path, id=0, all=FALSE)
{
  if (all)
  {
    id = as.integer(gsub(".csv", "", dir(path)))
  }
  data_frames = lapply(id, function(id_item){
    read_single_file(path, id_item)
  })
  data_frame = do.call(rbind, data_frames)
  data_frame
}

pullutant_mean = function(directory, pollutant, id = 1:332) {
  if (!(pollutant %in% c("sulfate" , "nitrate")))
  {
    stop(
      paste(
        "pollutant argument should be 'sulfate' or 'nitrate', but it's '", 
        pollutant, "'", sep=""
      )
    )
  }
  data_frame = read_multiple_files(directory, id)
  pollutant_column = data_frame[[pollutant]]
  non_na_pollutant_column = pollutant_column[!is.na(pollutant_column)]
  mean(non_na_pollutant_column)
}

complete = function(directory, id=0, all=FALSE){
  x = read_multiple_files(directory, id, all)
  ids = x[["ID"]]
  complete_ids = ids[(!is.na(x[["sulfate"]])) & (!is.na(x[['nitrate']]))]
  df = data.frame(table(as.character(complete_ids)))
  names(df) = c("id", "nobs")
  df[order(df$id, decreasing=TRUE),]
}

complete_single = function(directory, single_id) {
  x = read_single_file(directory, single_id)
  subset(x, (!is.na(x[["sulfate"]])) & (!is.na(x[['nitrate']])))
  
}

corr = function(directory, threshold=0){
  ids = as.integer(gsub(".csv", "", dir(directory)))
  data_frames = Filter(lapply(ids, function(id_item){
    complete = complete_single(path, id_item)
    complete
  }), f=function(df) { nrow(df) > threshold })
  cors = unlist(lapply(data_frames, function(data_frame){
    cor(data_frame[["nitrate"]], data_frame[["sulfate"]])
  }))
  cors
}

#print(paste("pullutant_mean:", pullutant_mean(path, "nitrate", 23)))
#print("complete:")
#print(complete(path, 3))
#print("Corr:")
#print(corr(path, 150))
#cr = corr(path, 400)
#print(head(cr))
#print(summary(cr))
#print(pullutant_mean(path, "nitrate"))

#set.seed(42)
#cc <- complete(path, 332:1)
#use <- sample(332, 10)
#print(cc[use, "nobs"])

#cr <- corr(path)                
#cr <- sort(cr)                
#set.seed(868)                
#out <- round(cr[sample(length(cr), 5)], 4)
#print(out)

#cr <- corr(path, 129)                
#cr <- sort(cr)                
#n <- length(cr)                
#set.seed(197)                
#out <- c(n, round(cr[sample(n, 5)], 4))
#print(out)

cr <- corr(path, 2000)                
n <- length(cr)                
cr <- corr(path, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
