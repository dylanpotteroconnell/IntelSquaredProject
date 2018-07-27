grab.results.url <- function(url){
  print(url)
  x <- readLines(url)
  results <- gsub("(.*)online.*", "\\1", x[grep("debate_vote_results", x)])
  pre <- gsub(".*pre(.*)post.*", "\\1", results)
  pre.results <- as.numeric(unlist(regmatches(pre, gregexpr("\\d+\\.*\\d*", pre))))
  post <- gsub(".*pre.*(post.*)", "\\1", results)
  post.temp <- gsub("post\":\\{\"f\":(\\d+\\.*\\d*),\"a\":(\\d+\\.*\\d*),\"u\":(\\d+\\.*\\d*),.*", "\\1 \\2 \\3", post)
  post.results <- as.numeric(unlist(strsplit(post.temp, split=" ")))
  post
#  post.temp.was.for <- gsub(".*\"f\":\\{\"f\":(\\d+\\.*\\d*),\"a\":(\\d+\\.*\\d*),\"u\":(\\d+\\.*\\d*)\\}.*", "\\1 \\2 \\3", post)
  post.temp.was.for <- gsub(".*\"f\":\\{\"f\":(\\d+\\.*\\d*),\"a\":(\\d+\\.*\\d*),\"u\":(\\d+\\.*\\d*).*", "\\1 \\2 \\3", post)
  post.was.for <- as.numeric(unlist(strsplit(post.temp.was.for, split=" ")))
  post.temp.was.aga <- gsub(".*,\"a\":\\{\"f\":(\\d+\\.*\\d*),\"a\":(\\d+\\.*\\d*),\"u\":(\\d+\\.*\\d*)\\}.*", "\\1 \\2 \\3", post)
  post.was.aga <- as.numeric(unlist(strsplit(post.temp.was.aga, split=" ")))
  post.temp.was.und <- gsub(".*\"u\":\\{\"f\":(\\d+\\.*\\d*),\"a\":(\\d+\\.*\\d*),\"u\":(\\d+\\.*\\d*)\\}\\}.*", "\\1 \\2 \\3", post)
  post.was.und <- as.numeric(unlist(strsplit(post.temp.was.und, split=" ")))
  return(list(pre.results=pre.results, post.results=post.results,
              post.was.for=post.was.for, post.was.aga=post.was.aga,
              post.was.und=post.was.und))
}


assemble.df <-  function(inputs,rows.to.skip=0, results.df){
  for(i in (rows.to.skip+1):nrow(inputs)){
    z <- grab.results.url(inputs[i,"url"])
    results.df[nrow(results.df)+1,] <- NA
    results.df[nrow(results.df),]$date <- inputs[i,"date"]
    results.df[nrow(results.df),]$url <- inputs[i, "url"]
    results.df[nrow(results.df),]$name <- inputs[i, "name"]
    results.df[nrow(results.df),4:18] <- c(z$pre.results, z$post.results,
                           z$post.was.for, z$post.was.aga, z$post.was.und)
    write.csv(results.df, file="votingresults.csv", row.names=FALSE)
  }
  return(results.df)
}

results.df <- data.frame(date=character(), url=character(), name=character(),
                         pre.for=numeric(), pre.aga=numeric(), pre.und=numeric(),
                         post.for=numeric(), post.aga=numeric(), post.und=numeric(),
                         for.to.for=numeric(), for.to.aga=numeric(), for.to.und=numeric(),
                         aga.to.for=numeric(), aga.to.aga=numeric(), aga.to.und=numeric(),
                         und.to.for=numeric(), und.to.aga=numeric(), und.to.und=numeric(),
                         stringsAsFactors=FALSE)
results.df <- read.csv("votingresults.csv", as.is=TRUE)
inputs <- read.table("inputdata.csv", sep=",", as.is=TRUE, header=TRUE)
results.df <- assemble.df(inputs,rows.to.skip=nrow(results.df), results.df=results.df)
View(results.df)
write.csv(results.df, file="votingresults.csv", row.names=FALSE)


#### Reworking data
# Results by the Intel Squared metric
results.df$abs.change.for <- results.df$post.for - results.df$pre.for
results.df$abs.change.aga <- results.df$post.aga - results.df$pre.aga
results.df$abs.change.und <- results.df$post.und - results.df$pre.und
results.df$winner.orig <- NA
results.df$winner.orig[results.df$abs.change.for > results.df$abs.change.aga] <- TRUE
results.df$winner.orig[results.df$abs.change.for < results.df$abs.change.aga] <- FALSE

# Calculating the proportional changes
# For those originally for
results.df$for.to.for.prop <- results.df$for.to.for/results.df$pre.for
results.df$for.to.aga.prop <- results.df$for.to.aga/results.df$pre.for
results.df$for.to.und.prop <- results.df$for.to.und/results.df$pre.for

# For those originally against
results.df$aga.to.for.prop <- results.df$aga.to.for/results.df$pre.aga
results.df$aga.to.aga.prop <- results.df$aga.to.aga/results.df$pre.aga
results.df$aga.to.und.prop <- results.df$aga.to.und/results.df$pre.aga

# For those originally undecided

results.df$und.to.for.prop <- results.df$und.to.for/results.df$pre.und
results.df$und.to.aga.prop <- results.df$und.to.aga/results.df$pre.und
results.df$und.to.und.prop <- results.df$und.to.und/results.df$pre.und




write.csv(results.df, "votingresultsfinal.csv", row.names=FALSE)

