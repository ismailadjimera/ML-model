####################################################################
#' Convert year month format YYYY-MM
#' 
#' This function lets the user convert a date into YYYY-MM format
#' 
#' @param date Date. Date we wish to transform 
#' @export
year_month <- function(date) {
  
  return(paste(
    lubridate::year(date),
    stringr::str_pad(lubridate::month(date), 2, pad = "0"),
    sep="-"))
}


####################################################################
#' Convert year week format YYYY-WW
#' 
#' This function lets the user convert a date into YYYY-WW format
#' 
#' @param date Date. Date we wish to transform
#' @export
year_week <- function(date) {
  
  return(paste(
    lubridate::year(date),
    stringr::str_pad(lubridate::week(date), 2, pad = "0"),
    sep="-"))
}


####################################################################
#' Count Categories on a Dataframe
#' 
#' This function lets the user count unique values in a categorical 
#' dataframe
#'
#' @param df Categorical Vector
#' @export
categoryCounter <- function (df) {
  
  cats <- df %>% select_if(is.character)
  result <- c()
  
  for (i in 1:ncol(cats)) {
    x <- freqs(cats, cats[,i])
    y <- colnames(cats)[i]
    x <- cbind(variable = y, x)
    result <- rbind(result, x)
  }
  result <- rename(result, category = `cats[, i]`)
  return(result)
}


####################################################################
#' Reduce categorical values
#' 
#' This function lets the user reduce categorical values in a vector. 
#' It is tidyverse friendly for use on pipelines
#' 
#' @param df Categorical Vector
#' @param ... Variables. Which variable do you wish to reduce?
#' @param nmin Integer. Number of minimum times a value is repeated
#' @param pmin Numerical. Porcentage of minimum times a value is repeated
#' @param pcummax Numerical. Top cumulative porcentage of most 
#' repeated values
#' @param top Integer. Keep the n most frequently repeated values
#' @param other_label Character. With which text do you wish to replace 
#' the filtered values with?
#' @export
categ_reducer <- function(df, ...,
                          nmin = 0, 
                          pmin = 0, 
                          pcummax = 100, 
                          top = NA, 
                          other_label = "other") {
  
  vars <- quos(...)
  
  dff <- df %>%
    group_by(!!!vars) %>%
    tally() %>% arrange(desc(n)) %>%
    mutate(p = round(100*n/sum(n),2), pcum = cumsum(p))
  
  if (!is.na(top)) {
    tops <- dff %>% slice(1:top)
  } else {
    tops <- dff %>% filter(n >= nmin & p >= pmin & p <= pcummax) 
  }
  
  name <- as.name(names(dff[,1]))
  vector <- df %>% select(as.character(name))
  new_vector <- ifelse(vector[[as.character(name)]] %in% tops[[as.character(name)]], 
                       as.character(vector[[as.character(name)]]), 
                       other_label)
  df[[as.character(name)]] <- new_vector
  
  return(df)
  
}


####################################################################
#' Normalize values
#' 
#' This function lets the user normalize numerical values into 
#' the 0 to 1 range
#' 
#' @param x Numeric Vector. Numbers to be transformed into 
#' normalized vector
#' @export
normalize <- function(x) {
  if (is.numeric(x)) {
    x <- (x-min(x)) / (max(x)-min(x))
    return(x) 
  } else {
    stop("Try with a numerical vector!")
  }
}


####################################################################
#' Convert a vector into a comma separated text
#' 
#' Convert a vector into a comma separated text
#' 
#' @param vector Vector. Vector with more than 1 observation
#' @param sep Character. String text wished to insert between values
#' @param quotes Boolean. Bring simple quotes for each 
#' observation (useful for SQL)
#' @export
vector2text <- function(vector, sep=", ", quotes = TRUE) {
  output <- paste(shQuote(vector), collapse=sep)
  if (quotes == FALSE) {
    output <- gsub("'", "", output)
  }
  return(output)
}


####################################################################
#' Clean text
#' 
#' This function lets the user clean text into getting only alphanumeric 
#' characters and no accents/symbols on letters.
#' 
#' @param text Character Vector
#' @param spaces Boolean. Keep spaces?
#' @export
cleanText <- function(text, spaces = TRUE) {
  text <- as.character(text)
  output <- tolower(gsub("[^[:alnum:] ]", "", 
                         iconv(text, from="UTF-8", to="ASCII//TRANSLIT")))
  if (spaces == FALSE) {
    output <- gsub(" ", "", output)
  }
  return(output)
}


####################################################################
#' Find country from a given IP
#' 
#' This function lets the user find a country from a given IP Address
#' 
#' @param ip Vector. Vector with all IP's we wish to search
#' @export
ip_country <- function(ip) {
  
  # require(rvest)
  # require(dplyr)
  
  ip <- ip[!is.na(ip)]
  ip <- ip[grep("^172\\.|^192\\.168\\.|^10\\.", ip, invert = T)]
  
  countries <- data.frame(ip = c(), country = c())
  for(i in 1:length(ip)) {
    message(paste("Searching for", ip[i]))
    url <- paste0("https://db-ip.com/", ip[i])
    scrap <- read_html(url) %>% html_nodes('.card-body tr') %>% html_text()
    country <- gsub("Country", "", trimws(scrap[grepl("Country", scrap)]))
    result <- cbind(ip = ip[i], country = country)
    countries <- rbind(countries, result)
  } 
  return(countries)
}


####################################################################
#' Distance from specific point to line
#' 
#' This function lets the user calculate the mathematical linear distance 
#' Between a specific point and a line (given geometrical 3 points)
#' 
#' @param a Vector. Coordinates of the point from which we want to 
#' measure the distance
#' @param b Vector. Coordinates of 1st point over the line
#' @param c Vector. Coordinates of 2st point over the line
#' @export
dist2d <- function(a, b = c(0, 0), c = c(1, 1)) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1, v2)
  d <- abs(det(m)) / sqrt(sum(v1 * v1))
}


####################################################################
#' Nicely Format Numerical Values
#' 
#' This function lets the user format numerical values nicely
#' 
#' @param x Numerical Vector
#' @param decimals Integer. Amount of decimals to display
#' @param type Integer. 1 for International standards. 2 for 
#' American Standards.  
#' @param scientific Boolean. Scientific notation
#' @export
formatNum <- function(x, decimals = 2, type = 1, scientific = FALSE) {
  if (scientific == FALSE) {
    options(scipen=999)
  } else {
    x <- formatC(numb, format = "e", digits = 2)
  }
  if (type == 1) {
    x <- format(round(as.numeric(x), decimals), nsmall=decimals, 
                big.mark=".", decimal.mark = ",")
  } else {
    x <- format(round(as.numeric(x), decimals), nsmall=decimals, 
                big.mark=",", decimal.mark = ".") 
  }
  return(trimws(x))
}


####################################################################
#' One Hot Encoding for a Vector with Comma Separated Values
#' 
#' This function lets the user do one hot encoding on a variable with 
#' comma separated values
#' 
#' @param df Dataframe. May contain one or more columns with comma separated
#' values which will be separated as one hot encoding
#' @param variables Character. Which variables should split into new columns?
#' @param sep Character. Which character separates the elements?
#' @export
one_hot_encoding_commas <- function(df, variables, sep=","){
  # Note that variables must be provided in strings
  for (var in seq_along(variables)) {
    variable <- variables[var]
    df[as.character(df) == "" | is.na(df)] <- "NAs" # Handling missingness
    x <- as.character(df[[variable]])
    x <- gsub(", ", ",", toString(x)) # So it can split on strings like "A1,A2" and "A1, A2"
    vals <- unique(unlist(strsplit(x, sep)))
    x <- paste(variable, vals, sep="_")
    new_columns <- sort(as.character(x))
    if (length(new_columns) >= 15) {
      message(paste("You are using more than 15 unique values on this variable:", variable))
    }
    for (i in seq_along(new_columns)){
      df$temp <- NA
      df$temp <- ifelse(grepl(vals[i], df[[variable]]), TRUE, FALSE)
      colnames(df)[colnames(df) == "temp"] <- new_columns[i]
    }
  }
  return(df)
}


####################################################################
#' Balance Binary Data by Resampling: Under-Over Sampling
#' 
#' This function lets the user balance a given data.frame by resampling
#' with a given relation rate and a binary feature.
#' 
#' @param df Vector or Dataframe. Contains different variables in each 
#' column, separated by a specific character
#' @param variable Character. Which binary variable should we use to resample df
#' @param rate Numeric. How many X for every Y we need? Default: 1. If there are
#' more than 2 unique values, rate will represent percentage for number of rows
#' @param seed Numeric. Seed to replicate and obtain same values
#' @export
balance_data <- function(df, variable, rate = 1, seed = 0) {
  
  # require(dplyr)
  
  set.seed(seed)
  
  names(df)[names(df) == variable] <- 'tag'
  tags <- unique(df$tag)
  
  if (length(tags) != 2) {
    # For numerical resampling:
    samp <- round(rate * nrow(df))
    balanced <- sample_n(df, samp)
    if (nrow(df) != samp) {
      message(paste("Resampled from", nrow(df), "to", samp, "rows")) 
    }
  } else {
    # For binary resampling:
    message(paste("Resampled from:", vector2text(formatNum(table(df$tag),0), sep = " x ", quotes = F)))
    ones <- df %>% filter(tag %in% as.character(tags[1]))
    zeros <- df %>% filter(tag %in% as.character(tags[2]))
    
    if (nrow(ones) <= nrow(zeros)) {
      message(paste("Reducing size for:", tags[2]))
      zeros <- sample_n(zeros, round(rate * nrow(ones)))
    } else {
      message(paste("Reducing size for:", tags[1]))
      ones <- sample_n(ones, round(rate * nrow(zeros)))
    }
    balanced <- rbind(ones, zeros)
    message(paste("Into:", vector2text(formatNum(table(balanced$tag),0), sep = " x ", quotes = F)))
  }
  
  balanced <- rename_at(balanced, vars("tag"), funs(paste0(variable)))
  return(balanced)
}


####################################################################
#' List files in a directory
#' 
#' This function lets the user list all files on a given directory.
#' It also lets filter files which contains a string.
#' 
#' @param folder Character. Directory which contains files
#' @param recursive Boolean. Should the listing recurse into directories?
#' @param regex Character. String to use for filtering files
#' @param images Boolean. Bring only image files?
#' @param export Boolean. Do you wish to export list as txt file?
#' @export
listfiles <- function(folder = getwd(), recursive = TRUE, regex = NA, images = FALSE, export = FALSE) {
  
  # require(dplyr)
  # require(lubridate)
  # require(exifr)
  
  if (!file.exists(folder)) {
    stop("That directory doesn't exist; please try again!")
  }
  
  files <- list.files(folder, recursive = recursive)
  address <- paste0(folder, "/", files)
  info <- file.info(address)
  
  df <- data.frame(filename = files, address, info)
  df$size <- as.integer(df$size/1024)
  imgs <- "jpg|JPG|jpeg|JPEG|png|PNG|gif|GIF"
  
  if (!is.na(regex)) {
    df <- df[grep(regex, df$filename),] 
  }
  
  if (images == TRUE) {
    
    if (!"exifr" %in% (.packages())){
      stop("The following library should be loaded. Please run: library(exifr)")
    }
    
    if (nrow(df) > 250) {
      message(paste("This might take a while... Analizing around", 
                    formatNum(nrow(df), decimals = 0), "files!"))
    }
    
    tags <- c("FileName", "SourceFile",
              "CreateDate", "DateTimeOriginal", "FileModifyDate",
              "FileTypeExtension", "Megapixels",
              "ImageSize", "ImageWidth", "ImageHeight", 
              "GPSLongitude", "GPSLatitude",
              "Rotation", "Flash", "Duration")
    
    df <- read_exif(folder, recursive = TRUE) %>% 
      select(one_of(tags)) %>%
      mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal),
             CreateDate = ymd_hms(CreateDate),
             FileModifyDate = ymd_hms(FileModifyDate))
    
    df <- df[,colSums(is.na(df)) < nrow(df)]
    
  }
  
  if (export == TRUE) {
    write.table(df$filename, file = "files.txt", quote = FALSE, row.names = FALSE) 
  }
  
  row.names(df) <- NULL
  df$address <- NULL
  
  return(df)
  
}

####################################################################
#' Replace Values With
#' 
#' This function lets the user replace all specific values in a 
#' vector or data.frame into another value. If replacing more than
#' one value, order matters so they will be replaced in the same
#' order that you pass them to the function.
#' 
#' @param df Data.frame or Vector
#' @param original String or Vector. Original text you wish to replace
#' @param change String or Vector. Values you wish to replace the originals with
#' @param quiet Boolean. Keep quiet? (or print replacements)
#' @export
replaceall <- function(df, original, change, quiet = TRUE) {
  if (is.vector(df)) {
    vector <- TRUE
    df <- data.frame(x = df)
    dic <- data.frame(original, change)
    original <- dic[,1]
    change <- dic[,2]
  } else { 
    vector <- FALSE 
  }
  if (length(original) != length(change)) {
    stop("Vectors original and change should have the same length!")
  }
  if (length(unique(original)) != length(original)) {
    which <- freqs(dic, original) %>% filter(n > 1) %>% .$original
    stop("You have repeated original values to replace: ", vector2text(which))
  }
  if (sum(is.na(original)) > 0) {
    df <- df %>% replace(is.na(.), change[is.na(original)])
    change <- change[!is.na(original)]
    original <- original[!is.na(original)]
  }
  if (sum(is.null(original)) > 0) {
    df <- df %>% replace(is.null(.), change[is.null(original)])
    change <- change[!is.null(original)]
    original <- original[!is.null(original)]
  }
  if (length(original) > 0) {
    for (i in 1:length(original)) {
      if (!quiet) {
        message(paste("Transforming all", original[i], "into", change[i])) 
      }
      df[] <- lapply(df, function(x) gsub(original[i], change[i], x))
    } 
  }
  if (vector) {
    df <- df[,1]
  }
  return(df)
}

####################################################################
#' Remove/Drop Columns in which ALL or SOME values are NAs
#' 
#' This function lets the user remove all columns that have some or
#' all values as NAs
#' 
#' @param df Data.frame
#' @param all Boolean. Remove columns which contains ONLY NA values.
#' If set to FALSE, columns which contains at least one NA will be removed
#' @export
removenacols <- function(df, all = TRUE) {
  if (all == TRUE) {
    return(df[,colSums(is.na(df)) != nrow(df)]) 
  } else {
    return(df[,complete.cases(t(df))] )
  }
}

####################################################################
#' Remove/Drop Rows in which ALL or SOME values are NAs
#' 
#' This function lets the user remove all rows that have some or
#' all values as NAs
#' 
#' @param df Data.frame
#' @param all Boolean. Remove rows which contains ONLY NA values.
#' If set to FALSE, rows which contains at least one NA will be removed
#' @export
removenarows <- function(df, all = TRUE) {
  if (all == TRUE) {
    return(df[rowSums(is.na(df)) != ncol(df), ]) 
  } else {
    return(df[complete.cases(df), ])
  }
}


####################################################################
#' Filter only Numerical Values and
#' 
#' This function lets the user remove all rows that have some or
#' all values as NAs
#' 
#' @param df Data.frame
#' @param dropnacols Boolean. Drop columns with only NA values?
#' @param logs Boolean. Calculate log(x)+1 for numerical columns?
#' @param natransform String. "mean" or 0 to impute NA values. If
#' set to NA no calculation will run.
#' @export
numericalonly <- function(df, dropnacols = TRUE, logs = FALSE, natransform = NA) {
  
  # Drop ALL NAs columns
  if (dropnacols == TRUE) {
    df <- removenacols(df, all = TRUE) 
  }
  
  # Which character columns may be used as numeric?
  transformable <- apply(df, 2, function(x) length(unique(x)))
  which <- names(transformable[transformable==2])
  dfn <- data.frame(df[,colnames(df) %in% which])
  colnames(dfn) <- which
  non_numeric <- mutate_all(dfn, function(x) as.integer(as.factor(x))-1)
  # Which are already numeric?
  numeric <- select_if(df, is.numeric)
  
  # Calculate logs
  if (logs == TRUE) {
    # Non binary numeric features
    whichlog <- colnames(numeric)[!colnames(numeric) %in% which]
    numeric <- numeric %>% mutate_at(vars(whichlog), funs(log = log(. + 1)))
    is.na(numeric) <- do.call(cbind, lapply(numeric, is.infinite))
  }
  
  # Join everything
  d <- cbind(numeric, non_numeric[!colnames(non_numeric) %in% colnames(numeric)])
  
  if (!is.na(natransform)) {
    if (natransform == 0) {
      d[is.na(d)] <- 0 
    }
    if (natransform == "mean") {
      for(i in 1:ncol(d)){
        if (median(d[,i], na.rm = TRUE) != 0) {
          d[is.na(d[,i]), i] <- mean(d[,i], na.rm = TRUE) 
        } else {
          d[is.na(d[,i]), i] <- 0
        }
      }
    }
  }
  return(d)
}


####################################################################
#' Transform any date input into Date
#' 
#' This function lets the user transform any date input format into
#' a conventional R date format. The following formats are some of 
#' the permitted: 10-05-2019, 2019-10-05 5/22/2015, 9:45:03 AM, 
#' 42348.44, 9/2/18 23:16, 10-05-19
#' 
#' 
#' @param dates Vector. Dates in any of the permitted formats
#' @param metric Boolean. Metric or Imperial inputs. The main 
#' difference is that Metric follows the DD/MM/YYYY pattern, and 
#' Imperial follows the MM/DD/YYYY pattern.
#' @param origin Date. When importing from Excel, integers usually
#' have 1900-01-01 as origin. In R, origin is 1970-01-01.
#' @export
dateformat <- function(dates, metric = TRUE, origin = '1900-01-01') {
  
  #require(dplyr)
  #require(stringr)
  #require(lubridate)
  options(warn=-1)
  
  # Check if all values are NA
  if(length(dates) == sum(is.na(dates))){
    message("No dates where transformed becase all values are NA")
    return(dates)
  }
  
  # Delete hours
  dates <- gsub(" .*", "", as.character(dates))
  dates <- gsub("\\..*", "", as.character(dates))
  
  # Is it in integer format?
  x <- dates[!is.na(dates)][1]
  int <- as.integer(as.character(x))
  
  # When date is not an integer:
  if (is.na(int)) {
    
    year <- ifelse(nchar(x) == 10, "Y", "y")
    sym <- ifelse(grepl("/", x),"/","-")
    pattern <- str_locate_all(x, "/")[[1]][,1]
    
    if(sum(pattern == c(3,6)) == 2) {
      return(as.Date(dates, format = paste0("%m",sym,"%d",sym,"%",year)))
    } else {
      return(as.Date(dates, format = paste0("%",year,sym,"%m",sym,"%d")))
    }
  }
  
  # When date is an integer:
  if (int %in% 30000:60000) {
    dates <- as.Date(as.integer(as.character(dates)), origin=origin)
  } else {
    comps <- unlist(strsplit(x, "/|-|\\."))
    lasts <- as.integer(comps[3])
    firsts <- as.integer(comps[1])
    firstlast <- nchar(paste0(firsts, lasts))
    if (metric == FALSE) {
      # Does dates end in year?
      if (nchar(lasts) == 4 | firstlast <= 4) {
        dates <- lubridate::mdy(dates)
      }
      # Does dates start in year?
      if (nchar(firsts) == 4) {
        dates <- lubridate::ydm(dates)
      }
    } else {
      # Does dates end in year?
      if (nchar(lasts) == 4 | firstlast <= 4) {
        dates <- lubridate::dmy(dates)
      }
      # Does dates start in year?
      if (nchar(firsts) == 4) {
        dates <- lubridate::ymd(dates)
      }
    }
  }
  return(dates)
}


####################################################################
#' Pass Through a dplyr's Pipeline
#' 
#' This function lets the user print, save or do something inside a
#' pipeline without affecting the output or pipeline.
#' 
#' @param df Dataframe
#' @param fun Function. What function do you wish to run? For example:
#' pass(. \%>\% ncol \%>\% print)
#' @export
pass <- function(df, fun) { 
  fun(df)
  return(df) 
}


####################################################################
#' What's my IP
#' 
#' This function lets the user find his IP quickly
#' 
#' @export
myip <- function(){
  # require(rvest)
  # require(xml2)
  ipify <- "https://api.ipify.org/"
  ip <- xml2::read_html(ipify) %>% rvest::html_text()
  return(ip)
}


####################################################################
#' Plot Result with Nothing to Plot
#' 
#' This function lets the user print a plot without plot, with a 
#' customizable message. It is quite useful for Shiny renderPlot when
#' using filters and no data is returned.
#' 
#' @param message Character. What message do you wish to show?
#' @export
noPlot <- function(message = "Nothing to show here!") {
  
  # require(ggplot2)
  
  p <- ggplot(data.frame(), aes(x = 0, y = 0, label = message)) + 
    geom_label() + theme_minimal() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  return(p)
}


####################################################################
#' Install latest version of H2O
#' 
#' This function lets the user un-install the current version of
#' H2O installed and update to latest stable version.
#' 
#' @param run Boolean. Do you want to run and start an H2O cluster?
#' @export
h2o_update <- function(run = TRUE){
  
  # require(rvest)
  
  url <- "http://h2o-release.s3.amazonaws.com/h2o/latest_stable.html"
  end <- xml2::read_html(url) %>% rvest::html_node("head") %>% 
    as.character() %>% gsub(".*url=","",.) %>% gsub("/index.html.*","",.)
  newurl <- paste0(gsub("/h2o/.*","",url), end, "/R")
  # The following commands remove any previously installed H2O version
  if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
  if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
  # Now we download, install and initialize the H2O package for R.
  message(paste("Installing h2o from", newurl))
  install.packages("h2o", type="source", repos=newurl)
  if(run == TRUE){
    # require(h2o)
    h2o.init()
  }
}

####################################################################
#' Install latest version of H2O
#' 
#' This function lets the user un-install the current version of
#' H2O installed and update to latest stable version.
#' 
#' @param p ggplot2 object. Plot to export
#' @param name Character. File's name or sufix if vars is not null
#' @param vars Vector. Variables in plot
#' @param sep Character. Separator for variables
#' @param width Numeric. Plot's width on file
#' @param height Numeric. Plot's height on file
#' @param subdir Character. Into which subdirectory do you wish to save the plot to?
#' @param quiet Boolean. Display succesful message with filename when saved?
#' @export
export_plot <- function(p, 
                        name = "plot", 
                        vars = NA, 
                        sep = ".vs.", 
                        width = 8, 
                        height = 6, 
                        subdir = NA,
                        quiet = FALSE) {
  
  # File name
  if (!is.na(vars)) {
    names <- vector2text(
      cleanText(as.character(vars), spaces = FALSE), sep=sep, quotes = FALSE)
    file_name <- paste0(name, "_", names, ".png")  
  } else {
    file_name <- paste0(name, ".png")  
  }
  
  # Create directory if needed
  if (!is.na(subdir)) {
    options(warn=-1)
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  # Export plot to file
  p <- p + ggsave(file_name, width = width, height = height)
  
  if (quiet == FALSE) {
    message(paste("Plot saved as", file_name)) 
  }
  
}


####################################################################
#' Calculate cuts by quantiles
#' 
#' This function lets the user quickly calculate cuts for quantiles
#' 
#' @param values Vector. Values to calculate quantile cuts
#' @param splits Integer. How many cuts should split the values?
#' @param return Character. Return "summary" or "labels"
#' @export
quants <- function(values, splits = 10, return = "summary") {
  if (splits > length(unique(values[!is.na(values)]))-1) {
    stop("There are not enough observations to split the data in ", splits)
  }
  value <- as.numeric(as.character(values))
  cuts <- quantile(values, 
                   probs = seq(0, 1, length = splits+1), 
                   na.rm = TRUE)
  decimals <- min(nchar(values), na.rm = TRUE) + 1
  decimals <- ifelse(decimals >= 5, 5, decimals)
  labels <- cut(values, unique(cuts), 
                dig.lab = decimals, 
                include.lowest = TRUE)
  if (return == "labels") {
    return(labels) 
  }
  if (return == "summary") {
    output <- data.frame(percentile = names(cuts)[-1], cut = cuts[-1]) %>%
      mutate(label = paste0("(", signif(lag(cut),4), "-", signif(cut,4),"]"),
             label = gsub("\\(NA", paste0("[", signif(min(cut), 4)), label),
             label = factor(label, levels = unique(label), ordered = T))
    return(output) 
  }
}


####################################################################
#' Download Historical Currency Exchange Rate
#' 
#' This function lets the user download historical currency exchange
#' rate between two currencies
#' 
#' @param currency_pair Character. Which currency exchange do you
#' wish to get the history from? i.e, USD/COP, EUR/USD...
#' @param from Date. From date
#' @param to Date. To date
#' @param fill Boolean. Fill weekends and non-quoted dates with 
#' previous values?
#' @export
get_currency <- function(currency_pair, from = Sys.Date() - 99, to = Sys.Date(), fill = FALSE) {
  
  options("getSymbols.warning4.0" = FALSE)
  options("getSymbols.yahoo.warning" = FALSE)
  string <- paste0(toupper(cleanText(currency_pair)), "=X")
  
  if (from == to) {
    to <- from + 1
  }
  
  if (to > Sys.Date()) {
    to <- Sys.Date()
  }
  
  if (Sys.Date() == from) {
    x <- getQuote(string, auto.assign = FALSE)
    rownames(x) <- Sys.Date()
    x[,1] <- NULL
  } else {
    x <- data.frame(getSymbols(
      string, 
      env = NULL,
      from = from, to = to,
      src = "yahoo"))
    if (substr(rownames(x),1,1)[1] == "X") {
      x <- x[1,]
      rownames(x) <- Sys.Date()
    }
  }
  
  rate <- data.frame(date = as.Date(rownames(x)), rate=x[,1])
  
  if (fill) {
    options(warn=-1)
    rate <- data.frame(date = as.character(
      as.Date(as.Date(from):Sys.Date(), origin="1970-01-01"))) %>%
      left_join(rate %>% mutate(date = as.character(date)), "date") %>%
      tidyr::fill(rate, .direction = "down") %>%
      tidyr::fill(rate, .direction = "up") %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= as.Date(from))
  }
  
  return(rate)
  
}


####################################################################
#' Convert JSON string to vector (data.frame with 1 row)
#' 
#' This function lets the user transform a JSON string into vector 
#' (data.frame with 1 row). You can also pass a Python's dictionary.
#' 
#' @param json Character. JSON string. Example of a string: '{"feat1": 
#' null, "feat2": "M"}'
#' @export
json2vector <- function(json) {
  string <- paste0("[", gsub('"',"\"", json), "]")
  string <- gsub("'",'"', string)
  string <- gsub("None","null", string)
  string <- gsub("True","true", string)
  string <- gsub("False","false", string)
  vector <- fromJSON(string)
  df <- data.frame(t(unlist(vector)))
  return(df)
}


####################################################################
#' Progressbar for for loops
#' 
#' This function lets the user view a progressbar for a 'for' loop. 
#' Taken from https://github.com/STATWORX/helfRlein/blob/master/R/statusbar.R
#' 
#' @param run Iterator. for loop or an integer with the current loop number
#' @param max.run Number. Maximum number of loops
#' @param percent.max Integer. Indicates how wide the progress bar is printed
#' @param info String. With additionaly information to be printed 
#' at the end of the line. The default is \code{run}.
#' @export
statusbar <- function (run, max.run, percent.max = 40L, info = run){
  
  if (length(run) > 1) {
    stop("run needs to be of length one!")
  }
  
  if (length(max.run) == 0) {
    stop("max.run has length 0")
  }
  
  if (length(max.run) > 1) {
    percent <- which(run == max.run) / length(max.run)
  } else {
    percent <- run / max.run
  }
  
  percent.step <- round(percent * percent.max, 0)
  progress <- paste0("[",
                     paste0(rep("=", percent.step), collapse = ""),
                     paste0(rep(" ", percent.max - percent.step), collapse = ""),
                     "] ",
                     sprintf("%7.1f", percent * 100, 2),
                     "% | ",
                     paste(info, ("       "))) 
  cat("\r", progress)
  flush.console()
  if(run == max.run) cat("", sep="\n\n")
}


####################################################################
#' Right: Last n characters
#' 
#' This function lets the user extract the last n characters of a
#' string or vector of strings.
#' 
#' @param string String or Vector
#' @param n Integer. How many characters from right to left?
#' @export
right <- function(string, n = 1){
  string <- as.character(string)
  r <- substr(string, nchar(string) - n + 1, nchar(string))
  return(r)
}


####################################################################
#' Left: First n characters
#' 
#' This function lets the user extract the first n characters of a
#' string or vector of strings.
#' 
#' @param string String or Vector
#' @param n Integer. How many characters from left to right?
#' @export
left <- function(string, n = 1){
  string <- as.character(string)
  l <- substr(string, 1, n)
  return(l)
}


####################################################################
#' Import Excel File with All Its Tabs
#' 
#' This function lets the user import an Excel file's tabs into a list
#' 
#' @param file String. Excel's name
#' @export
importxlsx <- function(file) {
  sheets <- getSheetNames(file)
  if (length(sheets) > 1) {
    mylist <- list()
    for (i in 1:length(sheets)) {
      sheet <- read.xlsx(file, sheet = i, skipEmptyRows=TRUE, detectDates=TRUE)  
      mylist[[i]] <- sheet
    } 
  } else {
    mylist <- read.xlsx(file, sheet = sheets, skipEmptyRows=TRUE, detectDates=TRUE)  
  }
  return(mylist)
}


####################################################################
#' Import Excel File with All Its Tabs
#' 
#' This function silences (verbose) output prints. Thanks to Hadley Wickham!
#' 
#' @param fx Function
#' @export
quiet <- function(fx) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(fx)) 
} 


####################################################################
#' Internet Connection Check
#' 
#' This function checks if your R session currently have Wifi or 
#' Internet connection.
#' 
#' @param thresh Numeric. How many seconds to consider a slow connection?  
#' @param url Character. URL to test the readLines 1 command
#' @export
haveInternet <- function(thresh = 3, url = "http://www.google.com") {
  start <- Sys.time()
  if (!capabilities(what = "http/ftp")) return(FALSE)
  test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
  if (as.numeric(Sys.time() - start) > thresh) {
    message("Slow internet connection but available...")
  }
  return(!inherits(test, "try-error"))
}


####################################################################
#' Zero Variance Columns
#' 
#' This function quickly detectes which columns have the same value
#' for each observation
#' 
#' @param df Dataframe
#' @export
zerovar <- function(df) {
  out <- lapply(df, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}


####################################################################
#' Read Files (Auto-detected)
#' 
#' This function lets the user import csv, xlsx, xls, sav files.
#' 
#' @param filename Character
#' @param current_wd Boolean. Use current working directory before
#' the file's name? Use this param to NOT get absolute root directory.
#' @export
read.file <- function(filename, current_wd = TRUE) {
  if (current_wd) {
    filename <- paste0(getwd(), "/", filename) 
  }
  if (!file.exists(filename)) {
    stop("That file doesn't exist.. try with another!")
  } else {
    filetype <- gsub("\\.","", right(filename, 4))
    if (filetype == "csv") {
      results <- data.frame(data.table::fread(filename))
    }
    if (filetype == "xlsx") {
      results <- openxlsx::read.xlsx(filename)
    }
    if (filetype == "xls") {
      results <- gdata::read.xls(filename)
    }
    if (filetype == "dat") {
      results <- read.table(filename, header = TRUE)
    }
    if (filetype == "sav") {
      results <- quiet(foreign::read.spss(filename, to.data.frame = T))
    }
    if (filetype == "dta") {
      # Stata version 5-12 .dta file
      #results <- foreign::read.dta(filename)
      # Stata version 13 .dta file
      results <- readstata13::read.dta13(filename)
    }
    
    message(paste("Imported", filetype, "file with", 
                  formatNum(nrow(results),0), "rows x", 
                  formatNum(ncol(results),0), "columns, succesfully!"))
    
  }
  if (nrow(results) == 0) {
    warning("There is no data in that file...")
  }
  return(results)
}


####################################################################
#' Bind Files into Dataframe
#' 
#' This function imports and binds multiple files into a single 
#' data.frame. Files must be inserted with absolute roots filenames. 
#' 
#' @param files Dataframe
#' @export
bindfiles <- function(files) {
  alldat <- data.frame()
  for (i in 1:length(files)) {
    file <- files[i]
    dfi <- read.file(file, current_wd = FALSE) 
    alldat <- rbind_full(alldat, dfi)
    statusbar(i, length(files))
  }
  return(data.frame(alldat))
}






####################################################################
#' Split a dataframe for training and testing sets
#'
#' This function splits automatically a dataframe into train and test datasets.
#' You can define a seed to get the same results every time, but has a default value.
#' You can prevent it from printing the split counter result.
#'
#' @param df Dataframe to split
#' @param size Split rate
#' @param seed Seed for random split
#' @param print Print summary results
#' @return A list with both datasets, summary, and split rate
#' @export
msplit <- function(df, size = 0.7, seed = NA, print=T) {
  
  if (!is.na(seed)) {
    set.seed(seed)
  }
  
  df <- data.frame(df)
  
  ind <- sample(seq_len(nrow(df)), size = floor(size * nrow(df)))
  train <- df[ind, ]
  test <- df[-ind, ]
  
  train_size <- dim(train)
  test_size <- dim(test)
  summary <- rbind(train_size, test_size)[,1]
  if (print == TRUE) {
    print(summary) 
  }
  
  sets <- list(train=train, test=test, summary=summary, split_size=size)
  
  return(sets)
  
}


####################################################################
#' Loggarithmic Loss Function for Binary Models
#'
#' This function calculates log loss/cross-entropy loss for binary 
#' models. NOTE: when result is 0.69315, the classification is neutral; 
#' it assigns equal probability to both classes.
#'
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param eps Numeric. Epsilon value
#' @export
loglossBinary <- function(tag, score, eps = 1e-15) {
  
  if (length(unique(tag)) != 2) {
    stop("Your 'tag' vector is not binary!")
  }
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag),
                       "rows and score has", length(score)))
    )
  }
  
  if (!is.numeric(tag)) {
    tag <- as.integer(tag) - 1
  }
  
  score <- pmax(pmin(score, 1 - eps), eps)
  LogLoss <- -mean(tag * log(score) + (1 - tag) * log(1 - score))
  
  return(LogLoss)
  
}


####################################################################
#' Automated H2O's AutoML
#'
#' This function lets the user create a robust and fast model, using 
#' H2O's AutoML function. The result is a list with the best model, 
#' its parameters, datasets, performance metrics, variables 
#' importances, and others. 
#'
#' @param df Dataframe. Dataframe containing all your data, including 
#' the independent variable labeled as 'tag'
#' @param train_test Character. If needed, df's column name with 'test' 
#' and 'train' values to split
#' @param split Numeric. Value between 0 and 1 to split as train/test 
#' datasets. Value is for training set.
#' @param seed Numeric. Seed for random stuff and reproducibility
#' @param thresh Integer. Threshold for selecting binary or regression 
#' models: this number is the threshold of unique values we should 
#' have in 'tag' (more than: regression; less than: classification)
#' @param max_time Numeric. Max seconds you wish for the function 
#' to iterate
#' @param max_models Numeric. Max models you wish for the function 
#' to create
#' @param start_clean Boolean. Erase everything in the current h2o 
#' instance before we start to train models?
#' @param alarm Boolean. Ping an alarm when ready!
#' @param save Boolean. Do you wish to save/export results into your 
#' working directory?
#' @param subdir Character. In which directory do you wish to save 
#' the results? Working directory as default.
#' @param plot Boolean. Do you want to plot the results with 
#' mplot_full function?
#' @param project Character. Your project's name
#' @export
h2o_automl <- function(df, 
                       train_test = NA,
                       split = 0.7,
                       seed = 0,
                       thresh = 5,
                       max_time = 5*60,
                       max_models = 25,
                       start_clean = TRUE,
                       alarm = TRUE,
                       save = FALSE,
                       subdir = NA,
                       plot = FALSE,
                       project = "Machine Learning Model") {
  
  options(warn=-1)
  
  start <- Sys.time()
  message(paste(start,"| Started process..."))
  
  df <- data.frame(df) %>% filter(!is.na(tag)) %>% mutate_if(is.character, as.factor)
  type <- ifelse(length(unique(df$tag)) <= as.integer(thresh), "Classifier", "Regression")
  
  message("Model type: ", type)
  if (type == "Classifier") {
    print(data.frame(freqs(df, tag)))
  }
  if (type == "Regression") {
    print(summary(df$tag)) 
  }
  
  ####### Validations to proceed #######
  if(!"tag" %in% colnames(df)){
    stop("You should have a 'tag' column in your data.frame!")
  }
  
  ####### Split datasets for training and testing #######
  if (is.na(train_test)) {
    message("Splitting datasets...")
    splits <- msplit(df, size = split, seed = seed)
    train <- splits$train
    test <- splits$test
    
  } else {
    # If we already have a default split for train and test (train_test)
    if ((!unique(train_test) %in% c('train', 'test')) & (length(unique(train_test)) != 2)) {
      stop("Your train_test column should have 'train' and 'test' values only!")
    }
    train <- df %>% filter(train_test == "train")
    test <- df %>% filter(train_test == "test")
    test$tag <- NULL
    print(table(train_test))
  }
  
  
  ####### Train model #######
  
  quiet(h2o.init(nthreads = -1, port=54321, min_mem_size="8g"))
  if (start_clean) {
    quiet(h2o.removeAll()) 
  } else {
    message("Previous trained models are not being erased. Use 'start_clean' parameter if needed.")
  }
  #h2o.shutdown()
  
  message(paste("Iterating until", max_models, "models or", max_time, "seconds..."))
  aml <- h2o::h2o.automl(x = setdiff(names(df), "tag"), 
                         y = "tag",
                         training_frame = as.h2o(train),
                         leaderboard_frame = as.h2o(test),
                         max_runtime_secs = max_time,
                         max_models = max_models,
                         exclude_algos = c("StackedEnsemble","DeepLearning"),
                         nfolds = 5, 
                         seed = seed)
  message(paste("Succesfully trained", nrow(aml@leaderboard), "models:"))
  print(aml@leaderboard[,1:3])
  flow <- "http://localhost:54321/flow/index.html"
  message("Check results in H2O Flow's nice interface: ", flow)
  
  # Select model (Best one by default)
  m <- h2o.getModel(as.vector(aml@leaderboard$model_id[1]))  
  
  # Calculations and variables
  scores <- h2o_predict_model(test, m)
  #scores_df <- as.vector(h2o_predict_model(df, m)[,1])
  imp <- data.frame(h2o.varimp(m)) %>%
  {if ("names" %in% colnames(.)) 
    dplyr::rename(., "variable" = "names", "importance" = "coefficients") else .
  } %>%
  {if ("percentage" %in% colnames(.)) 
    dplyr::rename(., "importance" = "percentage") else .
  }
  
  # CLASSIFICATION MODELS
  if (type == "Classifier") {
    if (length(unique(train$tag)) == 2) {
      # Binaries
      scores <- scores[,2]
    } else {
      # More than 2 cateogies
      scores <- scores[,1]
    }
    results <- list(
      project = project,
      model = m,
      scores_test = data.frame(
        tag = as.vector(test$tag),
        score = scores),
      metrics = NA,
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = imp,
      auc_test = NA, #h2o.auc(m, valid=TRUE)
      errors_test = NA,
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard,
      scoring_history = data.frame(m@model$scoring_history),
      seed = seed)
    
    results$metrics <- model_metrics(
      tag = results$scores_test$tag, 
      score = results$scores_test$score,
      plots = TRUE)
    
    if (length(unique(train$tag)) == 2) {
      results$errors_test <- errors(
        tag = results$scores_test$tag, 
        score = results$scores_test$score) 
      results$auc_test <- pROC::roc(
        results$scores_test$tag, 
        results$scores_test$score, ci=T)
      
    } else {
      results$logloss_test <- NULL
      results$errors_test <- NULL
      results$auc_test <- NULL
    }
    
  } 
  
  # REGRESION MODELS
  if (type == "Regression") {
    results <- list(
      project = project,
      model = m,
      scores_test = data.frame(
        index = c(1:nrow(test)),
        tag = as.vector(test$tag),
        score = as.vector(scores$predict)),
      metrics = NULL,
      scoring_history = data.frame(m@model$scoring_history),
      datasets = list(test = test, train = train),
      parameters = m@parameters,
      importance = imp,
      rmse = h2o::h2o.rmse(m),
      model_name = as.vector(m@model_id),
      algorithm = m@algorithm,
      leaderboard = aml@leaderboard
    )
    
    results$metrics <- model_metrics(
      tag = results$scores_test$tag, 
      score = results$scores_test$score,
      plots = TRUE)
  }
  
  message(paste0("Training duration: ", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  
  if (save) {
    export_results(results, subdir = subdir)
    message("Results and model files exported succesfully!")
  }
  
  if (plot) {
    mplot_full(tag = results$scores_test$tag,
               score = results$scores_test$score,
               subtitle = results$project,
               model_name = results$model_name)
  }
  
  if (alarm) {
    beepr::beep()
  }
  
  return(results)
  
}


####################################################################
#' Select Model from h2o_automl's Leaderboard
#' 
#' Select wich model from the h2o_automl function to use
#' 
#' @param results Object. h2o_automl output
#' @param which_model Integer. Which model from the leaderboard you wish to use?
#' @export
h2o_selectmodel <- function(results, which_model = 1) {
  
  # Select model (Best one by default)
  m <- h2o.getModel(as.vector(results$leaderboard$model_id[which_model]))  
  
  # Calculations and variables
  scores <- predict(m, as.h2o(results$datasets$test))
  output <- list(
    project = results$project,
    model = m,
    scores = data.frame(
      index = c(1:nrow(results$datasets$test)),
      tag = as.vector(results$datasets$test$tag),
      score = as.vector(scores[,3]),
      norm_score = normalize(as.vector(scores[,3]))),
    importance = data.frame(h2o.varimp(m)),
    auc_test = NA,
    errors_test = NA,
    logloss_test = NA,
    model_name = as.vector(m@model_id),
    algorithm = m@algorithm)
  roc <- pROC::roc(output$scores$tag, output$scores$score, ci=T)
  output$auc_test <- roc$auc
  output$errors_test <- errors(tag = results$scores_test$tag, 
                               score = results$scores_test$score)
  output$logloss_test <- loglossBinary(tag = results$scores_test$tag, 
                                       score = results$scores_test$score)
  return(output)
}


####################################################################
#' Export h2o_automl's Results
#' 
#' Export RDS, TXT, POJO, MOJO and all results from h2o_automl
#' 
#' @param results Object. h2o_automl output
#' @param txt Boolean. Do you wish to export the txt results?
#' @param rds Boolean. Do you wish to export the RDS results?
#' @param binary Boolean. Do you wish to export the Binary model?
#' @param pojo Boolean. Do you wish to export the POJO model?
#' @param mojo Boolean. Do you wish to export the MOJO model?
#' @param sample_size Integer. How many example rows do you want to show?
#' @param subdir Character. In which directory do you wish to save the results?
#' @param save Boolean. Do you wish to save/export results?
#' @export
export_results <- function(results, 
                           txt = TRUE, 
                           rds = TRUE, 
                           binary = TRUE,
                           pojo = TRUE, 
                           mojo = TRUE, 
                           sample_size = 10,
                           subdir = NA,
                           save = TRUE) {
  
  if (save) {
    
    options(warn=-1)
    quiet(h2o.init(nthreads = -1, port = 54321, min_mem_size = "8g"))
    
    # We create a directory to save all our results
    first <- ifelse(length(unique(results$scores_test$tag)) > 6,
                    signif(results$errors_test$rmse, 4),
                    round(100*results$metrics$metrics$AUC, 2))
    subdirname <- paste0(first, "-", results$model_name)  
    if (!is.na(subdir)) {
      subdir <- paste0(subdir, "/", subdirname)
    } else {
      subdir <- subdirname
    }
    dir.create(subdir)
    
    # Export Results List
    if (rds == TRUE) {
      saveRDS(results, file=paste0(subdir, "/results.rds")) 
    }
    
    # Export Model as POJO & MOJO for Production
    if (pojo == TRUE) {
      h2o.download_pojo(results$model, path=subdir)  
    }
    if (mojo == TRUE) {
      h2o.download_mojo(results$model, path=subdir)  
    }
    
    # Export Binary
    if (mojo == TRUE) {
      h2o.saveModel(results$model, path=subdir, force=TRUE)
    }
    
    if (txt == TRUE) {
      
      tags <- c(as.character(results$datasets$test$tag), 
                as.character(results$datasets$train$tag))
      tags_test <- results$datasets$test
      tags_train <- results$datasets$train
      random_sample <- sample(1:nrow(results$scores_test), sample_size)
      
      results_txt <- list(
        "Project" = results$project,
        "Model" = results$model_name,
        "Dimensions" = 
          list("Distribution" = table(tags),
               "Test vs Train" = c(paste(round(100*nrow(tags_test)/length(tags)),
                                         round(100*nrow(tags_train)/length(tags)), sep=" / "),
                                   paste(nrow(tags_test), nrow(tags_train), sep=" vs. ")),
               "Total" = length(tags)),
        "Metrics" = model_metrics(results$scores_test$tag, 
                                  results$scores_test$score, plots = FALSE),
        "Variable Importance" = results$importance,
        "Model Results" = results$model,
        "Models Leaderboard" = results$leaderboard,
        "10 Scoring examples" = cbind(
          real = results$scores_test$tag[random_sample],
          score = results$scores_test$score[random_sample], 
          results$datasets$test[random_sample, names(results$datasets$test) != "tag"])
      )
      capture.output(results_txt, file = paste0(subdir, "/results.txt"))
    } 
  }
}


####################################################################
#' Iterate and Search for Best Seed
#' 
#' This functions lets the user iterate and search for best seed. Note that if
#' the results change a lot, you are having a high variance in your data.
#' 
#' @param df Dataframe. Dataframe with all your data you wish to model (see h2o_automl's df)
#' @param tries Integer. How many seed do you wish to try?
#' @export
iter_seeds <- function(df, tries = 10) {
  
  seeds <- data.frame()
  
  for (i in 1:tries) {
    iter <- h2o_automl(df, seed = i)
    seeds <- rbind(seeds, cbind(seed = as.integer(i), auc = iter$auc_test))
    seeds <- arrange(seeds, desc(auc))
    print(seeds)
  }
  return(seeds)
}

####################################################################
#' Root Mean Squared Error (RMSE)
#' 
#' This function lets the user calculate Root Mean Squared Error
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
rmse <- function(tag, score){
  error <- tag - score
  sqrt(mean(error^2))
}


####################################################################
#' Mean Absolute Error (MAE)
#' 
#' This function lets the user calculate Mean Absolute Error
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
mae <- function(tag, score){
  error <- tag - score
  mean(abs(error))
}


####################################################################
#' Mean Squared Error (MSE)
#' 
#' This function lets the user calculate Mean Squared Error
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
mse <- function(tag, score){ 
  error <- tag - score
  mean(error^2)
}


####################################################################
#' Mean Absolute Percentage Error (MAPE)
#' 
#' This function lets the user calculate Mean Squared Error
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
mape <- function(tag, score){ 
  error <- (tag - score) / tag
  error <- error[!is.infinite(error)]
  tag <- tag[tag != 0]
  mean(abs(error/tag))
}


####################################################################
#' Calculate R Squared
#' 
#' This function lets the user calculate r squared
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
rsq <- function(tag, score){ 
  fit <- lm(score ~ tag)
  signif(summary(fit)$r.squared, 4)
}

####################################################################
#' Calculate Adjusted R Squared
#' 
#' This function lets the user calculate adjusted r squared
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
rsqa <- function(tag, score){ 
  fit <- lm(score ~ tag)
  signif(summary(fit)$adj.r.squared, 4)
}


####################################################################
#' Calculate Errors
#' 
#' This function lets the user calculate all errors and R squared 
#' simultaneously.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @export
errors <- function(tag, score){ 
  data.frame(
    rmse = rmse(tag, score),
    mae = mae(tag, score),
    mse = mse(tag, score),
    mape = mape(tag, score),
    rsq = rsq(tag, score),
    rsqa = rsqa(tag, score)
  )
}


####################################################################
#' H2O Predict using MOJO file
#' 
#' This function lets the user predict using the h2o .zip file 
#' containing the MOJO files. Note that it works with the files 
#' generated when using the function export_results()
#' 
#' @param df Dataframe. Data to insert into the model
#' @param model_path Character. Relative model_path directory
#' @param sample Integer. How many rows should the function predict?
#' @export
h2o_predict_MOJO <- function(df, model_path, sample = NA){
  
  df <- as.data.frame(df)
  zip <- paste0(model_path, "/", gsub(".*-","",model_path), ".zip")
  
  if(!is.na(sample)) {
    json <- toJSON(df[1:sample, ])
  } else {
    json <- toJSON(df)
  }
  
  quiet(h2o.init(nthreads = -1, port=54321, min_mem_size="8g"))
  x <- h2o.predict_json(zip, json)
  
  if (length(x$error) >= 1) {
    stop("Error:", x$error)
  } else {
    score_MOJO <- as.vector(unlist(data.frame(x[,3])[2,])) 
    return(score_MOJO)
  }
}


####################################################################
#' H2O Predict using Binary file
#' 
#' This function lets the user predict using the h2o binary file.
#' Note that it works with the files generated when using the 
#' function export_results(). Recommendation: use the 
#' h2o_predict_MOJO() function when possible - it let's you change
#' h2o's version without problem.
#' 
#' @param df Dataframe. Data to insert into the model
#' @param model_path Character. Relative model_path directory or zip file
#' @param sample Integer. How many rows should the function predict?
#' @export
h2o_predict_binary <- function(df, model_path, sample = NA){
  
  options(warn=-1)
  quiet(h2o.init(nthreads = -1, port=54321, min_mem_size="8g"))
  
  if (!right(model_path, 4) == ".zip") {
    binary <- paste(model_path, gsub(".*-", "", model_path), sep="/")  
  } else {
    binary <- model_path
  }
  
  model <- h2o.loadModel(binary)
  
  if(!is.na(sample)) {
    df <- df[1:sample, ]
  }
  
  score_binary <- as.vector(predict(model, as.h2o(df))[,3])
  
  return(score_binary)
  
}


####################################################################
#' H2O Predict using API Service
#' 
#' This function lets the user get the score from an API service
#' 
#' @param df Dataframe/Vector. Data to insert into the model
#' @param api Character. API's URL
#' @export
h2o_predict_API <- function(df, api) {
  
  post <- function(df, api) {
    df <- df %>%
      removenacols() %>% 
      select(-contains("tag"))
    x <- POST(
      api, 
      add_headers('Content-Type'='application/json'), 
      body = as.list(df), 
      encode = "json", 
      verbose())
    return(content(x)$probabilityToOne)
  }
  
  batch <- c()
  for (i in 1:nrow(df)) {
    x <- df[i,]
    score <- post(x, api)
    batch <- rbind(batch, score)
  }
  
  return(as.vector(batch))
  
}


####################################################################
#' H2O Predict using H2O Model Object
#' 
#' This function lets the user get scores from a H2O Model Object
#' 
#' @param df Dataframe/Vector. Data to insert into the model
#' @param model H2o Object. Model
#' @export
h2o_predict_model <- function(df, model){
  #model <- h2o.getModel(as.vector(aml@leaderboard$model_id[1]))
  scores <- as.data.frame(predict(model, as.h2o(df)))
  return(scores)
}


####################################################################
#' Classification Model Metrics
#' 
#' This function lets the user get a confusion matrix and accuracy, and 
#' for for binary classification models: AUC, Precision, Sensitivity, and
#' Specificity.
#' 
#' @param tag Vector. Real known label
#' @param score Vector. Predicted value or model's result
#' @param thresh Numeric. Value which splits the results for the 
#' confusion matrix.
#' @param plots Boolean. Include plots?
#' @param subtitle Character. Subtitle for plots
#' @export
model_metrics <- function(tag, score, thresh = 0.5, 
                          plots = TRUE, subtitle = NA){
  
  metrics <- list()
  type <- ifelse(length(unique(tag)) <= 10, "Classification", "Regression")
  
  if (type == "Classification") {
    
    labels <- sort(unique(as.character(tag)))
    
    if (is.numeric(score)) {
      new <- data.frame(score = score) %>%
        mutate(score = ifelse(score >= thresh, labels[1], labels[2])) %>%
        .$score
    } else {
      new <- score
    }
    
    conf_mat <- table(Real = as.character(tag), 
                      Pred = as.character(new))
    
    metrics[["confusion_matrix"]] <- conf_mat
    
    total <- sum(conf_mat)
    trues <- sum(diag(conf_mat))
    falses <- total - trues
    
    # For Binaries
    if (length(unique(tag)) == 2) {
      dic <- c("AUC: Area Under the Curve",
               "ACC: Accuracy",
               "PRC: Precision = Positive Predictive Value",
               "TPR: Sensitivity = Recall = Hit rate = True Positive Rate",
               "TNR: Specificity = Selectivity = True Negative Rate",
               "Logloss (Error): Logarithmic loss [Neutral classification: 0.69315]")
      metrics[["dictionary"]] <- dic
      ROC <- pROC::roc(tag, score, ci=T)
      metrics[["metrics"]] <- data.frame(
        AUC = signif(ROC$auc, 5),
        ACC = signif(trues / total, 5),
        PRC = signif(conf_mat[2,2] / (conf_mat[2,2] + conf_mat[1,2]), 5),
        TPR = signif(conf_mat[2,2] / (conf_mat[2,2] + conf_mat[2,1]), 5),
        TNR = signif(conf_mat[1,1] / (conf_mat[1,1] + conf_mat[1,2]), 5),
        Logloss = loglossBinary(tag, score)
      )
      # ROC CURVE PLOT
      if (plots) {
        plot_roc <- invisible(mplot_roc(tag, score))
        if (!is.na(subtitle)) {
          plot_roc <- plot_roc + labs(subtitle = subtitle)
        }
        metrics[["plot_ROC"]] <- plot_roc
      }
      
    } else {
      metrics[["ACC"]] <- signif(trues / total, 5)
    }
    
    # CONFUSION MATRIX PLOT
    if (plots) {
      plot_cf <- data.frame(conf_mat) %>%
        mutate(perc = round(100 * Freq / sum(Freq), 2)) %>%
        mutate(label = paste0(formatNum(Freq, 0),"\n", perc,"%")) %>%
        ggplot(aes(
          y = factor(Real, levels = rev(labels)), 
          x = as.character(Pred), 
          fill= Freq, size=Freq, 
          label = label)) +
        geom_tile() + theme_lares2() +
        geom_text(colour="white") + 
        scale_size(range = c(3, 4)) + coord_equal() + 
        guides(fill=FALSE, size=FALSE, colour=FALSE) +
        labs(x="Predicted values", y="Real values",
             title = ifelse(length(unique(tag)) == 2,
                            paste("Confusion Matrix with Threshold =", thresh),
                            paste("Confusion Matrix for", length(unique(tag)), "Categories")),
             subtitle = paste0("Accuracy: ", round(100*(trues / total), 2), "%")) +
        theme(axis.text.x = element_text(angle=30, hjust=0)) +
        scale_x_discrete(position = "top") +
        theme(axis.text.x.bottom = element_blank(), 
              axis.ticks.x.bottom = element_blank(),
              axis.text.y.right = element_blank(),
              axis.ticks.y.right = element_blank()) +
        theme_lares2()
      
      if (!is.na(subtitle)) {
        plot_cf <- plot_cf + labs(subtitle = subtitle)
      }
      metrics[["plot_ConfMat"]] <- plot_cf 
    }
  }
  
  if (type == "Regression") {
    # Needs further improvements
    metrics[["errors"]] <- errors(tag, score)
  }
  return(metrics)
}












################################################################
# Density plot for discrete and continuous values
mplot_density <- function(tag, score, model_name = NA, subtitle = NA, 
                          save = FALSE, subdir = NA,file_name = "viz_distribution.png") {
  
  require(ggplot2)
  require(gridExtra)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (length(unique(tag)) <= 6) {
    
    out <- data.frame(tag = as.character(tag),
                      score = as.numeric(score))
    
    p1 <- ggplot(out) + theme_minimal() +
      geom_density(aes(x = 100 * score, group = tag, fill = as.character(tag)), 
                   alpha = 0.6, adjust = 0.25) + 
      guides(fill = guide_legend(title="Tag")) + 
      xlim(0, 100) + 
      labs(title = "Classification Model Results",
           y = "Density by tag", x = "Score")
    
    p2 <- ggplot(out) + theme_minimal() + 
      geom_density(aes(x = 100 * score), 
                   alpha = 0.9, adjust = 0.25, fill = "deepskyblue") + 
      labs(x = "", y = "Density")
    
    p3 <- ggplot(out) + theme_minimal() + 
      geom_line(aes(x = score * 100, y = 100 * (1 - ..y..), color = as.character(tag)), 
                stat = 'ecdf', size = 1) +
      geom_line(aes(x = score * 100, y = 100 * (1 - ..y..)), 
                stat = 'ecdf', size = 0.5, colour = "black", linetype="dotted") +
      ylab('Cumulative') + xlab('') + guides(color=FALSE)
    
    if(!is.na(subtitle)) {
      p1 <- p1 + labs(subtitle = subtitle)
    }
    
    if(!is.na(model_name)) {
      p1 <- p1 + labs(caption = model_name)
    }
    
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir))
      file_name <- paste(subdir, file_name, sep="/")
    }
    
    if(save == TRUE) {
      png(file_name, height = 1800, width = 2100, res = 300)
      grid.arrange(
        p1, p2, p3, 
        ncol = 2, nrow = 2, heights = 2:1,
        layout_matrix = rbind(c(1,1), c(2,3)))
      dev.off()
    }
    
    return(
      grid.arrange(
        p1, p2, p3, 
        ncol = 2, nrow = 2, heights = 2:1,
        layout_matrix = rbind(c(1,1), c(2,3))))
    
  } else {
    
    require(scales)
    
    df <- data.frame(
      rbind(cbind(values = tag, type = "Real"), 
            cbind(values = score, type = "Model")))
    df$values <- as.numeric(as.character(df$values))
    
    p <- ggplot(df) + theme_minimal() +
      geom_density(aes(x = values, fill = as.character(type)), 
                   alpha = 0.6, adjust = 0.25) + 
      labs(title = "Values distribution",
           y = "Density", 
           x = "Continuous values") +
      scale_x_continuous(labels = comma) +
      guides(fill = guide_legend(size = 1)) +
      theme(legend.title=element_blank(),
            #legend.position = "bottom",
            legend.position = c(0.82, 1.1), 
            legend.direction = "horizontal",
            legend.spacing.x = unit(0.1, 'cm'))
    
    
    if(!is.na(model_name)) {
      p <- p + labs(caption = model_name)
    }
    
    if(!is.na(subtitle)) {
      p <- p + labs(subtitle = subtitle)
    }  
    
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir))
      file_name <- paste(subdir, file_name, sep="/")
    }
    
    if (save == TRUE) {
      p <- p + ggsave(file_name, width = 6, height = 6)
    }
    return(p)
  }
}


################################################################
# Variables Importances
mplot_importance <- function(var, imp, colours = NA, limit = 15, model_name = NA, subtitle = NA,
                             save = FALSE, subdir = NA, file_name = "viz_importance.png") {
  
  require(ggplot2)
  require(gridExtra)
  options(warn=-1)
  
  if (length(var) != length(imp)) {
    message("The variables and importance values vectors should be the same length.")
    stop(message(paste("Currently, there are",length(var),"variables and",length(imp),"importance values!")))
  }
  
  if (is.na(colours)) {
    colours <- "deepskyblue" 
  }
  
  out <- data.frame(var = var, imp = imp, Type = colours)
  
  if (length(var) < limit) {
    limit <- length(var)
  }
  
  output <- out[1:limit,]
  
  p <- ggplot(output, 
              aes(x = reorder(var, imp), y = imp * 100, 
                  label = round(100 * imp, 1))) + 
    geom_col(aes(fill = Type), width = 0.1) +
    geom_point(aes(colour = Type), size = 6) + 
    coord_flip() + xlab('') + theme_minimal() +
    ylab('Importance') + 
    geom_text(hjust = 0.5, size = 2, inherit.aes = TRUE, colour = "white") +
    labs(title = paste0("Variables Importances. (", limit, " / ", length(var), " plotted)"))
  
  if (length(unique(output$Type)) == 1) {
    p <- p + geom_col(fill = colours, width = 0.2) +
      geom_point(colour = colours, size = 6) + 
      guides(fill = FALSE, colour = FALSE) + 
      geom_text(hjust = 0.5, size = 2, inherit.aes = TRUE, colour = "white")
  }
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir))
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}

#################################################
# ROC Curve
mplot_roc <- function(tag, score, model_name = NA, subtitle = NA, interval = 0.2, plotly = FALSE,
                      save = FALSE, subdir = NA, file_name = "viz_roc.png") {
  
  require(pROC)
  require(ggplot2)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  roc <- pROC::roc(tag, score, ci=T)
  coords <- data.frame(
    x = rev(roc$specificities),
    y = rev(roc$sensitivities))
  ci <- data.frame(roc$ci, row.names = c("min","AUC","max"))
  
  
  p <- ggplot(coords, aes(x = x, y = y)) +
    geom_line(colour = "deepskyblue", size = 1) +
    geom_point(colour = "blue3", size = 0.9, alpha = 0.4) +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.2, linetype = "dotted") + 
    scale_x_reverse(name = "1 - Specificity [False Positive Rate]", limits = c(1,0), 
                    breaks = seq(0, 1, interval), expand = c(0.001,0.001)) + 
    scale_y_continuous(name = "Sensitivity [True Positive Rate]", limits = c(0,1), 
                       breaks = seq(0, 1, interval), expand = c(0.001, 0.001)) +
    theme_minimal() + 
    theme(axis.ticks = element_line(color = "grey80")) +
    coord_equal() + 
    ggtitle("ROC Curve: AUC") +
    annotate("text", x = 0.25, y = 0.10, size = 4.2, 
             label = paste("AUC =", round(100*ci[c("AUC"),],2))) +
    annotate("text", x = 0.25, y = 0.05, size = 2.8, 
             label = paste0("95% CI: ", round(100*ci[c("min"),],2),"-", round(100*ci[c("max"),],2)))
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if (plotly == TRUE) {
    require(plotly)
    p <- ggplotly(p)
  }
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir))
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}

#################################################
# Cuts by quantiles for score
mplot_cuts <- function(score, splits = 10, subtitle = NA, model_name = NA, 
                       save = FALSE, subdir = NA, file_name = "viz_ncuts.png") {
  
  require(ggplot2)
  
  if (splits > 25) {
    stop("You should try with less splits!")
  }
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  deciles <- quantile(score, 
                      probs = seq((1/splits), 1, length = splits), 
                      names = TRUE)
  deciles <- data.frame(cbind(Deciles=row.names(as.data.frame(deciles)),
                              Threshold=as.data.frame(deciles)))
  
  p <- ggplot(deciles, 
              aes(x = reorder(Deciles, deciles), y = deciles * 100, 
                  label = round(100 * deciles, 1))) + 
    geom_col(fill="deepskyblue") + 
    xlab('') + theme_minimal() + ylab('Score') + 
    geom_text(vjust = 1.5, size = 3, inherit.aes = TRUE, colour = "white", check_overlap = TRUE) +
    labs(title = paste("Cuts by score: using", splits, "equal-sized buckets"))
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  } 
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir))
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}


#################################################
# Cuts by quantiles on absolut and percentual errors
mplot_cuts_error <- function(tag, score, splits = 10, title = NA, model_name = NA, 
                             save = FALSE, subdir = NA, file_name = "viz_ncuts_error.png") {
  
  require(ggplot2)
  require(scales)
  require(gridExtra)
  
  if (splits > 25) {
    stop("You should try with less splits!")
  }
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  df <- data.frame(tag = tag, score = score) %>%
    mutate(real_error = tag - score,
           abs_error = abs(real_error),
           p_error = 100 * real_error/tag) %>%
    filter(abs(p_error) <= 100)
  
  # First: absolute errors
  deciles_abs <- quantile(df$abs_error, 
                          probs = seq((1/splits), 1, length = splits), 
                          names = TRUE)
  deciles_abs <- data.frame(cbind(Deciles=row.names(as.data.frame(deciles_abs)),
                                  Threshold=as.data.frame(deciles_abs)))
  p_abs <- ggplot(deciles_abs, 
                  aes(x = reorder(Deciles, deciles_abs), y = deciles_abs, 
                      label = signif(deciles_abs, 3))) +
    geom_col(fill="deepskyblue") + 
    xlab('') + theme_minimal() + ylab('Absolute Error') + 
    geom_text(vjust = 1.5, size = 2.7, inherit.aes = TRUE, colour = "white", check_overlap = TRUE) +
    labs(subtitle = paste("Cuts and distribution by absolute error")) +
    scale_y_continuous(labels = comma)
  
  # Second: percentual errors
  deciles_per <- quantile(abs(df$p_error), 
                          probs = seq((1/splits), 1, length = splits), 
                          names = TRUE)
  deciles_per <- data.frame(cbind(Deciles=row.names(as.data.frame(deciles_per)),
                                  Threshold=as.data.frame(deciles_per)))
  
  p_per <- ggplot(deciles_per, 
                  aes(x = reorder(Deciles, deciles_per), y = deciles_per, 
                      label = signif(deciles_per, 3))) +
    geom_col(fill="deepskyblue") + 
    xlab('') + theme_minimal() + ylab('% Error') + 
    geom_text(vjust = 1.5, size = 2.7, inherit.aes = TRUE, colour = "white", check_overlap = TRUE) +
    labs(subtitle = paste("Cuts and distribution by absolute porcentual error")) +
    scale_y_continuous(labels = comma)
  
  # Third: errors distribution
  pd_error <- ggplot(df) + 
    geom_density(aes(x=p_error), fill="deepskyblue", alpha = 0.7) +
    xlab('') + ylab('Error Density') + theme_minimal() +
    scale_x_continuous(labels=function(x) paste0(x,"%")) +
    geom_vline(xintercept = 0, alpha = 0.5, colour = "navy", linetype = "dotted")
  
  if(!is.na(title)) {
    p_abs <- p_abs + labs(title = title)
  } 
  
  if(!is.na(model_name)) {
    pd_error <- pd_error + labs(caption = model_name)
  }
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir))
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if(save == TRUE) {
    png(file_name, height = 1800, width = 1800, res = 300)
    grid.arrange(
      p_abs, p_per, pd_error,
      heights = c(1.8,1.8,1),
      ncol = 1, nrow = 3)
    dev.off()
  }
  
  return(
    grid.arrange(
      p_abs, p_per, pd_error,
      heights = c(2,2,1),
      ncol = 1, nrow = 3)
  )
  
}


#################################################
# Split and compare quantiles
mplot_splits <- function(tag, score, splits = 5, subtitle = NA, model_name = NA, facet = NA, 
                         save = FALSE, subdir = NA, file_name = "viz_splits.png") {
  
  require(ggplot2)
  require(dplyr)
  require(RColorBrewer)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (splits > 10) {
    stop("You should try with less splits!")
  }
  
  df <- data.frame(tag, score, facet)
  npersplit <- round(nrow(df)/splits)
  
  # For continuous tag values
  if (length(unique(tag))) {
    names <- df %>% 
      mutate(tag = as.numeric(tag), 
             quantile = ntile(tag, splits)) %>% group_by(quantile) %>%
      summarise(n = n(), 
                max_score = round(max(tag), 1), 
                min_score = round(min(tag), 1)) %>%
      mutate(quantile_tag = paste0(quantile," (",min_score,"-",max_score,")"))
    df <- df %>% 
      #mutate(score = score/100, tag = tag/100) %>%
      mutate(quantile = ntile(tag, splits)) %>%
      left_join(names, by = c("quantile")) %>% mutate(tag = quantile_tag) %>% 
      select(-quantile, -n, -max_score, -min_score)
    
  } else {
    # For categorical tag values
    names <- df %>% 
      mutate(quantile = ntile(score, splits)) %>% group_by(quantile) %>%
      summarise(n = n(), 
                max_score = round(100 * max(score), 1), 
                min_score = round(100 * min(score), 1)) %>%
      mutate(quantile_tag = paste0(quantile," (",min_score,"-",max_score,")")) 
  }
  
  p <- df %>% 
    mutate(quantile = ntile(score, splits)) %>% 
    group_by(quantile, facet, tag) %>% tally() %>%
    ungroup() %>% group_by(facet, tag) %>% 
    arrange(desc(quantile)) %>%
    mutate(p = round(100*n/sum(n),2),
           cum = cumsum(100*n/sum(n))) %>%
    left_join(names, by = c("quantile")) %>%
    ggplot(aes(x = as.character(tag), y = p, label = as.character(p),
               fill = as.character(quantile_tag))) + theme_minimal() +
    geom_col(position = "stack") +
    geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
    xlab("Tag") + ylab("Total Percentage by Tag") +
    guides(fill = guide_legend(title=paste0("~",npersplit," p/split"))) +
    labs(title = "Tag vs Score Splits Comparison") +
    scale_fill_brewer(palette = "Spectral")
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if(!is.na(facet)) {
    p <- p + facet_grid(. ~ facet, scales = "free")
  }  
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir))
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}


#################################################
# AUC and LogLoss Plots
mplot_metrics <- function(results, subtitle = NA, model_name = NA, 
                          save = FALSE, file_name = "viz_metrics.png", subdir = NA) {
  
  require(ggplot2)
  require(gridExtra)
  
  plots_data <- data.frame(
    trees = results$model@model$scoring_history$number_of_trees,
    train_ll = results$model@model$scoring_history$training_logloss,
    test_ll = results$model@model$scoring_history$validation_logloss,
    train_auc = results$model@model$scoring_history$training_auc,
    test_auc = results$model@model$scoring_history$validation_auc)
  ll <- ggplot(plots_data) + theme_minimal() +
    geom_hline(yintercept = 0.69315, alpha = 0.5, linetype = 'dotted') + 
    geom_line(aes(x=trees, y=train_ll, colour="Train"), size=0.5) +
    geom_line(aes(x=trees, y=test_ll, colour="Test"), size=1) +
    labs(title = "Logarithmic Loss vs Number of Trees",
         colour = "Dataset", x = "# of trees", y = "LogLoss") +
    scale_colour_brewer(palette = "Set1") + 
    geom_text(aes(x=trees, y=train_ll, colour="Train", 
                  label=round(train_ll,2)),
              check_overlap = TRUE, nudge_y=0.03, size=3) +
    geom_text(aes(x=trees, y=test_ll, colour="Test", 
                  label=round(test_ll,2)),
              check_overlap = TRUE, nudge_y=0.03, size=3) + 
    theme(strip.text.x = element_blank(),
          strip.background = element_rect(colour="white", fill="white"),
          legend.position=c(0.1, 0.05))
  au <- ggplot(plots_data) + theme_minimal() +
    geom_line(aes(x=trees, y=train_auc*100, colour="Train"), size=0.5) +
    geom_line(aes(x=trees, y=test_auc*100, colour="Test"), size=1) +
    geom_hline(yintercept = 50, alpha = 0.5, linetype = 'dotted', colour="black") + 
    labs(title = "Area Under the Curve vs Number of Trees",
         colour = "Dataset", x = "# of trees", y = "AUC") +
    scale_colour_brewer(palette = "Set1") + guides(colour=FALSE) +
    geom_text(aes(x=trees, y=train_auc*100, colour="Train", 
                  label=round(train_auc*100,2)),
              check_overlap = TRUE, nudge_y=3, size=3) +
    geom_text(aes(x=trees, y=test_auc*100, colour="Test", 
                  label=round(test_auc*100,2)),
              check_overlap = TRUE, nudge_y=3, size=3)
  
  if(!is.na(subtitle)) {
    ll <- ll + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    au <- au + labs(caption = model_name)
  }
  
  if (save == TRUE) {
    
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir))
      file_name <- paste(subdir, file_name, sep="/")
    }
    
    png(file_name, height = 1800, width = 2100, res = 300)
    grid.arrange(ll, au, ncol = 1, nrow = 2)
    dev.off()
  }
  
  return(grid.arrange(ll, au, ncol = 1, nrow = 2))
  
}


#################################################
# Linear regression plot
mplot_lineal <- function(tag, score, subtitle = NA, model_name = NA, 
                         save = FALSE, file_name = "viz_lineal.png", subdir = NA) {
  
  require(ggplot2)
  require(scales)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  results <- data.frame(tag = tag, score = score, dist = 0)
  for (i in 1:nrow(results)) { 
    results$dist[i] <- dist2d(c(results$tag[i],results$score[i]), c(0,0), c(1,1)) 
  }
  
  fit <- lm(tag ~ score)
  labels <- paste(
    paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 4)),
    #paste("Pval =", signif(summary(fit)$coef[2,4], 3)), 
    paste("RMSE =", signif(rmse(results$tag, results$score), 4)), 
    paste("MAE =", signif(mae(results$tag, results$score), 4)), 
    sep="\n")
  
  p <- ggplot(results, aes(x = tag, y = score, colour = dist)) +
    geom_abline(slope = 1, intercept = 0, alpha = 0.5, colour = "orange", size=0.6) +
    geom_point() + theme_minimal() + coord_equal() + 
    labs(title = "Regression Model Results",
         x = "Real value", y = "Predicted value",
         colour = "Deviation") +
    annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = labels, size = 2.8) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1)) +
    guides(colour = guide_colorbar(barwidth = 0.9, barheight = 4.5))
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}


#################################################
# MPLOTS Score Full Report (without importances)
mplot_full <- function(tag, score, splits = 8, subtitle = NA, model_name = NA, 
                       save = FALSE, file_name = "viz_full.png", subdir = NA) {
  
  require(ggplot2)
  require(gridExtra)
  require(dplyr)
  
  options(warn=-1)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has", length(tag), "rows and score has", length(score))))
  }
  
  # Categorical Models
  if (length(unique(tag)) <= 6) {
    p1 <- mplot_density(tag = tag, score = score, subtitle = subtitle, model_name = model_name)
    p2 <- mplot_splits(tag = tag, score = score, splits = splits)
    p3 <- mplot_roc(tag = tag, score = score)
    p4 <- mplot_cuts(score = score) 
    
    if(save == TRUE) {
      
      if (!is.na(subdir)) {
        dir.create(file.path(getwd(), subdir))
        file_name <- paste(subdir, file_name, sep="/")
      }
      
      png(file_name, height = 2000, width = 3200, res = 300)
      grid.arrange(
        p1, p2, p3, p4,
        widths = c(1.3,1),
        layout_matrix = rbind(c(1,2), c(1,2), c(1,3), c(4,3)))
      dev.off()
    }
    
    return(
      grid.arrange(
        p1, p2, p3, p4,
        widths = c(1.3,1),
        layout_matrix = rbind(c(1,2), c(1,2), c(1,3), c(4,3)))
    ) 
    
  } else {
    
    # Numerical models
    p1 <- mplot_lineal(tag = tag, score = score, subtitle = subtitle, model_name = model_name)
    p2 <- mplot_density(tag = tag, score = score)
    p3 <- mplot_cuts_error(tag = tag, score = score, splits = splits)
    
    if(save == TRUE) {
      
      if (!is.na(subdir)) {
        dir.create(file.path(getwd(), subdir))
        file_name <- paste(subdir, file_name, sep="/")
      }
      
      png(file_name, height = 2000, width = 3200, res = 300)
      grid.arrange(
        p1, p2, p3,
        heights = c(0.6, 0.4),
        widths = c(0.45, 0.55),
        layout_matrix = rbind(c(1,3), c(2,3)))
      dev.off()
    }
    
    return(grid.arrange(
      p1, p2, p3,
      heights = c(0.6, 0.4),
      widths = c(0.45, 0.55),
      layout_matrix = rbind(c(1,3), c(2,3)))
    )
    
  }
}