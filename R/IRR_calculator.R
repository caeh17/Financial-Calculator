# IRR CALCULATOR
# The purpose of this R script is to read a .csv file containing investment cash flow
# data and output another .csv with the internal rate of return (IRR) for each
# investment, calculated on a yearly, monthly and continuous basis, respectively.

# Auxiliary methods

#' Find the first index with a non-zero value in a vector.
#'
#' @param vect A vector.
#' @return The first index with a non-zero value in the vector.
#' @examples
#' findFirst(c(0,0,1,0,4,2))
#' findFirst(c(0,6,3,4))
findFirst <- function(vect) {
  i <- which(vect != 0)[1]
  if (is.na(i)) return(length(vect))
  return(i)
}

#' Convert dates in a list to a floating point value.
#'
#' The values express time elapsed since
#' the first date in the dataset used and is measured in whole years, with
#' days and months expressed as fractions of a year.
#'
#' Note that the day count convention used is 30E/360.
#'
#' @param dates A list of dates.
#' @return The dates in years and fractions of years in floating point format.
#' @examples
#' convertDatesToT(c("1.1.2000", "1.7.2002")) will return 0.0 and 2.5.
convertDatesToT <- function(dates) {
  dates <- t(dates)
  output <- vector(mode = "numeric", length = length(dates))
  for (i in 1:length(dates)) {
    q <- unlist(strsplit(dates[i], "\\."))
    q <- min(c(as.numeric(q[1]), 30)) + 30 * as.numeric(q[2]) + 360 * as.numeric(q[3])
    output[i] <- q / 360
  }
  first <- output[1]
  output <- sapply(output, function(date) date - first)
  return(output)
}

#' Order dates as number of whole months since start.
#'
#' Values express to which month in order the date in the dataset belongs.
#'
#' @param dates A list of dates.
#' @return The order of dates in months since start in integer format.
#' @examples
#' convertDatesToMonthAndY(c("1.1.2000", "1.3.2002")) would return 1 and 27.
convertDatesToMonthAndY <- function(dates) {
  dates <- t(dates)
  output <- vector(mode = "numeric", length = length(dates))
  for (i in 1:length(dates)) {
    output[i] <- 12 * (as.numeric(format(as.Date(dates[i], format = "%d.%m.%Y"), "%Y")) - as.numeric(format(as.Date(dates[1], format = "%d.%m.%Y"), "%Y"))) + as.numeric(format(as.Date(dates[i], format = "%d.%m.%Y"), "%m"))
  }
  first <- output[1] - 1
  for (i in 1:length(output)) {
    output[i] <- output[i] - first
  }
  return(output)
}

#' Order dates as number of whole years since start.
#'
#' Values express to which year in order the date in the dataset belongs.
#'
#' @param dates A list of dates.
#' @return The order of dates in years since start in integer format.
#' @examples
#' convertDatesToMonthAndY(c("1.1.2000", "1.3.2002")) would return 1 and 3.
convertDatesToY <- function(dates) {
  dates <- t(dates)
  output <- vector(mode = "numeric", length = length(dates))
  for (i in 1:length(dates)) {
    output[i] <- as.numeric(format(as.Date(dates[i], format = "%d.%m.%Y"), "%Y"))
  }
  first <- output[1] - 1
  for (i in 1:length(output)) {
    output[i] <- output[i] - first
  }
  return(output)
}

#' Convert a vector of cashflows into a continuous discount function
#'
#' The continuous discount function calculates NPV as the sum of the following
#' for each cash flow CF at point in time t:
#' e^(-rt) * CF
#' where r is the interest rate. The IRR is the interest rate for which the NPV = 0.
#'
#' @param irr The unknown IRR variable to be solved for.
#' @param vect A vector with the cashflows to be analysed.
#' @param dates A vector with dates as fractions of years corresponding to the cashflows.
#' @return A function for the NPV in the current case with an unknown
#' interest rate to be solved for.
convertVectorToFunction <- function(irr, vect, dates) {
    a <- 0
    start <- findFirst(vect)
    for(i in start:length(vect)) {
      a <- a + exp(-irr * dates[i]) * vect[i]
    }
    return (a)
}

# Methods forming the core functionality of the calculator

#' Solve for the IRR in the continuous case
#'
#' @param vect A vector with the cashflows to be analysed.
#' @param dates A vector with dates as fractions of years.
#' @return The IRR for cashflows provided.
findRootContinuous <- function(vect, dates) {
  dates <- convertDatesToT(dates)
  root <- NA

  try(root <- uniroot(function(x) convertVectorToFunction(x, vect, dates), c(-1,1))$root, silent=TRUE)

  if(is.na(root)) root <- 0

  return(root)
}

#' Solve for the IRR with monthly aggregation of cash flows.
#'
#' @param vect A vector with the cashflows to be analysed.
#' @param dates A vector with dates months from start.
#' @return The IRR for the cashflows provided.
findRootMonthly <- function(vect, dates) {
  dates <- convertDatesToMonthAndY(dates)
  monthlyCF <- vector(mode = "numeric", length = dates[length(dates)])
  for (i in 1:length(vect)) {
    monthlyCF[dates[i]] <- monthlyCF[dates[i]] + vect[i]
  }

  monthlyCF <- monthlyCF[findFirst(monthlyCF):length(monthlyCF)]
  roots <- polyroot(monthlyCF)
  return(Re(roots)[abs(Im(roots)) < 1e-2])
}

#' Solve for the IRR with yearly aggregation of cash flows.
#'
#' @param vect A vector with the cashflows to be analysed.
#' @param dates A vector with dates as years from start.
#' @return The IRR for the cashflows provided.
findRootYearly <- function(vect, dates) {
  dates <- convertDatesToY(dates)
  yearlyCF <- vector(mode = "numeric", length = dates[length(dates)])
  for (i in 1:length(vect)) {
    yearlyCF[dates[i]] <- yearlyCF[dates[i]] + vect[i]
  }
  yearlyCF <- yearlyCF[findFirst(yearlyCF):length(yearlyCF)]
  roots <- polyroot(yearlyCF)
  return(Re(roots)[abs(Im(roots)) < 1e-3])
}

#' Compile results of calculation.
#'
#' Compile continuous, yearly and monthly IRR in data frame.
#'
#' @param file The data frame being analysed containing cash flow time series for
#' any number of investment projects.
#' @return A data frame containing the IRR values for each investment.
compileResults <- function(file) {
  rowNames <- colnames(file)
  l <- length(file)
  output <- matrix(NA, nrow = l - 1, ncol = 3)

  for (i in 2:l) {
    output[i - 1,1] <- 1 / max(-Inf, findRootYearly(t(file[i]), file[1])) - 1
    output[i - 1,2] <- (1 / max(-Inf, findRootMonthly(t(file[i]), file[1])) - 1) * 12
    output[i - 1,3] <- findRootContinuous(t(file[i]), file[1])
  }

  output <- as.data.frame(output)
  colNames <- c("IRR, yearly", "IRR, monthly", "IRR, continuous")
  colnames(output) <- colNames
  output <- data.frame(Investments = rowNames[2:length(rowNames)], output)
  return(output)
}

# I/O methods

#' Read .csv file into data frame.
#'
#' Reads the data to be analysed from a .csv file into a data frame, then
#' reshapes it into a format more suitable for the calculations.
#'
#' @param fileName the name of the file to be read if in the working directory, else
#' complete URL of the file.
#' @return Data frame adapted to the needs of the program.
readFile <- function(fileName) {
  values <- read.csv2(file = fileName, stringsAsFactors = FALSE)
  values <- reshape(values, idvar = "Date", timevar = "Name", direction = "wide")
  values[is.na(values)] <- 0
  return(values)
}

#' Writes out the results of the calculations.
#'
#' This method creates a new .csv file with '-analysis-' and a timestamp appended
#' to the original name. If succesful, a message about the name of the new file
#' is printed to the console.
#'
#' @param results the data frame containing the results of the calculations.
#' @param fileName the name of the original file analysed.
writeFile <- function(results, fileName) {
  fileName <- unlist(strsplit(fileName, "[\\]"))
  fileName <- fileName[length(fileName)]
  fileName <- unlist(strsplit(fileName, ".", fixed = TRUE))
  fileName <- fileName[1]
  fileName <- paste0(fileName, "-analysis-")
  time <- gsub(":", "_", Sys.time())
  fileName <- paste0(fileName, time)
  fileName <- paste0(fileName, ".csv")
  write.csv2(results, file = fileName)
  print(paste0("Operation successful, you can find the results in the following file: ", fileName))
}

#' Main method
#'
#' This method executes the program, taking at least one command line parameter.
run <- function() {
  args = commandArgs(trailingOnly = TRUE)
  if (length(args) > 0 && length(args) < 5) {
    fileName <- args[1]
    file <- readFile(fileName)
    if (length(args) == 2) {
      mv <- read.csv2(args[2])[,3]
      date <- as.Date(as.character(file[,1])[length(file[,1])], format = "%d.%m.%Y") + 1
      lastRow <- list(as.character(format(date, "%d.%m.%Y")))
      for (i in 1:length(mv)) lastRow[[length(lastRow) + 1]] <- mv[i]
      file <- rbind(file, lastRow)
    }
    if (length(args) == 3) {
      if (identical(args[3], "-R")) {
        mv <- read.csv2(args[2])[,3]
        spread <- sample(1:5, 1) * 12
        parts <- matrix(nrow = length(mv), ncol = spread)
        for (k in 1:nrow(parts)) {
          if (mv[k] == 0) {
            parts[k,] <- vector(mode = "numeric", length = spread)
          } else {
            parts[k,] <- runif(spread, 0, mv[k])
            parts[k,] <- parts[k,] / sum(parts[k,])
            parts[k,] <- parts[k,] * mv[k]
          }
        }
        for (j in 1:spread) {
          date <- as.Date(as.character(file[,1])[length(file[,1])], format = "%d.%m.%Y") + 30
          lastRow <- list(as.character(format(date, "%d.%m.%Y")))
          for(i in 1:length(mv)) {
            lastRow[[length(lastRow) + 1]] <- parts[i, j]
          }
          file <- rbind(file, lastRow)
        }
      } else if (is.na(as.integer(args[3]))) {
        stop("Invalid third argment; should be either an integer or '-R'", call.=FALSE)
      } else {
        mv <- read.csv2(args[2])[,3]
        spread <- as.numeric(args[3]) * 365
        for (j in 1:spread) {
          date <- as.Date(as.character(file[,1])[length(file[,1])], format = "%d.%m.%Y") + 1
          lastRow <- list(as.character(format(date, "%d.%m.%Y")))
          for(i in 1:length(mv)) lastRow[[length(lastRow) + 1]] <- mv[i] / spread
          file <- rbind(file, lastRow)
        }
      }
    }
    if (length(args) == 4 && (is.na(as.integer(args[4]))) || !identical(args[3], "-r")) {
      stop("Invalid third or fourth argment; 3rd should be '-r' and 4th an integer", call.=FALSE)
    } else if (length(args) == 4) {
      mv <- read.csv2(args[2])[,3]
      spread <- as.numeric(args[4]) * 12
      parts <- matrix(nrow = length(mv), ncol = spread)
      for (k in 1:nrow(parts)) {
        if (mv[k] == 0) {
          parts[k,] <- vector(mode = "numeric", length = spread)
        } else {
          parts[k,] <- runif(spread, 0, mv[k])
          parts[k,] <- parts[k,] / sum(parts[k,])
          parts[k,] <- parts[k,] * mv[k]
        }
      }
      for (j in 1:spread) {
        date <- as.Date(as.character(file[,1])[length(file[,1])], format = "%d.%m.%Y") + 30
        lastRow <- list(as.character(format(date, "%d.%m.%Y")))
        for(i in 1:length(mv)) {
          lastRow[[length(lastRow) + 1]] <- parts[i, j]
        }
        file <- rbind(file, lastRow)
      }
    }
    #print(file[240:length(file[,1]),])
    results <- compileResults(file)
    writeFile(results, fileName)
  } else {
    stop("At least one and no more than four arguments must be supplied (input file).n", call.=FALSE)
  }
}

# Run the file!

run()
