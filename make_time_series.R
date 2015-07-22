#SV Scarpino, scarpino [at] utexas.edu
#22nd July 15
#The goal here is to create a time series and plot
#License (were applicable): GPL (>= 3.0)

#libraries

#acc. funcs.
make_ts_df <- function(df1, df2){
	mt_df2 <- match(colnames(df2), colnames(df1))
	if(length(which(is.na(mt_df2) == TRUE)) > 0){
		mt_df2_na <- which(is.na(mt_df2) == TRUE)
		for(i in mt_df2_na){
			df1 <- data.frame(df1, rep(0, nrow(df1)))
			colnames(df1)[ncol(df1)] <- colnames(df2)[i]
		}
	}
	mt_df1 <- match(colnames(df1), colnames(df2))
	if(length(which(is.na(mt_df1) == TRUE)) > 0){
		mt_df1_na <- which(is.na(mt_df1) == TRUE)
		for(i in mt_df1_na){
			df2 <- data.frame(df2, rep(df1[nrow(df1),i], nrow(df2)))
			colnames(df2)[ncol(df2)] <- colnames(df1)[i]
		}
	}
	mt_df2 <- match(colnames(df2), colnames(df1))
	if(length(mt_df2) == ncol(df1) & length(which(is.na(mt_df2) == TRUE)) ==0){
		df2_diff_reord <- df2[,-1] - colSums(as.matrix(df1[, mt_df2][,-1]), na.rm=TRUE)
		df2_diff_reord <- data.frame(df2[,1], df2_diff_reord)
		colnames(df2_diff_reord)[1] <- "date"
		df_out <- rbind(df1, df2_diff_reord)
	}else{
		stop("failed to align data")
	}
	return(df_out)
}

#reading in data and making time series
files <- list.files() #listing all files

#grabbing just those with data
files_use <- 1:length(files)
for(i in files_use){
	test_i <- strsplit(files[i], "-")
	if(length(unlist(test_i)) == 3){
		next
	}else{
		files_use <- files_use[-i]
	}
}

#loading files, creating time series
counter <- 0
for(i in files_use){
	dat_i <- read.table(files[i], sep = "\t", row.names = 1, header = FALSE)
	date_i <- unlist(strsplit(files[i], "[.]"))[1]
	dates_i <- rep(date_i, ncol(dat_i))
	dat_ts_i <- data.frame(dates_i, t(dat_i))
	colnames(dat_ts_i)[1] <- "date"
	if(counter == 0){
		dat_ts <- dat_ts_i
	}else{
		dat_ts <- make_ts_df(df1 = dat_ts, df2 = dat_ts_i)
	}
	counter <- counter + 1
}

write_new <- FALSE
if(write_new == TRUE){
	fname <- paste0(as.numeric(Sys.time()), "CA_measles_TS.csv")
	write.csv(dat_ts, file = fname, row.names = FALSE)
}

plot(as.Date(as.character(dat_ts[,"date"]), format = "%m-%d-%Y"), rowSums(dat_ts[,-1], na.rm = TRUE), type = "l", lwd = 3, xlab = "2015", ylab = "New measles cases in CA USA")

plot(as.Date(as.character(dat_ts[,"date"]), format = "%m-%d-%Y"), cumsum(rowSums(dat_ts[,-1], na.rm = TRUE)), type = "l", lwd = 3, xlab = "2015", ylab = "Cumulative measles cases in CA USA")