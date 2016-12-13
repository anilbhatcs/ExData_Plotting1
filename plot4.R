library(graphics)
library(grDevices)

#Initialize constants
data_file_name <- "C:/Users/anbhat/Documents/Anil/Exploratory Data Analysis/household_power_consumption.txt"
png_path <- "C:/Users/anbhat/Documents/gitrepo/ExData_Plotting1/"
date_format <- "%d/%m/%Y"
date_tm_format <- "%d/%m/%Y %T"
date1 <- as.Date("01/02/2007", date_format)
date2 <- as.Date("02/02/2007", date_format)
interval <- 100000

message("Reading ", data_file_name)

#Initialize file connection and read the header
con <- file(data_file_name, "r", blocking = FALSE)
epc <- scan(con, what = "a", sep = ";", allowEscapes = TRUE, nlines = 1, quiet = TRUE)

#Is the file empty? If yes, close the connection and stop the script
if (length(epc) == 0) {
  close(con)
  stop("0 lines read")
}

#Initialize data frame column names using the values in the file header
colNames <- vector()
colNames[1] <- paste(epc[1], epc[2], sep = "") #First column name with Date and Time concatenated
for(i in 2:8) { #Rest of the column names
  colNames[i] <- epc[i + 1]
}

#Initialize variables used in the loop
epc_df <- data.frame()
df_row <- 0
line_num <- 0

repeat {
  #Read the next line in the file
  epc <- scan(con, what = "a", sep = ";", allowEscapes = TRUE, nlines = 1, na.strings = "?", quiet = TRUE)
  if (length(epc) == 0) {
    #end of file reached
    break
  } else {
    line_num <- line_num + 1
    if (line_num %% interval == 0) message(line_num, " lines read")
    
    epc_date <- as.Date(epc[1], date_format) #Date column in the file
    if (epc_date == date1 || epc_date == date2) { #Compare Date column with Feb 1, 2007 and Feb 2, 2007
      #epc_dt <- strptime(paste(epc[1], epc[2]), date_tm_format)
      #Append row to the data frame
      df_row <- df_row + 1
      epc_df[df_row, 1] <- paste(epc[1], epc[2]) #Date and Time in the first column
      for(df_col in 2:8) { #Rest of the columns
        epc_df[df_row, df_col] <- epc[df_col + 1]  
      }
    }
  }
}

message(line_num, " lines read")
message(df_row, " rows identified for processing")
names(epc_df) <- colNames #Assign column names to data frame
close(con) #Close file connection

#Create the plot
png_file_name <- paste(png_path, "plot4.png", sep = "")
png(filename = png_file_name)  #default size is 480 x 480 pixels
plot.new()
par(mfcol = c(2,2))
with(epc_df, 
     { plot(strptime(DateTime, date_tm_format), as.numeric(Global_active_power), 
            type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
       plot(strptime(DateTime, date_tm_format), as.numeric(Sub_metering_1), type = "n", 
            xlab = "", ylab = "Energy sub metering")
       points(strptime(DateTime, date_tm_format), as.numeric(Sub_metering_1), type = "l", col = "black")
       points(strptime(DateTime, date_tm_format), as.numeric(Sub_metering_2), type = "l", col = "red")
       points(strptime(DateTime, date_tm_format), as.numeric(Sub_metering_3), type = "l", col = "blue")
       legend("topright", col = c("black", "red", "blue"), legend = leg_text, lty = 1)
       plot(strptime(DateTime, date_tm_format), as.numeric(Voltage), 
            type = "l", xlab = "datetime", ylab = "Voltage")
       plot(strptime(DateTime, date_tm_format), as.numeric(Voltage), 
            type = "l", xlab = "datetime", ylab = "Global_reactive_power")
     })
dev.off()
message("Plot saved in ", png_file_name)