##########################################################
##                    Combining                         ##
##                   Alexandra Hahn                     ##
##########################################################

#this document was used to create the final csv.files for data analysis

####KIMOCC temperature data####
col_names_df <- c("Year", "Month", "Day", "Hour", "Minute", "Second", "Depth..m", "SN", "C..mS.cm", "T..IPTS-90", "P..dbar", "Ox..ml.l", "PSAL..IPSS-78", "Sample")
col_names_master <- c("Date", "Time", "T..IPTS-90")

master <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(master) <- col_names_master

df <- read.csv("~/RStuff/masterarbeit/temperature/KIMOCC-temp.txt", header = FALSE, sep="", skip =  12)

# give meaningful column names
names(df) <- col_names_df

# reformatting date
df$Date <- paste(df$Year, df$Month, df$Day, sep = "-")
df$Time <- paste(df$Hour, df$Minute, df$Second, sep = ":")

# subset data to only include useful columns
df_useful <- subset(df, select = c(15,16,10))

# add to master
master <- rbind(master, df_useful)

# save master as csv
write.csv(master,
          file = paste0("~/RStuff/masterarbeit/temperature/", "/KIMOCC.csv"), row.names = FALSE)

#### heaters #####

file_list <- list.files()

files <- dir("C:/Users/A.Hahn/Documents/RStuff/masterarbeit/temperature", 
             full.names = TRUE)
df <- lapply(files, function(x) 
  read.delim(x, sep = '\t', header = FALSE)) %>% 
  plyr::ldply() 
write.csv(df, file="C:/Users/A.Hahn/Documents/RStuff/masterarbeit/temperature/temp_combined.csv")
temp <- read.csv("~/RStuff/masterarbeit/temperature/temp_combined.csv", dec = ",")
write.csv(temp, file="C:/Users/A.Hahn/Documents/RStuff/masterarbeit/temperature/temp.csv")

#final csv is edited in excel --> temp_running
