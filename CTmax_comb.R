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


#### temperature and wild CTmax ####
#code to create "sub.csv"
{#before running code load data sets "final" and "wild"
col_sub <- c("Date", "Temperature")

sub_temp <- subset(final, select = c(1,3))
colnames(sub_temp) <- col_sub
sub_temp$Origin <- "kimocc"

sub_wild <- subset(wild, select = c(2,10))
colnames(sub_wild) <-col_sub
sub_wild$Origin <- "ctmax"
sub_wild <- within(sub_wild, { Temperature = (Temperature -20)/0.58 })

sub <- rbind(sub_temp,sub_wild)
}

sub <- read.csv("~/RStuff/masterarbeit/temperature/sub.csv", header = TRUE)
mycolors <- c("kimocc" = "#D5968F", "ctmax"= "#178B76")
sub$Date <-  as.Date(sub$Date)

Sys.setlocale("LC_TIME", "English")#set locale to English to avoid German months

ggplot(sub, aes(x=Date, y=Temperature, group=Origin, color=Origin)) +
  geom_path() +
  geom_point() +
  theme_light(base_size = 12)+ 
  xlab("")+
  scale_y_continuous(name="SST in °C", sec.axis = sec_axis(~ 0.58*.+20, name="CTmax in °C")) +
  scale_color_manual(name="Origin", values = mycolors) +
  theme(
    axis.title.y = element_text(color = mycolors["kimocc"]),
    axis.text.y = element_text(color = mycolors["kimocc"]),
    axis.title.y.right = element_text(color = mycolors["ctmax"]),
    axis.text.y.right = element_text(color = mycolors["ctmax"]),
    legend.position = "none")

