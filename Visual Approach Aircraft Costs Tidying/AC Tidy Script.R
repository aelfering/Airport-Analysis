#install.packages("readxl")

library(readxl)

my_data <- read_excel("2020-07-27_Aircraft_Costs-2.xls")
years <- c(2000:2020)

y  <- NULL
for (i in years) {
  year <- ifelse(grepl(i, colnames(my_data)[1]), i, NA)
  y <- as.data.frame(rbind(y, year))
}

year <- as.integer(dplyr::filter(y, !is.na(V1)))
rate <- as.character(my_data[2,1])
airline <- as.character(my_data[2,2])

tidy_xl_vs <- function(df) {
mark1 <- my_data %>%
  filter(!is.na(`...2`)) %>%
  mutate(Airline = airline,
         Frequency = rate)

colnames(mark1)[1] <- "Metric"
colnames(mark1)[2] <- "Total"
mark2 <- mark1[-1, ]

return(mark2)}


files <- list.files(path = "~/Desktop/TestAircraft", pattern = "*.xls", full.names = T)

tbl <- sapply(files, read_excel, simplify=FALSE)
