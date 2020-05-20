library(bupaR)
library(dplyr)
library(anytime)

dat <- readxl::read_excel("Data/Sunderland discharge hub data PB.xlsx",sheet = "Sheet1")

dat$`Appointment Date Transformed` <- na_if(dat$`Appointment Date Transformed`,"no appt")

dat$`Appointment Date Transformed`<- as.integer(dat$`Appointment Date Transformed`)

dat$`Appointment Date Transformed`<- as.Date(dat$`Appointment Date Transformed`, origin = "1900-01-01")

simple <- simple_eventlog(dat,
                case_id = "Full Name",
                activity_id = "Slot Type",
                timestamp = "Appointment Date Transformed")

process_map(simple)
process_map(simple, performance(median, "days"))
