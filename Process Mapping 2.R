library(bupaR)
library(dplyr)

dat <- readxl::read_excel("Data/Sunderland discharge hub data PB.xlsx",sheet = "Sheet1")

dat$`Appointment Date Transformed` <- na_if(dat$`Appointment Date Transformed`,"no appt")

dat$`Appointment Date Transformed`<- as.integer(dat$`Appointment Date Transformed`)

dat$`Appointment Date Transformed`<- as.Date(dat$`Appointment Date Transformed`, origin = "1900-01-01")

small_dat <- head(dat, 11)

Event_Log <- simple_eventlog(small_dat,
                             case_id = "Full Name",
                             activity_id = "Slot Type Transformed",
                             timestamp = "Appointment Date Transformed",
                             resource_id = "Professional involved")

Event_log2 <- small_dat %>% 
              mutate(status = "complete",
                     activity_instance = 1:nrow(small_dat)) 

Event_log3 <- Event_log2 %>% 
              eventlog(
                case_id = "Full Name",
                activity_id = "Slot Type Transformed",
                activity_instance_id = "activity_instance",
                timestamp = "Appointment Date Transformed",
                lifecycle_id = "status",
                resource_id = "Professional involved")

Event_log3 %>% activities

Event_log3

Event_log3 %>% activities
Event_log3 %>% summary
process_map(Event_log3)

patients %>% 
  filter_activity(c("X-Ray", "Blood test")) %>% 
  activities

head(patients)

pt <- patients

example_log_1
