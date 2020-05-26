library(bupaR)
library(dplyr)
library(anytime)
library(tidyverse)

dat <- readxl::read_excel("Data/Sunderland Discharge Hub Data PB - 2nd Update.xls",sheet = "Sheet1")

dat$AppointmentDateTransformed <- as.integer(dat$AppointmentDateTransformed)

dat$AppointmentDateTransformed <- as.Date(dat$`AppointmentDateTransformed`, origin = "1900-01-01")


small_dat <- dat %>% 
  rename(`Professionalinvolved` = `Professional involved`)
  

Event_log2 <- small_dat %>% 
              mutate(status = "complete",
                     activity_instance = 1:nrow(small_dat)) 

Event_log3 <- Event_log2 %>% 
              eventlog(
                case_id = "PtReferralDateTime",
                activity_id = "SlotTypeTransformed",
                activity_instance_id = "activity_instance",
                timestamp = "AppointmentDateTransformed",
                lifecycle_id = "status",
                resource_id = "Professionalinvolved")

Event_log3 %>% process_map()
Event_log3 %>% filter_activity_frequency(percentage = 0.9) %>% process_map()
Event_log3 %>% precedence_matrix(type = "relative") %>% plot()

ResourceFrequency <- resource_frequency(Event_log3,"resource-activity")
write.csv(ResourceFrequency, file = "ResourceFrequency.csv")

traces <- Event_log3 %>% traces()
write.csv(traces, file = "Traces.csv")

performance1 <- Event_log3 %>% 
                process_matrix(type = 
                              performance(median,units = "days",
                                          flow_time = "inter_start_time")
                              )
write.csv(performance1, file = "FlowAnalysis.csv")
