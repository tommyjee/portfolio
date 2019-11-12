# ---------------------------------------------------------------------------------------
# Used to test code:
# ---------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)

# Given data
shift_test <- data_frame(shift_id = 1235, deliverer_id = 6745, 
                         shift_start_time = "2/29/16 7:00",
                         shift_end_time = "2/29/16 9:40",
                         total_pay = 46.78, number_of_deliveries = 9)

# Test for working through midnight
shift_test2 <- data_frame(shift_id = 1235, deliverer_id = 6745, 
                          shift_start_time = "2/29/16 20:00",
                          shift_end_time = "3/1/16 3:40",
                          total_pay = 146.78, number_of_deliveries = 19)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
# 1. EXTRACT
# 
# ASSUMPTIONS: database credentials, loading the whole `Shift` table is appropriate (e.g.,
# manageable size due  to the `Shift` table being updated monthly, weekly, daily, hourly,
# by region, etc.)
# ---------------------------------------------------------------------------------------
# library(DBI)
# con <- dbConnect(RMySQL::MySQL(),
#                  dbname = "dbname",
#                  host = "host",
#                  port = 234,
#                  user = "username",
#                  password = "password")
# 
# delivery <- dbGetQuery(con, '
#                    SELECT "shift_id", "deliverer_id", "shift_start_time", 
#                           "shift_end_time","total_pay"
#                    FROM "Shift"
#                    ')

# ---------------------------------------------------------------------------------------
# 2. TRANFORM
#
# ASSUMPTIONS: hours are in military time (i.e., 0 - 23), no deliverer works more than
# 24 hours per shift
# ---------------------------------------------------------------------------------------
# library(tidyverse)

for (shift in list(shift_test, shift_test2)) {
  shift_id <- unique(shift$shift_id)
  deliverer_id <- unique(shift$deliverer_id)
  
  to_upload <- data_frame()
  
  for (work_sesh in shift_id) {
    for (worker in deliverer_id) {
      # Obtaining data for each shift for each deliverer
      dat2 <- shift %>% filter(shift_id == work_sesh, deliverer_id == worker) %>%
        mutate(shift_end_time = as.POSIXct(strptime(shift_end_time, "%m/%d/%y %H:%M")),
               shift_start_time = as.POSIXct(strptime(shift_start_time, "%m/%d/%y %H:%M")))
      if (nrow(dat2) > 0) {
        hourly_earnings = dat2$total_pay /
          as.numeric(dat2$shift_end_time - dat2$shift_start_time)
        
        # If deliverer doesn't work through midnight
        if (hour(dat2$shift_start_time) < hour(dat2$shift_end_time)) {
          hour <- hour(dat2$shift_start_time):hour(dat2$shift_end_time)
          weekday <- rep(weekdays(dat2$shift_start_time, length(hour)))
          date <- rep(as.Date(dat2$shift_start_time), length(hour))
        }
        
        # If deliverer works through midnight
        else {
          before_midnight <- hour(dat2$shift_start_time):23
          after_midnight <- 0:hour(dat2$shift_end_time)
          hour <- c(before_midnight, after_midnight)
          weekday <- c(rep(weekdays(dat2$shift_start_time), length(before_midnight)),
                       rep(weekdays(dat2$shift_end_time), length(after_midnight)))
          date <- c(rep(as.Date(dat2$shift_start_time, "ymd"), length(before_midnight)),
                    rep(as.Date(dat2$shift_end_time, "ymd"), length(after_midnight)))
        }
        
        # Per shift_id and deliverer_id combination
        temp <- data_frame(shift_id = rep(shift_id, length(hour)),
                           deliverer_id = rep(deliverer_id, length(hour)),
                           hour = hour,
                           weekday = weekday,
                           date = date,
                           earnings = rep(hourly_earnings, length(hour)))
        
        # Combine all shift_id and deliverer_id
        to_upload <- rbind(to_upload, temp)
      }
    }
  }
  print(to_upload)
}

# ---------------------------------------------------------------------------------------
# 3. LOAD

# ASSUMPTION: replacing table is more appropriate than appending to existing table.
# ---------------------------------------------------------------------------------------
# dbWriteTable(con, "shift_long", to_upload, overwrite = TRUE)
