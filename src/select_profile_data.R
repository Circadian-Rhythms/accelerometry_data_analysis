library(dplyr)
library(readr)

crt <- read_csv("./CRT_environ_demograph/CRT171121_colnames.csv")
selected_data <- crt %>%
  select(eid, "_21003_0_0", "_31_0_0", "_21000_0_0", "_189_0_0", "_21001_0_0", 
         "_1180_0_0", "_1200_0_0", "_826_0_0", "_3426_0_0", "_1160_0_0", 
         "_20116_0_0", "_1558_0_0", "_6142_0_0", "_20118_0_0", "_2020_0_0", 
         "_2050_0_0", "_2090_0_0", "_767_0_0", "_24012_0_0", "_24011_0_0",
         "_24011_0_0", "_24010_0_0", "_24009_0_0", "_24022_0_0") %>%
  rename(
    age = "_21003_0_0",
    sex = "_31_0_0",
    ethnic = "_21000_0_0",
    ses = "_189_0_0",
    bmi = "_21001_0_0",
    chronotype = "_1180_0_0",
    insomnia = "_1200_0_0",
    shift_work = "_826_0_0",
    night_shift_work = "_3426_0_0",
    sleep_hours = "_1160_0_0",
    smoking = "_20116_0_0",
    alcohol = "_1558_0_0",
    employment = "_6142_0_0",
    population_density = "_20118_0_0",
    loneliness = "_2020_0_0",
    depressed = "_2050_0_0",
    gp_depressed = "_2090_0_0",
    work_hours = "_767_0_0",
    inv_dist_major_road = "_24012_0_0",
    traffic_major_road = "_24011_0_0",
    inv_dist_road = "_24010_0_0",
    traffic_road = "_24009_0_0",
    noise_pollution = "_24022_0_0"
  )

write_csv(selected_data, "./data/profile_data.csv")

