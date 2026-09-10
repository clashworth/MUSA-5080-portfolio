library (tidyverse)
library (tidycensus)
pa_income <- get_acs(geography = "county", variables = "B19013_001", state = "PA", year = 2023, survey = "acs5")
pa_income
dim (pa_income)
glimpse (pa_income)
head (pa_income)
pa_income$GEOID
as.numeric("01001")
#the zero disappeared, showing that the GEOID is always in text
filter(pa_income, estimate > 60000)
#exactly 67 rows
filter(pa_income, moe > 3000)
#there are 15 of them
filter(pa_income, estimate < 50000)
#only one, Cameron County
select(pa_income, NAME, estimate, moe)
#67 rows, 3 columns
select(pa_income, GEOID, estimate)
mutate (pa_income, moe_pct = moe / estimate * 180)
#new column is a calculation of moe / estumate *180
pa_income <- mutate (pa_income, moe_pct = moe/estimate * 180)
pa_income$moe_pct
#moe_pct is the percent margin of error
arrange(pa_income, moe_pct)
arrange (pa_income, desc(moe_pct))
#doesn't change what is in the table, just how it is ordered. The county with the top moe_pct is Cameron County.
step1 <- filter (pa_income, moe_pct > 5)
step2 <- arrange(step1, desc (moe_pct))
step3 <- select (step2, NAME, estimate, moe, moe_pct)
step3
pa_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select (NAME, estimate, moe, moe_pct)
#the pipe goes at the end of a line, never at the start of the next one

worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange (moe_pct) %>%
  select (NAME, moe_pct)

pa_income <- mutate(pa_income, reliable = moe_pct < 5)

pa_income %>%
  group_by (reliable) %>%
  summarize (n=n), avg_income = mean (estimate)

pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))
pa_income <- pa_income %>%
  mutate (reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE ~ "Low confidence"
  ))
count (pa_income, reliability)
# High confidence = 5, Low confidence = 34, Moderate = 28

pa_two <- get_acs(geography = "county", variables = c("B19013_001", "B01003_001"), state = "PA", year = 2023, survey = "acs5")
pa_two
# There are 134 rows instead of 67 because we are looking at two variables per each PA county (67x2)

pa_wide <- get_acs(
  geography = "county",
  varibles = c(income = "B191013_001",
               pop = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide")
pa_wide

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)
pa_wide
#E is estimate, and M is margin of error

pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
#These are the counties with the top ten highest percent margin of error

ny_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "NY", year = 2023, survey = "acs5",
  output = "wide"
)
ny_wide
ny_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)

#the same pattern holds

ny_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                median_gross_rent    = "B25064_001"),
  state = "NY", year = 2023, survey = "acs5",
  output = "wide"
)
ny_wide

pa_wide2 <- get_acs(
  geography = "tract",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", county = "Philadelphia", year = 2023, survey = "acs5",
  output = "wide"
)
pa_wide2
