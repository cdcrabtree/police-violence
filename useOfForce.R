# Look at the relationship between departments' use of force policies and
# violence.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

forcePolicy <- read_excel("UseofForcePolicyReview-bbdj.xlsx")
# The column names are pretty long
colnames(forcePolicy) <- sub(":.*", "", colnames(forcePolicy))

# Strip answers down to TRUE (yes) / FALSE (no) values
yesOrNo <- function(x) {
  ifelse(grepl("^Yes", x), 
         TRUE,
         ifelse(grepl("^No", x),
                FALSE,
                NA))
}
forcePolicy <- mutate_each(forcePolicy, funs(yesOrNo), 2:ncol(forcePolicy))

# Now look at the mapping police violence project's report to find these
# departments.
mpvReport <- read_excel("MPVDatasetDownload-83zj.xlsx", sheet = 2) 

mpvReport <- mpvReport %>%
  mutate(`Police Department` = sub("^[^[:alpha:]]*", "", `Police Department`),
         row_number = 1:nrow(.)) %>% 
  filter(row_number <= 60) %>%
  rename(`Police Department 2` = `Police Department`)

# We'll do a fuzzy left join of these tables. This code is inelegant and
# inefficient, but works OK for this few rows.
forcePolicy[["Police Department"]][7] <- "Las Vegas"
matches <- rep(NA, nrow(forcePolicy))
for (r in seq_len(nrow(forcePolicy))) {
  matches[r] <- agrep(forcePolicy[["Police Department"]][r],
                      mpvReport[["Police Department 2"]])
}
forcePolicy <- cbind(forcePolicy, mpvReport[matches, ])

# Now let's see how these policies may effect police killings
policyEffectiveness <- forcePolicy %>%
  select(department = `Police Department`, 2:9, rate_of_killings = `Rate of Police Killings per Million Population`) %>%
  gather("policy", "in_place", 2:9)

# Plot it
ggplot(policyEffectiveness, aes(x = in_place, y = rate_of_killings)) + 
  geom_point(aes(color = in_place), 
             position = position_jitter(width = 0.3, height = 0.0)) + 
  facet_wrap(~ policy) + 
  labs(title = "The effect of department policies on police killings", 
       x = "Policy in place", 
       y = "Rate of police killings (per million population)") + 
  theme(legend.position = "none")
ggsave("all police force policies.png")

# Wow. Zero in on de-escalation though.
policyEffectiveness %>% 
  filter(policy == "Requires De-Escalation") %>%
  ggplot(aes(x = in_place, y = rate_of_killings)) + 
  geom_boxplot(aes(fill = in_place)) + 
  labs(title="Policies requiring officers to de-escalate vs. police violence",
       x = "Are officers required to de-escalate situations?",
       y = "Rate of police killings (per million population)") + 
  theme(legend.position = "none")
ggsave("de-escalation reduces police killings.png")
