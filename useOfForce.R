# Look at the relationship between departments' use of force policies and
# violence.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)

forcePolicy <- read_excel("UseofForcePolicyReview-bbdj.xlsx")
# The column names are pretty long
colnames(forcePolicy) <- sub(":.*", "", colnames(forcePolicy))

# Strip answers down to TRUE (yes) / FALSE (no) values
yesOrNo <- function(x) {
  ifelse(grepl("^Yes", x), 
         "Yes",
         ifelse(grepl("^No", x),
                "No",
                NA))
}
forcePolicy <- mutate_each(forcePolicy, funs(yesOrNo), -`Police Department`)

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
  select(department = `Police Department`, 
         2:9, 
         rate_of_killings = `Rate of Police Killings per Million Population`,
         population = `2014 population (US Census)`) %>%
  gather("policy", "in_place", 2:9)

# Plot it
p <- ggplot(policyEffectiveness, aes(x = in_place, y = rate_of_killings)) + 
  geom_point(aes(color = in_place, size = population), 
             alpha = 0.5,
             position = position_jitter(width = 0.4, height = 0.0)) + 
  facet_wrap(~ policy) + 
  labs(title = "The effect of department policies on killings by police", 
       x = "Policy in place", 
       y = "Citizens killed by police (per million population)") + 
  scale_color_discrete(guide = FALSE) +
  scale_size_area(name = "City population",
                  breaks = seq(2e06, 8e06, length.out = 4),
                  labels = paste(seq(2, 8, length.out = 4), " million")) +
  theme(legend.position = c(0.85, 0.15))
print(p)
pCaption <- textGrob(paste(" ",
                           "Police killing data: http://mappingpoliceviolence.org/",
                           "Police departmental policy data: http://useofforceproject.org/",
                           sep = "\n"), 
                     x = 0, 
                     just = c("left", "bottom"), 
                     gp = gpar(fontface = "italic", fontsize = 12))
pWithCaption <- arrangeGrob(p, bottom = pCaption)
ggsave("all police force policies.png", pWithCaption, dpi=120)

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
