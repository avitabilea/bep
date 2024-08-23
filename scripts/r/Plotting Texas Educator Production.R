#ReadMe----
#Purpose: Plotting Texas educator production data
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(dplyr, tidyr, ggplot2, here, showtext)

#Clear all
rm(list=ls())

#Add fonts
font_add(family = "LMRoman", regular = here("lmroman10-regular.otf"))
showtext_auto()

# New teacher preparation by route----
# Data from dashboard 2: https://tea4avcastro.tea.state.tx.us/ELQ/teacherproduction/newlycertifiededucators.html
tea_prep_by_route <- tibble(
  `Academic Year` = c("2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23"),
  Alternative = c(11310, 12769, 13262, 14353, 12622, 12908, 10235, 13607, 10780, 9839),
  `Out of State` = c(3471, 3909, 3975, 3918, 3559, 4141, 3594, 4231, 4749, 4882),
  `Post Bac` = c(1110, 1187, 1103, 1029, 767, 749, 684, 732, 587, 485),
  Traditional = c(15157, 14867, 14371, 14129, 14239, 14354, 10090, 12419, 10601, 9403)
)

# Reshape the data into long format, calculate percentages, and positions for labels
teacher_data_long <- tea_prep_by_route %>%
  pivot_longer(cols = -`Academic Year`, names_to = "Certification Route", values_to = "Number of Teachers") %>%
  mutate(`Certification Route` = factor(`Certification Route`, levels = c("Traditional", "Alternative", "Post Bac", "Out of State"))) %>%
  arrange(`Academic Year`, `Certification Route`) %>%
  group_by(`Academic Year`) %>%
  mutate(Total = sum(`Number of Teachers`),
         Percentage =  (`Number of Teachers` / Total) * 100,
         ypos = cumsum(Percentage) - (Percentage / 2),
         ypos = 100-ypos) %>%
  ungroup()

# Create the stacked bar chart
ggplot(teacher_data_long, aes(x = `Academic Year`, y = Percentage, fill = `Certification Route`)) +
  geom_bar(stat = "identity", position = "stack") +
  # Add percentage labels for groups with more than 5%
  geom_text(data = teacher_data_long %>% filter(Percentage > 5),
            aes(y = ypos, label = sprintf("%.1f%%", Percentage)),
            color = "white",
            size = 3.5,
            family = "LMRoman") +
  labs(x = "Academic Year",
       y = "% of Newly Certified Teachers",
       fill = "Cert. Route") +
  theme_minimal() +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(family = "LMRoman", color = "black"),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(sub("Bilingual Education Program", "", here()), "Apps/Overleaf/Bilingual Education Program/Figures_and_Tables/TX_new_teachers_by_cert_path.pdf"), width = 6, height = 4)

# New teacher preparation by route----
# Replace with the actual data from your image
bilingual_prep_by_route <- tibble(
  `Academic Year` = c("2016-17", "2015-16", "2014-15", "2020-21", "2018-19", "2017-18", "2013-14", "2019-20", "2021-22", "2012-13", "2022-23"),
  Alternative = c(2426, 2341, 2243, 2113, 2059, 1945, 1860, 1837, 1386, 1384, 1076),
  `Cert by Exam` = c(9953, 10718, 10119, 11907, 17201, 9973, 10141, 13296, 9664, 9010, 8416),
  `Out of State` = c(188, 186, 150, 305, 204, 188, 114, 224, 350, 86, 648),
  `Post Bac` = c(116, 147, 162, 182, 112, 89, 103, 172, 148, 129, 123),
  Traditional = c(2161, 2190, 2145, 1851, 1953, 2090, 2444, 1498, 1475, 2392, 1211)
)

# Reshape the data into long format, calculate percentages, and positions for labels
bilingual_data_long <- bilingual_prep_by_route %>%
  pivot_longer(cols = -`Academic Year`, names_to = "Certification Route", values_to = "Number of Teachers") %>%
  mutate(`Certification Route` = factor(`Certification Route`, levels = c("Cert by Exam", "Traditional", "Alternative", "Post Bac", "Out of State"))) %>%
  arrange(`Academic Year`, `Certification Route`) %>%
  group_by(`Academic Year`) %>%
  mutate(Total = sum(`Number of Teachers`),
         Percentage =  (`Number of Teachers` / Total) * 100,
         ypos = cumsum(Percentage) - (Percentage / 2),
         ypos = 100-ypos) %>%
  ungroup()

# Create the stacked bar chart
ggplot(bilingual_data_long, aes(x = `Academic Year`, y = Percentage, fill = `Certification Route`)) +
  geom_bar(stat = "identity", position = "stack") +
  # Add percentage labels for groups with more than 5%
  geom_text(data = bilingual_data_long %>% filter(Percentage > 5),
            aes(y = ypos, label = sprintf("%.1f%%", Percentage)),
            color = "white",
            size = 3.5,
            family = "LMRoman") +
  labs(x = "Academic Year",
       y = "% of Newly Certified Teachers",
       fill = "Route") +
  theme_minimal() +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(family = "LMRoman", color = "black"),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom")
ggsave(paste0(sub("Bilingual Education Program", "", here()), "Apps/Overleaf/Bilingual Education Program/Figures_and_Tables/TX_new_bilingual_teachers_by_cert_path.pdf"), width = 6, height = 4)

# Bilingual educator production line graph
bilingual_prep_by_route %>%
  mutate(Total_Teachers = Alternative + `Cert by Exam` + `Out of State` + `Post Bac` + Traditional) %>%
  ggplot(aes(x = `Academic Year`, y = Total_Teachers, group = 1)) +
  geom_line(linewidth=1.4) +
  labs(x = "Academic Year",
       y = "# Newly Certified Bilingual Teachers")+
  theme_minimal() +
  scale_y_continuous(expand = c(0,0), limits = c(0,22500)) +
  theme(text=element_text(family="LMRoman", color = "black"), plot.caption = element_text(hjust = 0))
ggsave(paste0(sub("Bilingual Education Program", "", here()), "Apps/Overleaf/Bilingual Education Program/Figures_and_Tables/TX_new_bilingual_teachers_by_year.pdf"), width = 6, height = 4) 