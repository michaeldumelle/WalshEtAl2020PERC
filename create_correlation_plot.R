## making the correlation image
library(tidyverse)
library(viridis)

# reading in data
student_data <- read_csv("studentdata.csv")

cors <- cor(student_data[, c("final_grade_per", "problems_correct_per", "problems_tried_per", "quartiles_finished_per")])
cors[upper.tri(cors, diag = TRUE)] <- NA
cors <- data.frame(cors)
cors <- cors %>% 
  rownames_to_column(var = "var1") %>% 
  pivot_longer(-var1, names_to = "var2", values_to = "cors") %>%
  na.omit(.) %>%
  mutate(cor_breaks = cut(cors, breaks = seq(-1, 1, length.out = 11), include.lowest = T)) %>%
  as_tibble(.)
ggplot(cors, mapping = aes(x = var1, y = var2, fill = cor_breaks)) + 
         geom_tile() + 
         scale_fill_viridis(name = "Correlation", discrete = TRUE, drop = FALSE) +
         scale_x_discrete(breaks = c("problems_correct_per", "problems_tried_per", "quartiles_finished_per"),
                          labels = c("PCP", "PTP", "QFP")) +
         scale_y_discrete(breaks = c("final_grade_per", "problems_correct_per", "problems_tried_per"),
                          labels = c("FGP", "PCP", "PTP")) +
         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20, face = "bold"), 
               axis.text.y = element_text(size = 20, face = "bold"),
               axis.title = element_blank(),
               legend.text = element_text(size = 20, face = "bold"),
               legend.title = element_text(size = 20, face = "bold"))

       