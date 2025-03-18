# SETUP ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(seasonal)
library(dynlm)
library(clipr)
library(scales) 
library(patchwork)

inflation_decomposed <- readRDS("inflation_decomposed.rds")

# Plot: Inflation by Demand and Supply Components ------------------------------

pibycomponent <- inflation_decomposed %>%
  mutate(
    weighted_inflation = inflation * laspeyres_weight,
    shock_d_s_thresh1 = factor(shock_d_s_thresh1, levels = c("Demand", "Ambiguous", "Supply"))
  ) %>%
  group_by(date, shock_d_s_thresh1) %>%
  summarise(
    inflation = sum(weighted_inflation, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  drop_na(shock_d_s_thresh1)


p_pibycomponent <- ggplot(pibycomponent, aes(x = date, y = inflation, fill = shock_d_s_thresh1)) +
  annotate("rect", xmin = as.Date("2014-04-01"), xmax = as.Date("2015-03-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightblue") +
  annotate("rect", xmin = as.Date("2019-10-01"), xmax = as.Date("2020-09-30"),
           ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgreen") +
  annotate("text", x = as.Date("2014-11-30"),
           y = max(pibycomponent$inflation, na.rm = TRUE) + 0.3 * max(pibycomponent$inflation, na.rm = TRUE),
           label = "Consumption Tax +3%", vjust = 2, fontface = "bold", size = 6) +
  annotate("text", x = as.Date("2020-06-30"),
           y = max(pibycomponent$inflation, na.rm = TRUE) + 0.5 * max(pibycomponent$inflation, na.rm = TRUE),
           label = "Consumption Tax +2%", vjust = 2, fontface = "bold", size = 6) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_fill_manual(name = "", values = c("Supply" = "darkred", "Ambiguous" = "grey70", "Demand" = "darkblue")) +
  labs(title = "Decomposing Japan's Inflation  - Demand vs Supply Driven Components",
       x = "Year", y = "Inflation Rate") +
  theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.position = c(0.01, 0.99),
        legend.justification = c("left", "top"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 16),
        plot.caption = element_text(hjust = 0, size = 10))

# show p_pibycomponent
p_pibycomponent

# Plot: Net Demand Contributinon------------------------------------------------
# Helper function to compute zero-crossing points using linear interpolation
lin_zeros <- function(data, x, y){
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))
  x <- data[[xname]]
  y <- data[[yname]]
  ix <- which(c(FALSE, abs(diff(sign(y))) == 2))
  res <- lapply(ix, function(i) 
    approx(x = y[c(i - 1, i)], y = x[c(i - 1, i)], xout = 0))
  res <- do.call(rbind.data.frame, res)
  res <- res[2:1]
  row.names(res) <- NULL
  names(res) <- c(xname, yname)
  res
}

# Calculate Net Demand (Supply) Contribution
netdemand <- pibycomponent %>%
  filter(shock_d_s_thresh1 != "Ambiguous") %>%
  group_by(date) %>%
  mutate(DemandSupplyInflation = sum(abs(inflation))) %>%
  ungroup() %>%
  mutate(
    inflation_weight = abs(inflation)/ DemandSupplyInflation
  ) %>%
  select(date, shock_d_s_thresh1, inflation_weight) %>%
  pivot_wider(id_cols = date, names_from = shock_d_s_thresh1, values_from = inflation_weight) %>%
  mutate(
    net_demand_contribution = Demand - Supply
  ) %>%
  select(date, net_demand_contribution)


# Add points intersecting x axis
plot_data_netdemand <- netdemand %>%
  bind_rows(
    netdemand %>%
      lin_zeros(date, net_demand_contribution) %>%
      mutate(date = as.Date(date, origin = "1970-01-01"))
  ) %>%
  arrange(date) %>%
  mutate(xend = lead(date),
         yend = lead(net_demand_contribution, default = 0)) %>%
  mutate(line_color = (net_demand_contribution >= 0 & yend > 0) | 
           (net_demand_contribution > 0 & yend >= 0)) %>%
  mutate(DominantComponent = ifelse(line_color, "Demand Dominant", "Supply Dominant"),
         DominantComponent = factor(DominantComponent)) %>%
  select(-line_color) %>%
  arrange(date) %>%
  mutate(group = cumsum(DominantComponent != lag(DominantComponent, default = first(DominantComponent))))

# Add additional transition points so the the graph looks ok
plot_data_netdemand_trans <- plot_data_netdemand  %>%
  mutate(prev_group = lag(group),
         prev_DominantComponent = lag(DominantComponent),
         transition = !is.na(prev_group) & (group != prev_group)) %>%
  filter(transition) %>%
  mutate(group = prev_group,
         DominantComponent = prev_DominantComponent
  ) %>%
  select(-prev_group, -prev_DominantComponent, -transition)



plot_data_netdemand <- plot_data_netdemand %>%
  rbind(plot_data_netdemand_trans) %>%
  arrange(date, group)

x_pos <- min(plot_data_netdemand$date, na.rm = TRUE) - 30

p_netdemand <- ggplot(plot_data_netdemand, aes(x = date)) +
  annotate("rect", xmin = as.Date("2014-04-01"), xmax = as.Date("2015-03-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightblue") +
  annotate("rect", xmin = as.Date("2019-10-01"), xmax = as.Date("2020-09-30"),
           ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgreen") +
  geom_ribbon(aes(ymin = 0, ymax = net_demand_contribution, fill = DominantComponent, group = group), alpha = 0.3) +
  geom_segment(aes(y = net_demand_contribution, xend = xend, yend = yend, color = DominantComponent)) +
  scale_color_manual(name = "", breaks = c("Demand Dominant", "Supply Dominant"),
                     values = c("darkblue", "darkred")) +
  scale_fill_manual(name = "", breaks = c("Demand Dominant", "Supply Dominant"),
                    values = c("darkblue", "darkred")) +
  scale_y_continuous(labels = function(x) {
    sapply(x, function(v) if(v < 0) paste0("(", percent(abs(v), accuracy = 1), ")") else percent(v, accuracy = 1))
  }) +
  labs(title = "Net Contribution of Demand (Supply) Components", x = "Year", y = "",
       caption = "Reference: Shapiro, A. H. (2024). Decomposing Supply and Demand Driven Inflation. Journal of Money, Credit and Banking.\nData: This service uses API functions from e-Stat, however its contents are not guaranteed by government.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        plot.caption = element_text(hjust = 0, size = 10),
        plot.margin = margin(5.5, 60, 5.5, 5.5)) +
  annotate("text", x = x_pos,
           y = max(plot_data_netdemand$net_demand_contribution, na.rm = TRUE) * 0.9,
           label = "Demand Dominant", hjust = 0, vjust = 0.5, size = 5, fontface = "bold") +
  annotate("text", x = x_pos,
           y = min(plot_data_netdemand$net_demand_contribution, na.rm = TRUE) * 0.9,
           label = "Supply Dominant", hjust = 0, vjust = 0.5, size = 5, fontface = "bold")

p_netdemand

# Plot Combine: by Component and Net Demand ------------------------------------

combined_plot <- p_pibycomponent / p_netdemand + plot_layout(heights = c(3, 1))
ggsave("inflation_decomposition.png", plot = combined_plot, width = 18, height = 12, dpi = 300)


# Plot: Inflation Over Time ----------------------------------------------------
inflation_decomposed %>%
  mutate(
    weighted_inflation = inflation * weight
  ) %>%
  group_by(date) %>%
  # sum the weighted_inflation 
  summarise(
    inflation = sum(weighted_inflation, na.rm = TRUE)
  ) %>%
  ggplot(aes(x=date, y = inflation)) +
  geom_line()









