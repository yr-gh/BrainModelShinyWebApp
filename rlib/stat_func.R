library(dplyr)
library(magrittr)

BasicStatByGroup <- function(data) {
  data %>% group_by(group) %>% 
    summarise(mean = mean(value),
              sd = sd(value),
              sem = sd(value) / sqrt(n()),
              n = n())
}

TTestOverControl <- function(data, group_levels, sig_level_scaling_factor) {
  t_ls <- setNames(vector("list", length(group_levels[-1])), group_levels[-1])
  data %>% filter(group == group_levels[1]) %>% pull(value) -> control
  for (g in group_levels[-1]) {
    data %>% filter(group == g) %>% pull(value) -> test
    t.test(control, test, var.equal = F) -> t_res
    data.frame(x_y = paste0(g, "_vs_", group_levels[1]),
               t = t_res$statistic,
               df = t_res$parameter,
               p.value = t_res$p.value,
               conf.int = paste0("[", t_res$conf.int[1], ",", t_res$conf.int[2], "]"),
               alpha = 1 - attr(t_res$conf.int, "conf.level"),
               estimate.mean = paste0(t_res$estimate, collapse = "/"),
               null.value = t_res$null.value,
               stderr = t_res$stderr,
               alternative = t_res$alternative,
               method = t_res$method) %>% set_rownames(NULL) %>% 
      mutate(sig.level = if_else(p.value < 0.05,
                                 if_else(p.value < 0.01,
                                         if_else(p.value < 0.001,
                                                 if_else(p.value < 0.0001, "****", "***"),
                                                 "**"), "*"), "")) -> t_ls[[g]]
  }
  t_df <- do.call(rbind, t_ls)
  t_df$group <- row.names(t_df)
  data %>% group_by(group) %>% slice_max(value) %>% 
    slice_head(n = 1) %>% ungroup() %>% 
    mutate(label_y = value + max(data$value) / sig_level_scaling_factor) %>% 
    select(group, label_y) %>% inner_join(t_df, by = "group")
}
