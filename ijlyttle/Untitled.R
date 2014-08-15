my_data <- student2012 %>% filter(CNT == "Albania")
my_data$age <- factor(my_data$ST06Q01)

ggplot(
  data = my_data,
  aes_string(
    x = "age",
    y = "PV1MATH"
  )
) + 
  geom_violin(scale = "width")

str_sym <- c("CNT", "ST04Q01", "ST28Q01")
#str_sym <- c("CNT")

symbols = str_sym %>% lapply(as.symbol)

df <- 
  student2012[, str_sym] %>%
  regroup(symbols) %>%
  summarize(n = n()) %>%

df_cnt <-
  student2012[, "CNT"] %>%
  group_by(CNT) %>%
  summarize(n_country = n())

df_new <- 
  left_join(df, df_cnt, by = "CNT") %>%
  mutate(prop = n / n_country)
  
  
call <- substitute(
  select(student2012, sym),  
  list(sym = lapply(str_sym, as.symbol))
)

tmp <- student2012[, str_sym]
