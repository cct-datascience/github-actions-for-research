library(pointblank)
library(readr)
library(tidyr)
library(dplyr)

heliconia <- read_csv("data/heliconia_sample.csv")


heliconia_tidy <- heliconia |> 
  select(-starts_with("notes_")) |> 
  pivot_longer(
    cols = c(-ranch, -plot, -tag_number, -row, -column),
    names_sep = "_",
    names_to = c("variable", "year")
  ) |> 
  pivot_wider(names_from = "variable", values_from = "value")

al <- action_levels(
  warn_at = 1, #warn when 1 or more rows fail
  stop_at = 0.05 #error when 5% of rows or more fail
)

heliconia_tidy |> 
  col_vals_expr(
    ~infl %% 1 == 0,
    brief = "`infl` is integer-ish",
    actions = al
  ) |> 
  col_vals_expr(
    ~shoots %% 1 == 0,
    brief = "`shoots` is integer-ish",
    actions = al
  ) |> 
  col_vals_expr(
    ~height %% 1 == 0,
    brief = "`height` is measured to nearest cm",
    actions = al
  ) |> 
  col_vals_between(
    columns = shoots,
    left = 1,
    right = 20,
    na_pass = TRUE,
    actions = al
  ) |> 
  col_vals_between(
    columns = height,
    left = 0,
    right = 200,
    na_pass = TRUE,
    actions = al
  ) |> 
  col_vals_between(
    columns = infl,
    left = 0,
    right = 3,
    na_pass = TRUE,
    actions = al
  )
