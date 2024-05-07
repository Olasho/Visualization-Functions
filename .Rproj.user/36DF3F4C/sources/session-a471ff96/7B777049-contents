# Creating a function for summarizing a one-way or two-way or a three-way summary table using tabyl

tabyl_summary_table <- function(df, variable_1, variable_2 = NULL, variable_3 = NULL) {
  require(janitor)
  require(tidyverse)
  
  if(!is.null(variable_1) && is.null(variable_2) && is.null(variable_3)) {
    df %>%
      tabyl({{variable_1}}) %>% 
      adorn_totals("row") %>%
      adorn_pct_formatting()
  } else if (!is.null(variable_1) && !is.null(variable_2) && is.null(variable_3)) {
    df %>%
      tabyl({{variable_1}}, {{variable_2}}) %>% 
      adorn_totals("col") %>%
      adorn_percentages("row") %>%
      adorn_pct_formatting(digits = 2) %>%
      adorn_ns() %>%
      adorn_title()
  } else if(!is.null(variable_1) && !is.null(variable_2) && !is.null(variable_3)) {
    df %>%
      tabyl({{variable_1}}, {{variable_2}}, {{variable_3}}) %>% 
      adorn_totals("col") %>%
      adorn_percentages("row") %>%
      adorn_pct_formatting(digits = 2) %>%
      adorn_ns() %>%
      adorn_title()
  } else {
    # Handle other cases or provide a default behavior
    print("Invalid combination of arguments provided.")
  }
}



# creating a kable function to improve the summary table for the categorical variable

summary_kable <- function(df, header) {
  require(kableExtra)
  require(knitr)
  df %>%
    kbl(caption = header, align = "llr", format.args = list(big.mark = ",")) %>%
    kable_classic_2(full_width = F, html_font = "Cambria") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T, position = "center", font_size = 20)
}





# Creating the function for a two-way table that is aesthetically improved by datatable

tabyl_2_way_table <- function(df, variable_1, variable_2) {
  require(janitor)
  
  # Two-way table for two variables
  df %>%
    tabyl({{ variable_1 }}, {{ variable_2 }}) %>% 
    adorn_totals("col") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns() %>%
    adorn_title() %>%
    datatable(class = 'cell-border stripe', filter = "top")
}
