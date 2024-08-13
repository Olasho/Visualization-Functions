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


# Creating a function to convert a three-way table created by the tabyl function to a kable

convert_to_kable_list <- function(table_list) {
  require(tidyverse)
  require(kableExtra)
  
  # Initialize an empty list to store the kable outputs
  table_kables <- list()
  
  # Iterate over each table in the list
  
  for (i in seq_along(table_list)) {
    
    # Extract EuroStandard levels from the current table
    euro_standard <- attr(table_list[[i]], "dimnames")[["EuroStandard"]]
    
    # Convert the tabyl object to a data frame
    tbl_df <- as.data.frame(table_list[[i]])
    
    # Create the kable
    kbl_tbl <- tbl_df %>%
      kbl(caption = paste("Summary table of vehicles by taxi status for EuroStandard", euro_standard),
          align = "ccccc",
          format.args = list(big.mark = ",")
      ) %>%
      kable_minimal(full_width = F, html_font = "Cambria") %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = T,
        position = "center",
        font_size = 20
      ) %>%
      pack_rows(paste(names(table_list[i])), 1, nrow(table_list[[1]]))
    
    # Append the kable to the list
    table_kables[[i]] <- kbl_tbl
  }
  
  return(table_kables)
}



# Cleaning a table

north_absence_3term_nat_reg_la_region <- north_absence_3term_nat_reg_la %>%
  filter(geographic_level == "Regional", school_type == "Total") %>%
  mutate(Region = case_when(
    region_name %in% c("North East", "North West", "Yorkshire and The Humber") ~ "North",
    region_name %in% c("South East", "South West", "Outer London", "Inner London", "East of England") ~ "South",
    region_name %in% c("West Midlands", "East Midlands") ~ "Midlands")) %>%
  group_by(Year, Region) %>%
  summarise(`Overall absence rate` = sum(sess_overall_percent * sess_possible)/sum(sess_possible),
            `Authorised absence rate` = sum(sess_authorised_percent * sess_possible)/sum(sess_possible),
            `Unauthorised absence rate` = sum(sess_unauthorised_percent * sess_possible)/sum(sess_possible),
            `Overall persistent absence rate` = sum(sess_overall_percent_pa_10_exact * sess_possible_pa_10_exact)/sum(sess_possible_pa_10_exact),
            `Authorised persistent absence rate` = sum(sess_authorised_percent_pa_10_exact * sess_possible_pa_10_exact)/sum(sess_possible_pa_10_exact),
            `Unauthorised persistent absence rate` = sum(sess_unauthorised_percent_pa_10_exact * sess_possible_pa_10_exact)/sum(sess_possible_pa_10_exact))
select(Year, region_name, num_schools, enrolments_pa_10_exact_percent, enrolments_pa_50_exact_percent, sess_overall_percent, sess_authorised_percent, sess_unauthorised_percent, sess_overall_percent_pa_10_exact, sess_authorised_percent_pa_10_exact, sess_unauthorised_percent_pa_10_exact) %>%
  rename(Region = region_name,
         `Percentage of persistent absentees` = enrolments_pa_10_exact_percent,
         `Percentage of severe absentees` = enrolments_pa_50_exact_percent,
         `Overall absence rate` = sess_overall_percent,
         `Authorised absence rate` = sess_authorised_percent,
         `Unauthorised absence rate` = sess_unauthorised_percent,
         `Overall persistent absence rate` = sess_overall_percent_pa_10_exact,
         `Authorised persistent absence rate` = sess_authorised_percent_pa_10_exact,
         `Unauthorised persistent absence rate` = sess_unauthorised_percent_pa_10_exact) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))