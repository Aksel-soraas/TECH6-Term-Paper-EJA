scripts <- c(
    "00_Setup.R", "01_Clean_data.R", "02_helpers.R", 
    "10_table1.R", "11_table2.R", "12_table3.R",
    "13_table7.R", "20_figure1.R", "21_figure2.R",
    "22_figure3.R", "23_figure4.R"
)

for (s in scripts) source(here::here("R", s))