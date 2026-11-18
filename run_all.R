#Run every script in order, all tables and figures are saved to output/
scripts <- c(
    "00_Setup.R", "02_helpers.R", 
    "10_table1.R", "11_table2.R", "12_table3.R", "13_table7.R", 
    "20_figure1.R", "21_figure2.R", "22_figure3.R", "23_figure4.R", 
    "30_extension1.R", "31_extension2.R"
)

for (s in scripts) {
    message("Running ", s)
    source(here::here("R", s))
}


#Run the Python notebook for extension 1
system2("jupyter", c("nbconvert", "--to", "notebook", "--execute", "--inplace", 
         "--ExecutePreprocessor.kernel_name=python3", shQuote(here::here("ML.ipynb"))))

