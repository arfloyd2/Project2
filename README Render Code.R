rmarkdown::render(input = '/Users/angelicefloyd/Documents/ST558_Data_Science_R /Project2/Project2.github.io.git/Project2.Rmd',
                  output_file = "README.md", 
                  output_format = "github_document", 
                  output_options = list(
                    name_value_pairs = "value", 
                    toc = TRUE,
                    toc_depth = 3, 
                    number_of_sections = TRUE, 
                    df_print = "paged", 
                    or_something = TRUE
                  )
)