# Pap_Query
R code for designing a shiny app for text-based queries of PAP data.

The R scripts in this repository are required for designing a shiny app to query Policy Agendas Projects (PAP) data. server.R affects how the app operates and the output it creates. ui.R defines the user interface - what the users see, input and examine upon querying. Queries are based on keyword searches in PAP data to find the PAP codes that matching observations were coded into. Queries support regular expressions and exclusion of keywords.

The code is "dumb" and inefficient. It includes several repititions of the same pieces of code, tailored to match the structure of each dataset. It also lacks comments explaining code chunks. A more efficient code would import the relevant dataset, manipulate the data into a unified structure and then run one piece of code. Doing so returned several errors and requires further work to be released in future versions.

The full app can be found at https://freedmanguy.shinyapps.io/querypap/.
