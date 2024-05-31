This project to be included in a sibling folder of the Queries project, both within the same parent folder.
data_queries.R in the other project should be run after this project's folder exists in the parent directory, and before this app.R is published, as that script is what populates data for this application.



DASHBOARD IMPROVEMENTS:
For emp, just use Moody's historical data altogether instead of consulting public data source (BLS).
Moody's also uses FHFA for population history, so data could just be brought in from that altogether as well.
Create yearly/quarterly toggle for plots to give users timeline option (would require substantial reworking from queries side).

Contrast recession dates (bars) with FRED. Determine what the appropriate adjustments are.

Consider including values in RAWG tables if there is one - if so, reference taxable sales for formatting. This would require a reworking of the forecast summary table formatting (at the top of the app).

Compiling of all tables of each detailed indicator forecast into one Excel file (maybe cbinding with a column between if there's some identifier for each data frame?) with one download button for a multiple-sheet Excel file... Or maybe leave the plot one for independent downloading, and two Excel files: one for all histories, one for all forecasts?
Look into nature of what is already supplied (in downloads), and determine how best to provide tab/indicator-specific forecast download, as well as a comprehensive data download (history, forecasts, and all).

Update App Summary after all app updates are done.

Save acquired data remotely instead of locally (these storage options are compatible with shinyapps.io: MySQL, Dropbox, AWS, or Google Sheets). -https://shiny.rstudio.com/articles/persistent-data-storage.html
Put query script and app into same folder; include automatic app deployment at end of query script, so all (including app republication) will take place by running one file.

Combine data integration and application into one project.
