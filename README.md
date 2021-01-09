# Appsilon_mateo
This is sample application for appsilon. Please note that this project is live (for now) at https://mateodii.shinyapps.io/appsilon_mateo/

A few notes about this app:

- At the begining the user select the vessel type and name (the name dropdown depends on the type)
- The map shows the 2 longest distance between any 2 observation for the selected ship.
- The complete dataset is available at: https://drive.google.com/file/d/1IeaDpJNqfgUZzGdQmR6cz2H3EQ3_QfCV/view?usp=sharing 
- to run this on your local machine just clone this project and run the app.R
- if you want to run the whole project (including the propro part) on your local machine please download the dataset and save as data/ships.csv
- Distance between two points is calculated as geosphere::distGeo
- The in depth tab only loads after a click because the bigquery is not made for BI (at least the standard usage) so the query is to slow so much slower than the rest of the app.

Some coments about the dataset

- There are a few strange ships, for instance the if column matches multiple shipnames or types.
- For this app we consider an unique vessel the combination of shipname + shipid.

To do

- Organize app.R with individual files for each module
- test for in depth module