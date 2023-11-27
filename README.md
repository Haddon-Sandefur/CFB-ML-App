![logo](logo.png)

# College Football Prediction App with Chatbot:

### App Link:
https://jsandy.shinyapps.io/cfbapp/

### Current Version Notes:
* IMPORTANT: The API which this app relies on is currently having issues. The script, `01 get games bets and ranks.R`, will fail until this issue is resolved. 

* Added chatbot to application. Currently, this chatbot does not have the ability to utilize chat history nor interpret results via the ML model. Future updates seek to address this.

### Purpose:
This project acts as a start-to-finish pipeline of securing college football data from multiple sources, training an XGBoost model for point differential predictions, and creating an app which users can intuitively experiment with the model.

#### Data Ingestion Sources:
* CBS Sports' complete team rankings webpage.
* `cfbfastR` API

#### Usage:
Execute `00 runner.R` to pull/scrape data and train the models. Data will be saved to both the `downstream` folder and the `cfbapp` folder. Once this is complete, you can build the Shiny app within `app.R`

#### Coming soon:
* Altering locations of where data is saved.
* Chatbot ability to read data and ML results.
* Dockerizing the application.
* Flowchart for ths README.md