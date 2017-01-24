pan-interviews
===

a shiny interface for researchers collecting interview data

Requirements:
- R (3.3.1 or greater) https://cran.r-project.org/
- R libraries: `shiny`, `shinythemes`, `shinyjs`, `DT`

Installation:

1. Download the pan repository and unzip to a location of your choice.
2. Open R and get required packages, e.g. via
```
install.packages(c("shiny", "shinythemes", "shinyjs", "DT"))
```
3. Run the appropriate batch script to start the app:
   * For Windows users, click on 'run.bat'. If necessary, open `run.bat` in a text editor and change the path to your R installation (e.g. `C:\Program Files\R\R-3.3.1\bin\R.exe`).
   * For OSX or Linux, click on 'run.command'. If you do not have permissions for this, run `chmod +x run.command` in the Terminal to make the script executable.

If all goes well, a command terminal and your browser window should open with the shiny interface!

4. Navigate to 'options' and set the path for your response data to go. There is also a mirrored second destination path. The defaults can be changed inside `pan.r`.
