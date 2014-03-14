PilotPower
==========

Pilot Power is a program to allow research to simulate the implications of using pilot studies to inform research design decisions in experiments.

The quickest way to run the app natively on your computer is:

1. Ensure dependencies are installed:

  ```
  install.packages('shiny')
  install.packages('pwr')
  ```

2. Load Shiny and run from the GitHub zip:

  ```
  library(shiny)
  shiny::runGitHub('PilotPower', 'Table1')
  ```

To run Pilot Power locally, outside of R:

1. Download the program zip file (https://github.com/Table1/PilotPower/archive/master.zip) and unzip
2. Open your terminal/console and type:

  ```
  R -e "shiny::runGitHub('PilotPower', 'Table1')"
  ```

To run the application inside R, load R, and type:

  ```
  library(shiny)
  runGitHub('PilotPower', 'Table1')
  ```

If you'd like to make modifications to the program and run it, download the master code:  https://github.com/Table1/PilotPower/archive/master.zip. 

To run the program outside of R, open a terminal/console window and type:

  ```
  R -e "shiny::runApp('~/Downloads/PilotPower-master/')"
  ```

You may need to change the above code to the ensure the directory is the same as that into which you unzipped the file. (The above code should work on Mac OS X after unzipping the file into your Downloads folder.)