PilotPower
==========

Pilot Power is a program to allow research to simulate the implications of using pilot studies to inform research design decisions in experiments.

**Ensuring dependencies are installed**

Before PilotPower will run on your machine, you will need to make sure you have R installed, and that you have installed the ```shiny``` and ```pwr``` packages:

```
install.packages('shiny')
install.packages('pwr')
```

**Run PilotPower out of R**

The quickest way to run the app natively on your computer is is to open R, load Shiny, and run from the GitHub repository:

```
library(shiny)
shiny::runGitHub('PilotPower', 'Table1')
```

**Run PilotPower from the console or terminal**

To run Pilot Power locally, outside of R:

1. Download the program zip file (https://github.com/Table1/PilotPower/archive/master.zip) and unzip
2. Open your terminal/console and type:

  ```
  R -e "shiny::runGitHub('PilotPower', 'Table1')"
  ```

**Modify PilotPower**

If you'd like to make modifications to the program and run it, download the master code:  https://github.com/Table1/PilotPower/archive/master.zip. 

After making your edits, open a terminal/console window and type:

```
R -e "shiny::runApp('~/Downloads/PilotPower-master/')"
```

You may need to change the above code to the ensure the directory is the same as that into which you unzipped the file. (The above code should work on Mac OS X after unzipping the file into your Downloads folder.)