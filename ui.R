library(shiny)
library(shinyIncubator)

customCSS <- HTML('<link rel="stylesheet" type="text/css" href="assets/custom.css" media="all" />')

shinyUI(pageWithSidebar(

  headerPanel("Pilot Power", "Pilot Power: A Pilot Study Power Simulator"),

  sidebarPanel(
    tags$head(customCSS),
  
    progressInit(),
    
   	helpText(HTML("<h4>Assumed Parameters</h4>")),
  	helpText(HTML("<em>in standard deviation units</em>")),
   	
  	numericInput("true_effect_size", "Intervention True Effect Size:", 
  	            value=0.30, step=0.05),
  	              	
  	numericInput("practical_effect_size", "Practically Significant Effect Size:", 
  	             value=0.15, step=0.05),
  
  	HTML('<hr>'),
   	helpText(HTML("<h4>Design Choices</h4>")),
  	
  	numericInput("sample_size", "Pilot Study Sample Size:", 
  	            min=10, value=50, step=10),
  
  		
  	numericInput("alpha", HTML(paste('Significance Level (<span style="font-size: 1em;">&alpha;</span>):', sep='')), 
  	            min=0.01, max=0.99, value=0.05, step=0.01),
  	
  	numericInput("power", HTML(paste('Power (<span style="font-size: 1em;">1 - &beta;</span>):', sep='')), 
  	            min=0.01, max=0.99, value=0.80, step=0.01),
  	            
  	selectInput("proceed_method", "Basis To Conduct Full Trial:",
  		list("Point Estimate Comparison: pilot study effect size greater than practically significant effect size" = "pointestimate_practical",
  		"Point Estimate Comparison: pilot study effect size greater than zero" = "pointestimate_zero",
  		"T-Test: pilot study effect size not equal to zero (Two-tailed Test)" = "ttest_twotail",
  		"T-Test: pilot study effect size greater than zero (One-tailed Test)" = "ttest_onetail",
  		"Always" = "always")),
  	            
  	
  	
  	HTML('<hr>'),
   	helpText(HTML("<h4>Simulation Settings</h4>")),
  	
  	numericInput("simulations", "Number of Simulations:", 
  	            min=100, max=100000, value=500, step=100),
    HTML('<hr>'),
    actionButton('update', 'Update Simulation'),
    HTML('<div style="font-size: 0.9em; margin-top: 8px;">Initial simulation runs with default settings. Click above to update simulation with new parameters.<br><br> Please be patient: Calculations take some time and sometimes have dependencies leading to multiple steps being completed at once.</div>'),
    HTML('<hr>'),
    HTML('<div style="width: 190px; position: relative; margin: 0 auto;"><img src="assets/GitHub-Mark-32px.png" style="float: left; width: 16px;"> <span style="position: relative; margin: 0 0 0 8px; bottom: 2px; font-size: 1.15em;"><a href="https://github.com/Table1/PilotPower">PilotPower on GitHub</a></span></div>')
  ),

  mainPanel(
  	helpText(HTML("<strong>Pilot Power</strong> simulates the use of pilot studies in planning randomized controlled trials under different user-specified conditions.")),
  	tabsetPanel(
      id = 'tabID',
  		tabPanel("1. Pilot Study",
  		  htmlOutput("pilotStudyNotice"),
	    	tableOutput("pilotResults"),
				plotOutput("pilotStudyPlot"),
        downloadButton("downloadPilotPlotPNG", "PNG"),
				downloadButton("downloadPilotPlotPDF", "PDF"),
				downloadButton("downloadPilotPlotEPS", "EPS"),
        HTML('<br><br>'),
        value = 1
	    ),
	    tabPanel("2. Proceed Decision",
	    	HTML("Based upon our simulated data, the below table lists how often across simulations we would proceed to a full trial when applying certain decision rules to our pilot study results.<br><br>"),
	    	tableOutput("proceedResults"),
		  	htmlOutput("proceedMethodNotice"),
        value = 2
	    ),
	    tabPanel("3. Power Calculation",	
	    	HTML("We now do a power analysis to determine the sample size required to detect effects of a certain size in a full trial.  This is done in two ways: once using the pre-determined practically significant effect size, and once using the effect size seen in the simulated pilot study.  The table below shows, across all simulations, the average sample size needed to detect effects in a full trial.<br><br>"),
	    	htmlOutput("proceedPowerCalculateNotice"),	        
	    	tableOutput("powerResults"),
	    	htmlOutput("powerCalculationNotice"),
	    	htmlOutput("apparentCasesSaved"),
	    	htmlOutput("proceedAlwaysNote"),
	    	plotOutput("sampleSizePlot"),
	    	downloadButton("downloadPowerCalcPilotPlotPNG", "PNG"),
	    	downloadButton("downloadPowerCalcPilotPlotPDF", "PDF"),
	    	downloadButton("downloadPowerCalcPilotPlotEPS", "EPS"),
        HTML('<br><br>'),
        value = 3
	    ),
	    tabPanel("4. Full Trial",
  			htmlOutput("fullTrialNotice"),
  			htmlOutput("proceedCountNotice"),
  			tabsetPanel(
  				tabPanel("Using Pilot Effect Size to Calculate Sample Size",
  						tableOutput("fullTrialStats"),
  						plotOutput("fullTrialDensityPlot"),
  						downloadButton("downloadFullTrialPilotESPoweredPNG", "PNG"),
  						downloadButton("downloadFullTrialPilotESPoweredPDF", "PDF"),
  						downloadButton("downloadFullTrialPilotESPoweredEPS", "EPS"),
              HTML('<br><br>')
  				),
  				tabPanel("Using Practically Significant Effect Size to Calculate Sample Size",
  						tableOutput("noPilotFullTrialStats"),
  						plotOutput("noPilotFullTrialDensityPlot"),
  						downloadButton("downloadFullTrialPracticalESPoweredPNG", "PNG"),
  						downloadButton("downloadFullTrialPracticalESPoweredPDF", "PDF"),
  						downloadButton("downloadFullTrialPracticalESPoweredEPS", "EPS"),
  						HTML('<br><br>')            
          )
  			),
        value = 4
	    ),
	    tabPanel("5. Conclusions",
	    	HTML("We now make conclusions about the effectiveness of the intervention based upon our full trial.<br><br>"),
  			tabsetPanel(
  				tabPanel("Using Pilot Effect Size to Calculate Sample Size",
  					htmlOutput("proceedCountFullTrialNotice"),
  					tableOutput("conclusionResults")
  				),
  				tabPanel("Using Practically Significant Effect Size to Calculate Sample Size",
  					htmlOutput("noPilotProceedCountFullTrialNotice"),
  					tableOutput("noPilotConclusionResults")
  				)
  			),
        value = 5
	    ),
	    tabPanel("Implications",
	    	tabsetPanel(
	    		tabPanel("Decision to Conduct a Full Trial",
	    			HTML("<strong>Best Practice</strong>"),
		    		htmlOutput("bestPractice"),
		    		HTML("<strong>Your Choice</strong>"),
					htmlOutput("yourChoice"),
					HTML("<strong>In General</strong>"),
					htmlOutput("implicationsProceedStatsNotice"),
					tableOutput("implicationsProceedStats")
	    		),
	    		tabPanel("Summary of Achieved Power",
	    			htmlOutput("powerSettings"),	    		
  					tabsetPanel(
  						tabPanel("Using Pilot Effect Size to Calculate Sample Size",
  							htmlOutput("tradeoffTable")
  						),
  						tabPanel("Using Practically Significant Effect Size to Calculate Sample Size",
  							htmlOutput("tradeoffTableNoPilot")
  						)
  					)
	    		)
	    	),
        value = 6
	    ),
	    tabPanel("Data",
	    	tabsetPanel(
			    tabPanel("Pilot Simulation Data",
            downloadButton('downloadPilotData', 'CSV File'),
            HTML('<br><br>'),
			    	dataTableOutput("pilotResultsData")
			    ),
			    tabPanel("Trial Simulation Data (Powered by Pilot Effect Size)",
            downloadButton('downloadTrialPilotESData', 'CSV File'),
            HTML('<br><br>'),
			    	HTML("The below data are from simulations where all sizes are made to be large enough to detect the observed pilot study effect size.<br><br>"),
			      dataTableOutput("fullTrialResultsData")
			    ),
			    tabPanel("Trial Simulation Data (Powered by Practical Significance)",
            downloadButton('downloadTrialPracticalESData', 'CSV File'),
            HTML('<br><br>'),                   
			    	HTML("The below data are from simulations where all sample sizes are large enough to detect the practically significant effect size.<br><br>"),
			    	dataTableOutput("noPilotFullTrialResultsData")
			    )
			  ),
        value = 7
	  	)
  	)
  )
))
