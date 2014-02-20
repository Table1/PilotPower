library(shiny)

customCSS <- HTML('<link rel="stylesheet" type="text/css" href="assets/custom.css" media="all" />')

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Pilot Power", "Pilot Power: A Pilot Study Power Simulator"),

  sidebarPanel(
 	 tags$head(customCSS),

 	helpText(HTML("<h4>Assumed Parameters</h4>")),
	helpText(HTML("<em>in standard deviation units</em>")),
 	
	numericInput("true_effect_size", "Intervention True Effect Size:", 
	            value=0.25, step=0.05),
	            
	
	numericInput("clinical_effect_size", "Clinically Significant Effect Size:", 
	             value=0.25, step=0.05),

	HTML('<hr>'),
 	helpText(HTML("<h4>Design Choices</h4>")),
	
	numericInput("sample_size", "Pilot Study Sample Size:", 
	            min=10, value=50, step=10),

		
	numericInput("alpha", "Significance Level:", 
	            min=0.01, max=0.99, value=0.05, step=0.01),
	
	numericInput("power", "Power Level:", 
	            min=0.01, max=0.99, value=0.80, step=0.01),
	            
	selectInput("proceed_method", "Basis To Conduct Full Trial:",
		list("Point Estimate Comparison: pilot study effect size greater than clinically significant effect size" = "pointestimate_clinical",
		"Point Estimate Comparison: pilot study effect size greater than zero" = "pointestimate_zero",
		"T-Test: pilot study effect size not equal to zero (Two-tail Test)" = "ttest_twotail",
		"T-Test: pilot study effect size greater than zero (One-tail Test)" = "ttest_onetail",
		"T-Test vs. Clinical Significance: pilot study effect size not equal to clinically significant effect size (Two-tail Test)" = "ttest_clinical_twotail",
		"T-Test vs. Clinical Significance: pilot study effect size greater than clinically significant effect size (One-tail Test)" = "ttest_clinical_onetail",
		"Always" = "always")),
	            
	
	
	HTML('<hr>'),
 	helpText(HTML("<h4>Simulation Settings</h4>")),
	
	numericInput("simulations", "Number of Simulations:", 
	            min=100, max=100000, value=500, step=100)  	
  ),

  mainPanel(
  	helpText(HTML("<strong>Pilot Power</strong> simulates the use of pilot studies in planning randomized controlled trials under different user-specified conditions.")),

  	tabsetPanel(
  		tabPanel("1. Pilot Study",
  				htmlOutput("pilotStudyNotice"),
	    		tableOutput("pilotResults"),
				plotOutput("pilotStudyPlot")
	    ),
	    tabPanel("2. Proceed Decision",
	    	HTML("Based upon our simulated data, the below table lists how often across simulations we would proceed to a full trial when applying certain decision rules to our pilot study results.<br><br>"),
	    	tableOutput("proceedResults"),
			htmlOutput("proceedMethodNotice")	    	
	    ),
	    tabPanel("3. Power Calculation",	
	    	HTML("We now do a power analysis to determine the sample size required to detect effects of a certain size in a full trial.  This is done in two ways: once using the pre-determined clinically significant effect size, and once using the effect size seen in the simulated pilot study.  The table below shows, across all simulations, the average sample size needed to detect effects in a full trial.<br><br>"),
	    	htmlOutput("proceedPowerCalculateNotice"),	        
	    	tableOutput("powerResults"),
	    	htmlOutput("powerCalculationNotice"),
	    	htmlOutput("apparentCasesSaved"),
	    	htmlOutput("proceedAlwaysNote"),
	    	plotOutput("sampleSizePlot")
	    ),
	    tabPanel("4. Full Trial",
			htmlOutput("fullTrialNotice"),
			htmlOutput("proceedCountNotice"),
			tabsetPanel(
				tabPanel("Using Pilot Effect Size to Calculate Sample Size",
						tableOutput("fullTrialStats"),
						plotOutput("fullTrialDensityPlot")
				),
				tabPanel("Using Clinically Significant Effect Size to Calculate Sample Size",
						tableOutput("noPilotFullTrialStats"),
						plotOutput("noPilotFullTrialDensityPlot")
				)
			)						
	    ),
	    tabPanel("5. Conclusions",
	    	HTML("We now make conclusions about the effectiveness of the intervention based upon our full trial.<br><br>"),
			tabsetPanel(
				tabPanel("Using Pilot Effect Size to Calculate Sample Size",
					htmlOutput("proceedCountFullTrialNotice"),
					tableOutput("conclusionResults")
				),
				tabPanel("Using Clinically Significant Effect Size to Calculate Sample Size",
					htmlOutput("noPilotProceedCountFullTrialNotice"),
					tableOutput("noPilotConclusionResults")
				)
			)			
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
	    		tabPanel("Summary of Power Loss",
	    			htmlOutput("powerSettings"),	    		
					tabsetPanel(
						tabPanel("Using Pilot Effect Size to Calculate Sample Size",
							htmlOutput("tradeoffTable")
						),
						tabPanel("Using Clinically Significant Effect Size to Calculate Sample Size",
							htmlOutput("tradeoffTableNoPilot")
						)
					)

	    		)
	    	)	    
	    ),
	    tabPanel("Data",
	    	tabsetPanel(
			    tabPanel("Pilot Simulation Data",
			    	dataTableOutput("pilotResultsData")
			    ),
			    tabPanel("Trial Simulation Data",
			    	HTML("The below data are from simulations where all sizes are made to be large enough to detect the observed pilot study effect size.<br><br>"),
			    	dataTableOutput("fullTrialResultsData")
			    ),
			    tabPanel("Trial Simulation Data (No Pilot)",
			    	HTML("The below data are from simulations where all sample sizes are large enough to detect the clinically significant effect size.<br><br>"),
			    	dataTableOutput("noPilotFullTrialResultsData")
			    )
			)
		)
  	)
  

  )
))
