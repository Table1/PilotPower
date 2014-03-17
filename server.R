library(shiny)
library(pwr)
library(shinyIncubator)

tradeoffTableColumnHeaders <- c("Simulations", "Proceed to Trial", "Found Effect")

shinyServer(function(input, output, session) {
  
  ###################################################
  ### SETTINGS	
  ###################################################
  inputValues <- reactive({	
    # Compose data frame
    data.frame(
      Name = c("True Effect Size", 
               "Clinically Significant Effect Size",
               "Pilot Study Sample Size",
               "Significance Level",
               "Power Level",
               "Number of Simulations"),
      Value = as.character(c(input$true_effect_size, 
                             input$clinical_effect_size,
                             input$sample_size,
                             input$alpha,
                             input$power,
                             input$simulations)), 
      stringsAsFactors=FALSE)
  }) 
  
  output$settings <- renderTable({
    inputValues()
  })
  
  # Update status: only update simulation variables for a second time after update button is pressed
  update <- reactive({
    input$update    
  })
  
  last_update <- reactive({
    if(input$update == 0){
      return(0)
    }
    
    return(isolate({
      input$update 
    }))
  })  
  
  output$update <- renderText({ update() })
  output$last_update <- renderText({ last_update() })
  
  ## Simulation Variables
  
  sample_size <- reactive({
    if(update() != last_update()){
      return(NULL)
    }
    
    return(isolate({    
      input$sample_size
    }))
  })
  
  true_effect_size <- reactive({
    if(update() != last_update()){
      return(NULL)
    }
    
    return(isolate({    
      input$true_effect_size
    }))
  })
  
  clinical_level <- reactive({
    if(update() != last_update()){
      return(NULL)
    }
    
    return(isolate({    
      input$clinical_effect_size
    }))
  })
  
  alpha <- reactive({
    if(update() != last_update()){
      return(NULL)
    }
    
    return(isolate({    
      input$alpha
    }))
  })
  
  proceedMethod <- reactive({
    if(update() != last_update()){
      return(NULL)
    }
    
    return(isolate({
      input$proceed_method
    }))
  })
  
  
  power <- reactive({
    if(update() != last_update()){
      return(NULL)
    }
    
    return(isolate({    
      input$power
    }))
  })
  
  simulations <- reactive({
    if(update() != last_update()){
      return(NULL)
    }
    
    return(isolate({    
      input$simulations
    }))
  })
  

  # allow for easy output
  output$simulations <- renderText({ simulations() })
  
  ###################################################
  ### STEP 1:  Pilot Study
  ###################################################

  ## Simulation Function of Initial Pilot Studies
  simulatePilots <- function(pilotN, trueEffect, alpha, power, clinSig, totalSim){
    
    progress <- Progress$new(session)
    progress$set(message = 'Step One: Simulating pilot studies', value = 0)

    
    cxMeans <- matrix(NA, nrow=totalSim, ncol=1)
    cxSDs <- matrix(NA, nrow=totalSim, ncol=1)
    txMeans <- matrix(NA, nrow=totalSim, ncol=1)
    txSDs <- matrix(NA, nrow=totalSim, ncol=1)  			
    effectSizes <- matrix(NA, nrow=totalSim, ncol=1)
    
    #tests
    tTwoSampleTwoTail <- matrix(NA, nrow=totalSim, ncol=1)
    pTwoSampleTwoTail <- matrix(NA, nrow=totalSim, ncol=1)
    tTwoSampleOneTail <- matrix(NA, nrow=totalSim, ncol=1)
    pTwoSampleOneTail <- matrix(NA, nrow=totalSim, ncol=1)
    
    tClinSigTwoTail <- matrix(NA, nrow=totalSim, ncol=1)
    pClinSigTwoTail <- matrix(NA, nrow=totalSim, ncol=1)
    tClinSigOneTail <- matrix(NA, nrow=totalSim, ncol=1)
    pClinSigOneTail <- matrix(NA, nrow=totalSim, ncol=1)
    
    for(i in 1:totalSim){
      progress$set(value = (i/totalSim))
      
      cx <- rnorm(pilotN/2, mean=0, sd=1)
      tx <- rnorm(pilotN/2, mean=0+trueEffect, sd=1)
      
      cxMeans[i] <- mean(cx)
      cxSDs[i] <- sd(cx)
      txMeans[i] <- mean(tx)
      txSDs[i] <- sd(tx)
      
      effectSize <- mean(tx)-mean(cx)
      effectSizes[i] <- effectSize
      
      # t-test, two sample, two-taileded
      tTestTwoSampleTwoTail <- t.test(x=tx, y=cx, alternative="two.sided", var.equal = TRUE, conf.level = alpha)
      tTwoSampleTwoTail[i] <- tTestTwoSampleTwoTail$statistic
      pTwoSampleTwoTail[i] <- tTestTwoSampleTwoTail$p.value
      
      # t-test, two sample, one-taileded
      tTestTwoSampleOneTail <- t.test(x=tx, y=cx, alternative="greater", var.equal = TRUE, conf.level = alpha)
      tTwoSampleOneTail[i] <- tTestTwoSampleOneTail$statistic
      pTwoSampleOneTail[i] <- tTestTwoSampleOneTail$p.value
      
      # t-test, one sample clinical significance comparison, two-tailed
      tTestClinSigTwoTail <- t.test(x=tx, mu=clinSig, alternative="two.sided", var.equal = TRUE, conf.level = alpha)
      tClinSigTwoTail[i] <- tTestClinSigTwoTail$statistic
      pClinSigTwoTail[i] <- tTestClinSigTwoTail$p.value
      
      # t-test, two sample, one-taileded
      tTestClinSigOneTail <- t.test(x=tx, mu=clinSig, alternative="greater", var.equal = TRUE, conf.level = alpha)
      tClinSigOneTail[i] <- tTestClinSigOneTail$statistic
      pClinSigOneTail[i] <- tTestClinSigOneTail$p.value
      
    }
    
    pilot_stats <- data.frame(control_mean = cxMeans, control_sd = cxSDs, treatment_mean = txMeans, treatment_sd = txSDs, effect_size = effectSizes, t_two_sample = tTwoSampleTwoTail, p_two_sample_two_tail = pTwoSampleTwoTail,p_two_sample_one_tail = pTwoSampleOneTail, t_clin_sig = tClinSigTwoTail, p_clin_sig_two_tail = pClinSigTwoTail, p_clin_sig_one_tail = pClinSigOneTail)
    
    
    progress$set(value = 1)
    Sys.sleep(0.1)
    progress$close()      
    
    return(pilot_stats)
  }
  
  
  output$pilotStudyNotice <- renderText({
    paste('For each of the ', simulations(), ' simulations, we draw ', sample_size()/2, ' treatment outcomes each from a normal distrbution with a mean of ', true_effect_size(), 'and a standard deviation of 1. We repeat this process for the control outcomes, but set the normal distribution mean to 0. Effect sizes for each simulation are calculated by subtracting the treatment mean from the control mean.<br><br>', sep='')
  })	
  
  ## Reactive variables
  # Results
  pilotResults <- reactive({
    results <- simulatePilots(sample_size(), true_effect_size(), alpha(), power(), clinical_level(), simulations())
    return(results)
  })
  
  # Pilot Study Statistics
  pilotStudyStats <- reactive({	
    # Compose data frame
    data.frame(
      Name = c("Sample Size", 
               "Group Size",
               "Mean Treatment Group Mean",
               "SD",
               "Mean Control Group Mean",
               "SD",
               "Mean Effect Size",
               "SD"),
      Value = as.character(c(input$sample_size, 
                             input$sample_size/2,
                             round(mean(pilotResults()$treatment_mean), 2),
                             round(sd(pilotResults()$treatment_mean), 2),
                             round(mean(pilotResults()$control_mean), 2),
                             round(sd(pilotResults()$control_mean), 2),
                             round(mean(pilotResults()$effect_size), 2),
                             round(sd(pilotResults()$effect_size), 2)
      )), 
      stringsAsFactors=FALSE)
  }) 	
  
  output$pilotResults <- renderTable({
    pilotStudyStats()
  })	
  
  ## Density plots
  
  # Single plots
  densControl <- reactive({
    density(pilotResults()$control_mean)
  })
  
  densTreatment <- reactive({
    density(pilotResults()$treatment_mean)
  })
  
  # Ranges
  xlim <- reactive({
    range(densControl()$x, densTreatment()$x)
  })     
  
  ylim <- reactive({
    range(0, densControl()$y, densTreatment()$y)
  })
  
  controlColor <- rgb(0,0,0,0.1)
  treatmentColor <- rgb(0, 136/255, 204/255, .2)
  
  
  output$pilotStudyPlot <- renderPlot({
    plot(densControl(), xlim = xlim(), ylim = ylim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(densControl(), density = -1, col = controlColor)
    polygon(densTreatment(), density = -1, col = treatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(controlColor, treatmentColor), bty = 'n',
           border = NA)    
  })
  
  ## Download Plot.  
  # Annoyingly, this requires us to recreate the plot.  If I create the plot with a reactive function and call
  # the download link doesn't work.  Others have had this problem (e.g., see: https://groups.google.com/forum/#!msg/shiny-discuss/u7gwXc8_vyY/eFJtnMDTTUUJ)
  pilotStudyPlotDownloadPNG <- reactive({
    plot(densControl(), xlim = xlim(), ylim = ylim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(densControl(), density = -1, col = controlColor)
    polygon(densTreatment(), density = -1, col = treatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(controlColor, treatmentColor), bty = 'n',
           border = NA)       
  })
  
  pilotStudyPlotDownloadPDF <- reactive({
    plot(densControl(), xlim = xlim(), ylim = ylim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(densControl(), density = -1, col = controlColor)
    polygon(densTreatment(), density = -1, col = treatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(controlColor, treatmentColor), bty = 'n',
           border = NA)       
  })
  
  pilotStudyPlotDownloadEPS <- reactive({
    plot(densControl(), xlim = xlim(), ylim = ylim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(densTreatment(), density = 10, lty = 'dashed', angle=45)
    polygon(densControl(), density = -1, lty = 'solid',)
    
    legend('topleft',c('Control','Treatment'),
           bty = 'n',
           lty=c('solid', 'dashed'),
    )       
  })  
  
  output$downloadPilotPlotPNG <- downloadHandler(
    filename = function() { paste('pilotDensityPlot', Sys.Date(), '.png', sep='') },
    content = function(file) {
      png(file)
      print(pilotStudyPlotDownloadPNG())
      dev.off()
    }
  ) 
  
  output$downloadPilotPlotPDF <- downloadHandler(
    filename = function() { paste('pilotDensityPlot', Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(file)
      print(pilotStudyPlotDownloadPDF())
      dev.off()
    }
  )   
  
  output$downloadPilotPlotEPS <- downloadHandler(
    filename = function() { paste('pilotDensityPlot', Sys.Date(), '.eps', sep='') },
    content = function(file) {
      setEPS()
      postscript(file)
      print(pilotStudyPlotDownloadEPS())
      dev.off()
    }
  )   
  
  ###################################################
  ### STEP 2:  Decide Whether To Conduct Trial
  ###################################################
  
  ## Point Estimate Comparisons
  
  # Compare to zero
  pointEstimateZeroCount <- reactive({
    return(sum(pilotResults()$effect_size > 0))
  })
  
  pointEstimateZeroPercent <- reactive({
    round((pointEstimateZeroCount()/simulations())*100, 2)
  })
  
  # Compare to clin sig	
  pointEstimateClinSigCount <- reactive({
    return(sum(pilotResults()$effect_size > clinical_level()))
  })
  
  pointEstimateClinSigPercent <- reactive({
    round((pointEstimateClinSigCount()/simulations())*100, 2)
  })
  
  ### T-tests
  
  ## Two sample
  # Two-tailed	
  tTwoSampleTwoTailCount <- reactive({
    return(sum(pilotResults()$p_two_sample_two_tail < alpha()))
  })
  
  tTwoSampleTwoTailPercent <- reactive({
    round((tTwoSampleTwoTailCount()/simulations())*100, 2)
  })
  
  # One-tailed
  tTwoSampleOneTailCount <- reactive({
    return(sum(pilotResults()$p_two_sample_one_tail < alpha()))
  })
  
  tTwoSampleOneTailPercent <- reactive({
    round((tTwoSampleOneTailCount()/simulations())*100, 2)
  })	
  
  ## One sample
  
  # Two-tailed
  tClinSigTwoTailCount <- reactive({
    return(sum(pilotResults()$p_clin_sig_two_tail < alpha()))
  })
  
  tClinSigTwoTailPercent <- reactive({
    round((tClinSigTwoTailCount()/simulations())*100, 2)
  })	
  
  # One-tailed
  tClinSigOneTailCount <- reactive({
    return(sum(pilotResults()$p_clin_sig_one_tail < alpha()))    
  })
  
  tClinSigOneTailPercent <- reactive({
    round((tClinSigOneTailCount()/simulations())*100, 2)
  })
  
  
  proceedStats <- reactive({	
    # Compose data frame
    data.frame(
      Proceed = c("If effect size point estimate is greater than 0",
                  "If effect size point estimate is greater than clinically significant effect size",
                  "If treatment mean greater than control mean (one-tailed t-test)",
                  "If treatment mean not equal to control mean (two-tailed t-test)",
                  "If treatment mean greater than clinically significant effect size (one-tailed t-test)",
                  "If treatment mean not equal to clinically significant effect size (two-tailed t-test)",
                  "Always"						
      ),
      Percent = as.character(c(pointEstimateZeroPercent(),
                               pointEstimateClinSigPercent(),			
                               tTwoSampleOneTailPercent(),
                               tTwoSampleTwoTailPercent(),
                               tClinSigOneTailPercent(),
                               tClinSigTwoTailPercent(),
                               100
      )), 
      stringsAsFactors=FALSE)
  }) 		
  
  output$proceedResults <- renderTable({
    proceedStats()
  })		
  
  
  generateProceedMethodDescription <- function(proceedMethod){

    if(proceedMethod == "pointestimate_clinical"){
      description <- c('proceed to a full trial whenever the pilot study effect size point estimate is greater than the clinically significant effect size.')
    } else if(proceedMethod == "pointestimate_zero"){
      description <- c('proceed to a full trial whenever the pilot study effect size point estimate is greater than 0.')
    } else if(proceedMethod == "ttest_twotail"){
      description <- c('proceed to a full trial whenever the pilot study effect size is statistically significantly different from 0.')
    } else if(proceedMethod == "ttest_onetail"){
      description <- c('proceed to a full trial whenever the pilot study effect size is statistically significantly greater than 0.')
    } else if(proceedMethod == "ttest_clinical_twotail"){
      description <- c('proceed to a full trial whenever the pilot study effect size is statistically significantly different from  the clinically significant effect size.')
    } else if(proceedMethod == "ttest_clinical_onetail"){
      description <- c('proceed to a full trial whenever the pilot study effect size is statistically significantly greater than  the clinically significant effect size.')
    } else if(proceedMethod == 'always'){
      description <- 'proceed to a full trial regardless of the pilot study effect size.'
    }
      
    return(paste('You chose to ', description))
  }
  
  proceedMethodDescription <- reactive({


    results <- generateProceedMethodDescription(proceedMethod())
  
    
    return(results)
  })
  
  ###################################################
  ### STEP 3:  Power Calculation
  ###################################################
  
  output$proceedPowerCalculateNotice <- renderText({
    paste('<strong>Note:</strong> ', proceedMethodDescription(), ' Consequently, we would proceed to a full trial in <strong>', proceedCount() , '</strong> of the <strong>', simulations(), '</strong> simulations conducted, or <strong>', round((proceedCount()/simulations())*100, 2) ,'%</strong> of them. Power calculations are done using those ', proceedCount(), ' cases.<br><br>', sep='')
  })
  
  output$proceedMethodNotice <- renderText({
    paste('<br><strong>', proceedMethodDescription(), '</strong><br><br>In this simulation, you will proceed to a full trial in <strong>', proceedCount() , '</strong> of the <strong>', simulations(), '</strong> simulations conducted, or <strong>', round((proceedCount()/simulations())*100, 2) ,'%</strong> of them.<br><br><em>This setting is applied to all presented results except where otherwise noted. You can change this setting on the left panel.</em>', sep='')
  })
  
  output$powerCalculationNotice <- renderText({
    paste('<br>Power calculations are based upon two-taileded tests where power (1 - &beta;) = ', power(), '.<br><br>', sep='')
  })	
  
  # Calculate power needed to detect clinical significance
  calculatePower <- function(effect_size, alpha, power){
    group_size <- power.t.test(n=NULL, effect_size, sd=1, sig.level=alpha, power=power,
                               type="two.sample", alternative="two.sided")$n	
    sample_size <- ceiling(group_size * 2)		
    
    ifelse((sample_size %% 2) == 0, NA, sample_size <- sample_size + 1)
    
    return(sample_size)
    
  }
  
  clinSigTrialSampleSize <- reactive({
    calculatePower(clinical_level(), alpha(), power())
  })
  
  pilotStudyPowerCalculation <- function(data, alpha, power, proceedMethod, clinSig, totalSim, limit){
    progress <- Progress$new(session)
    progress$set(message = 'Step Three: Conducting power calculations', value = 0)  
    
    sample_size <- matrix(NA, nrow=totalSim, ncol=1)
    
    for(i in 1:totalSim){
      progress$set(value = (i/totalSim))
      
      pilotRow <- data[i, ]
      
      proceed <- 0		
      
      if(proceedMethod == "pointestimate_clinical"){
        pilotEffectSize = pilotRow$effect_size			
        ifelse(pilotEffectSize > clinSig, proceed <- 1, NA)
      } else if(proceedMethod == "pointestimate_zero"){
        pilotEffectSize = pilotRow$effect_size						
        ifelse(pilotEffectSize > 0, proceed <- 1, NA)
      } else if(proceedMethod == "ttest_twotail"){
        p_two_sample_two_tail = pilotRow$p_two_sample_two_tail
        ifelse(p_two_sample_two_tail < alpha, proceed <- 1, NA)
      } else if(proceedMethod == "ttest_onetail"){
        p_two_sampleone_tail = pilotRow$p_two_sample_one_tail
        ifelse(p_two_sampleone_tail < alpha, proceed <- 1, NA)
      } else if(proceedMethod == "ttest_clinical_twotail"){
        p_clin_sig_two_tail = pilotRow$p_clin_sig_two_tail
        ifelse(p_clin_sig_two_tail < alpha, proceed <- 1, NA)
      } else if(proceedMethod == "ttest_clinical_onetail"){
        p_clin_sig_one_tail = pilotRow$p_clin_sig_one_tail
        ifelse(p_clin_sig_one_tail < alpha, proceed <- 1, NA)
      } else if(proceedMethod == "always"){
        proceed <- 1
      }
      
      if(proceed == 1){
        effect_size <- pilotRow$effect_size
        
        # no interest in detecting less than clinical effect size, so if less, set row effect size to clinical effect size
        if(proceedMethod == 'always'){
          ifelse(effect_size < clinSig, effect_size <- clinSig, NA)
        } else if (proceedMethod == 'pointestimate_zero'){
          ifelse(effect_size < clinSig, effect_size <- clinSig, NA)
        }
        
        group_size <- power.t.test(n=NULL, effect_size, sd=1, sig.level=alpha, power=power,
                                   type="two.sample", alternative="two.sided")$n
        
        this_sample_size <- ceiling(group_size * 2)
        ifelse((this_sample_size %% 2) == 0, NA, this_sample_size <- this_sample_size + 1)
        
        ifelse(this_sample_size > limit, this_sample_size <- limit, NA)
        
        sample_size[i] <- this_sample_size
        
        
      } else {
        sample_size[i] <- NA
      }				
      
    }

    progress$set(value = 1)
    Sys.sleep(0.1)
    progress$close()    
    
    return (sample_size)
  }
  
  sampleSizes <- reactive({
    pilotStudyPowerCalculation(pilotResults(), alpha(), power(), proceedMethod(), clinical_level(), simulations(), clinSigTrialSampleSize())
  })
  
  meanSampleSize <- reactive({
    round(mean(sampleSizes(), na.rm = TRUE), 2)
  })
  
  sdSampleSize <- reactive({
    round(sd(sampleSizes(), na.rm = TRUE), 2)
  })
  
  clinicalSamplePilotSampleDifference <- reactive({
    round(clinSigTrialSampleSize() - meanSampleSize(), 2)
  })
  
  output$apparentCasesSaved <- renderText({
    paste('When calculating full trial sample sizes to detect the observed pilot study effect size, on mean you sample <strong>', clinicalSamplePilotSampleDifference() ,'</strong> fewer cases per simulation than had you calculated the full trial sample size to detect the clinically significant effect size.<br><br>', sep='')
  })
  
  
  powerStats <- reactive({	
    # Compose data frame
    data.frame(
      Calculation = c("Sample size needed to detect clinically significant effect size",
                      "Across simulations, mean sample size needed to detect pilot study effect size",
                      "Across simulations, minimum sample needed to detect pilot study effect size",
                      "Across simulations, maximum sample needed to detect pilot study effect size",						
                      "Across simulations, standard deviation of sample size needed to detect pilot study effect size"
      ),
      Sample = as.character(c(clinSigTrialSampleSize(),
                              meanSampleSize(),
                              min(sampleSizes(), na.rm=TRUE),
                              max(sampleSizes(), na.rm=TRUE),
                              sdSampleSize()
      )), 
      stringsAsFactors=FALSE)
  }) 		
  
  output$powerResults <- renderTable({
    powerStats()
  })
  
  # how often pilot study smaller than clinically significant
  numberCasesPilotESLessThanCSES <- reactive({
    round((sum(pilotResults()$effect_size < clinical_level())/proceedCount())*100, 2)
  })
  
  proceedAlwaysNote <- function(currentProceedMethod){
    proceedNote <- ''
    if(currentProceedMethod == 'always'){
      proceedNote <- paste('<strong>Alert:</strong> You chose to always proceed to trial regardless of the pilot study effect size. In these cases, the pilot study effect size is sometimes smaller than the clinically significant effect size. When this is the case, power calculations are done with the clinically significant effect size. For example, if a draw in the simulation results in a pilot study effect size of 0.08, and the clinically significant effect size is 0.20, the power calculation is done to allow detection of the clinically significant effect size of 0.20.<br><br>In this simulation, this occurs in <strong>', numberCasesPilotESLessThanCSES(), '%</strong> of the pilot studies simulated.', sep='')
    } else if (currentProceedMethod == 'pointestimate_zero') {
      proceedNote <- paste('<strong>Alert:</strong> You chose to always proceed to trial whenever the point estimate is greater than zero. In these cases, the pilot study effect size is often smaller than the clinically significant effect size. When this is the case, power calculations are done with the clinically significant effect size. For example, if a draw in the simulation results in a pilot study effect size of 0.08, and the clinically significant effect size is 0.20, the power calculation is done to allow detection of the clinically significant effect size of 0.20.<br><br>In this simulation, this occurs in <strong>', numberCasesPilotESLessThanCSES(), '%</strong> of the cases in which the pilot study effect size is large enough to proceed to a full trial.', sep='')
    }
    return(proceedNote)
  }
  
  currentProceedAlwaysNote <- reactive({
    proceedAlwaysNote(proceedMethod())
  })
  
  output$proceedAlwaysNote <- renderText({
    currentProceedAlwaysNote()
  })
  
  # create density plot of mean sample sizes
  densSampleSizes <- reactive({
    density(sampleSizes(), bw=20, na.rm = TRUE)
  })
  
  lightblue <- rgb(0, 136/255, 204/255, .2)
  darkblue <- rgb(0, 136/255, 204/255, .8)
  
  
  # Combined plot
  output$sampleSizePlot <- renderPlot({
    layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
    
    plot(densSampleSizes(), xlab = 'Sample Size',
         main = 'Smoothed Distribution of Sample Sizes Calculated Using Observed Pilot Study Effect Size', 
         panel.first = grid())
    
    abline(v=clinSigTrialSampleSize()) # clinical significance
    abline(v=min(sampleSizes(), na.rm=TRUE), lty = 3,col = "darkred") # minimum
    abline(v=max(sampleSizes(), na.rm=TRUE), lty = 3, col = "darkred") # maximum
    abline(v=meanSampleSize(),lty=2)
    
    polygon(densSampleSizes(), col=lightblue, density = -1)
    
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend('center',c('Mean Sample Size','Clinically Significant Sample Size','Minimum/Maximum Calculated Sample Sizes'),
           lty = c(2, 1, 3), col=c(1,1,'darkred'), bty = 'n')
  })
  
  output$sampleSizeData <- renderTable({
    sampleSizes()
  })
  
  ## Create download buttons
  powerCalcPilotStudyPlotDownloadPNG <- reactive({
    layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
    
    plot(densSampleSizes(), xlab = 'Sample Size',
         main = 'Smoothed Distribution of Sample Sizes Calculated Using Observed Pilot Study Effect Size', 
         panel.first = grid())
    
    abline(v=clinSigTrialSampleSize()) # clinical significance
    abline(v=min(sampleSizes(), na.rm=TRUE), lty = 3,col = "darkred") # minimum
    abline(v=max(sampleSizes(), na.rm=TRUE), lty = 3, col = "darkred") # maximum
    abline(v=meanSampleSize(),lty=2)
    
    polygon(densSampleSizes(), col=lightblue, density = -1)
    
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend('center',c('Mean Sample Size','Clinically Significant Sample Size','Minimum/Maximum Calculated Sample Sizes'),
           lty = c(2, 1, 3), col=c(1,1,'darkred'), bty = 'n')
  })
  
  powerCalcPilotStudyPlotDownloadPDF <- reactive({
    layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
    
    plot(densSampleSizes(), xlab = 'Sample Size',
         main = 'Smoothed Distribution of Sample Sizes Calculated Using Observed Pilot Study Effect Size', 
         panel.first = grid())
    
    abline(v=clinSigTrialSampleSize()) # clinical significance
    abline(v=min(sampleSizes(), na.rm=TRUE), lty = 3,col = "darkred") # minimum
    abline(v=max(sampleSizes(), na.rm=TRUE), lty = 3, col = "darkred") # maximum
    abline(v=meanSampleSize(),lty=2)
    
    polygon(densSampleSizes(), col=lightblue, density = -1)
    
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend('center',c('Mean Sample Size','Clinically Significant Sample Size','Minimum/Maximum Calculated Sample Sizes'),
           lty = c(2, 1, 3), col=c(1,1,'darkred'), bty = 'n')    
  })
  
  powerCalcPilotStudyPlotDownloadEPS <- reactive({
    layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
    
    plot(densSampleSizes(), xlab = 'Sample Size',
         main = 'Smoothed Distribution of Sample Sizes Calculated Using Observed Pilot Study Effect Size', 
         panel.first = grid())
    
    abline(v=clinSigTrialSampleSize()) # clinical significance
    abline(v=min(sampleSizes(), na.rm=TRUE), lty = 3) # minimum
    abline(v=max(sampleSizes(), na.rm=TRUE), lty = 3) # maximum
    abline(v=meanSampleSize(),lty=2)
    
    polygon(densSampleSizes(), col=0, density = -1)
    
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend('center',c('Mean Sample Size','Clinically Significant Sample Size','Minimum/Maximum Calculated Sample Sizes'),
           lty = c(2, 1, 3), col=c(1,1,1), bty = 'n')  
  })  
  
  output$downloadPowerCalcPilotPlotPNG <- downloadHandler(
    filename = function() { paste('pilotDensityPlot', Sys.Date(), '.png', sep='') },
    content = function(file) {
      png(file, width=960)
      print(powerCalcPilotStudyPlotDownloadPNG())
      dev.off()
    }
  ) 
  
  output$downloadPowerCalcPilotPlotPDF <- downloadHandler(
    filename = function() { paste('pilotDensityPlot', Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(file, width=10)
      print(powerCalcPilotStudyPlotDownloadPDF())
      dev.off()
    }
  )   
  
  output$downloadPowerCalcPilotPlotEPS <- downloadHandler(
    filename = function() { paste('pilotDensityPlot', Sys.Date(), '.eps', sep='') },
    content = function(file) {
      setEPS()
      postscript(file, width=10)
      print(powerCalcPilotStudyPlotDownloadEPS())
      dev.off()
    }
  )   
  
  ###################################################
  ### STEP 4:  FULL TRIAL
  ###################################################
  
  output$fullTrialNotice <- renderText({
    paste('For each of the ', simulations(), ' simulations, we draw treatment outcomes each from a normal distrbution with a mean of ', true_effect_size(), ' and a standard deviation of 1. We repeat this process for the control outcomes, but set the normal distribution mean to 0. The sample size for each simulation is determined by the calculations in step 3. Effect sizes for each simulation are calculated by subtracting the treatment mean from the control mean.<br><br>', sep='')
  })	
  
  # Function to simulate full trial  
  simulateFullTrial <- function(pilot_data, sample_data, trueEffect, alpha, proceedMethod, clinSig, totalSim){
    
    progressMessage <- 'Step Four: Simulating full trials'
    
    if(length(sample_data) == 1){
      progressMessage <- paste(progressMessage, 'using practically significant effect size')
    } else {
      progressMessage <- paste(progressMessage, 'using pilot study effect sizes')
    }
    
    progress <- Progress$new(session)
    progress$set(message = progressMessage, value = 0)  
    
    pilotTxMeans <- matrix(NA, nrow=totalSim, ncol=1)
    pilotCxMeans <- matrix(NA, nrow=totalSim, ncol=1)
    pilotEffectSizes <- matrix(NA, nrow=totalSim, ncol=1)
    sampleSizes <- matrix(NA, nrow=totalSim, ncol=1)
    groupSizes <- matrix(NA, nrow=totalSim, ncol=1)		
    proceedStatus <- matrix(NA, nrow=totalSim, ncol=1)		
    cxMeans <- matrix(NA, nrow=totalSim, ncol=1)
    cxSDs <- matrix(NA, nrow=totalSim, ncol=1)
    txMeans <- matrix(NA, nrow=totalSim, ncol=1)
    txSDs <- matrix(NA, nrow=totalSim, ncol=1)				
    effectSizes <- matrix(NA, nrow=totalSim, ncol=1)
    
    #tests
    tTwoSampleTwoTail <- matrix(NA, nrow=totalSim, ncol=1)
    pTwoSampleTwoTail <- matrix(NA, nrow=totalSim, ncol=1)
    tTwoSampleOneTail <- matrix(NA, nrow=totalSim, ncol=1)
    pTwoSampleOneTail <- matrix(NA, nrow=totalSim, ncol=1)
    
    tClinSigTwoTail <- matrix(NA, nrow=totalSim, ncol=1)
    pClinSigTwoTail <- matrix(NA, nrow=totalSim, ncol=1)
    tClinSigOneTail <- matrix(NA, nrow=totalSim, ncol=1)
    pClinSigOneTail <- matrix(NA, nrow=totalSim, ncol=1)
    
    for(i in 1:totalSim){
      progress$set(value = (i/totalSim))
      
      #determine whether to proceed
      pilotRow = pilot_data[i, ]
      
      pilotTxMeans[i] <- pilotRow$treatment_mean
      pilotCxMeans[i] <- pilotRow$control_mean			
      pilotEffectSizes[i] <- pilotRow$effect_size
      
      proceed <- 0		
      
      if(proceedMethod == "pointestimate_clinical"){
        pilotEffectSize = pilotRow$effect_size			
        ifelse(pilotEffectSize > clinSig, proceed <- 1, NA)
      } else if(proceedMethod == "pointestimate_zero"){
        pilotEffectSize = pilotRow$effect_size						
        ifelse(pilotEffectSize > 0, proceed <- 1, NA)
      } else if(proceedMethod == "ttest_twotail"){
        p_two_sample_two_tail = pilotRow$p_two_sample_two_tail
        ifelse(p_two_sample_two_tail < alpha, proceed <- 1, NA)
      } else if(proceedMethod == "ttest_onetail"){
        p_two_sampleone_tail = pilotRow$p_two_sample_one_tail
        ifelse(p_two_sampleone_tail < alpha, proceed <- 1, NA)
      } else if(proceedMethod == "ttest_clinical_twotail"){
        p_clin_sig_two_tail = pilotRow$p_clin_sig_two_tail
        ifelse(p_clin_sig_two_tail < alpha, proceed <- 1, NA)
      } else if(proceedMethod == "ttest_clinical_onetail"){
        p_clin_sig_one_tail = pilotRow$p_clin_sig_one_tail
        ifelse(p_clin_sig_one_tail < alpha, proceed <- 1, NA)
      } else if(proceedMethod == "always"){
        proceed <- 1
      }
      
      if(proceed == 1){
        proceedStatus[i] <- 1			
        
        ifelse(length(sample_data) == 1, sample_size <- sample_data, sample_size <- sample_data[i])
        cx <- rnorm(sample_size/2, mean=0, sd=1)			
        tx <- rnorm(sample_size/2, mean=0+trueEffect, sd=1)
        
        sampleSizes[i] <- sample_size
        groupSizes[i] <- sample_size/2
        cxMeans[i] <- mean(cx)
        cxSDs[i] <- sd(cx)
        txMeans[i] <- mean(tx)
        txSDs[i] <- sd(tx)
        
        effectSize <- mean(tx)-mean(cx)
        effectSizes[i] <- effectSize
        
        # t-test, two sample, two-tailed
        tTestTwoSampleTwoTail <- t.test(x=tx, y=cx, alternative="two.sided", var.equal = TRUE, conf.level = alpha)
        tTwoSampleTwoTail[i] <- tTestTwoSampleTwoTail$statistic
        pTwoSampleTwoTail[i] <- tTestTwoSampleTwoTail$p.value
        
        # t-test, two sample, one-tailed		      
        tTestTwoSampleOneTail <- t.test(x=tx, y=cx, alternative="greater", var.equal = TRUE, conf.level = alpha)
        tTwoSampleOneTail[i] <- tTestTwoSampleOneTail$statistic
        pTwoSampleOneTail[i] <- tTestTwoSampleOneTail$p.value
        
        # t-test, one sample clinical significance comparison, two-tailed
        tTestClinSigTwoTail <- t.test(x=tx, mu=clinSig, alternative="two.sided", var.equal = TRUE, conf.level = alpha)
        tClinSigTwoTail[i] <- tTestClinSigTwoTail$statistic
        pClinSigTwoTail[i] <- tTestClinSigTwoTail$p.value
        
        # t-test, two sample, one-tailed		      
        tTestClinSigOneTail <- t.test(x=tx, mu=clinSig, alternative="greater", var.equal = TRUE, conf.level = alpha)
        tClinSigOneTail[i] <- tTestClinSigOneTail$statistic
        pClinSigOneTail[i] <- tTestClinSigOneTail$p.value
        
        
      } else {
        proceedStatus[i] <- 0
      }			
    }
    
    trial_stats <- data.frame(proceed_status = proceedStatus, pilot_treatment_mean = pilotTxMeans, pilot_control_mean = pilotCxMeans, pilot_effect_size = pilotEffectSizes, sample_size = sampleSizes, group_size = groupSizes, control_mean = cxMeans, control_sd = cxSDs, treatment_mean = txMeans, treatment_sd = txSDs, effect_size = effectSizes, t_two_sample = tTwoSampleTwoTail, p_two_sample_two_tail = pTwoSampleTwoTail,p_two_sample_one_tail = pTwoSampleOneTail, t_clin_sig = tClinSigTwoTail, p_clin_sig_two_tail = pClinSigTwoTail, p_clin_sig_one_tail = pClinSigOneTail)
    
    progress$set(value = 1)
    Sys.sleep(0.1)
    progress$close()         
    
    return(trial_stats)
  }
  
  # get a data frame of simulated data for all trials	 when using pilot study effect sizes
  trialResults <- reactive({
    simulateFullTrial(pilotResults(), sampleSizes(), true_effect_size(), alpha(), proceedMethod(), clinical_level(), simulations())
  })
  
  proceedCount <- reactive({
    sum(trialResults()$proceed_status == 1)
  })
  
  output$proceedCountNotice <- renderText({
    paste('<strong>Note:</strong> ', proceedMethodDescription(), ' Consequently, we would proceed to trial in <strong>', proceedCount() , '</strong> of the <strong>', simulations(), '</strong> simulations conducted, or <strong>', round((proceedCount()/simulations())*100, 2) ,'%</strong> of them. Full trial data is simulated using those ', proceedCount(), ' cases.<br><br>', sep='')
  })
  
  # Full Trial Statistics
  fullTrialStats <- reactive({	
    # Compose data frame
    data.frame(
      Name = c("Mean Sample Size", 
               "Mean Group Size",
               "Mean Treatment Group Mean Across Simulations",
               "Mean Treatment Group SD Across Simulations",
               "Mean Control Group Mean Across Simulations",
               "Mean Control Group SD Across Simulations",
               "Mean Effect Size Across Simulations",
               "Mean Effect Size SD Across Simulations"),
      Value = as.character(c(round(mean(trialResults()$sample_size, na.rm = TRUE), 2),
                             round(mean(trialResults()$group_size, na.rm = TRUE), 2),
                             round(mean(trialResults()$treatment_mean, na.rm = TRUE), 2),
                             round(sd(trialResults()$treatment_mean, na.rm = TRUE), 2),
                             round(mean(trialResults()$control_mean, na.rm = TRUE), 2),
                             round(sd(trialResults()$control_mean, na.rm = TRUE), 2),
                             round(mean(trialResults()$effect_size, na.rm = TRUE), 2),
                             round(sd(trialResults()$effect_size, na.rm = TRUE), 2)
      )), 
      stringsAsFactors=FALSE)
  }) 		
  
  output$fullTrialStats <- renderTable({
    fullTrialStats()
  })
  
  ## Density plots
  
  # Single plots
  trialDensControl <- reactive({
    density(trialResults()$control_mean, na.rm = TRUE)
  })
  
  trialDensTreatment <- reactive({
    density(trialResults()$treatment_mean, na.rm = TRUE)
  })
  
  # Ranges
  trialXlim <- reactive({
    range(trialDensControl()$x, trialDensTreatment()$x, na.rm = TRUE)
  })     
  
  trialYlim <- reactive({
    range(0, trialDensControl()$y, trialDensTreatment()$y, na.rm = TRUE)
  })
  
  trialControlColor <- rgb(0,0,0,0.1)
  trialTreatmentColor <- rgb(0, 136/255, 204/255, .2)
  
  # Combined plot
  output$fullTrialDensityPlot <- renderPlot({
    plot(trialDensControl(), xlim = trialXlim(), ylim = trialYlim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(trialDensControl(), density = -1, col = trialControlColor)
    polygon(trialDensTreatment(), density = -1, col = trialTreatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(trialControlColor, trialTreatmentColor), bty = 'n',
           border = NA)
  })
  
  # Download links  
  fullTrialPilotPoweredPlotDownloadPNG <- reactive({
    plot(trialDensControl(), xlim = trialXlim(), ylim = trialYlim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(trialDensControl(), density = -1, col = trialControlColor)
    polygon(trialDensTreatment(), density = -1, col = trialTreatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(trialControlColor, trialTreatmentColor), bty = 'n',
           border = NA)     
  })
  
  fullTrialPilotPoweredPlotDownloadPDF <- reactive({
    plot(trialDensControl(), xlim = trialXlim(), ylim = trialYlim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(trialDensControl(), density = -1, col = trialControlColor)
    polygon(trialDensTreatment(), density = -1, col = trialTreatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(trialControlColor, trialTreatmentColor), bty = 'n',
           border = NA)     
  })
  
  fullTrialPilotPoweredPlotDownloadEPS <- reactive({
    plot(trialDensControl(), xlim = trialXlim(), ylim = trialYlim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(trialDensControl(), density = -1, lty = 'solid')
    polygon(trialDensTreatment(), density = 5, lty = 'dashed', angle=45)
    
    legend('topleft',c('Control','Treatment'),
           bty = 'n',
           lty = c('solid', 'dashed')
        )
  })  
  
  output$downloadFullTrialPilotESPoweredPNG <- downloadHandler(
    filename = function() { paste('fullTrialPilotPoweredDensityPlot', Sys.Date(), '.png', sep='') },
    content = function(file) {
      png(file)
      print(fullTrialPilotPoweredPlotDownloadPNG())
      dev.off()
    }
  ) 
  
  output$downloadFullTrialPilotESPoweredPDF <- downloadHandler(
    filename = function() { paste('fullTrialPilotPoweredDensityPlot', Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(file)
      print(fullTrialPilotPoweredPlotDownloadPDF())
      dev.off()
    }
  )   
  
  output$downloadFullTrialPilotESPoweredEPS <- downloadHandler(
    filename = function() { paste('fullTrialPilotPoweredDensityPlot', Sys.Date(), '.eps', sep='') },
    content = function(file) {
      setEPS()
      postscript(file)
      print(fullTrialPilotPoweredPlotDownloadEPS())
      dev.off()
    }
  )     
  
  
  ### T-tests for Full Trial (using Pilot ES to decide)
  
  ## Two sample
  # Two-tailed	
  FullTrialTTwoSampleTwoTailCount <- reactive({
    sum(trialResults()$p_two_sample_two_tail < alpha(), na.rm = TRUE)
  })
  
  FullTrialTTwoSampleTwoTailPercent <- reactive({
    round((FullTrialTTwoSampleTwoTailCount()/proceedCount())*100, 2)
  })
  
  # how often over-estimate effect given positive effect
  FullTrialTTwoSampleTwoTailCountGTTrueEffect <- reactive({
    sum(trialResults()$p_two_sample_two_tail < alpha() & trialResults()$effect_size > true_effect_size(), na.rm = TRUE)
  })
  
  FullTrialTTwoSampleTwoTailCountGTTrueEffectPercent <- reactive({
    round((FullTrialTTwoSampleTwoTailCountGTTrueEffect()/FullTrialTTwoSampleTwoTailCount())*100, 2)
  })	
  
  
  # One-tailed
  FullTrialTTwoSampleOneTailCount <- reactive({
    sum(trialResults()$p_two_sample_one_tail < alpha(), na.rm = TRUE)
  })
  
  FullTrialTTwoSampleOneTailPercent <- reactive({
    round((FullTrialTTwoSampleOneTailCount()/proceedCount())*100, 2)
  })	
  
  # how often over-estimate effect given positive effect
  FullTrialTTwoSampleOneTailCountGTTrueEffect <- reactive({
    sum(trialResults()$p_two_sample_one_tail < alpha() & trialResults()$effect_size > true_effect_size(), na.rm = TRUE)
  })
  
  FullTrialTTwoSampleOneTailCountGTTrueEffectPercent <- reactive({
    round((FullTrialTTwoSampleOneTailCountGTTrueEffect()/FullTrialTTwoSampleOneTailCount())*100, 2)
  })	
  
  ## One sample
  
  # Two-tailed
  FullTrialTClinSigTwoTailCount <- reactive({
    sum(trialResults()$p_clin_sig_two_tail < alpha(), na.rm = TRUE)
  })
  
  FullTrialTClinSigTwoTailPercent <- reactive({
    round((FullTrialTClinSigTwoTailCount()/proceedCount())*100, 2)
  })	
  
  # how often over-estimate effect given positive effect
  FullTrialTClinSigTwoTailCountGTTrueEffect <- reactive({
    sum(trialResults()$p_clin_sig_two_tail < alpha() & trialResults()$effect_size > true_effect_size(), na.rm = TRUE)
  })
  
  FullTrialTClinSigTwoTailCountGTTrueEffectPercent <- reactive({
    round((FullTrialTClinSigTwoTailCountGTTrueEffect()/FullTrialTClinSigTwoTailCount())*100, 2)
  })	
  
  
  # One-tailed
  FullTrialTClinSigOneTailCount <- reactive({
    sum(trialResults()$p_clin_sig_one_tail < alpha(), na.rm = TRUE)
  })
  
  FullTrialTClinSigOneTailPercent <- reactive({
    round((FullTrialTClinSigOneTailCount()/proceedCount())*100, 2)
  })
  
  # how often over-estimate effect given positive effect
  FullTrialTClinSigOneTailCountGTTrueEffect <- reactive({
    sum(trialResults()$p_clin_sig_one_tail < alpha() & trialResults()$effect_size > true_effect_size(), na.rm = TRUE)
  })
  
  FullTrialTClinSigOneTailCountGTTrueEffectPercent <- reactive({
    round((FullTrialTClinSigOneTailCountGTTrueEffect()/FullTrialTClinSigOneTailCount())*100, 2)
  })	  
  
  
  ## No Pilot Study
  # repeat, but only use clinically significant calculated sample size
  noPilotTrialResults <- reactive({
    simulateFullTrial(pilotResults(), clinSigTrialSampleSize(), true_effect_size(), alpha(), proceedMethod(), clinical_level(), simulations())
  })	
  
  # No Pilot Full Trial Statistics
  noPilotFullTrialStats <- reactive({	
    # Compose data frame
    data.frame(
      Name = c("Mean Sample Size", 
               "Mean Group Size",
               "Mean Treatment Group Mean Across Simulations",
               "Mean Treatment Group SD Across Simulations",
               "Mean Control Group Mean Across Simulations",
               "Mean Control Group SD Across Simulations",
               "Mean Effect Size Across Simulations",
               "Mean Effect Size SD Across Simulations"),
      Value = as.character(c(round(mean(noPilotTrialResults()$sample_size, na.rm = TRUE), 2),
                             round(mean(noPilotTrialResults()$group_size, na.rm = TRUE), 2),
                             round(mean(noPilotTrialResults()$treatment_mean, na.rm = TRUE), 2),
                             round(sd(noPilotTrialResults()$treatment_mean, na.rm = TRUE), 2),
                             round(mean(noPilotTrialResults()$control_mean, na.rm = TRUE), 2),
                             round(sd(noPilotTrialResults()$control_mean, na.rm = TRUE), 2),
                             round(mean(noPilotTrialResults()$effect_size, na.rm = TRUE), 2),
                             round(sd(noPilotTrialResults()$effect_size, na.rm = TRUE), 2)
      )), 
      stringsAsFactors=FALSE)
  }) 		
  
  output$noPilotFullTrialStats <- renderTable({
    noPilotFullTrialStats()
  })
  
  ## Density plots
  
  # Single plots
  noPilotTrialDensControl <- reactive({
    density(noPilotTrialResults()$control_mean, na.rm = TRUE)
  })
  
  noPilotTrialDensTreatment <- reactive({
    density(noPilotTrialResults()$treatment_mean, na.rm = TRUE)
  })
  
  # Ranges
  noPilotTrialXlim <- reactive({
    range(noPilotTrialDensControl()$x, noPilotTrialDensTreatment()$x, na.rm = TRUE)
  })     
  
  noPilotTrialYlim <- reactive({
    range(0, noPilotTrialDensControl()$y, noPilotTrialDensTreatment()$y, na.rm = TRUE)
  })
  
  noPilotTrialControlColor <- rgb(0,0,0,0.1)
  noPilotTrialTreatmentColor <- rgb(0, 136/255, 204/255, .2)
  
  
  # Combined plot
  output$noPilotFullTrialDensityPlot <- renderPlot({
    plot(noPilotTrialDensControl(), xlim = noPilotTrialXlim(), ylim = noPilotTrialYlim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(noPilotTrialDensControl(), density = -1, col = noPilotTrialControlColor)
    polygon(noPilotTrialDensTreatment(), density = -1, col = noPilotTrialTreatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(noPilotTrialControlColor, noPilotTrialTreatmentColor), bty = 'n',
           border = NA)
  })
  
  # Download links  
  fullTrialPracticalPoweredPlotDownloadPNG <- reactive({
    plot(noPilotTrialDensControl(), xlim = noPilotTrialXlim(), ylim = noPilotTrialYlim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(noPilotTrialDensControl(), density = -1, col = noPilotTrialControlColor)
    polygon(noPilotTrialDensTreatment(), density = -1, col = noPilotTrialTreatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(noPilotTrialControlColor, noPilotTrialTreatmentColor), bty = 'n',
           border = NA)
  })
  
  fullTrialPracticalPoweredPlotDownloadPDF <- reactive({
    plot(noPilotTrialDensControl(), xlim = noPilotTrialXlim(), ylim = noPilotTrialYlim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(noPilotTrialDensControl(), density = -1, col = noPilotTrialControlColor)
    polygon(noPilotTrialDensTreatment(), density = -1, col = noPilotTrialTreatmentColor)
    
    legend('topleft',c('Control','Treatment'),
           fill = c(noPilotTrialControlColor, noPilotTrialTreatmentColor), bty = 'n',
           border = NA)  
  })
  
  fullTrialPracticalPoweredPlotDownloadEPS <- reactive({
    plot(noPilotTrialDensControl(), xlim = noPilotTrialXlim(), ylim = noPilotTrialYlim(), xlab = 'Effect Sizes',
         main = 'Smoothed Distribution of Effect Sizes by Treatment Group', 
         panel.first = grid())
    
    polygon(noPilotTrialDensControl(), density = -1, lty = 'solid')
    polygon(noPilotTrialDensTreatment(), density = 5, lty = 'dashed', angle=45)
    
    legend('topleft',c('Control','Treatment'),
           bty = 'n',
           lty = c('solid', 'dashed')
    )
  })  
  
  output$downloadFullTrialPracticalESPoweredPNG <- downloadHandler(
    filename = function() { paste('fullTrialPracticalPoweredDensityPlot', Sys.Date(), '.png', sep='') },
    content = function(file) {
      png(file)
      print(fullTrialPracticalPoweredPlotDownloadPNG())
      dev.off()
    }
  ) 
  
  output$downloadFullTrialPracticalESPoweredPDF <- downloadHandler(
    filename = function() { paste('fullTrialPracticalPoweredDensityPlot', Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(file)
      print(fullTrialPracticalPoweredPlotDownloadPDF())
      dev.off()
    }
  )   
  
  output$downloadFullTrialPracticalESPoweredEPS <- downloadHandler(
    filename = function() { paste('fullTrialPracticalPoweredDensityPlot', Sys.Date(), '.eps', sep='') },
    content = function(file) {
      setEPS()
      postscript(file)
      print(fullTrialPracticalPoweredPlotDownloadEPS())
      dev.off()
    }
  )     
  
  
  ### T-tests for Full Trial (no pilot)
  
  ## Two sample
  # Two-tailed	
  noPilotFullTrialTTwoSampleTwoTailCount <- reactive({
    sum(noPilotTrialResults()$p_two_sample_two_tail < alpha(), na.rm = TRUE)
  })
  
  noPilotFullTrialTTwoSampleTwoTailPercent <- reactive({
    round((noPilotFullTrialTTwoSampleTwoTailCount()/proceedCount())*100, 2)
  })
  
  # how often over-estimate effect given positive effect
  noPilotFullTrialTTwoSampleTwoTailCountGTTrueEffect <- reactive({
    sum(noPilotTrialResults()$p_two_sample_two_tail < alpha() & trialResults()$effect_size > true_effect_size(), na.rm = TRUE)
  })
  
  noPilotFullTrialTTwoSampleTwoTailCountGTTrueEffectPercent <- reactive({
    round((noPilotFullTrialTTwoSampleTwoTailCountGTTrueEffect()/noPilotFullTrialTTwoSampleTwoTailCount())*100, 2)
  })	  
  
  
  # One-tailed
  noPilotFullTrialTTwoSampleOneTailCount <- reactive({
    sum(noPilotTrialResults()$p_two_sample_one_tail < alpha(), na.rm = TRUE)
  })
  
  noPilotFullTrialTTwoSampleOneTailPercent <- reactive({
    round((noPilotFullTrialTTwoSampleOneTailCount()/proceedCount())*100, 2)
  })	
  
  # how often over-estimate effect given positive effect
  noPilotFullTrialTTwoSampleOneTailCountGTTrueEffect <- reactive({
    sum(noPilotTrialResults()$p_two_sample_one_tail < alpha() & trialResults()$effect_size > true_effect_size(), na.rm = TRUE)
  })
  
  noPilotFullTrialTTwoSampleOneTailCountGTTrueEffectPercent <- reactive({
    round((noPilotFullTrialTTwoSampleOneTailCountGTTrueEffect()/noPilotFullTrialTTwoSampleOneTailCount())*100, 2)
  })
  
  ## One sample
  
  # Two-tailed
  noPilotFullTrialTClinSigTwoTailCount <- reactive({
    sum(noPilotTrialResults()$p_clin_sig_two_tail < alpha(), na.rm = TRUE)
  })
  
  noPilotFullTrialTClinSigTwoTailPercent <- reactive({
    round((noPilotFullTrialTClinSigTwoTailCount()/proceedCount())*100, 2)
  })	
  
  # how often over-estimate effect given positive effect
  noPilotFullTrialTClinSigTwoTailCountGTTrueEffect <- reactive({
    sum(noPilotTrialResults()$p_clin_sig_two_tail < alpha() & trialResults()$effect_size > true_effect_size(), na.rm = TRUE)
  })
  
  noPilotFullTrialTClinSigTwoTailCountGTTrueEffectPercent <- reactive({
    round((noPilotFullTrialTClinSigTwoTailCountGTTrueEffect()/noPilotFullTrialTClinSigTwoTailCount())*100, 2)
  })  
  
  # One-tailed
  noPilotFullTrialTClinSigOneTailCount <- reactive({
    sum(noPilotTrialResults()$p_clin_sig_one_tail < alpha(), na.rm = TRUE)
  })
  
  noPilotFullTrialTClinSigOneTailPercent <- reactive({
    round((noPilotFullTrialTClinSigOneTailCount()/proceedCount())*100, 2)
  })
  
  # how often over-estimate effect given positive effect
  noPilotFullTrialTClinSigOneTailCountGTTrueEffect <- reactive({
    sum(noPilotTrialResults()$p_clin_sig_one_tail < alpha() & trialResults()$effect_size > true_effect_size(), na.rm = TRUE)
  })
  
  noPilotFullTrialTClinSigOneTailCountGTTrueEffectPercent <- reactive({
    round((noPilotFullTrialTClinSigOneTailCountGTTrueEffect()/noPilotFullTrialTClinSigOneTailCount())*100, 2)
  })  
  
  
  ### STEP 5.  Interpret results
  
  output$proceedCountFullTrialNotice <- renderText({
    paste('<strong>Note:</strong> ', proceedMethodDescription(), ' Consequently, we would proceed to trial in <strong>', proceedCount() , '</strong> of the <strong>', simulations(), '</strong> simulations conducted, or <strong>', round((proceedCount()/simulations())*100, 2) ,'%</strong> of them. The below conclusions are thus made for only those ', proceedCount(), ' cases.<br><br>', sep='')
  })
  
  # based upon Pilot ES
  conclusionStats <- reactive({	
    # Compose data frame
    data.frame(
      Inference = c("Reject Null Hypothesis: Full trial treatment mean is not equal to control mean (two-tailed t-test)",
                    "Reject Null Hypothesis: Full trial treatment mean greater than control mean (one-tailed t-test)",
                    "Reject Null Hypothesis: Full trial treatment mean not equal to clinically significant effect size (two-tailed t-test)",
                    "Reject Null Hypothesis: Full trial treatment mean greater than clinically significant effect size (one-tailed t-test)"						
      ),
      Percent = as.character(c(FullTrialTTwoSampleTwoTailPercent(),
                               FullTrialTTwoSampleOneTailPercent(),
                               FullTrialTClinSigTwoTailPercent(),
                               FullTrialTClinSigOneTailPercent()									
      )), 
      stringsAsFactors=FALSE)
  }) 		
  
  output$conclusionResults <- renderTable({
    conclusionStats()
  })		
  
  # No Pilot Es
  noPilotConclusionStats <- reactive({	
    # Compose data frame
    data.frame(
      Inference = c("Reject Null Hypothesis: Full trial treatment mean is not equal to control mean (two-tailed t-test)",
                    "Reject Null Hypothesis: Full trial treatment mean greater than control mean (one-tailed t-test)",
                    "Reject Null Hypothesis: Full trial treatment mean not equal to clinically significant effect size (two-tailed t-test)",
                    "Reject Null Hypothesis: Full trial treatment mean greater than clinically significant effect size (one-tailed t-test)"						
      ),
      Percent = as.character(c(noPilotFullTrialTTwoSampleTwoTailPercent(),
                               noPilotFullTrialTTwoSampleOneTailPercent(),
                               noPilotFullTrialTClinSigTwoTailPercent(),
                               noPilotFullTrialTClinSigOneTailPercent()									
      )), 
      stringsAsFactors=FALSE)
  }) 		
  
  output$noPilotConclusionResults <- renderTable({
    noPilotConclusionStats()
  })		
  
  ### IMPLICATIONS
  
  # Decision to Proceed to Trial
  
  testForProceed <- function (true_effect_size, clinical_effect_size){
    if(true_effect_size > clinical_effect_size){
      shouldProceed <- 1	
    } else if(true_effect_size == clinical_effect_size){
      shouldProceed <- 2
    } else {
      shouldProceed <- 0
    }		
    
    return(shouldProceed)
  }
  
  shouldProceed <- reactive({
    testForProceed(true_effect_size(), clinical_level())
  })
  
  bestPracticeNotice <- function(should_proceed, true_effect_size, clinical_effect_size){
    if(should_proceed == 1){
      message <- paste('Because the true effect size of <strong>', true_effect_size, '</strong> is <strong>greater than</strong> the clinically significant effect size of <strong>', clinical_effect_size, '</strong>, it is <span class="correct">correct</span> to conduct a full to trial.<br><br>', sep='')
    } else if(should_proceed == 2){
      message <- paste('Because the true effect size of <strong>', true_effect_size, '</strong> is <strong>equal to</strong> the clinically significant effect size of <strong>', clinical_effect_size, '</strong>, it is <span class="correct">correct</span> to conduct a full trial.<br><br>', sep='')		
    } else {
      message <- paste('Because the true effect size of <strong>', true_effect_size, '</strong> is <strong>less than</strong> the clinically significant effect size of <strong>', clinical_effect_size, '</strong>, it is <span  class="incorrect">incorrect</span> to conduct a full trial.<br><br>', sep='')
    }
    
    return(message)
  }
  
  output$bestPractice <- renderText({
    bestPracticeNotice(shouldProceed(), true_effect_size(), clinical_level())
  })
  
  yourChoiceNotice <- function(should_proceed, true_effect_size, clinical_effect_size){
    if(should_proceed == 1 | should_proceed == 2){
      message <- 	paste(proceedMethodDescription(), ' Consequently, you would <span class="correct">correctly</span> proceed to trial in <strong>', proceedCount() , '</strong> of the <strong>', simulations(), '</strong> simulations conducted, or <strong>', round((proceedCount()/simulations())*100, 2) ,'%</strong> of them. This means you <span class="incorrect">incorrectly</span> did not conduct a full trial in <strong>', simulations() - proceedCount(), '</strong> of <strong>', simulations(), '</strong> simulations, or <strong>', round(((simulations() - proceedCount())/simulations())*100, 2) ,'%</strong> of the time.<br><br>', sep='')
    } else {
      message <- 	paste(proceedMethodDescription(), ' Consequently, you <span class="incorrect">incorrectly</span> proceeded to trial in <strong>', proceedCount() , '</strong> of the <strong>', simulations(), '</strong> simulations conducted, or <strong>', round((proceedCount()/simulations())*100, 2) ,'%</strong> of them. You <span class="correct">correctly</span> did not proceeed to a full trial in <strong>', simulations() - proceedCount(), '</strong> of <strong>', simulations(), '</strong> simulations, or <strong>', round(((simulations() - proceedCount())/simulations())*100, 2) ,'%</strong> of the time.<br><br>', sep='')
    }
    
    return(message)
  }
  
  output$yourChoice <- renderText({
    yourChoiceNotice(shouldProceed(), true_effect_size(), clinical_level())
  })	
  
  generateImplicationsProceedStats <- function(shouldProceed){
    rules <- c(
      "Proceed if effect size point estimate is greater than 0",
      "Proceed if effect size point estimate is greater than clinically significant effect size",
      "Proceed if treatment mean greater than control mean (one-tailed t-test)",						
      "Proceed if treatment mean not equal to control mean (two-tailed t-test)",
      "Proceed if treatment mean greater than clinically significant effect size (one-tailed t-test)",
      "Proceed if treatment mean not equal to clinically significant effect size (two-tailed t-test)",
      "Proceed always"						
    )
    
    proceed <- as.character(c(
      pointEstimateZeroPercent(),
      pointEstimateClinSigPercent(),
      tTwoSampleOneTailPercent(),
      tTwoSampleTwoTailPercent(),
      tClinSigOneTailPercent(),
      tClinSigTwoTailPercent(),
      100
    ))
    
    stop <- as.character(c(
      100-pointEstimateZeroPercent(),
      100-pointEstimateClinSigPercent(),
      100-tTwoSampleOneTailPercent(),
      100-tTwoSampleTwoTailPercent(),
      100-tClinSigOneTailPercent(),
      100-tClinSigTwoTailPercent(),
      0
    ))
    
    if(shouldProceed == 1 | shouldProceed == 2){
      stats <- data.frame(
        Rule = rules,
        Proceed = proceed, 
        Stop = stop,
        Correct = proceed, 
        stringsAsFactors=FALSE)
    } else {
      stats <- data.frame(
        Rule = rules,
        Proceed = proceed, 
        Stop = stop,
        Correct = stop, 
        stringsAsFactors=FALSE)	
    }
    
    return(stats)	
  }
  
  implicationsProceedStats <- reactive({
    generateImplicationsProceedStats(shouldProceed())
  })
  
  output$implicationsProceedStats <- renderTable({
    implicationsProceedStats()
  })
  
  generateImplicationsProceedStatsNotice <- function (should_proceed){
    if(should_proceed == 1 | should_proceed == 2){
      message <- 'Given the assumed parameters in the simulation, in this case the best practice would be <span class="correct">proceed to trial</span> in all cases.'
    } else {
      message <- 'Given the assumed parameters in the simulation, in this case the best practice would be to <span class="cinorrect">not to proceed to trial</span> in all cases.'		
    }
    
    messageFull <- paste(message, 'The below table represents how often across all simulations you would proceed to a full trial or stop after the pilot study when applying specific decision rules. The final column quantifies how often you make the correct decision given the assumed simulation parameters.<br><br>')
    
    return(messageFull)
  }
  
  implicationsProceedStatsNotice <- reactive({
    generateImplicationsProceedStatsNotice(shouldProceed())
  })
  
  output$implicationsProceedStatsNotice <- renderText({
    implicationsProceedStatsNotice()
  })
  
  
  # Achieved Statistical Power
  
  output$powerSettings <- renderText({
    paste('You have set your power to <strong>', power(), '</strong>. In theory, this means that in <strong>', round(power()*100, 2), '%</strong> of simulations, your trial will be sufficiently powered to detect the effect size on which the sample size calculations were based.<br><br>', sep='')
  })	
  
  ## Power Loss Summary calculations
  
  # to detect pilot study effect size
  tradeoffTable <- function(true_effect, clinical_effect, simulations_count, proceed_count, proceed_method){
    
    if(proceed_method == "pointestimate_clinical"){
      conduct_trial_n <- pointEstimateClinSigCount()
      conduct_trial_percent <- pointEstimateClinSigPercent()
    } else if(proceed_method == "pointestimate_zero"){
      conduct_trial_n <- pointEstimateZeroCount()
      conduct_trial_percent <- pointEstimateZeroPercent()
    } else if(proceed_method == "ttest_twotail"){
      conduct_trial_n <- tTwoSampleTwoTailCount()
      conduct_trial_percent <- tTwoSampleTwoTailPercent()
    } else if(proceed_method == "ttest_onetail"){
      conduct_trial_n <- tTwoSampleOneTailCount()
      conduct_trial_percent <- tTwoSampleOneTailPercent()
    } else if(proceed_method == "ttest_clinical_twotail"){
      conduct_trial_n <- tClinSigTwoTailCount()
      conduct_trial_percent <- tClinSigTwoTailPercent()
    } else if(proceed_method == "ttest_clinical_onetail"){
      conduct_trial_n <- tClinSigOneTailCount()
      conduct_trial_percent <- tClinSigOneTailPercent()
    } else if(proceed_method == 'always'){
      conduct_trial_n <- simulations()
      conduct_trial_percent <- 100
    }	
    
    proceed_partial_power_loss = (100 - conduct_trial_percent)/100
    
    twotail_diff_reject_null_n <- FullTrialTTwoSampleTwoTailCount()
    twotail_diff_reject_null_percent <- FullTrialTTwoSampleTwoTailPercent()
    
    twotail_diff_reject_null_n_lt_true <- FullTrialTTwoSampleTwoTailCountGTTrueEffect()
    twotail_diff_reject_null_n_lt_true_percent <- FullTrialTTwoSampleTwoTailCountGTTrueEffectPercent()
    
    twotail_diff_reject_null_all_percent <- (twotail_diff_reject_null_n/simulations_count)*100
    twotail_diff_achieved_power_full_trial <- twotail_diff_reject_null_percent/100
    twotail_diff_achieved_power_all <- twotail_diff_reject_null_all_percent/100
    
    twotail_diff_total_power_loss <- (100 - twotail_diff_reject_null_all_percent)/100
    twotail_diff_reject_null_partial_power_loss <- twotail_diff_total_power_loss - proceed_partial_power_loss
    
    
    onetail_diff_reject_null_n <- FullTrialTTwoSampleOneTailCount()
    onetail_diff_reject_null_percent <- FullTrialTTwoSampleOneTailPercent()
    
    onetail_diff_reject_null_n_lt_true <- FullTrialTTwoSampleOneTailCountGTTrueEffect()
    onetail_diff_reject_null_n_lt_true_percent <- FullTrialTTwoSampleOneTailCountGTTrueEffectPercent()		
    
    onetail_diff_reject_null_all_percent <- (onetail_diff_reject_null_n/simulations_count)*100
    onetail_diff_achieved_power_full_trial <- onetail_diff_reject_null_percent/100
    onetail_diff_achieved_power_all <- onetail_diff_reject_null_all_percent/100
    onetail_diff_total_power_loss <- (100 - onetail_diff_reject_null_all_percent)/100
    onetail_diff_reject_null_partial_power_loss <- onetail_diff_total_power_loss - proceed_partial_power_loss
    
    
    twotail_clinsig_reject_null_n <- FullTrialTClinSigTwoTailCount()
    twotail_clinsig_reject_null_percent <- FullTrialTClinSigTwoTailPercent()
    
    twotail_clinsig_reject_null_n_lt_true <- FullTrialTClinSigTwoTailCountGTTrueEffect()
    twotail_clinsig_reject_null_n_lt_true_percent <- FullTrialTClinSigTwoTailCountGTTrueEffectPercent()		
    
    twotail_clinsig_reject_null_all_percent <- (twotail_clinsig_reject_null_n/simulations_count)*100
    twotail_clinsig_total_power_loss <- (100 - twotail_clinsig_reject_null_all_percent)/100
    twotail_clinsig_reject_null_partial_power_loss <- twotail_clinsig_total_power_loss - proceed_partial_power_loss
    twotail_clinsig_achieved_power_full_trial <- twotail_clinsig_reject_null_percent/100
    twotail_clinsig_achieved_power_all <- twotail_clinsig_reject_null_all_percent/100
    
    onetail_clinsig_reject_null_n <- FullTrialTClinSigOneTailCount()
    onetail_clinsig_reject_null_percent <- FullTrialTClinSigOneTailPercent()
    
    onetail_clinsig_reject_null_n_lt_true <- FullTrialTClinSigOneTailCountGTTrueEffect()
    onetail_clinsig_reject_null_n_lt_true_percent <- FullTrialTClinSigOneTailCountGTTrueEffectPercent()		
    
    onetail_clinsig_reject_null_all_percent <- (onetail_clinsig_reject_null_n/simulations_count)*100
    onetail_clinsig_achieved_power_full_trial <- onetail_clinsig_reject_null_percent/100
    onetail_clinsig_achieved_power_all <- onetail_clinsig_reject_null_all_percent/100
    onetail_clinsig_total_power_loss <- (100 - onetail_clinsig_reject_null_all_percent)/100
    onetail_clinsig_reject_null_partial_power_loss <- onetail_clinsig_total_power_loss - proceed_partial_power_loss
    
    #Treatment mean is not equal to control mean (two-tailed t-test)
    
    if(true_effect >= clinical_effect || true_effect < clinical_effect){
      # should proceed to trial, effect exists
      
      HTML <- paste('<br><br>
                    <table class="tradeoffTable">
                    
                    <tr>
                    <td class="headingCol leftCol lastCol"></td>
                    <td class="headingCol lastCol" colspan="2">Over All Simulations, Conduct Full Trial?</td>
                    <td class="headingCol lastCol" colspan="2">Over Simulations With Full Trial, Reject H<sub>0</sub>?</td>
                    <td class="headingCol lastCol" colspan="2">Over All Simulations, Reject H<sub>0</sub>?</td>
                    <td class="headingCol lastCol" colspan="2">Over All Simulations Where Reject H<sub>0</sub>, Overestimate True Effect?</td>
                    <td class="headingCol" colspan="2">"Achieved Power" (Probability of Reject H<sub>0</sub>):</td>
                    </tr>
                    <tr>
                    <th class="headingCol leftCol lastCol">Statistical Test</th>
                    <th class="headingCol">n</th>
                    <th class="headingCol lastCol">%</th>
                    <th class="headingCol">n</th>
                    <th class="headingCol lastCol">%</th>
                    <th class="headingCol">n</th>
                    <th class="headingCol lastCol">%</th>
                    <th class="headingCol">n</th>
                    <th class="headingCol lastCol">%</th>
                    <th class="headingCol">Over Simulations With Full Trial</th>
                    <th class="headingCol">Over All Simulations</th>					
                    </tr>

                    <tr>
                    <td class="lastCol leftCol">Treatment mean is not equal to control mean (two-tailed t-test)</td>
                    <td>', conduct_trial_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(conduct_trial_percent, 2), '</td>
                    <td>', twotail_diff_reject_null_n, '/', proceedCount(), '</td>
                    <td class="lastCol">', round(twotail_diff_reject_null_percent, 2), '</td>
                    <td>', twotail_diff_reject_null_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(twotail_diff_reject_null_all_percent, 2), '</td>
                    <td>', twotail_diff_reject_null_n_lt_true, '/', twotail_diff_reject_null_n, '</td>
                    <td class="lastCol">', round(twotail_diff_reject_null_n_lt_true_percent, 2), '</td>
                    <td>', round(twotail_diff_achieved_power_full_trial, 2), '</td>
                    <td>', round(twotail_diff_achieved_power_all, 2),'</td>
                    </tr>

                    <tr>
                    <td class="lastCol leftCol">Treatment mean is not equal to control mean (one-tailed t-test)</td>
                    <td>', conduct_trial_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(conduct_trial_percent, 2), '</td>
                    <td>', onetail_diff_reject_null_n, '/', proceedCount(), '</td>
                    <td class="lastCol">', round(onetail_diff_reject_null_percent, 2), '</td>
                    <td>', onetail_diff_reject_null_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(onetail_diff_reject_null_all_percent, 2), '</td>
                    <td>', onetail_diff_reject_null_n_lt_true, '/', onetail_diff_reject_null_n, '</td>
                    <td class="lastCol">', round(onetail_diff_reject_null_n_lt_true_percent, 2), '</td>
                    <td>', round(onetail_diff_achieved_power_full_trial, 2), '</td>
                    <td>', round(onetail_diff_achieved_power_all, 2), '</td>					
                    </tr>

                    <tr>
                    <td class="lastCol leftCol">Treatment mean is not equal to clinical significance level (two-tailed t-test)</td>
                    <td>', conduct_trial_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(conduct_trial_percent, 2), '</td>
                    <td>', twotail_clinsig_reject_null_n, '/', proceedCount(), '</td>
                    <td class="lastCol">', round(twotail_clinsig_reject_null_percent, 2), '</td>
                    <td>', twotail_clinsig_reject_null_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(twotail_clinsig_reject_null_all_percent, 2), '</td>
                    <td>', twotail_clinsig_reject_null_n_lt_true, '/', twotail_clinsig_reject_null_n, '</td>
                    <td class="lastCol">', round(twotail_clinsig_reject_null_n_lt_true_percent, 2), '</td>
                    <td>', round(twotail_clinsig_achieved_power_full_trial, 2), '</td>
                    <td>', round(twotail_clinsig_achieved_power_all, 2), '</td>					
                    </tr>

                    <tr>
                    <td class="lastCol leftCol">Treatment mean is not equal to clinical significance level (one-tailed t-test)</td>
                    <td>', conduct_trial_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(conduct_trial_percent, 2), '</td>
                    <td>', onetail_clinsig_reject_null_n, '/', proceedCount(), '</td>
                    <td class="lastCol">', round(onetail_clinsig_reject_null_percent, 2), '</td>
                    <td>', onetail_clinsig_reject_null_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(onetail_clinsig_reject_null_all_percent, 2), '</td>
                    <td>', onetail_clinsig_reject_null_n_lt_true, '/', onetail_clinsig_reject_null_n, '</td>
                    <td class="lastCol">', round(onetail_clinsig_reject_null_n_lt_true_percent, 2), '</td>                    
                    <td>', round(onetail_clinsig_achieved_power_full_trial, 2), '</td>
                    <td>', round(onetail_clinsig_achieved_power_all, 2), '</td>					
                    </tr>				
                    
                    
                    </table><br><br>
                    ', sep='')
    } else {
      # should not proceed to trial, no effect
      HTML <- '<strong>Note:</strong> You have set the true effect size lower than the clinically significant effect size. Since we are only interested in detecting effect sizes greater than or equal to the clinically significant effect size, we do not here quantify power loss. Any detection of an effect greater than or equal to the clinically significant effect size would reflect a Type 1 Error, or false positive, vis-a-vis clinical significance.'
    }
    
    return(HTML)
  }
  
  tradeoffTableOutput <- reactive({
    tradeoffTable(true_effect_size(), clinical_level(), simulations(), proceedCount(), proceedMethod())
  })
  
  output$tradeoffTable <- renderText({
    tradeoffTableOutput()
  })
  
  # to detect clinically significant effect size
  tradeoffTableNoPilot <- function(true_effect, clinical_effect, simulations_count, proceed_count, proceed_method){
    
    if(proceed_method == "pointestimate_clinical"){
      conduct_trial_n <- pointEstimateClinSigCount()
      conduct_trial_percent <- pointEstimateClinSigPercent()
    } else if(proceed_method == "pointestimate_zero"){
      conduct_trial_n <- pointEstimateZeroCount()
      conduct_trial_percent <- pointEstimateZeroPercent()
    } else if(proceed_method == "ttest_twotail"){
      conduct_trial_n <- tTwoSampleTwoTailCount()
      conduct_trial_percent <- tTwoSampleTwoTailPercent()
    } else if(proceed_method == "ttest_onetail"){
      conduct_trial_n <- tTwoSampleOneTailCount()
      conduct_trial_percent <- tTwoSampleOneTailPercent()
    } else if(proceed_method == "ttest_clinical_twotail"){
      conduct_trial_n <- tClinSigTwoTailCount()
      conduct_trial_percent <- tClinSigTwoTailPercent()
    } else if(proceed_method == "ttest_clinical_onetail"){
      conduct_trial_n <- tClinSigOneTailCount()
      conduct_trial_percent <- tClinSigOneTailPercent()
    } else if(proceed_method == 'always'){
      conduct_trial_n <- simulations()
      conduct_trial_percent <- 100
    }	
    
    proceed_partial_power_loss = (100 - conduct_trial_percent)/100
    
    twotail_diff_reject_null_n <- noPilotFullTrialTTwoSampleTwoTailCount()
    twotail_diff_reject_null_percent <- noPilotFullTrialTTwoSampleTwoTailPercent()
    
    twotail_diff_reject_null_n_lt_true <- noPilotFullTrialTTwoSampleTwoTailCountGTTrueEffect()
    twotail_diff_reject_null_n_lt_true_percent <- noPilotFullTrialTTwoSampleTwoTailCountGTTrueEffectPercent()		
    
    twotail_diff_reject_null_all_percent <- (twotail_diff_reject_null_n/simulations_count)*100
    twotail_diff_achieved_power_full_trial <- twotail_diff_reject_null_percent/100
    twotail_diff_achieved_power_all <- twotail_diff_reject_null_all_percent/100
    twotail_diff_total_power_loss <- (100 - twotail_diff_reject_null_all_percent)/100
    twotail_diff_reject_null_partial_power_loss <- twotail_diff_total_power_loss - proceed_partial_power_loss
    
    onetail_diff_reject_null_n <- noPilotFullTrialTTwoSampleOneTailCount()
    onetail_diff_reject_null_percent <- noPilotFullTrialTTwoSampleOneTailPercent()
    
    onetail_diff_reject_null_n_lt_true <- noPilotFullTrialTTwoSampleOneTailCountGTTrueEffect()
    onetail_diff_reject_null_n_lt_true_percent <- noPilotFullTrialTTwoSampleOneTailCountGTTrueEffectPercent()
    
    onetail_diff_reject_null_all_percent <- (onetail_diff_reject_null_n/simulations_count)*100
    onetail_diff_achieved_power_full_trial <- onetail_diff_reject_null_percent/100
    onetail_diff_achieved_power_all <- onetail_diff_reject_null_all_percent/100
    onetail_diff_total_power_loss <- (100 - onetail_diff_reject_null_all_percent)/100
    onetail_diff_reject_null_partial_power_loss <- onetail_diff_total_power_loss - proceed_partial_power_loss
    
    twotail_clinsig_reject_null_n <- noPilotFullTrialTClinSigTwoTailCount()
    twotail_clinsig_reject_null_percent <- noPilotFullTrialTClinSigTwoTailPercent()
    
    twotail_clinsig_reject_null_n_lt_true <- noPilotFullTrialTClinSigTwoTailCountGTTrueEffect()
    twotail_clinsig_reject_null_n_lt_true_percent <- noPilotFullTrialTClinSigTwoTailCountGTTrueEffectPercent()
    
    twotail_clinsig_reject_null_all_percent <- (twotail_clinsig_reject_null_n/simulations_count)*100
    twotail_clinsig_total_power_loss <- (100 - twotail_clinsig_reject_null_all_percent)/100
    twotail_clinsig_reject_null_partial_power_loss <- twotail_clinsig_total_power_loss - proceed_partial_power_loss
    twotail_clinsig_achieved_power_full_trial <- twotail_clinsig_reject_null_percent/100
    twotail_clinsig_achieved_power_all <- twotail_clinsig_reject_null_all_percent/100
    
    onetail_clinsig_reject_null_n <- noPilotFullTrialTClinSigOneTailCount()
    onetail_clinsig_reject_null_percent <- noPilotFullTrialTClinSigOneTailPercent()
    
    onetail_clinsig_reject_null_n_lt_true <- noPilotFullTrialTClinSigOneTailCountGTTrueEffect()
    onetail_clinsig_reject_null_n_lt_true_percent <- noPilotFullTrialTClinSigOneTailCountGTTrueEffectPercent()    
    
    onetail_clinsig_reject_null_all_percent <- (onetail_clinsig_reject_null_n/simulations_count)*100
    onetail_clinsig_achieved_power_full_trial <- onetail_clinsig_reject_null_percent/100
    onetail_clinsig_achieved_power_all <- onetail_clinsig_reject_null_all_percent/100
    onetail_clinsig_total_power_loss <- (100 - onetail_clinsig_reject_null_all_percent)/100
    onetail_clinsig_reject_null_partial_power_loss <- onetail_clinsig_total_power_loss - proceed_partial_power_loss
    
    #Treatment mean is not equal to control mean (two-tailed t-test)
    
    if(true_effect >= clinical_effect || true_effect < clinical_effect){
      # should proceed to trial, effect exists
      
      HTML <- paste('<br><br>
                    <table class="tradeoffTable">
                    
                    <tr>
                    <td class="headingCol leftCol lastCol"></td>
                    <td class="headingCol lastCol" colspan="2">Over All Simulations, Conduct Full Trial?</td>
                    <td class="headingCol lastCol" colspan="2">Over Simulations With Full Trial, Reject H<sub>0</sub>?</td>
                    <td class="headingCol lastCol" colspan="2">Over All Simulations, Reject H<sub>0</sub>?</td>
                    <td class="headingCol lastCol" colspan="2">Over All Simulations Where Reject H<sub>0</sub>, Overestimate True Effect?</td>
                    <td class="headingCol" colspan="2">"Achieved Power" (Probability of Reject H<sub>0</sub>):</td>
                    </tr>

                    <tr>
                    <th class="headingCol leftCol lastCol">Statistical Test</th>
                    <th class="headingCol">n</th>
                    <th class="headingCol lastCol">%</th>
                    <th class="headingCol">n</th>
                    <th class="headingCol lastCol">%</th>
                    <th class="headingCol">n</th>
                    <th class="headingCol lastCol">%</th>
                    <th class="headingCol">n</th>
                    <th class="headingCol lastCol">%</th>
                    <th class="headingCol">Over Simulations With Full Trial</th>
                    <th class="headingCol">Over All Simulations</th>									
                    </tr>

                    <tr>
                    <td class="lastCol leftCol">Treatment mean is not equal to control mean (two-tailed t-test)</td>
                    <td>', conduct_trial_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(conduct_trial_percent, 2), '</td>
                    <td>', twotail_diff_reject_null_n, '/', proceedCount(), '</td>
                    <td class="lastCol">', round(twotail_diff_reject_null_percent, 2), '</td>
                    <td>', twotail_diff_reject_null_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(twotail_diff_reject_null_all_percent, 2), '</td>
                    <td>', twotail_diff_reject_null_n_lt_true, '/', twotail_diff_reject_null_n, '</td>
                    <td class="lastCol">', round(twotail_diff_reject_null_n_lt_true_percent, 2), '</td>
                    <td>', round(twotail_diff_achieved_power_full_trial, 2), '</td>
                    <td>', round(twotail_diff_achieved_power_all, 2),'</td>
                    </tr>

                    <tr>
                    <td class="lastCol leftCol">Treatment mean is not equal to control mean (one-tailed t-test)</td>
                    <td>', conduct_trial_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(conduct_trial_percent, 2), '</td>
                    <td>', onetail_diff_reject_null_n, '/', proceedCount(), '</td>
                    <td class="lastCol">', round(onetail_diff_reject_null_percent, 2), '</td>
                    <td>', onetail_diff_reject_null_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(onetail_diff_reject_null_all_percent, 2), '</td>
                    <td>', onetail_diff_reject_null_n_lt_true, '/', onetail_diff_reject_null_n, '</td>
                    <td class="lastCol">', round(onetail_diff_reject_null_n_lt_true_percent, 2), '</td>
                    <td>', round(onetail_diff_achieved_power_full_trial, 2), '</td>
                    <td>', round(onetail_diff_achieved_power_all, 2), '</td>					
                    </tr>				

                    <tr>
                    <td class="lastCol leftCol">Treatment mean is not equal to clinical significance level (two-tailed t-test)</td>
                    <td>', conduct_trial_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(conduct_trial_percent, 2), '</td>
                    <td>', twotail_clinsig_reject_null_n, '/', proceedCount(), '</td>
                    <td class="lastCol">', round(twotail_clinsig_reject_null_percent, 2), '</td>
                    <td>', twotail_clinsig_reject_null_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(twotail_clinsig_reject_null_all_percent, 2), '</td>
                    <td>', twotail_clinsig_reject_null_n_lt_true, '/', twotail_clinsig_reject_null_n, '</td>
                    <td class="lastCol">', round(twotail_clinsig_reject_null_n_lt_true_percent, 2), '</td>
                    <td>', round(twotail_clinsig_achieved_power_full_trial, 2), '</td>
                    <td>', round(twotail_clinsig_achieved_power_all, 2), '</td>					
                    </tr>

                    <tr>
                    <td class="lastCol leftCol">Treatment mean is not equal to clinical significance level (one-tailed t-test)</td>
                    <td>', conduct_trial_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(conduct_trial_percent, 2), '</td>
                    <td>', onetail_clinsig_reject_null_n, '/', proceedCount(), '</td>
                    <td class="lastCol">', round(onetail_clinsig_reject_null_percent, 2), '</td>
                    <td>', onetail_clinsig_reject_null_n, '/', simulations(), '</td>
                    <td class="lastCol">', round(onetail_clinsig_reject_null_all_percent, 2), '</td>
                    <td>', onetail_clinsig_reject_null_n_lt_true, '/', onetail_clinsig_reject_null_n, '</td>
                    <td class="lastCol">', round(onetail_clinsig_reject_null_n_lt_true_percent, 2), '</td>
                    <td>', round(onetail_clinsig_achieved_power_full_trial, 2), '</td>
                    <td>', round(onetail_clinsig_achieved_power_all, 2), '</td>					
                    </tr>				
                    
                    
                    </table><br><br>
                    ', sep='')
    } else {
      # should not proceed to trial, no effect
      HTML <- '<strong>Note:</strong> You have set the true effect size lower than the clinically significant effect size. Since we are only interested in detecting effect sizes greater than or equal to the clinically significant effect size, we do not here quantify power loss. Any detection of an effect greater than or equal to the clinically significant effect size would reflect a Type 1 Error, or false positive, vis-a-vis clinical significance.'
    }
    
    return(HTML)
  }
  
  tradeoffTableOutputNoPilot <- reactive({
    tradeoffTableNoPilot(true_effect_size(), clinical_level(), simulations(), proceedCount(), proceedMethod())
  })
  
  output$tradeoffTableNoPilot <- renderText({
    tradeoffTableOutputNoPilot()
  })	
  
  ###################################################	
  ### Show all results and allow for data download
  ###################################################

  # Pilot Simulation
  output$downloadPilotData <- downloadHandler(
    filename = function() { paste('pilot_data', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(pilotResults(), file)
    }
  )
  
  output$pilotResultsData <- renderDataTable({
    pilotResults()
  })			
  
  # Full Trial Simulation, powered by observed pilot study effect size
  output$downloadTrialPilotESData <- downloadHandler(
    filename = function() { paste('full_trial_pilotES_powered_data', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(trialResults(), file)
    }
  )
  
  output$fullTrialResultsData <- renderDataTable({
    trialResults()
  })	
  
  # Full Trial Simulation, powered by practically significant effect size
  output$downloadTrialPracticalESData <- downloadHandler(
    filename = function() { paste('full_trial_practicalES_powered_data', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(noPilotTrialResults(), file)
    }
  )
  
  output$noPilotFullTrialResultsData <- renderDataTable({
    noPilotTrialResults()
  })	
  
  
  
})