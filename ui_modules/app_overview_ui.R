app_overview_ui <- function() {
  tabPanel("App Overview",
    HTML(paste0(
      "<div style='text-align: left;'>",
      "<h2><strong> Basic Statistics for Basic Neuroscientists </strong/> <i class='fa fa-brain' aria-hidden='true'></i> </h2>",
      "<p> This is an open source Shiny R app created to provide users an useful and practical method to analyze and visualize basic neuroscience datasets through estimation statistics. </p>",
      
      
      "<h3><b>Features:</b></h3>",
      "<p style='margin-bottom: 10px;'> The app reads .xlsx or .cvs files with the first row used as column data name. Generated graphs can be customized and plot settings saved and uploaded. All the plots and statistics generated can be downloaded. </p>",
      "<ul>",
      "<li><b>Data Overview</b></li>",
      "<p style='margin-bottom: 0px;'> This feature is intended for initial data selection.</p>",
      "<p style='margin-bottom: 10px;'> It uses the available functions in the <a href='https://rstudio.github.io/DT/' target='_blank'>DT package</a> to visualize and edit the data in a <a href='https://datatables.net/manual/' target='_blank'>DataTable</a> format.
      This generated DataTable is dynamic and permits the user to select, modify and filter data which can be plotted below the table display. After data curation, the DataTable can be save into .xlsx or .cvs format for further processing </p>",
      "<li><b>Variance Components Explorer</b></li>",
      "<p style='margin-bottom: 5px;'> Neuroscience datasets usually have a hierarchical structure. For example, observations are taken from neurons, which belong to different animals.
      In the example below, the neurons from the 'Control' group are more variable between mice (inter-animal variability), whereas in the 'Test' group the main source of variability resides within the same animal (intra-animal variability). 
      Therefore data dependencies need to be taken into account.</p/>",
      "<p style='margin-bottom: 5px;'> <img src='example_hierarchical_structure.png' alt='Hierarchical structure' style='width:673px;height:289px; margin-right: 10px;'> </p>",
      "<p style='margin-bottom: 5px;'> The <i>Variance Components Explorer</i/> allows the user to estimate the sources of variability so that data dependencies can be considered.</p>",
      "<p style='margin-bottom: 5px;'> <b>Intraclass correlation coefficient (ICC) for 2 levels</b> tab uses the ICCest function from the <a href='https://cran.r-project.org/web/packages/ICC/ICC.pdf' target='_blank'>ICC package</a> to extract the ICC and confidence intervals using the one-way ANOVA variance variance components.
      It also estimates the design effect and effective sample size. 
      A customizable plot with data grouped according to the hierarchical levels and color coded by grouping variable, and a summary table with all the calculated statistics, are also generated. </p>",
      
      "<p style='margin-bottom: 0px;'> The <b> Linear Mixed Model (LMM) </b> tab uses the lmer function from the <a href='https://cran.r-project.org/web/packages/lme4/lme4.pdf' target='_blank'>lme4 package</a>  and other acessory functions to calculate and display LMM fixed and random effects. </p>",
      "<p style='margin-bottom: 0px;'> The variance component for the random effects is calculated as well as the R squared (marginal and conditonal). </p>",
      "<p style='margin-bottom: 0px;'> Effect sizes for fixed effects are estimated from the t-statistics using functions from the <a href='https://cran.r-project.org/web/packages/effectsize/effectsize.pdf' target='_blank'>effectsize package</a>. </p>",
      "<p style='margin-bottom: 10px;'> Selection of the number of hierarchical levels can go up to 4, but more can be included and further lmer functionalities explored if the LMM equation is manually edited when selecting the option '<i> Write lmm formula ? </i/>'.</p>",
      #</p>",
      "<li><b>Plots with effect sizes</b></li>",
      "<p style='margin-bottom: 5px;'> The <b>Superplot</b> tab allows the user to generate a <a href='https://rupress.org/jcb/article/219/6/e202001064/151717/SuperPlots-Communicating-reproducibility-and' target='_blank'>superplot plot</a>. This is scatter plot ideal for data with a 2 level structure, that dispays superimposed information from all datapoints, median/median per experimental level/replica and summary statistics (standard deviation or interquartile range).</p>",
      "<p style='margin-bottom: 0px;'> The <b> Box, Bar, Scatter, Violin and Raincloud </b> tab provides the multiple plotting style options that are also available in the other sections of this app.</p>",
      "<p style='margin-bottom: 0px;'> The left panel allows the user to choose between several options for estimation statistics. Simple or hierarchical bootstrap is used for effect size calculation where a control group is compared against one or multiple other test groups. 
      Mean or median difference provides a measure of absolute unitary changes between groups whereas Hedges g', Cohen's d and Glass' delta provides an effect size coefficient that can be referred as small (0.20), medium (0.50) and large (0.80).</p>",
      "<p style='margin-bottom: 50px;'> </p>",
      "</div>",
      
      "<h5><b>Useful bibliography</b></h5>",
      "<div style='font-size: 14px;'>",
      "<b>Data Presentation </b>",
      "<p style='margin-bottom: 0px;'> Allen EA, Erhardt EB, Calhoun VD. Data visualization in the neurosciences: overcoming the curse of dimensionality. Neuron. 2012 May 24;74(4):603-8. doi: 10.1016/j.neuron.2012.05.001. PMID: 22632718; PMCID: PMC4427844. 0px;'> </p>",
      "<p style='margin-bottom: 0px;'> Allen M, Poggiali D, Whitaker K et al. Raincloud plots: a multi-platform tool for robust data visualization [version 2; peer review: 2 approved]. Wellcome Open Res 2021, 4:63 (https://doi.org/10.12688/wellcomeopenres.15191.2) </p>",
      "<p style='margin-bottom: 0px;'>Samuel J. Lord, Katrina B. Velle, R. Dyche Mullins, Lillian K. Fritz-Laylin; SuperPlots: Communicating reproducibility and variability in cell biology. J Cell Biol 1 June 2020; 219 (6): e202001064. doi: https://doi.org/10.1083/jcb.202001064 </p>",
      "<p style='margin-bottom: 10px;'>Goedhart J. SuperPlotsOfData-a web app for the transparent display and quantitative comparison of continuous data from different conditions. Mol Biol Cell. 2021 Mar 15;32(6):470-474. doi: 10.1091/mbc.E20-09-0583. Epub 2021 Jan 21. PMID: 33476183; PMCID: PMC8101441.</p>",
      "<b>Intraclass correlation coefficient </b>",
      "<p style='margin-bottom: 0px;'> Koo TK, Li MY. A Guideline of Selecting and Reporting Intraclass Correlation Coefficients for Reliability Research. J Chiropr Med. 2016 Jun;15(2):155-63. doi: 10.1016/j.jcm.2016.02.012. Epub 2016 Mar 31. Erratum in: J Chiropr Med. 2017 Dec;16(4):346. PMID: 27330520; PMCID: PMC4913118. </p>",
      "<p style='margin-bottom: 10px;'> Fischer, R., Statistical methods for research workers. En S. Kotz & NL Johnson (Eds.), 1992. Breakthroughs in statistics: Methodology and distribution (pp. 66-70).</p>",
      "<b>Linear mixed models </b>",
      "<p style='margin-bottom: 0px;'>Yu Z, Guindani M, Grieco SF, Chen L, Holmes TC, Xu X. Beyond t test and ANOVA: applications of mixed-effects models for more rigorous statistical analysis in neuroscience research. Neuron. 2022 Jan 5;110(1):21-35. doi: 10.1016/j.neuron.2021.10.030. Epub 2021 Nov 15. PMID: 34784504; PMCID: PMC8763600. </p>",
      "<p style='margin-bottom: 10px;'> Hajduk GK. Introduction to linear mixed models. 2017 https://gkhajduk.github.io/2017-03-09-mixed-models/ </p>",
      "<b> Estimation statistics and boostrapping:</b>",
      "<p style='margin-bottom: 0px;'> Calin-Jageman RJ, Cumming G (2019) Estimation for better inference in neuroscience. eNeuro 6:ENEURO.0205-19.2019. 10.1523/ENEURO.0205-19.2019  </p>",
      "<p style='margin-bottom: 0px;'> Bernard C. Estimation Statistics, One Year Later. eNeuro. 2021 Apr 1;8(2):ENEURO.0091-21.2021. doi: 10.1523/ENEURO.0091-21.2021. PMID: 33795354; PMCID: PMC8021395 </p>",
      "<p style='margin-bottom: 0px;'>Saravanan V, Berman GJ, Sober SJ. Application of the hierarchical bootstrap to multi-level data in neuroscience. Neuron Behav Data Anal Theory. 2020;3(5):https://nbdt.scholasticahq.com/article/13927-application-of-the-hierarchical-bootstrap-to-multi-level-data-in-neuroscience. Epub 2020 Jul 21. PMID: 33644783; PMCID: PMC7906290 </p>",
      "</div>",
      
      "</ul>"
     
    ))
  )
}
##, either through appropriate resampling techniques (e.g. hierarchical bootstrap) or models that account for random effects (e.g. linear mixed models).