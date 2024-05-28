# library(shiny)
# library(readxl)
# library(dplyr)
# library(magrittr)
# library(ICC) # for intraclass coefficient calculation
# library(ggplot2)
# library(reshape2)
# library(cowplot)
# library(gridExtra)
# library(ggpp)
# library(DT)
# source("functions/calculate_and_plot_icc.R")
# library(shinythemes)
# library(shinyWidgets)
# library(shinyBS)
# library(shinydashboard)
# library(dplyr)
# library(shinydashboardPlus)
# library(shinydisconnect) #to allow disconnect message display
# library(reactlog)
# library(plotly)
# library(shinyjs)
# library(shinycssloaders)
# library(ggh4x)#not used?
# library(colourpicker)
# library(ggprism)
# library(patchwork)
# library(boot)
# library(randomcoloR)
# library(RColorBrewer)
# library(colorspace)
# library(shinydlplot)##not used?
# library(openxlsx)
# library(ggnewscale)
# library(scales)
# library(ggbreak)# not used yet (for scale breaks)
# library(purrr)
# library(ggExtra)
# library(ggdist) # for raincloudplot
# library(ggbeeswarm) # for raincloudplot
# library(sjPlot) # for obtained HTML table from lmer
# library(sjtable2df) # to convert HTML table
# library(effectsize) # for lmer effect sizes
# library(merDeriv) #for random effect confidence interval
# library(knitr) #for LMM table
# library(kableExtra) #for LMM table
# library(MuMIn) #for getting R squared from LMM
# library(datawizard)
# library(berryFunctions) #to add empty rows
# #library(bslib) #for popovers and tooltips
# library(extrafont)
# library(stringr) # to trim variable name
# library(shinybusy) # to display additional info if server is busy
# library(shinyFeedback)
# library(shinyToastify)
# library(ggpubr)
# library(shinyFiles)

# installs and calls the install.load package as required
if (!require(install.load)) {
  install.packages("install.load")
  library(install.load)
}

# list of packages required
packages <- c(
  "shiny", "readxl", "dplyr", "magrittr", "ICC", "ggplot2", "reshape2", 
  "cowplot", "gridExtra", "ggpp", "DT", "shinythemes", "shinyWidgets", 
  "shinyBS", "shinydashboard", "shinydashboardPlus", "shinydisconnect", 
  "reactlog", "plotly", "shinyjs", "shinycssloaders", "ggh4x", "colourpicker", 
  "ggprism", "patchwork", "boot", "randomcoloR", "RColorBrewer", "colorspace", 
  "shinydlplot", "openxlsx", "ggnewscale", "scales", "ggbreak", "purrr", 
  "ggExtra", "ggdist", "ggbeeswarm", "sjPlot", "sjtable2df", "effectsize", 
  "merDeriv", "knitr", "kableExtra", "MuMIn", "datawizard", "berryFunctions", 
  "extrafont", "stringr", "shinybusy", "shinyFeedback", "shinyToastify", 
  "ggpubr", "shinyFiles"
)

# checks if packages are installed loads the packages
install_load(packages)

#UI modules
  #Data Selection and visualization
  source("ui_modules/data_overview_ui.R")
  #Tools for assessing variability components
  source("ui_modules/tools_variability_ui.R")
  #Plots for data representation
  source("ui_modules/plots_data_representation_superplot_ui.R")
  #Bar, box, scatter and violin plots
  source("ui_modules/plots_data_representation_bar_box_scatter_violin_ui.R")
  #About
  source("ui_modules/app_overview_ui.R")
  #Contact
  source("ui_modules/contact_ui.R")

#Server modules
  #Superplots
  source("server_modules/superplot_server_module.R")

  # one level bootstrap
  source("functions/boot_one_level.R")
  
  #hierarchical boot
  source("functions/hierarchical_boot.R")
  
  #Hedges' g
  source("functions/hedges_g.R")

  #Cohen's d
  source("functions/cohens_d.R")

  #Cliff's delta
  source("functions/glass_delta.R")

  source("functions/plot_modal.R")
