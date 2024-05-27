contact_ui <- function() {
  tabPanel("Developers & Contact",
           HTML(paste0(
             "<div style='text-align: left;'>",
             "<p>This Shiny R app was developed by <a href='https://profiles.ucl.ac.uk/66600-filipe-nascimento' target='_blank'>Filipe Nascimento</a> from the Queen Square Institute of Neurology, University College London (UCL), United Kingdom.</p>",
             "<p> Full code can be found <a href='https://github.com/fnascimento-ucl/shiny_basic_statistics_app' target='_blank'>https://github.com/fnascimento-ucl/shiny_basic_statistics_app</a>. 
             Please report any problems or suggestions at <a href='https://github.com/fnascimento-ucl/shiny_basic_statistics_app/issues' target='_blank'>https://github.com/fnascimento-ucl/shiny_basic_statistics_app/issues</a>. </p>",
             
             "<p>For any inquiries and support, please send your request to <a href='mailto:f.nascimento@ucl.ac.uk.com'>f.nascimento@ucl.ac.uk</a>.</p>",
             "<p>This work was supported by a Sir Henry Wellcome Postdoctoral Fellowship 221610/Z/20/Z.</p>",
             
             # Including the Wellcome Logo with added space
             "<img src='wellcome-logo-black.jpg' alt='Wellcome Logo' style='width:80px;height:80px; margin-right: 10px;'>",
             
             # Including the Shiny R Logo
             "<img src='shiny_logo.png' alt='Shiny R' style='width:80px;height:80px;'>",
             
             "</div>"
           ))
  )
}
