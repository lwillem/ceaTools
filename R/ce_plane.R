# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# add tutorial data
if(0==1){

  num_sim <- 100
  flu_data <- data.frame(incremental_cost   = c(sample(-50:200,num_sim),sample(150:300,num_sim),sample(400:500,num_sim)),
                         incremental_effect = c(sample(-30:250,num_sim),sample(50:400,num_sim),sample(100:500,num_sim)),
                         course_of_action   = paste0('intervention_',rep(1:3,each=num_sim)))
  devtools::use_data(flu_data,overwrite = T)

  data('flu_data',package = 'ceaTools')

}

plot_ce_plane <- function(incremental_cost,incremental_effect,course_of_action) {

  # convert "course_of_action" into levels
  course_of_action <- factor(sim_data$course_of_action)

  # create plot
  plot(incremental_cost ~ incremental_effect, col = course_of_action,
       xlab='Incremental Effect',
       ylab='Incremental Cost')
  abline(h=1,lty=3)
  abline(v=1,lty=3)
  legend('bottomright',levels(course_of_action),col=1:length(levels(course_of_action)),pch=1,cex=0.8)
}
