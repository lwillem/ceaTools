#REF: https://CRAN.R-project.org/package=BCEA


# add tutorial data
if(0==1){

  incremental_cost   <- sim_data$incremental_cost_disc
  incremental_effect <- sim_data$total_DALYs_averted_disc
  course_of_action   <- sim_data$scenario_name


}

library('BCEA')

bcea_ce_plane <- function(incremental_cost,incremental_effect,course_of_action) {


  opt_interventions <- unique(course_of_action)
  num_interventions <- length(opt_interventions)
  num_sim <-  length(incremental_cost) / num_interventions

  e_bcea <- matrix(0,nrow = num_sim, ncol=num_interventions)
  c_bcea <- matrix(0,nrow = num_sim, ncol=num_interventions)
  i <- 1
  for(i in 1:num_interventions){
    flag <- course_of_action == opt_interventions[i]
    e_bcea[,i] <- incremental_effect[flag]
    c_bcea[,i] <- incremental_cost[flag]
  }

  # add current
  e_bcea <- cbind(e_bcea,0)
  c_bcea <- cbind(c_bcea,0)
  opt_interventions <- c(opt_interventions,"current")

  pdf('bcea_ceplane.pdf')
  sim_bcea <-   bcea(e = -e_bcea,
                   c = -c_bcea,
                   ref = 4,
                   interventions = opt_interventions,
                   plot = TRUE)

  ceplane.plot(sim_bcea,      # plots the Cost-Effectiveness plane
               wtp=25000,     # selects the relevant willingness to pay   (default: 25,000)
               graph="ggplot2",
               ICER.size=2
  )
  contour(sim_bcea,
          comparison = 1:3,
          graph = "ggplot2"
          )
  contour2(sim_bcea,
          comparison = 1:3,
          xl=c(0,6000),
          yl=c(3000000,21000000),
          ICER.size=2,
          graph = "ggplot2"
  )

  ceac.plot(sim_bcea,
            comparison = 1:3,
            graph="ggplot2"
  )

  names(sim_bcea)
  sim_bcea$ICER

  x <- multi.ce(sim_bcea)

  names(x)
  x$m.ce
  x$interventions
  head(x$k)
  head(x$ceaf)
  head(x$n.comparators)
  plot(x$ceaf)
  plot(x$m.ce[,4],type='l')


  x <- ceaf.plot(multi.ce(sim_bcea),
            graph="ggplot2"
  )
  x$data$k

dev.off()


}

dim(e_bcea)
dim(c_bcea)
