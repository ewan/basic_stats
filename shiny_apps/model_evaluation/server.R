library(shiny)
library(devtools)
library(plyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(emdplot)
library(rocauc)
library(doParallel)
registerDoParallel()
source("src/gmm.R")
source("src/stats.R")
secondary_palette <- c(Different=emd_colours()$lightest$blue,
                       Same=emd_colours()$darkest$purple)
tertiary_palette <- c("False positive rate"=emd_colours()$dark$maroon,
                      "True positive rate"=emd_colours()$darkest$grey)

tr_simple <- read.csv("data/text_ratings/text_ratings_simple.csv")
tr_2groups <- read.csv("data/text_ratings/parody_2groups.csv")

test_stat_varnames <- list("# of s.d. difference in means"="amds",
                           "Absolute difference in means"="amd",
                           "Smoothed symmetric KL divergence"="skld",
                           "Absolute Welch's t statistic"="abststat")

dist_gens <- list("Gaussian mixture"=gmm, "Gaussian"=gauss,
                  "Reference density"=list(dist_matchdns, dist_pair_matchdns),
                  "Reference bootstrap"=list(dist_boot, dist_pair_boot),
                  "Reference normal"=list(gauss_matchvar, gauss_pair_matchvar))
dist_samples <- list("Gaussian mixture"=sample_gmm, "Gaussian"=sample_gauss,
                     "Reference density"=sample_dns, "Reference bootstrap"=sample_boot,
                     "Reference normal"=sample_gauss)
group_sample_types <- list("Gaussian mixture"="prior", "Gaussian"="prior",
                           "Reference density"="reference", "Reference bootstrap"="reference",
                           "Reference normal"="reference")

shinyServer(function(input, output, session) {

  n_expts <- reactive(as.numeric(input$n_expts))
  observe({
    iters <- as.list(paste0("Iter", 1:n_expts()))
    updateSelectInput(session, "select_iter", choices=iters, selected="Iter1")
  })
  n_type1 <- reactive(as.numeric(input$n_type1))
  n_type2 <- reactive(as.numeric(input$n_type2))
  seed <- reactive(as.numeric(input$seed))
  distribution <- reactive(input$distribution)
  dist_gen <- reactive(dist_gens[[distribution()]])
  dist_sample <- reactive(dist_samples[[distribution()]])
  group_sample_type <- reactive(group_sample_types[[distribution()]])
  fake_samples <- reactive({n1 <- n_type1(); n2 <- n_type2(); seed <- seed();
                            dist_gen <- dist_gen(); dist_sample <- dist_sample();
                            gst <- group_sample_type(); rd <- refdata();
                            if (gst == "prior") {
                            sample_2groups_samediff(n_expts(), n1, n2,
                                                    dist_gen, dist_sample,
                                                    seed=seed)
                            } else {
                            sample_2groups_fromsample(n_expts(), 
                                                      list(x=rd$rating,
                                                           group=rd$text_type),
                                                      dist_gen[[1]],
                                                      dist_gen[[2]],
                                                      dist_sample, seed=seed)                              
                            }
                              })
  fs_stats <- reactive(ddply(fake_samples(), .(theory, iter), summarize,
                       amd=absmeandiff_by(x, group),
                       amds=absmeandiff_sd_by(x, group),
                       skld=skld_by(x, group),
                       abststat=abststat_by(x, group)))
  p_stats <- reactive(with(fs_stats(),
                           positive_prediction_stats_linear_classifier_by_rank(get(test_stat()),
                            factor(theory, levels=c("Same", "Different")), extend=F)))
  p_stats_melted <- reactive({p_stats_melted <- melt(p_stats(), .(crit), variable.name="stat");
                              p_stats_melted$stat <- factor(p_stats_melted$stat);
                              levels(p_stats_melted$stat) <- c("True positive rate",
                                                               "False positive rate");
                              p_stats_melted})
  decision_0.05_fp <- reactive(with(p_stats(), crit[which.min(abs(fpr-0.05))]))
  
  refdata <- reactive({
    if (input$data == "Simulated") tr_simple
    else tr_2groups
  })
  
  exp_type <- reactive(input$select_fake)
  exp_iter <- reactive(input$select_iter)
  exp_fake <- reactive(dplyr::filter(fake_samples(), theory == exp_type(),
                                     iter == exp_iter()))
  test_stat_name <- reactive(input$test_stat)
  test_stat <- reactive(test_stat_varnames[[test_stat_name()]])
  
  test_stat_trs <- reactive(list("# of s.d. difference in means"=with(refdata(), absmeandiff_sd_by(rating, text_type)),
                   "Absolute difference in means"=with(refdata(), absmeandiff_by(rating, text_type)),
                   "Signed # of s.d. difference in means"=with(refdata(), meandiff_sd_by(rating, text_type)),
                   "Smoothed symmetric KL divergence"=with(refdata(), skld_by(rating, text_type)),
                   "Absolute Welch's t statistic"=with(refdata(), abststat_by(rating, text_type))))
  
  test_stat_tr <- reactive(test_stat_trs()[[test_stat_name()]])
  output$plot_stat <- renderPlot({ts_tr <- test_stat_tr();
                                  d5fp <- decision_0.05_fp(); 
                                  p <- with(fs_stats(),
                                  hist_overlapping(get(test_stat()), theory,
                                                   test_stat_name(), "Theory", line_width=1,
                                                   colour_palette=secondary_palette))
                                  p <- p + geom_vline(xintercept=d5fp, lwd=2, lty="dashed") 
                                  p <- p + geom_vline(xintercept=ts_tr, lwd=1.5)
                                  p <- p + emd_theme(text_size=13); p})
  output$plot_pstats <- renderPlot({
    psm <- p_stats_melted(); d5fp <- decision_0.05_fp();
    tstr <- test_stat_tr();
    ggplot(psm, aes(x=crit, colour=stat, y=value)) +
      scale_colour_manual(values=tertiary_palette, name="Score") +
      geom_line(lwd=2) +
      geom_vline(xintercept=d5fp, lwd=2, lty="dashed") + 
      geom_vline(xintercept=tstr, lwd=2) + 
      xlab("Decision criterion") +
      emd_theme(text_size=12)    
  })
  output$plot_real <- renderPlot({
    with(refdata(), hist_overlapping(rating, text_type, "Rating",
                                     "Text type", line_width=0.8)) +
      emd_theme(text_size=13)
  }, width=250, height=250, res=100)  
  output$plot_fake <- renderPlot({
    with(exp_fake(), hist_overlapping(x, group, "x", "Group", line_width=0.8)) +
      emd_theme(text_size=13)
  }, width=250, height=250, res=100)
})