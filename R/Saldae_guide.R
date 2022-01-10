# #----------------
# shinyhelper::create_help_files(files  = c("sald_explorer","sald_growth_date",
#                                           "sald_aggregator","sald_anomaly",
#                                           "sald_special","sald_forecast",
#                                           "sald_report","sald_data"),help_dir = "inst")

#' Saldae guide and help generator
#' @author Farid Azouaou
#' @description ticki
#' @export
sald_guide_loader <- function(){


  rmd_files          <- c("sald_explorer.md","sald_growth_date.md",
                          "sald_aggregator.md","sald_anomaly.md",
                          "sald_special.md","sald_forecast.md",
                          "sald_report.md","sald_data.md","sald_growth_rate.md")
  png_files <- c("sald_forecast.png","sald_anomaly.png","sald_reporting1.png",
                 "sald_reporting2.png","sald_reporting3.png","sald_growth_rate.png")

  rmd_files_package  <- system.file(rmd_files, package = "SaldaeModulesUI")
  png_files_package  <- system.file(png_files, package = "SaldaeModulesUI")

  dir.create("./saldae_guide/images",recursive = TRUE)
  file.copy(from = rmd_files_package,to = paste0("./saldae_guide/",rmd_files),overwrite = TRUE)
  file.copy(from = png_files_package,to = paste0("./saldae_guide/images/",png_files),overwrite = TRUE)

  return("done")
}
