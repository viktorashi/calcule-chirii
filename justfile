default: run

run:
    Rscript -e 'shiny::runApp(".", host = "0.0.0.0", port = 7474, launch.browser = TRUE)'

