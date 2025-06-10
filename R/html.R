#' @export
jsPsych <- tibble(
  source = "https://unpkg.com/jspsych@8.2.1",
  version = "8.2.1",
  css = "https://unpkg.com/jspsych@8.2.1/css/jspsych.css"
)


get_plugins <- function(object) if (class(object) == 'trial') object@Plugin else unique(unlist(lapply(object@Trials, \(trial) trial@Plugin)))

#' Compile experiment to html/javascript
#' 
#' This function takes a [experiment] object and renders it into an html file which you open in your web browser.
#' @rdname make
#' @export
make <- function(experiment, filename = str_remove_all(experiment@Title, '\\s*') |> str_to_title())  {
  if (class(experiment) != 'experiment') stop("The first argument to make() must be an experiment object, created with experiment().")

  filename <- str_replace(filename, '(\\.html)?$|\\.js$', '.html')

  
  preload <- loadFiles(experiment)
  
  plugins <- unlist(lapply(experiment@Parts, get_plugins)) |> unique()
  if (length(preload)) plugins <- c(plugins, "preload")
  plugins <- lapply(plugins, \(plug) tags$script(src = paste0('https://unpkg.com/@jspsych/plugin-', plug)))
  
  
  js <- experiment2js(experiment)
  js <- append(js, after = which(js == '')[1], preload)
  js <- gsub('var timeline = \\[', 'var timeline = [preload, ', js) # instert into timeline
  
  tags$html(
    tags$head(c(list(tags$title(experiment@Title),
                    tags$script(src = jsPsych$source),
                    tags$link(href = jsPsych$css, rel = 'stylesheet')),
              plugins)),
    tags$body(),
    tags$script(HTML(c(paste0('/* Created on ', as.character(experiment@DateCreated), 
                              if (experiment@Author != '') paste0(' by ', experiment@Author), '.*/'),
                       '', js) |> paste(collapse = '\n')))
  ) |>
    save_html(file = filename)

  invisible(filename)


}

#' @rdname make
#' @export
run <- function(x, ...) {
  if (class(x) == 'experiment') {
    filename <- tempfile()
    filename <- make(x, ...)
    on.exit(file.remove(filename))
    
  } else {
    filename <- x
  }
  browseURL(filename)
}


