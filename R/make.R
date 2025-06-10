#' @export
jsPsych_info <- tibble(
  source = "https://unpkg.com/jspsych@8.2.1",
  version = "8.2.1",
  css = "https://unpkg.com/jspsych@8.2.1/css/jspsych.css"
)


get_plugins <- function(object) if (class(object) == 'trial') object@Plugin else unique(unlist(lapply(object@Trials, \(trial) trial@Plugin)))

php_script <- "
<?php
  $post_data = json_decode(file_get_contents('php://input'), true);
  $data = $post_data['filedata'];
  $file = uniqid('session-');
  $name = 'data/{$file}.csv'; 
  file_put_contents($name, $data);
?>"

#' Compile experiment to html/javascript
#' 
#' This function takes a [experiment] object and renders it into an html file which you open in your web browser.
#' @rdname make
#' @export
make <- function(experiment, directory = experiment@Title)  {
  if (class(experiment) != 'experiment') stop("The first argument to make() must be an experiment object, created with experiment().")

  # Prepping directory
  directory <- directory |> str_remove_all('\\s*') |> str_to_title()
  filename <- paste0(directory, .Platform$file.sep, 'index.html')
  if (dir.exists(directory)) {
    if (!(readline(paste0('A directory called "', directory, '".', "Do you want to overwrite it's content?",
            '\n\t\t Type "y" then press "Enter" to overwrite. Otherwise, just press "Enter."')) |>
      str_remove_all('\\s*') |>
      tolower() |>
      pmatch('yes', nomatch = 0))) return(NULL)
  } else {
    dir.create(directory)
  }
  
  data_directory <- paste0(directory, .Platform$file.sep, 'response_data')
  if (!dir.exists(data_directory)) dir.create(data_directory)
  writeLines(php_script, paste0(directory, .Platform$file.sep, 'write_data.php'))
  
  stimuli_directory <- paste0(directory, .Platform$file.sep, 'stimuli')
  if (!dir.exists(stimuli_directory)) dir.create(stimuli_directory)
  
  # Preload files (and copy to stimuli directory)
  preload <- loadFiles(experiment, directory)
  experiment <- preload$experiment # new experiment has had file paths updated
  preload <- preload$js
  
  # Identify plugins we need, for html <head>
  plugins <- unlist(lapply(experiment@Parts, get_plugins)) |> unique()
  if (length(preload)) plugins <- c(plugins, "preload")
  plugins <- lapply(plugins, \(plug) tags$script(src = paste0('https://unpkg.com/@jspsych/plugin-', plug)))
  
  # Generate javascript
  js <- experiment2js(experiment)
  
  ## insert preload js (not a great way to do this)
  js <- append(js, after = which(js == '')[1], preload)
  js <- gsub('var timeline = \\[', 'var timeline = [preload, ', js) # insert into timeline
  
  
  # Make html
  tags$html(
    tags$head(c(list(tags$title(experiment@Title),
                    tags$script(src = jsPsych_info$source),
                    tags$link(href = jsPsych_info$css, rel = 'stylesheet')),
              plugins)),
    tags$body(),
    tags$script(HTML(c(paste0('/* Created on ', as.character(experiment@DateCreated), 
                              if (experiment@Author != '') paste0(' by ', experiment@Author), '.*/'),
                       '', js) |> paste(collapse = '\n')))
  ) |>
    save_html(file = filename)
  
  invisible(filename)

}



#' If you can use the run() function, after make() or instead of it, the experiment will
#' immediately open the experiment in your web browser.
#' @rdname make
#' @export
run <- function(x, ...) {
  if (class(x) == 'experiment') {
    filename <- make(x, ...)
    
  } else {
    filename <- x
  }
  browseURL(filename)
}


