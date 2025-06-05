#' @export
jsPsych <- tibble(
  source = "https://unpkg.com/jspsych@8.2.1",
  version = "8.2.1",
  css = "https://unpkg.com/jspsych@8.2.1/css/jspsych.css"
)


#' @export
makeExperiment <- function(experiment_title, filename = 'interface.html', ...) {

  exprs <- rlang::enexprs(...)
  rlang::expr(generateJS(!!!exprs)) |>
    rlang::eval_tidy(data = htmltools::tags) -> js

  list2env(htmltools::tags, envir = rlang::current_env())

  plugins <- lapply(js$plugin,
                    \(plug) tags$script(src = paste0('https://unpkg.com/@jspsych/plugin-', plug)))
  html(
    do.call('head',
            c(list(title(experiment_title),
                   script(src = jsPsych$source),
                   link(href = jsPsych$css)),
              plugins)),
    body(),
    script(js$js)
  ) |>
    save_html(file = filename )

  invisible(filename)


}

#' @export
run <- function(filename) browseURL(filename)
