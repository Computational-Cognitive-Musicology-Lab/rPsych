
generateJS <- function(...) {


  items <- interface(...)
  lines <- c("",

    "/* initialize jsPsych */",
    "var jsPsych = initJsPsych({",
    "    on_finish: function() {",
    "    jsPsych.data.displayData();",
    "    }",
    "});",
    "",
    items$items |> unlist(),
    "",
    paste0('var timeline = [', paste(names(items$items), collapse = ', ' ), '];'),
    "",
    "/* start the experiment */",
    "jsPsych.run(timeline)",
    ""
  )

  lines <- paste0("\t\t", lines)
  lines <- lines |>
    str_replace_all('FALSE', 'false') |>
    str_replace_all("TRUE", "true")

  list(js = paste(lines, collapse = '\n'),
       pluginsUsed = items$plugins)
}



# make
responseTypes <- c('key', 'slider', 'textbox', 'button')
stimulusTypes <- c('text', 'audio', 'image', 'video')

trialTypes <- expand.grid(responseType = responseTypes, #, 'likert', 'mpc'),
                          stimulusType = stimulusTypes) |>
  as_tibble()

arguments <- c('labels', 'button_label', 'choices', 'prompt',
               'trial_duration', 'response_ends_trial',
               'trial_ends_after_audio', 'stimulus_duration',
               'min', 'max', 'slider_start', 'step', 'slider_width', 'require_movement',
               'render_on_canvas',
               'response_allowed_while_playing',
               'enable_button_after'

    )

trialTypes |>
  mutate(labels = ifelse(responseType == 'slider', '["Enter"]', NA),
         button_label = ifelse(responseType == 'slider', 'Continue', NA),
         choices = ifelse(responseType %in% c('button', 'key'), '["Enter"]', NA),
         prompt = "null",
         trial_duration = "null",
         response_ends_trial = 'true',
         stimulus_duration = ifelse(stimulusType %in% c('image', 'text'), 'null', NA),
         min = ifelse(responseType == 'slider', 0, NA),
         max = ifelse(responseType == 'slider', 100, NA),
         slider_start = ifelse(responseType == 'slider', 50, NA),
         step = ifelse(responseType == 'slider', 1, NA),
         slider_width = ifelse(responseType == 'slider', 1, NA),
         require_movement = ifelse(responseType == 'slider', 'false', NA),
         render_on_canvas = ifelse(stimulusType == 'image', 'true', NA),
         response_allowed_while_playing = ifelse(stimulusType == 'audio', 'true', NA),
         trial_ends_after_audio = ifelse(stimulusType == 'audio' & responseType %in% c('key', 'button'), 'false', NA),
         enable_button_after = ifelse(responseType == 'button', 0, NA)
         ) -> trialTypes





makeResponseFunc <- function(response_type) {

  function(text, audio, image, video, ...) {

    stimulus_type <- c(if (!missing(text)) 'text',
                       if (!missing(audio)) 'audio',
                       if (!missing(image)) 'image',
                       if (!missing(video)) 'video')[1]

    stimulus <- paste0('"', get(stimulus_type), '"')

    trialTypes |>
      filter(responseType == response_type & stimulusType == stimulus_type) |>
      select(-responseType, -stimulusType) |>
      as.list() |>
      Filter(f = Negate(is.na)) -> allowed_arguments


    given_arguments <- list(...)
    given_arguments <- if (is.null(names(given_arguments))) list() else given_arguments[names(given_arguments) != '']
    given_arguments <- lapply(given_arguments, as.character)

    # ID appropriate plug in
    response <- switch(response_type,
                       key = 'keyboard',
                       response_type)
    stimulus_type <- switch(stimulus_type,
                       text = 'html',
                       stimulus_type)


    plugin <- paste0(stimulus_type, '-', response, '-', 'response')

    type <- paste0('jsPsych', plugin  |> str_to_title() |> str_remove_all('-') )

    if (any(!names(given_arguments) %in% names(allowed_arguments))) {
      stop(call. = FALSE,
           "The arguments",
           paste(setdiff(names(given_arguments), names(allowed_arguments)), collapse = ', '),
           "are not valid arguments for a ", plugin, "jsPsych plugin.")
    }


    allowed_arguments[names(given_arguments)] <- given_arguments

    arguments <- unlist(Map(allowed_arguments, names(allowed_arguments),
                            f = \(arg, name) {

                              if (length(arg) > 1) arg <- paste0("[", paste(arg, collapse = ','), ']')
                              paste0(name, ": ", arg)
                            }))


    f <- function(name, timeline = FALSE) {
      if (timeline) stimulus <- paste0("jsPsych.timelineVariable(", stimulus, ")")

      code <- c("",
                paste0('var ', name, ' = {'),
                paste0("    type: ", type, ','),
                paste0('    stimulus: ', stimulus, ','),
                paste0("    ", arguments, ','),
                "};")

      code
    }

    list(func = f, type = type, plugin = plugin)

  }
}

#' @export
response <- setNames(lapply(responseTypes, makeResponseFunc), responseTypes)
# response <- lapply(unique(trialTypes$responseType), makeResponseFunc)



#' @export
block <- function(stimuli.table, trials, randomize_order = TRUE) {
  if (names(trials)[1] == 'func') trials <- list(trials)

  plugins <- lapply(trials, '[[', 'plugin') |> unlist() |> unique()


  f = function(name) {
    names(trials) <- paste0(name, '_trial_', seq_along(trials))

    trialcode <- Map(trials, names(trials), f = \(trial, name) trial$func(name, timeline = TRUE)) |> unlist()



    rows <- apply(stimuli.table, 1,
                  \(row) paste0(strrep(' ', nchar(name) + 7),'{', paste(paste0(names(stimuli.table), ': "', row, '"'), collapse = ', '), '},'))

    stimulivar <- paste0(name, '_variables')

    code <- c("",
              paste0("var ", stimulivar, ' = ['),
              rows,
              '];',
              "",
              trialcode,
              "",
              paste0("var ", name, ' = {'),
              paste0("   timeline: [", paste(names(trials), collapse = ','), "],"),
              paste0('   timeline_variables: ', stimulivar, ','),
              paste0('   randomize_order: ', if (randomize_order) 'true' else 'false'),
              "}"
              )
    code
  }

  list(func = f, plugin = plugins)
}


#' @export
interface <- function(...) {

  items <- list(...)

  plugins <- lapply(items, \(item) item$plugin) |> unlist() |> unique()
  items <- Map(items, names(items), f = \(item, name) item$func(name))

  list(items = items, plugins = plugins)

}
##

# idea:
stim_table <- tibble(AudioFile = c("/home/nat/Bridge/Research/Projects/Rhythm/TheBackbeat/Stimuli/StimuliFiles/O_____O_________.wav",
                                   "/home/nat/Bridge/Research/Projects/Rhythm/TheBackbeat/Stimuli/StimuliFiles/O_____O___O_____.wav"), Type = c("fuck", "you"))
#
#
makeExperiment("test experiment",
  welcome = response$key(text = p("Welcome to the experiment.")),
  consent = response$button(text = p("Here is the consent information."),
                            choices = "['I consent to participate']"),
  block1 = block(stimuli.table = stim_table,
                 trials = response$key(audio = "AudioFile", choices = "['m']", trial_ends_after_audio = TRUE)),
  debrief = response$key(text = "Do you have any comments?")
) |> run()

