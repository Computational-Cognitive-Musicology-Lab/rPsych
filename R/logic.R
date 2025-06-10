#####################################################################################-
### This file defines some (very simple) objects representing response items,
### stimuli, trials, blocks, and experiments.
#####################################################################################-

# preamble ----

## define trial types and associated arguments ----


### response/stimulus
trialTypes <- c('keypress/audio', 'button/audio', 'slider/audio',
                'audio/text', 'button/text', 'keypress/text', 'slider/text', 'video/text',
                'button/image', 'keypress/image', 'slider/image',
                'samedifferent/text', 'samedifferent/image',
                'video/button', 'video/keypress', 'video/slider',
                'text/text', 'likert/text', 'choice/text') |>
  strsplit(split = '/') |>
  do.call(what = 'rbind') |>
  as.data.frame() |> 
  setNames(c('response', 'stimulus'))

responseArgs <- list(audio = list(show_done_button = TRUE, allow_playback = FALSE, 
                                  recording_duration = 2000),
                     button = list(choices = c(), response_ends_trial = TRUE,
                                   enable_button_after = 0),
                     keypress = list(choices = c(), response_ends_trial = TRUE),
                     slider = list(prompt = NULL, button_label = "Continue",
                                   min = 0, max = 100, start = 50, slider_start = 50,
                                   step = 1, slider_width = NULL,
                                   require_movement = FALSE,
                                   response_ends_trial = TRUE),
                     samedifferent = list(answer = c('same', 'different'),
                                          same_key = 'q',
                                          different_key = 'p',
                                          first_stim_duration = 1000,
                                          gap_duration = 500,
                                          second_stim_duration = 1000),
                     text = list(autocomplete = FALSE, 
                                 button_label = "Submit"),
                     choice = list(choices = c('Yes', 'No'), 
                                   allow_multiple_choices = FALSE, 
                                   required = TRUE, 
                                   button_label = 'Submit'),
                     likert = list(labels = as.character(1:7), 
                                   scale_width = NULL, 
                                   button_label = 'Submit'))

stimulusArgs <- list(audio = alist(stimulus = , prompt = NULL, stimulus_duration = NULL,
                                  trial_duration = NULL, trial_ends_after_audio = FALSE, 
                                  response_allowed_while_playing = TRUE),
                     image = alist(stimulus =, prompt = NULL, stimulus_height = NULL, stimulus_width = NULL,
                                  maintain_aspect_ratio = TRUE,
                                  stimulus_duration = NULL, trial_duration = NULL),
                     text = alist(stimulus =,  prompt = NULL, stimulus_duration = NULL,
                                  trial_duration = NULL, randomize_question_order = TRUE),
                     video = alist(stimulus = , show_done_button = TRUE, allow_playback = TRUE,
                                  recording_duration = 2000)) 

valid_stimuli <- function(response) {
  trialTypes$stimulus[trialTypes$response == response@Type]
}

valid_response <- function(stimulus) {
  trialTypes$response[trialTypes$stimulus == stimulus@Type]
}

# response and stimulus ----

## class definitions ----

#' @export
setClass('response', slots = c(Type = 'character', Args = 'list'))
#' @export
setClass('stimulus', slots = c(Type = 'character', Args = 'list'))

### show methods ----

#' @export
setMethod('show', 'response',
          \(object) {
            cat('This is a response$', object@Type, '() item: ',
                'It can be combined with stimulus$', valid_stimuli(object) |> paste(collapse = '/'), '() objects.\n', 
                '\tArguments:\n\t\t',
                paste(paste0(names(object@Args), ' = ', sapply(object@Args, args2js)), collapse = '\n\t\t'), '\n', 
                sep = '')
            
          })

#' @export
setMethod('show', 'stimulus',
          \(object) {
            cat('This is a stimulus$', object@Type, '() item: ',
                'It can be combined with response$', valid_response(object) |> paste(collapse = '/'), '() objects.\n', 
                '\tArguments:\n\t\t',
                paste(paste0(names(object@Args), ' = ', sapply(object@Args, args2js)), collapse = '\n\t\t'), '\n', 
                sep = '')
          })

## actual user functions ----

#' @export
response <- Map(responseArgs, names(responseArgs),
                f = \(args, name) {
                  rlang::new_function(as.pairlist(args),
                                      rlang::expr(list(!!!(setNames(rlang::syms(names(args)), names(args)))) |>
                                                    new(Class = 'response', Type = !!(name), Args = _)))
                }) |> setNames(names(responseArgs))

#' @export
stimulus <- Map(stimulusArgs, names(stimulusArgs),
                f = \(args, name) {
                  rlang::new_function(as.pairlist(args),
                                      rlang::expr(list(!!!(setNames(rlang::syms(names(args)), names(args)))) |>
                                                    new(Class = 'stimulus', Type = !!(name), Args = _)))
                }) |> setNames(names(stimulusArgs))


# trials  ----

### trials are a combination of stimulus and response ###

## class definition ----

setClass('trial', slots = c(Name = 'character', 
                            Plugin = 'character',
                            Type = 'character',
                            Args = 'list'))

### show method ----

#' @export
setMethod('show', 'trial',
          \(object) cat(trial2js(object), sep = '\n'))

## creating trials ----

## this is to go from my response/stimulus names to the more verbose names used by jsPsych
get_jsPsych_type <- function(response, stimulus, multi = FALSE) {
  if (!any(response == trialTypes$response & stimulus == trialTypes$stimulus)) {
    stop("Unfortunately, jsPsych doesn't include a plugin which combines ",
         response, ' responses with ', stimulus, ' stimuli.', call. = FALSE)
  }
  
  response <- switch(response, 
                     keypress = 'Keyboard',
                     choice = if (is.null(multi) || !multi) 'Multi-Choice' else 'Multi-Select',
                     stringr::str_to_sentence(response))
  stimulus <- switch(stimulus, text = 'Html', 
                     stringr::str_to_sentence(stimulus))
  if (response %in% c('Likert', 'Multi-Choice', 'Text', 'Multi-Select') && stimulus == 'Html') {
    stimulus <- 'Survey'
    postfix <- ''
  } else {
    postfix <- 'Response'
  }
  
  
  type <- paste0('jsPsych', stimulus, response,  postfix) |> str_remove_all('-')
  plugin <- paste0(stimulus, '-', response, if (postfix != '') paste0('-', postfix)) |> tolower()
  
  list(Type = type, Plugin = plugin)
  
}

surveyArgs <- function(response, stimulus, type) {
  args <- list(preamble = stimulus@Args$prompt)
  
  isLikert <- grepl('likert', type$Plugin)
  
  questions <- paste0('"', stimulus@Args$stimulus, '"')
  qnames <- if (is.null(names(questions))) paste0('Question ', seq_along(questions)) else ifelse(names(questions) == '', 
                                                                                                 paste0('Question ', seq_along(questions)),
                                                                                                 names(questions))
  qnames <- paste0('"', qnames, '"')
  
  choices <- paste0('[', paste(paste0('"', response@Args[[if (isLikert) 'labels' else 'choices']], '"'), collapse = ', '), ']')
  choices <- rep(choices, length.out = length(questions))
  
  required <- if (length(response@Args$required)) rep(response@Args$required, length.out = length(questions)) |> tolower() 
  
  questions <- if (isLikert) {
    Map(\(prompt, name, options) {
      paste0('    {\n', paste(paste0(c('        prompt: ', 'name: ', 'labels: '), 
                                     c(prompt, name, options), 
                                     ','), 
                              collapse = '\n        '),
             '\n    },\n')
    }, questions, qnames, choices)
  } else  {
    Map(\(prompt, name, options, req) {
      paste0('    {\n', paste(paste0(c('        prompt: ', 'name: ', 'options',  'required: '), 
                                     c(prompt, name, options, req), 
                                     ','), 
                              collapse = '\n        '),
             '\n    },\n')
    }, questions, qnames, choices, required)
      
  }

  browser()
  args$questions <- paste0('[\n', paste(unlist(questions), collapse = '\n'), '\n    ]') 
  args$randomize_question_order <- stimulus@Args$randomize_question_order
  args
  
} 

### making trials by combining stimulus and response objects ----

#' @export
setMethod('+', c('response', 'stimulus'),
          \(e1, e2) {
            type <- get_jsPsych_type(e1@Type, e2@Type, e1@Args$allow_multiple_choices)
            
            args <-  if (grepl('^survey', type$Plugin)) surveyArgs(e1, e2, type) else c(e1@Args, e2@Args)
            
            new('trial', Name = 'Trial', Type = type$Type, Plugin = type$Plugin, Args = args)
            
          })

#' @export
setMethod('+', c('stimulus', 'response'),
          \(e1, e2) e2 + e1)


### forcing text stimulus to trial ----

text2trial <- function(stimulus) {
  args <- stimulus@Args
  if (!'choices' %in% names(args)) args$choices <- 'Continue'
  
  new('trial', Name = 'Instructions', Plugin = 'html-button-response', Type = 'jsPsychHtmlButtonResponse',
      Args = args)
}




# blocks ----

## class definition ----

#' @export
setClass('block', slots = c(Name = 'character', Trials = 'list', Stimuli = 'data.frame', Args = 'list'))

#' @export
setMethod('show', 'block',
          \(object) cat(block2js(object), sep = '\n'))

## making blocks with block() ----


#' @export
block <- function(stimuli, ..., randomize_order = FALSE) {
  trials <- list(...)
  if (any(!sapply(trials, inherits, what = 'trial'))) stop('The block() function can only be provided full trial objects, ',
                                                           'which means you need both a stimulus() and a response() for each trial.')
  if (is.null(names(trials)) || any(names(trials) == '')) stop("Each trial item in a block must be named.", call. = FALSE)
  if (nrow(stimuli) == 0 || ncol(stimuli) == 0) stop("The stimuli table provided to block() cannot be empty.")
  
  args <- list(randomize_order = tolower(randomize_order))
  
  timelineVar <- unlist(lapply(trials, \(trial) trial@Args$stimulus)) |> unique()
  dataVar <-  setdiff(names(stimuli), timelineVar)
  if (length(dataVar)) {
    args$data <- paste0('{\n', 
                        '          ', paste(paste0(dataVar, ': jsPsych.timelineVariable("', dataVar, '")'), collapse = ',\n    '),
                        '\n    }')
  }
  
  
  new('block', Name = 'block', Trials = trials, 
      Stimuli = stimuli, Args = args)
}



# experiment ----

setClass('experiment', slots = c(Title = 'character', Author = 'character', DateCreated = 'Date', Parts = 'list'))

 
#' @export
experiment <- function(..., title = 'Unnamed Experiment', author = '') {
  
  parts <- list(...)
  if (is.null(names(parts)) || any(names(parts) == '')) stop("When creating an experiment(), each element of the experiment",
                                                            "must be explicitely named.", call. = FALSE)
  
  parts <- lapply(parts, \(part) if (class(part) == 'stimulus' && part@Type == 'text') text2trial(part) else part)
  if (!all(sapply(parts, \(part) class(part) %in% c('trial', 'block')))) stop('rPsych experiments must be constructed from only trial and block objects, created using (response() + stimulus()) or block().', call. = FALSE)
  parts <- Map(\(part, name) { part@Name <- name; part}, parts, names(parts))
  
  new('experiment', Title = title, Author = author, DateCreated = Sys.Date(),  Parts = parts)
}

### show method ----

setMethod('show', 'experiment', \(object) cat(experiment2js(object), sep = '\n'))


# idea:
stim_table <- tibble(AudioFile = c("/home/nat/Bridge/Research/Projects/Rhythm/TheBackbeat/Stimuli/StimuliFiles/O_____O_________.wav",
                                   "/home/nat/Bridge/Research/Projects/Rhythm/TheBackbeat/Stimuli/StimuliFiles/O_____O___O_____.wav"), Type = c("fuck", "you"))
#
#
# makeExperiment("test experiment",
  # welcome = response$key(text = p("Welcome to the experiment.")),
  # consent = response$button(text = p("Here is the consent information."),
                            # choices = "I consent to participate"),
  # block1 = block(stimuli.table = stim_table,
                 # trials = response$key(audio = "AudioFile", response_ends_trial = FALSE,
                                       # choices = c("m", 'l'), trial_ends_after_audio = TRUE)),
  # debrief = response$key(text = "Do you have any comments?")
# ) |> run()

# experiment('Test experiment',
#            welcome = stimulus$text(p('Welcome to the experiment,')),
#            consent = stimulus$text(p("Here is the consent information.")) + response$button(choices = "I consent"))


