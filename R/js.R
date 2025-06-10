#####################################################################-
### this file creates the jsPsych javascript snipets from the objects
### created in logic.
####################################################################-



# Create js from various objects:

args2js <- function(arg, argname) {
  if (class(arg) == 'shiny.tag') return(as.character(arg))
  if (is.null(arg)) return('null')
  if (is.logical(arg)) return(tolower(arg))
  if (class(arg) == 'character') {
    if (grepl('timelineVariable', arg[1])) return(arg)
    arg <- if (is.null(arg) || is.logical(arg)) {
      tolower(arg)
      } else {
        paste0('"', arg, '"')
      }
  }
  arg <- paste0('[', paste(arg, collapse = ', '), ']')
  
  arg
  
}

trial2js <- function(trial) {
  args <- Map(args2js, trial@Args, names(trial@Args))
  c(paste0('var ', trial@Name, ' = {'),
    paste0('    type: ', trial@Type, ','),
    paste0('    ', names(trial@Args), ': ', args, ','),
    '};')
}

block2js <- function(block) {

  stimuli <- lapply(block@Stimuli, \(stim) paste0('"stimuli/', stim, '"'))
    
  stimuli <- Map(names(stimuli), stimuli, f = \(n, s) paste0(n, ': ', s))
  stimuli <- do.call('paste', c(list(sep = ', '), stimuli))
  stimuli <- paste0('{', do.call('paste', c(list(sep = ', ', stimuli))), '},')
  stimuli_lines <- c(paste0('var ', block@Name, '_stimuli = ['),
                     paste0(strrep(' ', 4 + nchar(block@Name) + 12), 
                            stimuli),
                     '];')
  
  
  trials <- Map(block@Trials, paste0(block@Name, '_', names(block@Trials)), f = \(trial, name) {
    trial@Name <- name
    trial@Args$stimulus <- paste0('jsPsych.timelineVariable("', trial@Args$stimulus, '")')
    trial
  })
  trial_lines <- lapply(trials, trial2js) |> unlist()
  
  block_lines <- c(paste0('var ', block@Name, ' = {'),
                   paste0('    timeline: [', paste0(block@Name, '_', paste(names(block@Trials), collapse = ', ')), '],'),
                   paste0('    timeline_variables: ', block@Name, '_stimuli,'),
                   paste0('    ', names(block@Args), ': ', block@Args, ','),
                   '};'
                   )
  
  multi <- length(block@Trials) > 1
  c(paste0('/* The following sections define an experimental block called "', block@Name, '" */'),
    paste0('/* This includes defining stimuli for the block, as well as the ', if (multi) 'trials that happen' else 'trial that happens ',
    'inside the block */'),
    '',
    paste0('/* Stimuli used in "', block@Name, '" block */'),
    stimuli_lines, '', 
    paste0('/* ', if (multi) 'Trials' else 'Trial', ' used in "', block@Name, '" block */'),
    trial_lines, '',
    paste0('/* Actual definition of "', block@Name, '" block */'),
    block_lines)
  
  
}

savefunc <- c('/* function for saving data at the end */',
             'function padZero(date) {',
             '    return ("0" + date).slice(-2);',
             '}',
             'function saveData(data, status){',
             '    var date = new Date(Date.now());',
             '    var timestamp = date.toLocaleDateString();', # slice nosense is to pad 
             "    timestamp = timestamp.replaceAll('/', '-') + '_' + padZero(date.getHours()) + ':' + padZero(date.getMinutes()) + ':' + padZero(date.getSeconds());",
             "    var filename =  status + 'Session_' + timestamp + '.csv'",
             '    if (window.location.protocol == "file:") {',
             '        console.log("LOCAL" + filename);',
             '        jsPsych.data.get().localSave("csv", filename);',
             '    } else { ',
             '        var xhr = new XMLHttpRequest();',
             "        xhr.open('POST', 'write_data.php');", # // 'write_data.php' is the path to the php file
             "        xhr.setRequestHeader('Content-Type', 'application/json');",
             "        xhr.send(JSON.stringify({filedata: data, filename: 'response_data/' + filename}));",
             "    }",
             '}')

experiment2js <- function(experiment, preloadedFiles) {
  preamble <- c(
    savefunc,
    '',
    'var finished = false;',
    "/* initialize jsPsych */",
    "var jsPsych = initJsPsych({",
    "    on_finish: function() {  saveData(jsPsych.data.get().csv(), 'complete'); finished = true; },",
    "    on_close: function() { if (!finished) { saveData(jsPsych.data.get().csv(), 'incomplete')} ; },",
    "    show_progress_bar: true",
    "});")
  parts <- lapply(experiment@Parts,
                  \(part) if (class(part) == 'trial') trial2js(part) else block2js(part))
  
  post <- c( 
    paste0('var timeline = [', paste(names(experiment@Parts), collapse = ', ' ), '];'),
    "",
    "/* start the experiment */",
    "jsPsych.run(timeline)",
    "")
  
  lines <- c(preamble, '', unlist(parts), '', post)
  
  lines
  
}


