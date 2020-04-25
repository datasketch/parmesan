

output_parmesan <- function(){

  map(all_sections, function(section){
    output[[gsub('[[:space:]]', '_',section)]] <- renderUI({
      parmesan_render_ui(section = section, config_path = config_path, input = input, env = react_env)
    })
  })

  map(all_sections, function(section){
    insertUI(".controls", ui = uiOutput(gsub('[[:space:]]', '_',section)))
  })

}


