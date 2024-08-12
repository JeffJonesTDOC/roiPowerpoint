get_pptx_template <- function(pptx_dir,template_name) {
  current_dir = getwd()
  require(officer)
  setwd(pptx_dir) # Your local directory that contains the PowerPoint template.
  ppt <- officer::read_pptx(template_name)
  setwd(current_dir)
  return(ppt)
}
