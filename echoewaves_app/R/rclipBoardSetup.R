rclipboardSetup <- function()
{
  tagList(
    singleton(
      tags$head(
        tags$script(src = 'JS/clipboard.min.js')
      )
    )
  )
}