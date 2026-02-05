waiter_loading_screen_html <- htmltools::tagList(
  shiny::img(src = "www/flyable-logo.png", width = "300px"),
  shiny::h3("Weather at Altitude", style="color:#2b4364")
)

waiter_spinner <- waiter::spin_3()

waiter_calculating_html <- function(message = ""){
  waiter::waiter_show(
    html =
      htmltools::tagList(
        waiter_spinner,
        p(message, style="color:#2b4364")
      ),
    color = "rgba(255,255,255,0.5)"
  )}
