waiter_loading_screen_html <- tagList(
  img(src = "www/flyable-logo.png", height = "150px"),
  h3("Weather at Altitude", style="color:#2b4364")
)

waiter_spinner <- waiter::spin_3()

waiter_calculating_html <- function(message = ""){
  waiter::waiter_show(
    html =
      tagList(
        waiter_spinner,
        p(message, style="color:#2b4364")
      ),
    color = "rgba(255,255,255,0.5)"
  )}
