# Function for plotting retailers
plot_retailers <- function(data, p_market, prod_id) {
  store_names <- data |>
    filter(primary_market == p_market, product_id == prod_id) |>
    distinct(store_id, .keep_all = TRUE) |>
    distinct(store_id, store_name) |>
    deframe()
  
  data |>
    filter(primary_market == p_market, product_id == prod_id) |>
    ggplot(aes(x = collected, y = excl_shipping, colour = factor(store_id, labels = store_names))) +
    geom_step() +
    labs(title = paste0("Product ID ", prod_id),
         x = "Date",
         y = "Price",
         colour = "Store") +
    theme_minimal()
}