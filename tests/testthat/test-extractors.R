test_that("extract_table's link_columns works with multiple columns", {
  html <- rvest::minimal_html('
    <table>
      <tr>
        <th>x</th>
        <th>y</th>
        <th>z</th>
      </tr>
      <tr>
        <td colspan="3"><a href="https://www.google.com">1</td>
        <td colspan="3"><a href="https://www.google.com">2</td>
        <td colspan="2">2</td>
      <tr>
        <td colspan="3"><a href="https://www.google.com">3</td>
        <td colspan="3"><a href="https://www.google.com">5</td>
        <td colspan="2">2</td>
      </tr>
      <tr>
        <td>1</td>
        <td colspan="2">2</td>
        <td colspan="2">2</td>
      </tr>
    </table>
  ')

  table <- extract_table(html, link_columns = TRUE)

  expect_equal(ncol(table), 5)
})

test_that("test link_columns with pretty table", {
  html <- httr::GET("http://www.olympedia.org/results/354290") |>
    httr::content(as = "text") |>
    rvest::read_html()

  table <- extract_table(html, link_columns = TRUE)

  expect_equal(ncol(table), 13)
})
