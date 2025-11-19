# Plot a flowchart of the sample size flow in a `match_time` object

This function is an attempt at creating a CONSORT style flowchart for
the sequential time-dependent matching methods implemented in this
package. It shows the number of potential cases and potential controls
as filtered throughout the matching process. Some of the included
numbers may seem confusing at first glance. See details for more
information.

## Usage

``` r
plot_flowchart(
  x,
  digits=2,
  n_fontface="italic",
  inclusion_text=NULL,
  remove_0_lines=TRUE,
  remove_0_boxes=FALSE,
  perc_inclusion=TRUE,
  perc_inclusion_total=FALSE,
  perc_other=FALSE,
  perc_type="all",
  number_format=format,
  box_main_style="n_last",
  box_main_text=list(),
  box_main_halign=0.5,
  box_main_nudge_x=0,
  box_main_nudge_y=0,
  box_main_padding=ggplot2::unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
  box_main_margin=ggplot2::unit(c(0, 0, 0, 0), "pt"),
  box_main_r=ggplot2::unit(5.5, "pt"),
  box_main_width=ggplot2::unit(2, "inch"),
  box_main_minwidth=NULL,
  box_main_maxwidth=NULL,
  box_main_height=NULL,
  box_main_minheight=NULL,
  box_main_maxheight=NULL,
  box_main_colour="black",
  box_main_fill="lightblue",
  box_sec_text=list(),
  box_sec_halign=0,
  box_sec_nudge_x=box_main_nudge_x,
  box_sec_nudge_y=box_main_nudge_y,
  box_sec_padding=box_main_padding,
  box_sec_margin=box_main_margin,
  box_sec_r=box_main_r,
  box_sec_width=box_main_width,
  box_sec_minwidth=box_main_minwidth,
  box_sec_maxwidth=box_main_maxwidth,
  box_sec_height=box_main_height,
  box_sec_minheight=box_main_minheight,
  box_sec_maxheight=box_main_maxheight,
  box_sec_colour=box_main_colour,
  box_sec_fill="cornsilk",
  box_sec_xshift=0,
  box_sec_yshift=0,
  line_main_colour="black",
  line_main_linetype="solid",
  line_main_linewidth=0.5,
  line_sec_colour=line_main_colour,
  line_sec_linetype="dashed",
  line_sec_linewidth=line_main_linewidth,
  line_sec_yshift=box_sec_yshift,
  arrow=TRUE,
  arrow_type="closed",
  arrow_angle=30,
  arrow_vjust=-0.4,
  arrow_size=0.1,
  xlim=c(-20, 20),
  ylim=c(-6, 11),
  ...
)
```

## Arguments

- x:

  A `match_time` object created using the
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  function.

- digits:

  A single integer, specifying the number of digits the percentages
  should be rounded to.

- n_fontface:

  Either `TRUE` or `FALSE`, specifying whether the "n = X" texts in the
  main boxes should be `"normal"`, `"italic"`, `"bold"` or
  `"bolditalic"`.

- inclusion_text:

  A named list specifying the text that should be used for the exclusion
  criteria reasons. This list should contain only names that have been
  used as inclusion criteria in the `inclusion` argument of the
  [`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md)
  function.

- remove_0_lines:

  Either `TRUE` or `FALSE`, specifying whether lines about 0 occurrences
  should be removed from the secondary boxes.

- remove_0_boxes:

  Either `TRUE` or `FALSE`, specifying whether boxes that are basically
  empty because of 0 occurrences should be removed from the plot.

- perc_inclusion:

  Either `TRUE` or `FALSE`, specifying whether percentages should be
  shown for the individual exclusion criteria reasons. What these
  percentages use as reference can be controlled using the `perc_type`
  argument.

- perc_inclusion_total:

  Either `TRUE` or `FALSE`, specifying whether percentages should be
  shown for total numbers of individuals excluded because of the
  inclusion criteria.

- perc_other:

  Either `TRUE` or `FALSE`, specifying whether percentages should be
  shown for all numbers in the side-boxes that are not related to the
  inclusion criteria.

- perc_type:

  A single character string controlling what the calculated percentages
  for the inclusion criteria items should be in reference to. If
  `"all"`, the percentages will be in reference to the previous main
  box, e.g. the percentage of people not fulfilling the inclusion
  criteria X out of all people it was applied to. If `"excluded"` the
  percentage will be in reference to the number of individuals excluded
  due to the inclusion criteria instead.

- number_format:

  A function that can be called on a vector of numbers and outputs those
  numbers as strings in some form. This can be used to ensure that the
  same number of digits is shown regardless of number, or to break up
  large numbers with commas or points. Defaults to the
  [`format`](https://rdrr.io/r/base/format.html) function. Further
  arguments to this function may be passed through the `...` syntax.

- box_main_style:

  A single character string, controlling which "style" the text in the
  main boxes should have. Should be either `"n_first"`, in which case
  the numbers appear before the text, or `"n_last"` in which the numbers
  appear after the text (default).

- box_main_text:

  A named list of single character strings, specifying alternative text
  to the defaults printed in the main boxes. The names of the list
  should be one of `"box1"` (defining the text in the first box from the
  top), `"box2l"` (defining the text in the second box from the top on
  the left), `"box2r"` (defining the text in the second box from the top
  on the right), `"box3l"` (defining the text in the third box from the
  top on the left), `"box3r"` (defining the text in the third box from
  the top on the right), `"box4l"` (fourth box on the left), `"box4r"`
  (fourth box on the right). Set to
  [`list()`](https://rdrr.io/r/base/list.html) to not change the text
  (default).

- box_main_halign:

  Passed to the `halign` aesthetic in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_nudge_x:

  Passed to the `nudge_x` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_nudge_y:

  Passed to the `nudge_y` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_padding:

  Passed to the `box.padding` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_margin:

  Passed to the `box.margin` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_r:

  Passed to the `box.r` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_width:

  Passed to the `width` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_minwidth:

  Passed to the `minwidth` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_maxwidth:

  Passed to the `maxwidth` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_height:

  Passed to the `height` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_minheight:

  Passed to the `minheight` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_maxheight:

  Passed to the `maxheight` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_colour:

  Passed to the `box.colour` aesthetic in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_main_fill:

  Passed to the `fill` aesthetic in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the main boxes.

- box_sec_text:

  A named list of single character strings, specifying alternative text
  to the defaults printed in the secondary boxes. The names of the list
  should be one of `"box1l"` (defining the text in the first secondary
  box from the top on the left side), `"box1r"` (defining the text in
  the first secondary box from the top on the right), `"box2l1"`
  (defining the text for the unmatched in the second secondary box from
  the top on the left side), `"box2l2"` (defining the text for the
  inclusion criteria in the second secondary box from the top on the
  left side), `"box2r1"` (defining the text on how many controls were
  not used in the bottom right box), `"box2r2"` (defining the text on
  how many controls were used more than once in the bottom right box).
  Set to [`list()`](https://rdrr.io/r/base/list.html) to not change the
  text (default).

- box_sec_halign:

  Passed to the `halign` aesthetic in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_nudge_x:

  Passed to the `nudge_x` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_nudge_y:

  Passed to the `nudge_y` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_padding:

  Passed to the `box.padding` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_margin:

  Passed to the `box.margin` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_r:

  Passed to the `box.r` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_width:

  Passed to the `width` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_minwidth:

  Passed to the `minwidth` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_maxwidth:

  Passed to the `maxwidth` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_height:

  Passed to the `height` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_minheight:

  Passed to the `minheight` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_maxheight:

  Passed to the `maxheight` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_colour:

  Passed to the `box.colour` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_fill:

  Passed to the `fill` argument in the
  [`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
  function used for the secondary boxes.

- box_sec_xshift:

  Either a single value or a numeric vector of length 4, specifying the
  amount the secondary boxes should be shifted to the left (negative
  values) or to the right (positive values) in regards to the x-axis.
  The connecting lines are automatically extended or shortened based on
  these values as well. If a single number is supplied, all boxes are
  shifted in the same direction equally (similar to when using
  `box_sec_nudgex`). By supplying four values, the boxes can be shifted
  individually.

- box_sec_yshift:

  Same as `box_sec_xshift` but for the y-axis. Note that by default, the
  connecting lines are shifted equally to keep the lines in the middle
  of the boxes. If this is not desired, users may additionally use the
  `line_sec_yshift` argument (set to 0 to keep the lines in their
  default place).

- line_main_colour:

  The colour of the main lines connecting the main boxes.

- line_main_linetype:

  The type of the lines connecting the main boxes.

- line_main_linewidth:

  The width of the lines connecting the main boxes.

- line_sec_colour:

  The color of the lines connecting the secondary boxes to the main
  lines.

- line_sec_linetype:

  The type of the lines connecting the secondary boxes to the main
  lines.

- line_sec_linewidth:

  The width of the lines connecting the secondary boxes to the main
  lines.

- line_sec_yshift:

  Either a single value or a numeric vector of length 4, specifying the
  amount the secondary lines should be shifted up (positive values) or
  down (negative values) in regards to the y-axis. If a single number is
  supplied, all lines are shifted equally. If four values are supplied,
  the lines are shifted separately by each other. Defaults to the values
  supplied to `box_sec_yshift`.

- arrow:

  Either `TRUE` or `FALSE`, specifying whether to draw arrows on the
  main lines.

- arrow_type:

  The angle of the arrow head in degrees (smaller numbers produce
  narrower, pointier arrows). Essentially describes the width of the
  arrow head. Ignored if `arrow=FALSE`.

- arrow_angle:

  The type of the arrow heads drawn, ignored if `arrow=FALSE`.

- arrow_vjust:

  A single number that may be used to shift the arrow heads up or down.
  This is probably needed in order to correctly place the arrows. If
  this does not work, users are encouraged to set `arrow=FALSE` and add
  their own arrows using the
  [`geom_segment`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
  function.

- arrow_size:

  The size of the drawn arrow heads, ignored if `arrow=FALSE`.

- xlim:

  The x limits of the plot. Should usually stay at the values given
  here, because the positions of the boxes are hard-coded. They can,
  however, be changed to allow drawing more boxes etc. To change the
  size of the boxes, arguments starting with `box_main_` or `box_sec_`
  should be more useful.

- ylim:

  The y limits of the plot. See argument `xlim`.

- ...:

  Further arguments passed to the `number_format` function.

## Details

Below are some FAQ about this type of flowchart and the function itself.

***Why are the shown values so weird?***

Multiple of the numbers in the resulting flowchart may seem confusing at
first. First of all, the number of potential cases + the number of
potential controls is (at least at first) usually much larger than the
total amount of data available. The reason for this is that all
individuals that do not get treated at \\t = 0\\ can, theoretically, be
used as control before they receive the treatment.

Similarly, sometimes some people do not meet multiple inclusion criteria
at the same time, resulting in them being counted more than once in the
respective list of reasons for inclusion. The percentages in parenthesis
(if shown) are therefore in respect to the total number of potential
controls / cases still available at time of exclusion, not to the total
number of excluded individuals.

Finally, the resulting matched dataset (matched cases + matched
controls) may be larger than the total input data, even if
`replace_over_t=FALSE` and `replace_at_t=FALSE`. This is again because,
unless `replace_cases=FALSE` is used, individuals used as controls may
later become cases and are thus included twice in the output data. To
get the number of distinct individuals in the resulting data instead,
one simply needs to call `length(unique(data$id))` on the final matched
dataset.

***How do I make all the text fit on a page?***

Usually, the resulting plot will look very poorly in the lower right
corner of an interactive R studio session. Only when clicking the "zoom"
button, a much better version will appear. This is because the plot is
usually way too big to fit in the corner. When saving to a file it
therefore usually makes sense to make the `width` and `height`
parameters rather large to get the same effect. Users are encouraged to
play around with these parameters. Additional optimisations can be done
through the various arguments relating to the boxes.

***Can I add more boxes?***

When using this function for a publication, it might make sense to add
more boxes below the plot with additional information. This may be done
using regular ggplot2 syntax. The boxes can be created using the
[`geom_textbox`](https://wilkelab.org/ggtext/reference/geom_textbox.html)
function from the ggtext package. Lines and arrows are drawn simply
using the
[`geom_segment`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
function.

***Why are the arrows placed so poorly?***

Because the actual width of the boxes is dependent on the size of the
plot and internally the boxes and lines are independent of one another,
there is no simple way to make the arrows fit right on top of the boxes
naturally. Users can usually create the desired effect by changing the
`arrow_hjust` argument through trial and error. If this fails, users may
set `arrow=FALSE` and add their own arrows using the
[`geom_segment`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
function.

## Value

Returns a standard `ggplot2` object that can be saved with a
[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html) call.

## Author

Robin Denz

## See also

[`match_time`](https://robindenz1.github.io/MatchTime/reference/match_time.md),
[`plot.match_time`](https://robindenz1.github.io/MatchTime/reference/plot.match_time.md)

## Examples

``` r
library(data.table)
library(MatchTime)

set.seed(12341234)

if (requireNamespace("survival") & requireNamespace("ggplot2") &
    requireNamespace("ggplot2") & requireNamespace("ggtext")) {

library(ggtext)
library(ggplot2)
library(survival)

# load heart dataset from survival package
data("heart", package="survival")

# add some random variables that will be used as inclusion criteria
# (in reality you would already have them)
heart$A <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.9, 0.1))
heart$B <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.9, 0.1))
heart$C <- sample(c(TRUE, FALSE), size=nrow(heart), replace=TRUE,
                  prob=c(0.9, 0.1))

# perform some time-dependent matching with inclusion criteria
m_objs <- match_time(transplant ~ surgery, data=heart, id="id",
                     match_method="fast_exact", method="brsm",
                     replace_over_t=TRUE, ratio=1,
                     inclusion=c("A", "B", "C"))

# plot the flowchart
plot_flowchart(m_objs)
}
#> Loading required namespace: ggtext
```
