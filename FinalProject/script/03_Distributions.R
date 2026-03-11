## Evaluate Data

## KS tests

yrs <- unique(penob_events$y_leave)
yrs


for (i in 1:length(yrs)) {
  yr <- yrs[i]
  yr.rows <- penob_events[penob_events[,"y_leave"] == yr, 'd_leave']
    assign(paste0("penob.d.", yr), yr.rows$d_leave)
}

ks.test(penob.d.2016, penob.d.2018, alternative = "two.sided")

