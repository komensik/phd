deserving_vars <- c("d_incppl_pris_abc", "d_crim_abc", "d_incwomen_abc")

targets <- c(
  d_incppl_pris_abc = "Incarcerated people",
  d_crim_abc = "Criminals",
  d_incwomen_abc = "Incarcerated women"
)

groups <- list(
  impact = "Impacted",
  incarc = "Ever incarcerated",
  knowincarc = "Knows incarcerated",
  vict = "Victim"
)