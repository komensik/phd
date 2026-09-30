# Variable Groups
# as of Sept 30 moore for reminding me how I have stored/grouped things


deserving_block_drop <- c(
  # Block A
  "d_hsteen_a",
  "d_athiest_a",
  "d_cath_a",
  "d_evan_a",
  "d_jews_a",
  "d_pal_a",
  "d_muslim_a",
  "d_blackwom_a",
  "d_blackmen_a",
  "d_lds_a",
  "d_longcov_a",
  "d_whitecol_a",
  "d_tech_a",
  "d_smokers_a",
  "d_genx_a",
  "d_boomer_a",
  "d_mill_a",
  "d_genz_a",
  
  # Block B
  "d_metoo_b",
  "d_christnat_b",
  "d_ruralam_b",
  "d_urbanam_b",
  "d_suburbam_b",
  "d_billion_b",
  "d_blm_b",
  "d_maga_b",
  "d_progs_b",
  "d_soc_b",
  "d_proudboys_b",
  "d_attnys_b",
  "d_military_b",
  "d_vets_b",
  "d_solds_b",
  "d_essentials_b",
  "d_nurses_b",
  "d_drs_b",
  "d_dems_b",
  "d_reps_b",
  "d_media_b",
  
  # Block C
  "d_bluecol_c",
  "d_working_c",
  "d_midclass_c",
  "d_migrantwork_c",
  "d_smallbiz_c",
  "d_taxpayers_c",
  "d_elderly_c",
  "d_moms_c",
  "d_retires_c",
  "d_homeown_c",
  "d_regufee_c",
  "d_unins_c",
  "d_mentill_c",
  "d_aids_c",
  "d_gay_c",
  "d_lesbians_c"
)

```

```{r}
# ------------------------------------------------------------
# 3. Abortion, Period Products, Sexism
# ------------------------------------------------------------

abortion_drop <- c(
  "abortion_op",
  "abortion_death",
  "abortion_health",
  "abortion_rape",
  "abortion_defect",
  "abortion_miscar",
  "abortion_debt",
  "abortion_violence",
  "abortion_incarc",
  "abortion_teen",
  "abortion_want",
  "abortion_ability",
  "aborted",
  "knowaborted",
  "noaborted",
  "ration_period",
  "women_power",
  "women_leash",
  "women_xagg",
  "feminist"
)


# ------------------------------------------------------------
# 4. Inequality policy items
# ------------------------------------------------------------

inequality_drop <- c(
  "reduce_ineq_minwage",
  "reduce_ineq_studdebt",
  "reduce_ineq_taxrich",
  "reduce_ineq_moremedcare",
  "reduce_ineq_lessmig",
  "reduce_ineq_poorben",
  "reduce_ineq_morecoll"
)


# ------------------------------------------------------------
# 5. Guns
# ------------------------------------------------------------

guns_drop <- c(
  "gun_sales",
  "guns_1",
  "guns_2",
  "guns_3",
  "guns_4",
  "guns_5",
  "guns_6",
  "livew_guns"
)


# ------------------------------------------------------------
# 6. LGBT policy
# ------------------------------------------------------------

lgbt_policy_drop <- c(
  "lgbt_employers",
  "lgbt_medical",
  "lgbt_business"
)


# ------------------------------------------------------------
# 7. Immigration policy
# ------------------------------------------------------------

immigration_drop <- c(
  "increase_deportations",
  "increase_border_security",
  "accept_refugees",
  "easier_family_sponsorship",
  "allow_daca_stay",
  "legal_status_undocumented"
)


# ------------------------------------------------------------
# 8. Regulation items
# ------------------------------------------------------------

regulation_drop <- c(
  "reg_banks",
  "reg_socmedia",
  "reg_six_fig",
  "reg_biz"
)


# ------------------------------------------------------------
# 9. Who respondent knows
# ------------------------------------------------------------

known_people_drop <- unique(c(
  "no_known_gay",
  "gay_friend",
  "gay_fam",
  "no_known_trans",
  "trans_friend",
  "trans_fam"
))

# ------------------------------------------------------------
# 9. Raw Vars
# ------------------------------------------------------------
raw_vars <- unique(c(
  "incarc_raw",
  "knowincarc_raw",
  "noincarc_raw",
  "vict_raw",
  "knowvict_raw"
))

# ------------------------------------------------------------
# 10. Social Media use
# ------------------------------------------------------------
sm_use <- unique(c(
  "facebook", "twitter", "youtube", "truthsocial", "reddit", "parlor1", "parlor2", "tiktok"
))

# ------------------------------------------------------------
# 10. Classic Racial Resentment (using FIRE atm)
# ------------------------------------------------------------
classic_racism <- unique(c(
  "aa_special_favors", "aa_been_cheated", "aa_try_harder", "legacy_slavery"
))

# ------------------------------------------------------------
# 11. experience items 
# ------------------------------------------------------------

#binaries

experience_bins <- unique(c(
  "on_medicaid_bin",
  "no_medicaid_bin",
  "knowmedicaid_bin",
  
  "tanf_bin",
  "notanf_bin",
  "knowtanf_bin",
  
  "snap_bin",
  "knowsnap_bin",
  "nosnap_bin",
  
  "immigrated_bin",
  "knowimmigrated_bin",
  "noimmigrated_bin",
  
  "unionmember_bin",
  "nounion_bin",
  "knowunion_bin",
  
  "aborted_bin",
  "knowaborted_bin",
  "noaborted_bin",
  
  "unemployed_bin",
  "knowunemployed_bin",
  "nounemployed_bin",
  
  "uninsured_bin",
  "knowuninsured_bin",
  "nouninsured_bin",
  
  "unable_vote_bin",
  "know_unable_vote_bin",
  "no_unable_vote_bin"
))

# know items

experience_knows <- unique(c(
  "knowmedicaid", 
  "knowtanf",
  "knowsnap",
  "knowimmigrated",
  "knowunion",
  "knowunemployed",
  "know_unable_vote"
))

# no experience
no_experience <- unique(c(
  "no_medicaid", 
  "notanf",
  "nosnap",
  "nonion",
  "nounemployed",
  "nouninsured",
  "no_unable_vote"
))