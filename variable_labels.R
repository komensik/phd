
impact_vars<- c("impact", "incarc", "knowincarc", "vict")


impact_labels <- c(
  impact = "Any Impact",
  incarc = "Direct",
  knowincarc = "Proximal",
  vict = "Victim"
)

##

deserving_vars <- c("d_incppl_pris_abc", "d_crim_abc", "d_incwomen_abc")

targets <- c(
  d_incppl_pris_abc = "Incarcerated people",
  d_crim_abc = "Criminals",
  d_incwomen_abc = "Incarcerated women"
)

##

all_deserving_vars <- c(
  "d_black_afam_abc", "d_native_ind_abc", "d_asianam_abc", "d_latino_his_abc",
  "d_whiteam_abc", "d_mideastam_abc", "d_incwomen_abc", "d_collegewom_abc",
  "d_enviro_abc", "d_feminists_abc", "d_proabort_choice_abc", "d_abortprov_abc",
  "d_welfare_tanf_abc", "d_fstamps_snap_abc", "d_unions_abc", "d_medicaid_abc",
  "d_medicare_abc", "d_unemployed_abc", "d_police_abc", "d_poorfam_abc",
  "d_wealthy_abc", "d_teachers_abc", "d_sci_experts_abc", "d_child_abc",
  "d_crim_abc", "d_incppl_pris_abc", "d_homeless_abc", "d_immigrants_abc",
  "d_unauth_undoc_abc", "d_gunowners_abc", "d_voters_abc", "d_lgbt_abc",
  "d_trans_abc", "d_transkids_abc", "d_gunmans_abc", "d_ice_abc",
  "d_socmed_abc", "d_nra_abc", "d_bigbank_abc", "d_bigcorp_abc",
  "d_uni_abc", "d_comcoll_abc", "d_noncit_parent_a", "d_incparents_a",
  "d_hsteen_a", "d_athiest_a", "d_cath_a", "d_evan_a", "d_jews_a", "d_pal_a",
  "d_muslim_a", "d_blackwom_a", "d_blackmen_a", "d_lds_a", "d_longcov_a",
  "d_exon_a", "d_weed_a", "d_opioid_a", "d_whitecol_a", "d_tech_a",
  "d_smokers_a", "d_genx_a", "d_boomer_a", "d_mill_a", "d_genz_a",
  "d_metoo_b", "d_christnat_b", "d_ruralam_b", "d_urbanam_b", "d_suburbam_b",
  "d_billion_b", "d_blm_b", "d_maga_b", "d_progs_b", "d_soc_b",
  "d_proudboys_b", "d_attnys_b", "d_military_b", "d_vets_b", "d_solds_b",
  "d_essentials_b", "d_nurses_b", "d_drs_b", "d_dems_b", "d_reps_b",
  "d_media_b", "d_bluecol_c", "d_working_c", "d_midclass_c", "d_migrantwork_c",
  "d_smallbiz_c", "d_taxpayers_c", "d_elderly_c", "d_moms_c", "d_retires_c",
  "d_sexoffenders_c", "d_sexhar_vict_c", "d_sexharrasser_c", "d_formerlyinc_c",
  "d_homeown_c", "d_regufee_c", "d_gunvi_vict_c", "d_polbrut_vict_c",
  "d_unins_c", "d_mentill_c", "d_aids_c", "d_gay_c", "d_lesbians_c"
)

##

policy_vars <- c(
  "prisonhelp", "prisonpen",
  "q23_2_rev", "q23_7_rev", "q25_2_rev", "q25_3_rev", "q25_4_rev", "q25_6_rev",
  "prisonpen_1", "prisonpen_5"
)

policy_map <- tibble::tribble(
  ~var,          ~label,                                              ~section,
  "prisonhelp",  "Help-oriented policy index",                        "Help",
  "q23_2_rev",   "Voting rights after serving felony sentences",      "Help",
  "q23_7_rev",   "Ensuring eligible incarcerated people can vote",    "Help",
  "q25_2_rev",   "Minimum wage for prison labor",                     "Help",
  "q25_3_rev",   "Free calls with family members",                    "Help",
  "q25_4_rev",   "Funding GED and college courses in prisons",        "Help",
  "q25_6_rev",   "Sentencing alternatives for parents of young children", "Help",
  "prisonpen",   "Punitive policy index",                             "Punish",
  "prisonpen_1", "Death penalty for people convicted of murder",      "Punish",
  "prisonpen_5", "Life without parole sentences",                     "Punish"
)

##