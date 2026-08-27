##
# Objective of this script is to store conceptual groups and names so that analysis.qmd
# can consult to know which variables belong in which groups, and use groups to generate a bunch of regressions
# in an iterative, systematic way (lots of loops) rather than running a million regressions seperately
##

## Independent Variables: Impact, Incarc, Knowincarc, Vict

impact_vars<- c("impact", "incarc", "knowincarc", "vict")


impact_labels <- c(
  impact = "Any Impact",
  incarc = "Direct",
  knowincarc = "Proximal",
  vict = "Victim"
)


## Outcome A: Policy Questions

q25_help_vars <- c("25_2_rev", "q25_3_rev","q25_4_rev","q25_6_rev")

policy_vars <- c("prisonhelp", "prisonpen", "incarc_voting_items",
  "post_felony_vote_rev_1_to_4", "jail_access_rev_1_to_4", "q25_2_rev", "q25_3_rev", "q25_4_rev", "q25_6_rev",
  "prisonpen_1", "prisonpen_5" 
)

policy_map <- tibble::tribble(
  ~var,          ~label,                                              ~section,
  "prisonhelp",  "Help-oriented policy index",                        "Help",
  "post_felony_vote_rev_1_to_4",   "Voting rights after serving felony sentences",      "Help",
  "jail_access_rev_1_to_4",   "Ensuring eligible incarcerated people can vote",    "Help",
  "q25_2_rev",   "Minimum wage for prison labor",                     "Help",
  "q25_3_rev",   "Free calls with family members",                    "Help",
  "q25_4_rev",   "Funding GED and college courses in prisons",        "Help",
  "q25_6_rev",   "Sentencing alternatives for parents of young children", "Help",
  "prisonpen",   "Punitive policy index",                             "Punish",
  "prisonpen_1", "Death penalty for people convicted of murder",      "Punish",
  "prisonpen_5", "Life without parole sentences",                     "Punish"
)


# check where policy_map is used, remember go back to making sure variable prep correctly sets 
# up so that I can group all the prison help items and then separately the two election 


##

police_items_1_to_5 <- c(
  "q15_1_1_to_5",
  "q15_2_1_to_5",
  "q15_3_1_to_5",
  "q15_4_1_to_5",
  "q15_5_1_to_5"
)

police_items_labels <- c(
  q15_1_1_to_5 = "Local Police",
  q15_2_1_to_5 = "No Strangle",
  q15_3_1_to_5 = "Police Tracker",
  q15_4_1_to_5 = "Police Training",
  q15_5_1_to_5 = "Charge Police"
)

police_map <- tibble::tribble(
  ~var,          ~label,                                              ~section,
  "q15_1_1_to_5",  "Local Police",                              "Misc. Police",
  "q15_2_1_to_5",   "No Strangle",                          "Police Violence",
  "q15_3_1_to_5",   "Police Tracker",                          "Police Police",
  "q15_4_1_to_5",   "Police Training",                       "Police Violence",
  "q15_5_1_to_5",   "Charge Police",                           "Police Police",
)


##
## Outcome B: Deservingness

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

#Gov pays attention to preferences? 1 not much, 2 some, 3 a lot
political_behavior_vars <- c("gov_attn", "vote2020", "willvote")

political_behavior_labels <- c(
  gov_attn = "Government pays attention",
  vote2020 = "Voted in 2020",
  willvote = "Plans to vote"
)

## CONTROLS

# race vars used in basic impact table

race_vars <- c("White", "Black", "Hispanic", "OtherRace")

race_labels <- c(
  White = "White",
  Black = "Black",
  Hispanic = "Hispanic",
  OtherRace = "Other Race"
)

##

demo_controls <- c(
  "Black", "Hispanic", "OtherRace", "gender"
)

demo_controls_labels <- c(
  Black = "Black",
  Hispanic = "Hispanic", 
  OtherRace = "Other Race", 
  gender = "Male"
)

party_ideo_controls <- c(
  "Republican",
  "Democrat",
  "fiscal_ideology",
  "social_ideology"
)

party_ideo_controls_labels <- c(
  Republican = "Republican",
  Democrat = "Democrat",
  fiscal_ideology = "Fiscal ideology",
  social_ideology = "Social ideology"
)

#below is missing policy disposition index bc problems so deprioritized

desor_core <- c("desor_core")

desor_core_label <- c(
  desor_core = "Deservingness orientation"
)

#FIRE controls 
fire_controls <- c(
  "fire_rare",
  "fire_privilege",
  "fire_angry",
  "fire_fear"
)

fire_controls_labels <- c(
  fire_rare = "Racism rare",
  fire_privilege = "White privilege",
  fire_angry = "Angry about racism",
  fire_fear = "Fear other races"
)

all_attitudinal_controls <- c(
  party_ideo_controls,
  desor_core,
  fire_controls
)
