#####################################################################
# Keys (variables)
#####################################################################
# IDCNTRY   : country identifier
# IDSCHOOL  : school identifier (cluster/ICC; school-level effects)
# IDCLASS   : classroom identifier
# IDSTUD    : student identifier
# ASMMAT01  : mathematics plausible value 1
# TOTWGT    : total student weight (use if applying sampling weights)
# ITLANG_SA : test language (achievement test)
# LCID_SA   : locale ID (local questionnaire/test version)
#
# Student-level process summaries (from ASA/BSA)
# Resume_F  : mean number of revisits to math items (variables matching ^M.*_F$),
#             with missing codes (9999999999, 9999999996) recoded to NA
# Resume_S  : mean response time/on-screen time for math items (variables matching ^M.*_S$),
#             with the same missing-code handling
#
# Non-cognitive scales (from ASG/BSG; SCL)
# ASBGICM   : Instructional Clarity in Math
# ASBGSSB   : School Belonging
# ASBGSB    : Student Bullying
# ASBGSLM   : Like Learning Math
# ASBGSCM   : Confidence in Math

############################################################
### Libraries
############################################################

setwd("~/scr")

library(haven)
library(dplyr)
library(cdmTools)
library(ggplot2)
library(forcats)
library(tidyr)
library(ggh4x)
library(tidytext)
library(patchwork)
library(rnaturalearth)
library(sf)
library(purrr)
library(lme4)
library(broom.mixed)
library(labelled)
library(tibble)


############################################################
#### Reads one country’s eTIMSS SPSS files (A=G4 or B=G8), 
##   builds student-level process summaries (mean of M*_F and M*_S 
## after recoding missing to NA), and merges them with non-cognitive scales by student/school IDs.
###########################################################


# Grage 4

timss_one_country_4 <- function(cntry3,
                              dir_sav,
                              noncog_vars = c(
                                "ASBGICM","ASBGSSB","ASBGSB","ASBGSLM","ASBGSCM","ASBGSVM"
                              ),
                              id_vars = c("IDCNTRY","IDSCHOOL","IDCLASS","IDSTUD"),
                              pv_math = "ASMMAT01",
                              wgt = "TOTWGT",
                              miss_codes = c(9999999999, 9999999996)) {

  cntry3 <- toupper(cntry3)

  # --- 1) listar archivos
  files <- list.files(dir_sav, pattern = "\\.sav$", full.names = TRUE, ignore.case = TRUE)

  # --- 2) busca BSA y BSG del país 
  bsa_pat <- paste0("^asa", tolower(cntry3), ".*\\.sav$")  
  bsg_pat <- paste0("^asg", tolower(cntry3), ".*\\.sav$")  

  bsa_file <- files[grepl(bsa_pat, tolower(basename(files)))]
  bsg_file <- files[grepl(bsg_pat, tolower(basename(files)))]

  if (length(bsa_file) != 1) stop("ASA: esperaba 1 archivo y encontré ", length(bsa_file), ":\n", paste(basename(bsa_file), collapse="\n"))
  if (length(bsg_file) != 1) stop("ASG: esperaba 1 archivo y encontré ", length(bsg_file), ":\n", paste(basename(bsg_file), collapse="\n"))

  # --- 3) lee
  BSA <- read_sav(bsa_file)
  BSG <- read_sav(bsg_file)

  # --- 4) Construye Resume_F y Resume_S SOLO para math
  f_vars <- names(BSA)[grepl("^M.*_F$", names(BSA))]
  s_vars <- names(BSA)[grepl("^M.*_S$", names(BSA))]

  clean_proc <- function(x) {
    x <- as.numeric(x)
    x[x %in% miss_codes] <- NA
    x
  }

  BSA2 <- BSA %>%
    mutate(across(all_of(c(f_vars, s_vars)), clean_proc)) %>%
    mutate(
      Resume_F = if (length(f_vars) == 0) NA_real_ else rowMeans(across(all_of(f_vars)), na.rm = TRUE),
      Resume_S = if (length(s_vars) == 0) NA_real_ else rowMeans(across(all_of(s_vars)), na.rm = TRUE)
    ) %>%
    mutate(
      Resume_F = ifelse(is.nan(Resume_F), NA_real_, Resume_F),
      Resume_S = ifelse(is.nan(Resume_S), NA_real_, Resume_S)
    ) %>%
    select(any_of(c(id_vars, pv_math, wgt, "ITLANG_SA","LCID_SA")), Resume_F, Resume_S)

  # --- 5) BSG: seleccionar no-cognitivas + ids
  BSG2 <- BSG %>%
    select(any_of(c(id_vars, noncog_vars)))

  # --- 6) merge final
  out <- BSA2 %>%
    left_join(BSG2, by = intersect(names(BSA2), names(BSG2)))

  out
}


# Ejemplo hungary:


df_hun <- timss_one_country("hun", dir_sav = "~/T19_G8_SPSS_Data/eTPSI")


############################################################################
### List of countries of eTIMS 4
############################################################################

path <- "~/T19_G4_SPSS Data/eTPSI"

fn <- list.files(path, pattern = "\\.sav$", full.names = FALSE)

xxx <- tolower(substr(fn, 4, 6))

xxx_unique_4 <- sort(unique(xxx))

xxx_unique_4

tbl_complete_4 = data.frame()
for(kk in xxx_unique_4){
  df_ttmp <- timss_one_country_4(kk, dir_sav = "~/T19_G4_SPSS Data/eTPSI")
  tbl_complete_4 = rbind(tbl_complete_4, df_ttmp)
}


lis_unique_country_4 = unique(tbl_complete_4[, c('IDCNTRY', 'LCID_SA')])
print(lis_unique_country_4, n = 52)

#   IDCNTRY   LCID_SA                             
#   <dbl+lbl> <dbl+lbl>                           
# 1   7842    14342 [English (U.A.E. Abu Dhabi)]  
# 2   7842    14341 [Arabic (U.A.E. Abu Dhabi)]   
# 3   7841    14343 [English (U.A.E. Dubai)]      
# 4   7841    14338 [Arabic (U.A.E. Dubai)]       
# 5    784    14337 [Arabic (U.A.E.)]             
# 6    784    20573 [English (U.A.E.)]            
# 7     40     3079 [German (Austria)]            
# 8    124     4105 [English (Canada)]            
# 9    124     3084 [French (Canada)]             
#10    152    13322 [Spanish (Chile)]             
#11   9132    41052 [English (Canada - Ontario)]  
#12   9132    30842 [French (Canada - Ontario)]   
#13   9133    30848 [French (Canada - Quebec)]    
#14   9133    41058 [English (Canada - Quebec)]   
#15    203     1029 [Czech (Czech Republic)]      
#16    276     1031 [German (Germany)]            
#17    208     1030 [Danish (Denmark)]            
#18 724005    30821 [Spanish (Spain, Madrid)]     
#19    926     2057 [English (United Kingdom)]    
#20    724     3082 [Spanish (Spain)]             
#21    724    10273 [Catalan (Spain)]             
#22    724    10271 [Valencian (Spain)]           
#23    724    10691 [Basque (Spain)]              
#24    246     1035 [Finnish (Finland)]           
#25    246     2077 [Swedish (Finland)]           
#26    250     1036 [French (France)]             
#27    268     1079 [Georgian (Georgia)]          
#28    344     3076 [Chinese (Hong Kong SAR)]     
#29    344    15369 [English (Hong Kong SAR)]     
#30    191     1050 [Croatian (Croatia)]          
#31    348     1038 [Hungarian (Hungary)]         
#32    380     1040 [Italian (Italy)]             
#33    410     1042 [Korean (Korea)]              
#34    440    10451 [Polish (Lithania)]           
#35    440     1063 [Lithuanian (Lithuania)]      
#36    440    10621 [Russian (Lithuania)]         
#37    470    10821 [English (Malta)]             
#38    528     1043 [Dutch (Netherlands)]         
#39    578     1044 [Norwegian (Bokmål) (Norway)] 
#40    578     2068 [Norwegian (Nynorsk) (Norway)]
#41    620     2070 [Portuguese (Portugal)]       
#42    634    16385 [Arabic (Qatar)]              
#43    634    20577 [English (Qatar)]             
#44 643001    10491 [Russian (Moscow)]            
#45    643     1049 [Russian (Russia)]            
#46    702    18441 [English (Singapore)]         
#47    703    10382 [Hungarian (Slovak Republic)] 
#48    703     1051 [Slovak (Slovak Republic)]    
#49    752     1053 [Swedish (Sweden)]            
#50    792     1055 [Turkish (Turkey)]            
#51    158     1028 [Chinese (Taiwan)]            
#52    840     1033 [English (United States)]     

########################################################################
### Selection of european countries:
########################################################################

eu_idcntry <- c(
  40,   # Austria
  191,  # Croatia
  203,  # Czech Republic
  208,  # Denmark
  246,  # Finland
  250,  # France
  268,  # Georgia
  276,  # Germany
  348,  # Hungary
  380,  # Italy
  440,  # Lithuania
  470,  # Malta
  528,  # Netherlands
  578,  # Norway
  620,  # Portugal
  643,  # Russia
  643001, # Russia - Moscow
  703,  # Slovak Republic
  724,  # Spain
  724005, # Spain - Madrid
  752,  # Sweden
  792,  # Turkey
  926   # United Kingdom
)

# ## Filter

df_eu_4 <- tbl_complete_4[tbl_complete_4$IDCNTRY %in% eu_idcntry, ]


## note:   ASBGSVM = Students Value Mathematics does not exist for fourth grade

#######################################################################
# Descritpives by country:
#######################################################################

vars <- c("ASMMAT01","Resume_F","Resume_S","ASBGICM","ASBGSSB","ASBGSB","ASBGSLM","ASBGSCM")

#X <- df_eu_4[, vars] |> 
#  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |> 
#  as.data.frame()

#pairs(X, pch = 16, cex = 0.3)

# as country explains all, better not generalize.

#######################################################################
## First graph by means of achievement
#######################################################################

df_plot <- df_eu_4 %>%
  mutate(
    y = as.numeric(ASMMAT01),
    id = as.numeric(IDCNTRY),
    country = recode(id,
      `40` = "Austria",
      `191` = "Croatia",
      `203` = "Czech Republic",
      `208` = "Denmark",
      `246` = "Finland",
      `250` = "France",
      `268` = "Georgia",
      `276` = "Germany",
      `348` = "Hungary",
      `380` = "Italy",
      `440` = "Lithuania",
      `470` = "Malta",
      `528` = "Netherlands",
      `578` = "Norway",
      `620` = "Portugal",
      `643` = "Russia",
      `643001` = "Russia",
      `703` = "Slovak Republic",
      `724` = "Spain",
      `724005` = "Spain",
      `752` = "Sweden",
      `792` = "Turkey",
      `926` = "United Kingdom",
      .default = as.character(id)
    ),
    country = fct_reorder(country, y, .fun = median, na.rm = TRUE)
  )

means <- df_plot %>%
  group_by(country) %>%
  summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop")

yr <- range(df_plot$y, na.rm = TRUE)
ymin <- floor(yr[1] / 25) * 25
ymax <- ceiling(yr[2] / 25) * 25

ggplot(df_plot, aes(x = country, y = y)) +
  geom_boxplot(outlier.alpha = 0.1) +
  geom_text(
    data = means,
    aes(x = country, y = mean_y),
    label = "*",
    size = 5
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(ymin, ymax, by = 50),
    minor_breaks = seq(ymin, ymax, by = 25)
  ) +
  labs(x = NULL, y = "Math score") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#EAF2FF", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),

    # grillas más delgadas y transparentes
    panel.grid.major = element_line(colour = alpha("grey25", 0.15), linewidth = 0.25),
    panel.grid.minor = element_line(colour = alpha("grey25", 0.05), linewidth = 0.15),

    panel.border = element_rect(colour = alpha("grey10", 0.7), fill = NA, linewidth = 0.7),

    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )+ theme(panel.grid.minor = element_blank())


#######################################################################
### Then descriptive for descriptive of log process
#######################################################################


wmean <- function(x, w) sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)

means_long <- df_eu_4 %>%
  mutate(
    id = as.numeric(IDCNTRY),
    country = recode(id,
      `40` = "Austria",
      `191` = "Croatia",
      `203` = "Czech Republic",
      `208` = "Denmark",
      `246` = "Finland",
      `250` = "France",
      `268` = "Georgia",
      `276` = "Germany",
      `348` = "Hungary",
      `380` = "Italy",
      `440` = "Lithuania",
      `470` = "Malta",
      `528` = "Netherlands",
      `578` = "Norway",
      `620` = "Portugal",
      `643` = "Russia",
      `643001` = "Russia",
      `703` = "Slovak Republic",
      `724` = "Spain",
      `724005` = "Spain",
      `752` = "Sweden",
      `792` = "Turkey",
      `926` = "United Kingdom",
      .default = as.character(id)
    ),
    w = as.numeric(TOTWGT),
    Resume_F = as.numeric(Resume_F),
    Resume_S = as.numeric(Resume_S)
  ) %>%
  group_by(country) %>%
  summarise(
    Resume_F = wmean(Resume_F, w),
    Resume_S = wmean(Resume_S, w),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Resume_F, Resume_S),
               names_to = "measure",
               values_to = "mean_value") %>%
  mutate(
    measure = factor(measure, levels = c("Resume_S", "Resume_F")),
    measure = recode(measure,
                     Resume_S = "Time spent on each item",
                     Resume_F = "Frequency of distinct visits per item")
  )


country_order <- means_long %>%
  filter(measure == "Time spent on each item") %>%
  arrange(mean_value) %>%
  pull(country)

means_long <- means_long %>%
  mutate(country = factor(country, levels = country_order))

ggplot(means_long, aes(x = mean_value, y = country)) +
  geom_point(size = 2) +
  facet_wrap(~ measure, nrow = 1, scales = "free_x") +
  labs(x = "Country mean", y = "Countries")

ggplot(means_long, aes(x = mean_value, y = country)) +
  geom_point(size = 2) +
  facet_wrap2(~ measure, nrow = 1, scales = "free_x") +
  ggh4x::facetted_pos_scales(
    x = list(
      NULL,  
      scale_x_continuous(
        limits = c(1.4, 2.6),
        breaks = seq(1.4, 2.6, by = 0.2)
      )
    )
  ) +
  labs(x = "Country mean", y = "Countries")

#######################################################################
### Descriptives of ASBGICM   ASBGSSB   ASBGSB ASBGSLM ASBGSCM Resume_F Resume_S
#######################################################################

wmean <- function(x, w) sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)

wsd <- function(x, w){
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  w <- w / sum(w)
  mu <- sum(w * x)
  sqrt(sum(w * (x - mu)^2))
}

means_long <- df_eu_4 %>%
  mutate(
    id = as.numeric(IDCNTRY),
    country = recode(id,
      `40` = "Austria",
      `191` = "Croatia",
      `203` = "Czech Republic",
      `208` = "Denmark",
      `246` = "Finland",
      `250` = "France",
      `268` = "Georgia",
      `276` = "Germany",
      `348` = "Hungary",
      `380` = "Italy",
      `440` = "Lithuania",
      `470` = "Malta",
      `528` = "Netherlands",
      `578` = "Norway",
      `620` = "Portugal",
      `643` = "Russia",
      `643001` = "Russia",
      `703` = "Slovak Republic",
      `724` = "Spain",
      `724005` = "Spain",
      `752` = "Sweden",
      `792` = "Turkey",
      `926` = "United Kingdom",
      .default = as.character(id)
    ),
    w = as.numeric(TOTWGT),
    ASBGICM = as.numeric(ASBGICM),
    ASBGSSB = as.numeric(ASBGSSB),
    ASBGSB  = as.numeric(ASBGSB),
    ASBGSLM = as.numeric(ASBGSLM),
    ASBGSCM = as.numeric(ASBGSCM)
  ) %>%
  group_by(country) %>%
  summarise(
    ASBGICM_mean = wmean(ASBGICM, w), ASBGICM_sd = wsd(ASBGICM, w),
    ASBGSSB_mean = wmean(ASBGSSB, w), ASBGSSB_sd = wsd(ASBGSSB, w),
    ASBGSB_mean  = wmean(ASBGSB,  w), ASBGSB_sd  = wsd(ASBGSB,  w),
    ASBGSLM_mean = wmean(ASBGSLM, w), ASBGSLM_sd = wsd(ASBGSLM, w),
    ASBGSCM_mean = wmean(ASBGSCM, w), ASBGSCM_sd = wsd(ASBGSCM, w),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = -country,
    names_to = c("scale", ".value"),
    names_pattern = "(ASBGICM|ASBGSSB|ASBGSB|ASBGSLM|ASBGSCM)_(mean|sd)"
  ) %>%
  rename(mean_value = mean, sd_value = sd) %>%
  mutate(
    scale = recode(scale,
      ASBGICM = "Instructional Clarity",
      ASBGSSB = "School Belonging",
      ASBGSB  = "Student Bullying",
      ASBGSLM = "Like Learning Math",
      ASBGSCM = "Confident in Math"
    ),
    scale = factor(scale, levels = c(
      "Instructional Clarity",
      "School Belonging",
      "Student Bullying",
      "Like Learning Math",
      "Confident in Math"
    ))
  )

# tabla de resultados

means_wide <- means_long %>%
  mutate(msd = sprintf("%.2f (%.2f)", mean_value, sd_value)) %>%
  select(country, scale, msd) %>%
  pivot_wider(
    names_from = scale,
    values_from = msd
  ) %>%
  arrange(country)



#######################################################
### four panel descriptie dot plot
#######################################################


scales_left  <- c("School Belonging", "Like Learning Math")
scales_right <- c("Instructional Clarity", "Confident in Math")

make_col <- function(dat, scales_vec, y_pos = c("left","right")) {
  y_pos <- match.arg(y_pos)

  d <- dat %>%
    filter(scale %in% scales_vec) %>%
    mutate(
      scale = factor(scale, levels = scales_vec),
      country_ord = tidytext::reorder_within(country, mean_value, scale, sep = "___")
    )

  p <- ggplot(d, aes(x = mean_value, y = country_ord)) +
    geom_point(size = 2) +
    facet_wrap(~ scale, ncol = 1, scales = "free_y") +
    scale_y_discrete(
      position = y_pos,
      labels = function(x) sub("___.*$", "", x),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(x = "Country mean", y = NULL) +
    theme_minimal()

  if (y_pos == "right") {
    p <- p + theme(
      axis.text.y.left  = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.text.y.right = element_text(hjust = 0, margin = margin(l = 2))  # <-- aquí      
    )
  } else {
    p <- p + theme(
      axis.text.y.right  = element_blank(),
      axis.ticks.y.right = element_blank()
    )
  }

  p
}

p_left  <- make_col(means_long, scales_left,  y_pos = "left")
p_right <- make_col(means_long, scales_right, y_pos = "right")

(p_left | p_right) + plot_annotation()


#######################################################################
### Bullying map
#######################################################################

#wmean <- function(x, w) sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)

country_key <- tibble::tribble(
  ~id,     ~country,            ~iso3,
  40,      "Austria",           "AUT",
  191,     "Croatia",           "HRV",
  203,     "Czech Republic",    "CZE",
  208,     "Denmark",           "DNK",
  246,     "Finland",           "FIN",
  250,     "France",            "FRA",
  268,     "Georgia",           "GEO",
  276,     "Germany",           "DEU",
  348,     "Hungary",           "HUN",
  380,     "Italy",             "ITA",
  440,     "Lithuania",         "LTU",
  470,     "Malta",             "MLT",
  528,     "Netherlands",       "NLD",
  578,     "Norway",            "NOR",
  620,     "Portugal",          "PRT",
  643,     "Russia",            "RUS",
  643001,  "Russia",            "RUS",
  703,     "Slovak Republic",   "SVK",
  724,     "Spain",             "ESP",
  724005,  "Spain",             "ESP",
  752,     "Sweden",            "SWE",
  792,     "Turkey",            "TUR",
  926,     "United Kingdom",    "GBR"
)

# SD ponderada
wsd <- function(x, w){
  w <- w / sum(w)
  mu <- sum(w * x)
  sqrt(sum(w * (x - mu)^2))
}

country_means <- df_eu_4 %>%
  mutate(
    w = as.numeric(TOTWGT),
    y = as.numeric(ASBGSB),
    id = as.numeric(IDCNTRY)
  ) %>%
  left_join(country_key, by = "id") %>%
  filter(!is.na(country), !is.na(iso3), !is.na(y), !is.na(w)) %>%
  group_by(country, iso3) %>%
  summarise(
    bullying_mean = wmean(y, w),
    bullying_sd   = wsd(y, w),
    .groups = "drop"
  )

country_means <- df_eu_4 %>%
  mutate(
    w = as.numeric(TOTWGT),
    y = as.numeric(ASBGSB),
    id = as.numeric(IDCNTRY)
  ) %>%
  left_join(country_key, by = "id") %>%
  filter(!is.na(country), !is.na(iso3), !is.na(y), !is.na(w)) %>%
  group_by(country, iso3) %>%
  summarise(bullying = wmean(y, w), .groups = "drop")

world <- ne_countries(scale = "medium", returnclass = "sf")

map_df <- world %>%
  left_join(country_means, by = c("adm0_a3" = "iso3")) %>%
  filter(continent == "Europe" | adm0_a3 %in% c("RUS","TUR","GEO"))

# 4) Plot
ggplot(map_df) +
  geom_sf(aes(fill = bullying), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(-12, 50), ylim = c(35, 70)) +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7") +
  labs(fill = "Student Bullying") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  ) + theme(panel.grid = element_blank())


#######################################################################
### Relationships within and between countries
#######################################################################

xvar <- "ASBGSSB" 

tmp <- df_eu_4 %>%
  transmute(
    country = haven::as_factor(IDCNTRY),
    y = as.numeric(ASMMAT01),
    x = as.numeric(.data[[xvar]]),
    w = as.numeric(TOTWGT)
  )

# Between: correlación entre medias país
between <- tmp %>%
  group_by(country) %>%
  summarise(
    y_bar = wmean(y, w),
    x_bar = wmean(x, w),
    .groups = "drop"
  )

cor_between <- cor(between$y_bar, between$x_bar, use = "complete.obs")

# Within: correlación entre desviaciones dentro de país
within <- tmp %>%
  group_by(country) %>%
  mutate(
    y_c = y - wmean(y, w),
    x_c = x - wmean(x, w)
  ) %>%
  ungroup()

cor_within <- cor(within$y_c, within$x_c, use = "complete.obs")

cor_between
cor_within

###############################################################
### Scaterplot removing the country effect
###############################################################
# En vez de graficar x vs y en bruto se grafica el x_c vs y_c, centrados por paises:
# estudiantes con X más alto que el promedio de su país tienden a tener math más alto (o más bajo) que el promedio de su país.

#wmean <- function(x, w) sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)

xvar <- "ASBGSSB"  

df_within <- df_eu_4 %>%
  transmute(
    country = haven::as_factor(IDCNTRY),
    y = as.numeric(ASMMAT01),
    x = as.numeric(.data[[xvar]]),
    w = as.numeric(TOTWGT)
  ) %>%
  group_by(country) %>%
  mutate(
    y_bar = wmean(y, w),
    x_bar = wmean(x, w),
    y_c = y - y_bar,
    x_c = x - x_bar
  ) %>%
  ungroup()

ggplot(df_within, aes(x = x_c, y = y_c)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = paste0(xvar, " (desviación del promedio del país)"),
    y = "ASMMAT01 (desviación del promedio del país)"
  )


vars <- c("BSBGICM","BSBGSSB","BSBGSB","BSBGSLM","BSBGSCM","BSBGSVM",
          "Resume_F","Resume_S", "BSMMAT01")

df_cor <- df_hun[, vars]

R <- cor(df_cor, use = "pairwise.complete.obs", method = "pearson")

R

# Histogram

hist(df_hun$BSBGSB,
     breaks = 30,
     main = "Histogram",
     xlab = "PV1")


#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
# # Models ---- research question 1
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################

########### ---------------------------
# 1) setup
########### ---------------------------
outcome   <- "ASMMAT01"
predictors <- c("ASBGICM", "ASBGSSB", "ASBGSB", "ASBGSLM", "ASBGSCM", "Resume_F", "Resume_S")
school_id <- "IDSCHOOL"
wgt       <- "TOTWGT"  

########### ---------------------------
# 2) country + (Moscow->Russia, Madrid->Spain)
########### ---------------------------
df <- df_eu_4 %>%
  mutate(
    country_raw = as.character(labelled::to_factor(IDCNTRY)),
    country = case_when(
      country_raw %in% c("Moscow", "Russian Federation (Moscow)", "Russia (Moscow)") ~ "Russian Federation",
      country_raw %in% c("Madrid", "Spain (Madrid)", "Madrid (Spain)") ~ "Spain",
      TRUE ~ country_raw
    )
  ) %>%
  mutate(across(all_of(c(outcome, predictors, wgt)), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(!is.na(country), !is.na(.data[[school_id]]), !is.na(.data[[outcome]]))

df <- df %>%
  mutate(
    country = case_when(
      country == "724005" ~ "724",
      country == "643001" ~ "643",
      TRUE ~ country
    )
  )

###### ---------------------------
# 3) function one: modelo for one predictor
####### ---------------------------
fit_one <- function(dat, x_var) {
  dat2 <- dat %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[school_id]]), !is.na(.data[[outcome]]))
  fml <- as.formula(paste0(outcome, " ~ ", x_var, " + (1|", school_id, ")"))
  ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  mod <- tryCatch(
    {
      if (!is.null(wgt)) {
        lmer(fml, data = dat2, weights = dat2[[wgt]], REML = FALSE, control = ctrl)
      } else {
        lmer(fml, data = dat2, REML = FALSE, control = ctrl)
      }
    },
    error = function(e) e
  )
  if (inherits(mod, "error")) {
    return(tibble(
      term = NA_character_, estimate = NA_real_, std.error = NA_real_,
      statistic = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
      predictor = x_var,
      n = nrow(dat2),
      n_schools = n_distinct(dat2[[school_id]]),
      error = mod$message
    ))
  }
  broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      predictor = x_var,
      n = nobs(mod),
      n_schools = lme4::ngrps(mod)[[school_id]],
      error = NA_character_
    )
}

####### ---------------------------
# 4) Run: country*predictor
####### ---------------------------
by_country <- split(df, df$country)

coeficientes <- imap_dfr(by_country, function(dat_cty, cty_name) {
  map_dfr(predictors, ~ fit_one(dat_cty, .x)) %>%
    mutate(country = cty_name, .before = 1)
})

### ##--------------------------
# 5) filter
##### ---------------------------
alpha1 <- coeficientes %>%
  filter(term %in% predictors) %>%
  arrange(country, predictor)


coeficientes %>%
  filter(effect %in% predictors) %>%
  arrange(country, predictor)

####################################################################
#### Dot points plus CI 
####################################################################

scales_left  <- c("School Belonging", "Like Learning Math")
scales_right <- c("Instructional Clarity", "Confident in Math")

scale_labels <- c(
  ASBGSSB = "School Belonging",
  ASBGSLM = "Like Learning Math",
  ASBGICM = "Instructional Clarity",
  ASBGSCM = "Confident in Math"
)


country_key <- tibble::tribble(
  ~id,     ~country,            ~iso3,
  40,      "Austria",           "AUT",
  191,     "Croatia",           "HRV",
  203,     "Czech Republic",    "CZE",
  208,     "Denmark",           "DNK",
  246,     "Finland",           "FIN",
  250,     "France",            "FRA",
  268,     "Georgia",           "GEO",
  276,     "Germany",           "DEU",
  348,     "Hungary",           "HUN",
  380,     "Italy",             "ITA",
  440,     "Lithuania",         "LTU",
  470,     "Malta",             "MLT",
  528,     "Netherlands",       "NLD",
  578,     "Norway",            "NOR",
  620,     "Portugal",          "PRT",
  643,     "Russia",            "RUS",
  643001,  "Russia",            "RUS",
  703,     "Slovak Republic",   "SVK",
  724,     "Spain",             "ESP",
  724005,  "Spain",             "ESP",
  752,     "Sweden",            "SWE",
  792,     "Turkey",            "TUR",
  926,     "United Kingdom",    "GBR"
)


betas_long <- alpha1 %>%
  filter(predictor %in% names(scale_labels), is.na(error)) %>%
  mutate(id = as.numeric(country)) %>%                         # alpha1$country = id
  left_join(country_key %>% select(id, country_name = country), by = "id") %>%
  mutate(
    country = if_else(is.na(country_name), as.character(id), country_name),
    scale = recode(predictor, !!!scale_labels),
    beta  = estimate,
    se    = std.error,
    lo_se = conf.low,
    hi_se = conf.high
  ) %>%
  select(-id, -country_name)


make_col_beta <- function(dat, scales_vec, y_pos = c("left","right")) {
  y_pos <- match.arg(y_pos)

  d <- dat %>%
    filter(scale %in% scales_vec) %>%
    mutate(
      scale = factor(scale, levels = scales_vec),
      country_ord = tidytext::reorder_within(country, beta, scale, sep = "___")
    )

  p <- ggplot(d, aes(y = country_ord)) +
    # Barras horizontales (muescas)
geom_errorbarh(aes(xmin = lo_se, xmax = hi_se),
               height = 0.18, linewidth = 0.4, color = "darkblue") +
geom_point(aes(x = beta), size = 2, color = "darkblue") +
    facet_wrap(~ scale, ncol = 1, scales = "free_y") +
    scale_y_discrete(
      position = y_pos,
      labels = function(x) sub("___.*$", "", x),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(x = expression(beta_1~"(per 1-unit increase)"), y = NULL) +
    theme_minimal()

  if (y_pos == "right") {
    p <- p + theme(
      axis.text.y.left  = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.text.y.right = element_text(hjust = 0, margin = margin(l = 2))
    )
  } else {
    p <- p + theme(
      axis.text.y.right  = element_blank(),
      axis.ticks.y.right = element_blank()
    )
  }

  p
}


p_left  <- make_col_beta(betas_long, scales_left,  y_pos = "left")
p_right <- make_col_beta(betas_long, scales_right, y_pos = "right")

(p_left | p_right) + plot_annotation()


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
#### Research question 2: Correlation matrix within - between
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################

vars <- c("ASBGICM","ASBGSSB","ASBGSB","ASBGSLM","ASBGSCM","Resume_F","Resume_S")

school_means <- df %>%
  group_by(country, IDSCHOOL) %>%
  summarise(across(all_of(vars), ~ mean(., na.rm = TRUE)), .groups = "drop")

cor_mats_between <- school_means %>%
  group_split(country) %>%
  setNames(school_means %>% distinct(country) %>% pull(country)) %>%
  map(~ cor(.x[, vars], use = "pairwise.complete.obs"))

cor_pairs_between <- imap_dfr(cor_mats_between, function(M, cty) {
  as.data.frame(as.table(M), stringsAsFactors = FALSE) %>%
    rename(var1 = Var1, var2 = Var2, r = Freq) %>%
    mutate(
      country = cty,
      var1 = as.character(var1),
      var2 = as.character(var2)
    ) %>%
    filter(var1 != var2) %>%
    mutate(
      vmin = pmin(var1, var2),
      vmax = pmax(var1, var2)
    ) %>%
    distinct(country, vmin, vmax, .keep_all = TRUE) %>%
    transmute(country, var1 = vmin, var2 = vmax, r)
})


cor_resume_between <- cor_pairs_between %>%
  filter(
    (var1 %in% c("Resume_F","Resume_S") & !(var2 %in% c("Resume_F","Resume_S"))) |
    (var2 %in% c("Resume_F","Resume_S") & !(var1 %in% c("Resume_F","Resume_S")))
  ) %>%
  mutate(
    resume = if_else(var1 %in% c("Resume_F","Resume_S"), var1, var2),
    other  = if_else(var1 %in% c("Resume_F","Resume_S"), var2, var1)
  ) %>%
  select(country, resume, other, r) %>%
  arrange(country, resume, desc(abs(r)))


cor_resume_between


summary_between <- cor_resume_between %>%
  group_by(resume, other) %>%
  summarise(
    med = median(r, na.rm = TRUE),
    q25 = quantile(r, .25, na.rm = TRUE),
    q75 = quantile(r, .75, na.rm = TRUE),
    min = min(r, na.rm = TRUE),
    max = max(r, na.rm = TRUE),
    n_pos = sum(r > 0, na.rm = TRUE),
    n_neg = sum(r < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(resume, desc(abs(med)))

summary_between

##########################################################
## Graph
##########################################################

other_labels <- c(
  ASBGSSB = "School Belonging",
  ASBGSLM = "Like Learning Math",
  ASBGICM = "Instructional Clarity",
  ASBGSCM = "Confident in Math",
  ASBGSB  = "Student Bullying"
)

# panels
scales_left  <- c("School Belonging", "Like Learning Math")
scales_right <- c("Instructional Clarity", "Confident in Math")

plot_dat <- cor_resume_between %>%
  filter(other != "ASBGSB") %>%                                  
  mutate(id = as.numeric(country)) %>%
  left_join(country_key %>% distinct(id, country_name = country), by = "id") %>%
  mutate(
    country = if_else(is.na(country_name), as.character(id), country_name),
    scale   = recode(other, !!!other_labels),
    r       = as.numeric(r),
    resume = factor(
  resume,
  levels = c("Resume_F","Resume_S"),
  labels = c("Item revisit frequency", "Response time")
)
  ) %>%
  select(country, scale, resume, r)

make_col_r <- function(dat, scales_vec, y_pos = c("left","right")) {
  y_pos <- match.arg(y_pos)


  d <- dat %>%
    filter(scale %in% scales_vec) %>%
    group_by(scale, country) %>%
    mutate(order_ref = r[resume == "Response time"][1]) %>%  ######### Ordern aqui
    ungroup() %>%
    mutate(
      scale = factor(scale, levels = scales_vec),
      country_ord = tidytext::reorder_within(country, order_ref, scale, sep = "___"),
      resume = factor(resume, levels = c("Item revisit frequency", "Response time"))
    )

  pd <- position_dodge(width = 0.5)

  p <- ggplot(d, aes(x = r, y = country_ord, shape = resume, color = resume)) +
    geom_vline(xintercept = 0, linewidth = 0.3) +
    geom_point(size = 2, position = pd) +
    facet_wrap(~ scale, ncol = 1, scales = "free_y") +
    scale_y_discrete(
      position = y_pos,
      labels = function(x) sub("___.*$", "", x),
      expand = expansion(mult = c(0.01, 0.01))
    ) +

    scale_shape_manual(
      values = c("Item revisit frequency" = 16, "Response time" = 17),
      breaks = c("Item revisit frequency", "Response time"),
      name = NULL
    ) +
    scale_color_manual(
      values = c("Item revisit frequency" = "black", "Response time" = "darkblue"),
      breaks = c("Item revisit frequency", "Response time"),
      name = NULL
    ) +
    labs(x = "Between-school correlation (r)", y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal"
    )

  if (y_pos == "right") {
    p <- p + theme(
      axis.text.y.left  = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.text.y.right = element_text(hjust = 0, margin = margin(l = 2))
    )
  } else {
    p <- p + theme(
      axis.text.y.right  = element_blank(),
      axis.ticks.y.right = element_blank()
    )
  }

  p
}


p_left  <- make_col_r(plot_dat, scales_left,  y_pos = "left")
p_right <- make_col_r(plot_dat, scales_right, y_pos = "right")


(p_left | p_right) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "top",
                                legend.direction = "horizontal"))

####################################################################
#### Coefficient maps
####################################################################


country_key <- tibble::tribble(
  ~country_id, ~country,            ~iso3,
  40,          "Austria",           "AUT",
  191,         "Croatia",           "HRV",
  203,         "Czech Republic",    "CZE",
  208,         "Denmark",           "DNK",
  246,         "Finland",           "FIN",
  250,         "France",            "FRA",
  268,         "Georgia",           "GEO",
  276,         "Germany",           "DEU",
  348,         "Hungary",           "HUN",
  380,         "Italy",             "ITA",
  440,         "Lithuania",         "LTU",
  470,         "Malta",             "MLT",
  528,         "Netherlands",       "NLD",
  578,         "Norway",            "NOR",
  620,         "Portugal",          "PRT",
  643,         "Russia",            "RUS",
  643001,      "Russia",            "RUS",  # Moscow -> Russia
  703,         "Slovak Republic",   "SVK",
  724,         "Spain",             "ESP",
  724005,      "Spain",             "ESP",  # Madrid -> Spain
  752,         "Sweden",            "SWE",
  792,         "Turkey",            "TUR",
  926,         "United Kingdom",    "GBR"
)

bullying_alpha1 <- alpha1 %>%
  filter(predictor == "ASBGSB") %>%
  mutate(country_id = as.numeric(country)) %>%
  rename(country_code = country) %>%     # <- tu código (191,203,...)
  left_join(country_key, by = "country_id") %>%
  select(country_id, country, iso3, estimate, std.error, conf.low, conf.high, n, n_schools, error) %>%
  arrange(country)

bullying_alpha1

# write.csv(bullying_alpha1, "alpha1_bullying_by_country.csv", row.names = FALSE)


world <- ne_countries(scale = "medium", returnclass = "sf")

map_df <- world %>%
  left_join(bullying_alpha1, by = c("adm0_a3" = "iso3")) %>%
  filter(continent == "Europe" | adm0_a3 %in% c("RUS","TUR","GEO"))

ggplot(map_df) +
  geom_sf(aes(fill = estimate), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(-12, 50), ylim = c(35, 70)) +
  scale_fill_gradient2(
    midpoint = 0,
    low = "#B2182B", mid = "white", high = "#2166AC",
    name = "Alpha1 (Bullying)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )



plot_mapa = function(parA, parB){


bullying_alpha1 <- alpha1 %>%
  filter(predictor == parA) %>%
  mutate(country_id = as.numeric(country)) %>%
  rename(country_code = country) %>%     # <- tu código (191,203,...)
  left_join(country_key, by = "country_id") %>%
  select(country_id, country, iso3, estimate, std.error, conf.low, conf.high, n, n_schools, error) %>%
  arrange(country)

# 2) Mapa: color = estimate (efecto del bullying)
world <- ne_countries(scale = "medium", returnclass = "sf")

map_df <- world %>%
  left_join(bullying_alpha1, by = c("adm0_a3" = "iso3")) %>%
  filter(continent == "Europe" | adm0_a3 %in% c("RUS","TUR","GEO"))

pp = ggplot(map_df) +
  geom_sf(aes(fill = estimate), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(-12, 50), ylim = c(35, 70)) +
  scale_fill_gradient2(
    midpoint = 0,
    low = "#B2182B", mid = "white", high = "#2166AC",
    name = parB
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
  return(list(pp, print(bullying_alpha1, n = Inf)))
}


plot_mapa("ASBGSB", "Bullying")
plot_mapa("ASBGICM", "Instructional Clarity")
plot_mapa("ASBGSSB", "School Belonging")
plot_mapa("ASBGSLM", "Like Learning Math")
plot_mapa("ASBGSCM", "Confident in Math")
