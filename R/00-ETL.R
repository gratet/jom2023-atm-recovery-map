# 00-ETL.R

# -------------------------------------
# Data Extraction, Transformation, Loading (ETL)
# -------------------------------------

# Load data and perform initial transformations
atm_data <- 
  read_excel("data/Estadística_linia_parada_global.xlsx",
             col_types = c("text", "numeric", "text", "text", 
                           "text", "text", 
                           "text", "text", "text", "numeric", "numeric", 
                           "text", "text","text","text",
                           "numeric", "numeric", "numeric","numeric","numeric", "numeric", 
                           "numeric", "numeric", "numeric","numeric", "numeric", 
                           "numeric", "numeric", "numeric", "numeric","numeric", 
                           "numeric", "numeric",
                           "text","text"), 
             col_names = c('id',
                           'year', 'month', 'trimester',
                           'route_id', 'stop_id', 
                           'municipality', 'county', 'zone', 'lat', 'lon', 
                           'type', 'agency', 'route', 'stop',
                           't10_z1','t10_30_z1','t50_30_z1','t70_90_z1','tmes_z1','t12_z1',
                           't10_z2','t10_30_z2','t50_30_z2','t70_90_z2','tmes_z2',
                           't10_z3','t10_30_z3','t50_30_z3','t70_90_z3','tmes_z3',
                           'tp','tp_ext',
                           'month_calc','date_calc')) %>%
  filter(municipality != '-',
         type != 'Ferrocarril',
         lat != 0) %>% # Valid data check
  mutate(type = case_when(
    type == 'Urbà' ~ 'Urban',
    type == 'Interubà' ~ 'Interurban'),
    month = case_when(
    month == 'Gener' ~ '01-01',
    month == 'Febrer' ~ '02-01',
    month == 'Març' ~ '03-01',
    month == 'Abril' ~ '04-01',
    month == 'Maig' ~ '05-01',
    month == 'Juny' ~ '06-01',
    month == 'Juliol' ~ '07-01',
    month == 'Agost' ~ '08-01',
    month == 'Setembre' ~ '09-01',
    month == 'Octubre' ~ '10-01',
    month == 'Novembre' ~ '11-01',
    month == 'Desembre' ~ '12-01'),
    trimester = case_when(
    trimester == 'Trim.1' ~ "1",
    trimester == 'Trim.2' ~ "2",
    trimester == 'Trim.3' ~ "3",
    trimester == 'Trim.4' ~ "4"),
    yearmon = as.Date(paste0(year, '-', month), format = '%Y-%m-%d'),
    t10 = t10_z1 + t10_z2 + t10_z3,
    t10_30 = t10_30_z1 + t10_30_z2 + t10_30_z3,
    t50_30 = t50_30_z1 + t50_30_z2 + t50_30_z3,
    t70_90 = t70_90_z1 + t70_90_z2 + t70_90_z3,
    tmes = tmes_z1 + tmes_z2 + tmes_z3,
    t12 = t12_z1,
    tp = tp + tp_ext) %>%
  select(lat, lon, stop, type, municipality, yearmon,
         t10, t10_30, t50_30, t70_90, tmes, t12, tp)

# Convert data to long format
atm_data_long <-
  atm_data %>%
  pivot_longer(t10:tp, names_to = "fare", values_to = "validations") %>%
  filter(yearmon > "2018-12-31")

