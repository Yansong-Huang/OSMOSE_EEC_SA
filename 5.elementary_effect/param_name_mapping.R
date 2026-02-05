rm(list = ls())
library(data.table)
library(stringr)

# ------------ 准备工作 ------------
# 原始参数名
param_names <- readRDS("2.get-doe/doe/par_names_0425.rds")

# ------------ 替换映射表 ------------
sp_names <- c(
  "sp0" = "SYC",  "sp1" = "MUR",  "sp2" = "BIB",  "sp3" = "WHG",
  "sp4" = "POD",  "sp5" = "COD",  "sp6" = "LYY",  "sp7" = "SOL",
  "sp8" = "PLE",  "sp9" = "HOM",  "sp10" = "MAC", "sp11" = "HER",
  "sp12" = "PIL", "sp13" = "SQZ", "sp14" = "CTC", "sp15" = "RJC",
  "sp16" = "DIA",   # Diatoms
  "sp17" = "MPHY",  # Micro-phytoplanktons
  "sp18" = "HF",    # Heterotrophic Flagellates
  "sp19" = "MZP",   # Micro-zooplanktons
  "sp20" = "MEZP",  # Meso-zooplanktons
  "sp21" = "MAZP",  # Macro-zooplanktons
  "sp22" = "MB",    # Meio-benthos
  "sp23" = "DFB",   # Deposit-feeding benthos
  "sp24" = "SFB",   # Suspension-feeding benthos
  "sp25" = "LB",    # Large benthos
  "sp26" = "VLB"    # Very large benthos
)

fleet_names <- c(
  "fsh0"="BT",  # bottom trawlers
  "fsh1"="MT",  # mid-water trawlers
  "fsh2"="NT",  # netters
  "fsh3"="OT"   # others
)

# ------------ 分类函数 ------------
get_param_type <- function(pn) {
  fcase(
    str_detect(pn, "^mortality\\.additional\\.(rate|larva\\.rate)"), "Mortality",
    str_detect(pn, "^(fisheries\\.rate\\.base|species\\.catchability)"), "Fisheries",
    str_detect(pn, "^(species\\.(length2weight|k|l0|linf|maturity\\.size))"), "Growth",
    str_detect(pn, "^species\\.accessibility2fish"), "PreyField",
    str_detect(pn, "^predation\\.predPrey\\.sizeRatio"), "Predation",
    default = "Other"
  )
}

# ------------ 映射转换函数 ------------
transform_param_label <- function(pn) {
  new <- pn
  
  # 替换物种编号为简称
  for (s in names(sp_names)) {
    new <- str_replace_all(
      new,
      paste0("(?<=\\.)", s, "(?=\\.|$)"), 
      sp_names[[s]]
    )
  }
  
  # 替换船队编号为简称
  for (f in names(fleet_names)) {
    new <- str_replace_all(new, paste0("(?<=\\.)", f, "(?=\\.|$)"), fleet_names[[f]])
  }
  
  # 替换字段
  new <- str_replace(new, "^fisheries\\.rate\\.base", "fleet.catchability")
  new <- str_replace(new, "additional.larva", "larva")
  new <- str_replace(new, "sizeRatio", "size.ratio")
  new <- str_replace(new, "species.length2weight", "length2weight") 
  new <- str_replace(new, "species.k", "vb.growth.k") 
  new <- str_replace(new, "species.l0", "vb.growth.l0") 
  new <- str_replace(new, "species.linf", "vb.growth.linf") 
  new <- str_replace(new, "species.maturity", "maturity")  
  new <- str_replace(new, "species.accessibility2fish", "resource.accessibility2fish") 
  
  # 删除冗余字段
  new <- str_remove_all(new, "(^|\\.)predPrey")
  
  return(new)
}

# ------------ 物种提取函数 ------------
get_species <- function(pn) {
  sp_match <- str_extract(pn, "sp\\d+")
  if (is.na(sp_match)) {
    return("fleet")
  } else if (sp_match %in% names(sp_names)[1:16]) {
    return(sp_names[[sp_match]])  # sp0 到 sp15
  } else {
    return("resource")  # sp16 到 sp26
  }
}

# ------------ 应用映射生成表 ------------
mapping <- data.table(param_name = param_names)
mapping[, param_type := get_param_type(param_name)]
mapping[, param_label := transform_param_label(param_name)]
mapping[, param_species := sapply(param_name, get_species)]
setcolorder(mapping, c("param_name", "param_type", "param_species", "param_label"))

# ------------ 正则规则映射表 ------------
# ------------ 正则规则映射表 ------------
process_patterns <- list(
  "^mortality\\.additional\\.rate"              = c("additional_mortality", 1),
  "^mortality\\.additional\\.larva\\.rate"      = c("larval_mortality", 2),
  "^fisheries\\.rate\\.base"                    = c("fleet_catchability", 1),
  "^species\\.catchability"                     = c("species_catchability", 2),
  "^species\\.length2weight\\.condition\\.factor" = c("allometric_growth", 1),
  "^species\\.k"                                = c("vb_growth_k", 2),
  "^species\\.l0"                               = c("vb_growth_l0", 3),
  "^species\\.linf"                             = c("vb_growth_linf", 4),
  "^species\\.maturity\\.size\\.ratio"          = c("maturity_size", 5),
  "^predation\\.predPrey\\.sizeRatio\\.teta"    = c("predation_teta", 1),  # 默认值
  "^predation\\.predPrey\\.sizeRatio\\.alpha"   = c("predation_alpha", 2), # 默认值
  "^species\\.accessibility2fish"               = c("prey_field", 1)
)

# ------------ 匹配函数（含 stage 特殊逻辑）------------
get_process_and_order <- function(pn) {
  # 特殊情况：predation + stage
  if (str_detect(pn, "predation\\.predPrey\\.sizeRatio\\.teta") & str_detect(pn, "stage1")) {
    return(list("predation_teta", 1))
  }
  if (str_detect(pn, "predation\\.predPrey\\.sizeRatio\\.alpha") & str_detect(pn, "stage1")) {
    return(list("predation_alpha", 2))
  }
  if (str_detect(pn, "predation\\.predPrey\\.sizeRatio\\.teta") & str_detect(pn, "stage2")) {
    return(list("predation_teta", 3))
  }
  if (str_detect(pn, "predation\\.predPrey\\.sizeRatio\\.alpha") & str_detect(pn, "stage2")) {
    return(list("predation_alpha", 4))
  }
  
  # 通用匹配
  for (pat in names(process_patterns)) {
    if (str_detect(pn, pat)) {
      vals <- process_patterns[[pat]]
      return(list(vals[1], as.integer(vals[2])))
    }
  }
  return(list(NA_character_, NA_integer_))
}

# ------------ 增加 param_process 与 param_order ------------
mapping[, c("param_process", "param_order") := get_process_and_order(param_name), 
        by = param_name]

# ------------ 导出 CSV ------------
fwrite(mapping, "5.elementary_effect/param_name_map.csv")

# 示例输出
# print(mapping)

