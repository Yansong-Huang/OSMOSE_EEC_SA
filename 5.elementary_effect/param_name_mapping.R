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
  "sp20" = "MeZP",  # Meso-zooplanktons
  "sp21" = "MaZP",  # Macro-zooplanktons
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
    return("unspecified")
  } else if (sp_match %in% names(sp_names)[1:16]) {
    return(sp_names[[sp_match]])  # sp0 到 sp15
  } else {
    return("ressource")  # sp16 到 sp26
  }
}

# ------------ 应用映射生成表 ------------
mapping <- data.table(param_name = param_names)
mapping[, param_type := get_param_type(param_name)]
mapping[, param_label := transform_param_label(param_name)]
mapping[, param_species := sapply(param_name, get_species)]
setcolorder(mapping, c("param_name", "param_type", "param_species", "param_label"))

# ------------ 导出 CSV ------------
fwrite(mapping, "5.elementary_effect/param_name_map.csv")

# 示例输出展示
print(mapping)
