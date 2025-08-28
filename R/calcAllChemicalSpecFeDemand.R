#' Calculates chemical energy demand from 2005 to 2020 from chemical production per route (AllChemicalRoute2005_2020) 
#' and specific energy consumption for the different routes (retrieved from IEA_PetrochemEI and other literature sources).
#' The energy demand for OtherChem is calculated as the remaining share of total chemical industry energy demand. 
#' Results are aggregated to the country level.
#' 
#' @author Qianzhi Zhang, Leonie Schweiger
#'
#' @param CCS boolean parameter whether CCS technologies are considered as such in 2020 or assumed to be technologies without CCS
#' 
calcAllChemicalSpecFeDemand <- function(CCS=FALSE) {
  
  # ---------------------------------------------------------------------------
  # Define Conversion Factor
  #    - Only the factor for MWh to GJ is used.
  # ---------------------------------------------------------------------------
  sm_Mwh_2_GJ <- 3.6  # Convert MWh to GJ
  
  # ---------------------------------------------------------------------------
  # Load Base Data
  #    a) Load chemical route data (2005-2020)
  #    b) Load regional historical energy intensity data for overall methanol&ammonia synthesis and steam crakcing (IEA_PetrochemEI)
  #    c) Collect specific BAT energy intensity data (target energy demand)
  # ---------------------------------------------------------------------------
  AllChemicalRoute2005_2020 <- calcOutput("AllChemicalRoute2005_2020", warnNA = FALSE, aggregate = TRUE, CCS=CCS) %>% 
    as.data.frame() %>% 
    select(-Cell) %>%
    rename(tePrc = Data1, opmoPrc = Data2, ChemFlow = Value)
  
  IEA_PetrochemEI <- calcOutput("IEA_PetrochemEI", aggregate = TRUE) %>% 
    as.data.frame() %>% 
    select(-Cell, -Year)
  
  # IEA_PetrochemEI energy intensities do not include feedstock use for steamcracker and methanol
  # the data is therefore adjusted with reported literature values for energy intensities including feedstock use
  # feedstock input is assumed to correspond to the difference between reported literature value and IEA_PetrochemEI energy intensity of the most efficient region
  specFeDem <- IEA_PetrochemEI %>% # in GJ/t
    group_by(Data1) %>%
    mutate(Value_min = min(Value[Value>0])) %>%
    ungroup() %>% 
    mutate(Value_new =
             case_when(Data1=="Steam cracking, fuel & steam" & Value>0 ~ Value+(66-Value_min),    # Source: Spallina17 Table 5; Value for naptha steam cracking, as the region with the lowest energy intensity JPN has 96% naphta steam crackers (Saygın et al. 2009. Chemical and Petrochemical Sector.Table 11)
                       Data1=="Methanol, fuel & steam"  & Value>0 ~ Value+(33.9-Value_min),        # Source: IEA, The Future of Hydrogen19 PAGE | 5; Value the natural gas based route as this route is employed in regions with lowest energy intensity
                       TRUE ~ Value))       
  
  specFeDemTarget <- tibble::tribble( # in MWh/t
    ~entyFe, ~tePrc,      ~opmoPrc,   ~value,
    "fehos", "stCrNg",    "standard", 15.8,    # Yang & You 2017 Table1 NGL input/(Ethylene + By Products)
    "fegas", "stCrNg",    "standard", 0.60,    # Yang & You 2017 (External Energy input - Hydrogen Output)/(Ethylene + By Products)
    "feels", "stCrNg",    "standard", 0.54,    # Yang & You 2017 Electricity input/(Ethylene + By Products)
    
    "fehos", "stCrLiq",   "standard", 14.6,    # Layritz 2021 Naphtha input/(Ethylene + By Products)
    "feels", "stCrLiq",   "standard", 0.069,   # Layritz 2021 Electricity input/(Ethylene + By Products)
    
    "fegas", "stCrChemRe","standard", 0.84,    # Yadav 2023 Table S20 Using co-producted naphtha and NGLs as fuel
    "feels", "stCrChemRe","standard", 0.24,    # Yadav 2023 Table S12+S13 electricity costs
    
    "fegas", "mechRe",    "standard", 0.29,    # Uekert 2023 Weighted avg PE, PP, PET
    "feels", "mechRe",    "standard", 0.54,    # Uekert 2023 Weighted avg PE, PP, PET
    
    "fesos", "meSySol",   "standard", 10.3,    # Wang 2021 Table 9
    "feels", "meSySol",   "standard", 0.14,    # Wang 2021 Table 9
    
    "fesos", "meSySol",   "greenh2",  4.6,     # Wang 2021 Table 9
    "feh2s", "meSySol",   "greenh2",  3.3,     # Wang 2021 Table 9
    "feels", "meSySol",   "greenh2",  0.14,    # Wang 2021 Table 9
    
    "fegas", "meSyNg",    "standard", 8.8,     # IEA 2019 Future of Hydrogen Annex p.5
    "feels", "meSyNg",    "standard", 0.083,   # IEA 2019 Future of Hydrogen Annex p.5
    
    "fehos", "meSyLiq",   "standard", 9.7,     # IEA 2018 Future of Petrochemicals Table A4
    "feels", "meSyLiq",   "standard", 0.56,    # IEA 2018 Future of Petrochemicals Table A4
    
    "fesos", "meSySol_cc","standard", 2.8,     # IEA 2019 Future of Hydrogen Annex p.5 MWh/tC
    "feels", "meSySol_cc","standard", 0.065,   # IEA 2019 Future of Hydrogen Annex p.5 MWh/tC
    
    "feels", "meSyNg_cc", "standard", 0.54,    # IEA 2019 Future of Hydrogen Annex p.5 MWh/tC
    "feels", "meSyLiq_cc","standard", 0.54,    # Assume same as meSyNg_cc
    
    "feh2s", "meSyH2",    "standard", 6.4+0.58,# DEA 2017 Methanol from hydrogen
    "feels", "meSyH2",    "standard", 0.10,    # DEA 2017 Methanol from hydrogen
    
    "fegas", "meSyChemRe","standard", 2.6,     # Shaik Afzal 2023 MSP breakdown
    "feels", "meSyChemRe","standard", 0.92,    # Shaik Afzal 2023 MSP breakdown
    
    "fesos", "amSyCoal",  "standard", 10.7,    # IEA 2019 Future of Hydrogen Annex p.5
    "feels", "amSyCoal",  "standard", 1.0,     # IEA 2019 Future of Hydrogen Annex p.5
    
    "fegas", "amSyNG",    "standard", 8.9,     # IEA 2019 Future of Hydrogen Annex p.4
    "feels", "amSyNG",    "standard", 0.083,   # IEA 2019 Future of Hydrogen Annex p.4
    
    "fehos", "amSyLiq",   "standard", 9.0,     # IEA 2018 Future of Petrochemicals Table A4
    "feels", "amSyLiq",   "standard", 0.56,    # IEA 2018 Future of Petrochemicals Table A4
    
    "feels", "amSyCoal_cc","standard", 0.44,   # IEA 2019 Future of Hydrogen Annex p.5 MWh/tC
    "feels", "amSyNG_cc", "standard", 0.46,    # IEA 2019 Future of Hydrogen Annex p.5 MWh/tC
    "feels", "amSyLiq_cc","standard", 0.46,    # Assume same as amSyNG_cc
    
    "feh2s", "amSyH2",    "standard", 6.0,     # DEA 2017 Hydrogen to Ammonia
    "feels", "amSyH2",    "standard", 0.49,    # Grinberg Dana 2016 Supplementary Table 4
    
    "feels", "mtoMta",    "standard", 1.4,     # Bazzanella 2017 Section 4.5.3
    "feels", "mtoMtaH2",  "standard", 1.4,     # Bazzanella 2017 Section 4.5.3
    "feels", "fertProd",  "standard", 0.39,    # Palys 2023 Sec 2.3 p.6
    "feels", "fertProdH2","standard", 0.39     # Palys 2023 Sec 2.3 p.6
  ) %>% 
    mutate(value = value*sm_Mwh_2_GJ) %>% # MWh/t to GJ/t
    # map routes to steam cracking/ammonia/methanol synthesis
    mutate(Data1 =
             case_when(tePrc %in% c("stCrLiq", "stCrNg") & opmoPrc=="standard" & entyFe != "feels" ~ "Steam cracking, fuel & steam",
                       tePrc %in% c("amSyCoal", "amSyNG", "amSyLiq") & opmoPrc=="standard" & entyFe != "feels" ~ "Ammonia, fuel & steam",
                       tePrc %in% c("meSySol", "meSyNg", "meSyLiq") & opmoPrc=="standard" & entyFe != "feels" ~ "Methanol, fuel & steam",
                       TRUE ~ tePrc
             ))
  
  # ---------------------------------------------------------------------------
  # To get specific energy demands per route, calculate the ratio between total actual and target FE demand for steam cracker, ammonia and methanol synthesis
  # The ratio between actual and target demand for each route is assumed to be the same as for the overall steam cracking/ammonia/methanol synthesis
  # ---------------------------------------------------------------------------
  demFeTarget <- AllChemicalRoute2005_2020 %>% filter(Year==2020) %>%
    left_join(specFeDemTarget, by = c("tePrc", "opmoPrc"), relationship="many-to-many") %>%
    mutate(demFeTarget = ChemFlow*value) %>%
    group_by(Region, Year, Data1)%>%
    summarise(demFeTarget_total = sum(demFeTarget), totalFlow = sum(ChemFlow))
  
  demFeActual <- AllChemicalRoute2005_2020 %>% filter(Year==2020) %>%
    mutate(Data1 =
             case_when(tePrc %in% c("stCrLiq", "stCrNg") & opmoPrc=="standard" ~ "Steam cracking, fuel & steam",
                       tePrc %in% c("amSyCoal", "amSyNG", "amSyLiq") & opmoPrc=="standard" ~ "Ammonia, fuel & steam",
                       tePrc %in% c("meSySol", "meSyNg", "meSyLiq") & opmoPrc=="standard" ~ "Methanol, fuel & steam",
                       TRUE ~ tePrc
             ))%>%
    group_by(Region, Year, Data1) %>%
    summarise(ChemFlow_total = sum(ChemFlow)) %>%
    merge(specFeDem, by = c("Region", "Data1")) %>%
    mutate(demFeActual_total = Value_new * ChemFlow_total)
  
  demFeRatio <- merge(demFeTarget, demFeActual, by=c("Region","Year", "Data1")) %>%
    mutate(demFeRatio = demFeActual_total/demFeTarget_total) %>% 
    select(c(Region, Data1, demFeRatio))%>%
    mutate(demFeRatio = case_when(demFeRatio<1 ~ 1, TRUE ~ demFeRatio)) # regional energy intensity cannot be lower than the target (possibly coal-based methanol in IND and OAS in AllChemicalRoute higher than the one assumed by IEA_PetrochemEI)
  
  Regions <- AllChemicalRoute2005_2020 %>% select(Region) %>% distinct()
  specFeDem_byRoute <- specFeDemTarget %>%  
    crossing(Regions) %>%
    left_join(demFeRatio, by=c("Data1", "Region")) %>%
    mutate(specFeDem = case_when(entyFe != "feels" & !is.na(demFeRatio) ~ value*demFeRatio, # regional energy intensities only refer to fuel&steam, electricity demand is assumed to be the same globally
                                 # for the following electricity demands, assume other (higher) demands than target
                                 entyFe == "feels" & tePrc == "stCrLiq" ~ 1.0, # GJ/t Source: Spallina17 Table 5
                                 entyFe == "feels" & tePrc == "meSySol" ~ 3.7, # GJ/t Source: IEA, The Future of Hydrogen19 PAGE | 5 
                                 TRUE ~ value)) %>%
    select(-c(Data1, demFeRatio, value))
  
  # ---------------------------------------------------------------------------
  # Compute Energy Demand from Chemical Routes and SpecFeDemand and summarize to total energy demand per region, year and FE type
  # ---------------------------------------------------------------------------
  feChemical <- specFeDem_byRoute %>%
    left_join(AllChemicalRoute2005_2020, by = c("Region", "tePrc"), relationship="many-to-many") %>%
    mutate(Energy_demand = ChemFlow * specFeDem) %>%
    group_by(Region, Year, entyFe) %>%
    summarise(Total_Energy_Demand = sum(Energy_demand, na.rm = TRUE), .groups = "drop") 
  
  # ---------------------------------------------------------------------------
  # Load Industry Demand Data for Chemicals and map FE types
  # ---------------------------------------------------------------------------
  feIndustry <- calcOutput("FeDemandIndustry", scenarios=c("SSP2"), signif = 4, warnNA = FALSE, aggregate = TRUE)[, 
                                                                                                                  c("y2005", "y2010", "y2015", "y2020"),
                                                                                                                  c("SSP2.feelhth_chemicals", "SSP2.feelwlth_chemicals", 
                                                                                                                    "SSP2.feh2_chemicals", "SSP2.fega_chemicals", 
                                                                                                                    "SSP2.feli_chemicals", "SSP2.feso_chemicals")
  ] %>%
    as.data.frame() %>%
    select(-Cell) %>%
    mutate(
      entyFe = case_when(
        Data2 %in% c("feelwlth_chemicals", "feelhth_chemicals") ~ "feels",
        Data2 %in% c("feh2_chemicals", "fega_chemicals") ~ "fegas",
        Data2 == "feli_chemicals" ~ "fehos",
        Data2 == "feso_chemicals" ~ "fesos",
        TRUE ~ Data2
      )
    ) %>%
    group_by(Region, Year, entyFe) %>%
    summarise(feChemicals = sum(Value, na.rm = TRUE), .groups = "drop") 
  
  # ---------------------------------------------------------------------------
  # Calculate "OtherChem" Energy Demand.
  #    - Merge with feIndustry and calculate the difference between feIndustry Value and the total energy demand.
  # ---------------------------------------------------------------------------
  OtherChem_FE <- feIndustry %>%
    left_join(feChemical, by = c("Region", "Year", "entyFe")) %>%
    mutate(
      Total_Energy_Demand = ifelse(is.na(Total_Energy_Demand), 0, Total_Energy_Demand),
      Total_Difference = feChemicals - Total_Energy_Demand
    ) %>%
    mutate(tePrc = "chemOld", opmoPrc= "standard")
  
  # ---------------------------------------------------------------------------
  # Calculate specific Energy Demand of OtherChem and merge with other specific Energy demands (assumed to be constant from 2005-2020)
  # ---------------------------------------------------------------------------
  OtherChem_SpecFeDem <- OtherChem_FE %>%
    left_join(AllChemicalRoute2005_2020, by=c("Region","Year","tePrc","opmoPrc")) %>%
    mutate(specFeDem = Total_Difference/ChemFlow) %>%
    select(Region, Year, tePrc, opmoPrc, entyFe, specFeDem)
  
  years <- OtherChem_SpecFeDem %>% select(Year) %>% distinct()
  
  AllChem_SpecFeDem <- specFeDem_byRoute %>% crossing(years) %>%
    rbind(OtherChem_SpecFeDem) %>%
    select(Region,Year,tePrc, opmoPrc, entyFe, specFeDem)
    
  # ---------------------------------------------------------------------------
  # Aggregate Data to Country Level
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(AllChem_SpecFeDem, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = NULL
  )
  x[is.na(x)] <- 0
  weight <- x # get the same dimensions of the data
  weight[, , ] <- 1
  
  # ---------------------------------------------------------------------------
  # Return Final Aggregated Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = weight,
    unit = "GJ/t-output",
    description = "Chemicals specific final energy demand per technology, final energy carrier, region and year"
  ))
}
