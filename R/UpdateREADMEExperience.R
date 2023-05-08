# Read README
READMEString <- readLines("README.md")

# Calculate time periods
Dates <- list(
  
  RPythonExp = lubridate::dmy("01032010"),
  HealthAnalysis = lubridate::dmy("01082017"),
  SurgDesign = lubridate::dmy("21032018"),
  AusHC = lubridate::dmy("28012014")
  
)

fCalcYears <- function(Date, Today) {
  
  YearsRaw <- lubridate::interval(Date, Today) |>
    as.numeric('years') 
  
  ReturnYear <- floor(YearsRaw / 0.1) * 0.1
  
}

Years <- purrr::map(Dates,
                    fCalcYears,
                    Today = Sys.Date())

# Insert into README
## R/Python experience
READMEString <- gsub(pattern = "<!--RPythonExp-->.*<!--END-->",
                     replacement = paste0("<!--RPythonExp-->", Years$RPythonExp, "<!--END-->"),
                     x = READMEString)
## Health analysis
READMEString <- gsub(pattern = "<!--HealthAnalysis-->.*<!--END-->",
                     replacement = paste0("<!--HealthAnalysis-->", Years$HealthAnalysis, "<!--END-->"),
                     x = READMEString)
## Surgical design
READMEString <- gsub(pattern = "<!--SurgDesign-->.*<!--END-->",
                     replacement = paste0("<!--SurgDesign-->", Years$SurgDesign, "<!--END-->"),
                     x = READMEString)
## Aus Healthcare
READMEString <- gsub(pattern = "<!--AusHC-->.*<!--END-->",
                     replacement = paste0("<!--AusHC-->", Years$AusHC, "<!--END-->"),
                     x = READMEString)

writeLines(READMEString, con = file("README.md"))
