### SOURCING DATA ###
source("global.R")
source("model.R")
data <- getModelTrain()

# Get Appropriate Datasets
whiffData <- data %>% prep()
csData <- data %>% prep(response = "StrikeCalled", forWhiff = F)

# Create training datasets, broken down by handedness
whiffLL <- whiffData %>% filter(BatterSide == "Left" & PitcherThrows == "Left")
whiffLR <- whiffData %>% filter(BatterSide == "Left" & PitcherThrows == "Right")
whiffRL <- whiffData %>% filter(BatterSide == "Right" & PitcherThrows == "Left")
whiffRR <- whiffData %>% filter(BatterSide == "Right" & PitcherThrows == "Right")
csLL <- csData %>% filter(BatterSide == "Left" & PitcherThrows == "Left")
csLR <- csData %>% filter(BatterSide == "Left" & PitcherThrows == "Right")
csRL <- csData %>% filter(BatterSide == "Right" & PitcherThrows == "Left")
csRR <- csData %>% filter(BatterSide == "Right" & PitcherThrows == "Right")

# Make the XGBoost models (rounds imputed by observing output -- rerun if variables changed)
wLLXGB <- makeXGBMod(whiffLL, rounds = 12) # Rounds imputed from output
wLRXGB <- makeXGBMod(whiffLR, rounds = 10)
wRLXGB <- makeXGBMod(whiffRL, rounds = 9)
wRRXGB <- makeXGBMod(whiffRR, rounds = 19)

csLLXGB <- makeXGBMod(csLL, rounds = 31)
csLRXGB <- makeXGBMod(csLR, rounds = 49)
csRLXGB <- makeXGBMod(csRL, rounds = 38)
csRRXGB <- makeXGBMod(csRR, rounds = 49)

# dir.create("./WhiffMods")
# dir.create("./CSMods")

# Save the XGB models
xgb.save(wLLXGB, "./WhiffMods/llXGB.model")
xgb.save(wLRXGB, "./WhiffMods/lrXGB.model")
xgb.save(wRLXGB, "./WhiffMods/rlXGB.model")
xgb.save(wRRXGB, "./WhiffMods/rrXGB.model")
xgb.save(csLLXGB, "./CSMods/llXGB.model")
xgb.save(csLRXGB, "./CSMods/lrXGB.model")
xgb.save(csRLXGB, "./CSMods/rlXGB.model")
xgb.save(csRRXGB, "./CSMods/rrXGB.model")
