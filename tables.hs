module Tables where

import qualified Data.Map.Strict as HashMap
import qualified Data.Maybe as Maybe

printMapEntry :: HashMap.Map String String -> String -> String
printMapEntry hashMap key =
    key ++ " - " ++ Maybe.fromJust (HashMap.lookup key hashMap)

moderateInjuries = HashMap.fromList 
     [ ("Black Eye", "Until fully Recuperated, you must flip the resutls to fail Skill Tests which rely on vision.")
     , ("Bruised Ribs", "Until fully Recuperated, reduce your Encumbrance Limit by 3.")
     , ("Dislocated Shoulder", "Until fully Recuperated, you start your Turn with 1 less AP.")
     , ("Hyperextended Elbow", "Until fully recuperated, you must flip the results to fail all Actions in Combat that rely on ranged weapons.")
     , ("Jammed Finger", "You immediately drop whatever you are holding. Until fully Recuperated, you must flip the results to fail all melee weapon attacks with your primary hand.")
     , ("Pulled Muscle", "Until fully Recuperated, you must flip the results to fail all Brawn-based Skill Tests.")
     , ("Rattled Brain", "Until fully Recuperated, reduce your Initiative by 3.")
     , ("Sprained Wrist", "Until fully Recuperated, you cannot hold anything in your primary hand, and must rely on your off-hand.")
     , ("Strained Groin", "Until fully Recuperated, you cannot use any Movement Actions besides Take Cover or Walk in combat.")
     , ("Twisted Ankle", "Until fully Recuperated, reduce your Movement by 3.") ]

seriousInjuries = HashMap.fromList
    [ ("Broken Rib", "Your armor gains the Ruined! Quality.  Until fully Recuperated, you cannot add Skill Ranks to Combat, Brawn or Agility-based Skills.")
    , ("Busted Kneecap", "Until fully Recuperated, any time you fail a Skill Test that relies on Brawn or Agility, you suffer 2d10+2 Physical Peril.")
    , ("Fractured Larynx", "Until fully Recuperated, you must succeed at a Scrutinize Test to speak.")
    , ("Head Trauma", "Until fully Recuperated, you cannot use Special Actions in combat.")
    , ("Minor Concussion", "Until fully Recuperated, you remain Incapacitated!")
    , ("Shell Shock", "Until fully Recuperated, you cannot add Fury Dice to Damage.")
    , ("Skull Fracture", "Until fully Recuperated, you must flip the results to fail all Skill Tests.")
    , ("Stress Fracture", "Until fully Recuperated, you cannot Counterspell, Dodge or Parry.")
    , ("Temporary Paralysis", "You are knocked Prone.  Until fully Recuperated, you cannot move as you're paralyzed.")
    , ("Torn Shoulder", "Whatever you are holding in your primary hand gains the Ruined! Quality.  Until fully Recuperated, you start your Turn with 2 less AP.") ]

moderateInjury :: Integer -> String 
moderateInjury n 
    | elem n [1..8]    = "Fortune's Mercy! - Ignore Injury, keep your Fate Point and continue fighting onwards!"
    | elem n [9..17]   = printModerateInjury "Black Eye"
    | elem n [17..25]  = printModerateInjury "Bruised Ribs"
    | elem n [26..33]  = printModerateInjury "Dislocated Shoulder"
    | elem n [34..41]  = printModerateInjury "Hyperextended Elbow"
    | elem n [42..49]  = printModerateInjury "Jammed Finger"
    | elem n [50..58]  = printModerateInjury "Pulled Muscle"
    | elem n [59..67]  = printModerateInjury "Rattled Brain"
    | elem n [68..76]  = printModerateInjury "Sprained Wrist"
    | elem n [77..85]  = printModerateInjury "Strained Groin"
    | elem n [86..92]  = printModerateInjury "Twisted Ankle"
    | elem n [93..100] = "Misfortune! - Roll on the Serious Injury table instead!"
    | otherwise = error ("out of range die roll: " ++ (show n))
    where printModerateInjury = printMapEntry moderateInjuries

seriousInjury :: Integer -> String
seriousInjury n 
    | elem n [1..8]    = "Fortune's Mercy! - Ignore Injury, keep your Fate Point and continue fighting onwards!"
    | elem n [9..17]   = printSeriousInjury "Broken Rib"
    | elem n [17..25]  = printSeriousInjury "Busted Kneecap"
    | elem n [26..33]  = printSeriousInjury "Fractured Larynx"
    | elem n [34..41]  = printSeriousInjury "Head Trauma"
    | elem n [42..49]  = printSeriousInjury "Minor Concussion"
    | elem n [50..58]  = printSeriousInjury "Shell Shock"
    | elem n [59..67]  = printSeriousInjury "Skull Fracture"
    | elem n [68..76]  = printSeriousInjury "Stress Fracture"
    | elem n [77..85]  = printSeriousInjury "Temporary Paralysis"
    | elem n [86..92]  = printSeriousInjury "Torn Shoulder"
    | elem n [93..100] = "Misfortune! - Roll on the Grievous Injury table instead!"
    | otherwise = error ("out of range die roll: " ++ (show n))
    where printSeriousInjury = printMapEntry seriousInjuries
