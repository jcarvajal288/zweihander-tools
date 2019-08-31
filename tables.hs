module Tables where

import qualified Data.Map.Strict as HashMap
import qualified Data.Maybe as Maybe

printMapEntry :: HashMap.Map String String -> String -> String
printMapEntry hashMap key =
    key ++ " - " ++ Maybe.fromJust (HashMap.lookup key hashMap)

moderateInjuries :: HashMap.Map String String
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

seriousInjuries :: HashMap.Map String String
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

grievousInjuries :: HashMap.Map String String
grievousInjuries = HashMap.fromList
    [ ("Butchered Leg", "Until fully Recuperated, you cannot move as you're in pain.  You must undergo a successful surgery or suffer the consequences.  Once a Butchered Leg has undergone a failed surgery, you gain the Veteran's Leg Drawback.  If you already have this Drawback, you permanently lose 9% Agility.")
    , ("Cerebral Contusion", "Until fully Recuperated, you remain unconscious.  You must undergo a successful surgery or suffer the consequences.  Once a Cerebral Contusion has undergone a failed surgery, you gain the Dunderhead Drawback.  If you already have this Drawback, you permanently lose 9% Intelligence.")
    , ("Detached Ear", "Until fully Recuperated, you cannot hear as you're in pain.  You must undergo a successful surgery or suffer the consequences.  Once a Detached Ear has undergone a failed surgery, you suffer the Crop Ear Drawback.  If you already have this Drawback, you permanently lose 9% Perception.")
    , ("Maimed Foot", "Until fully Recuperated, you cannot use any weapon with an Encumbrance Value of 2 or more, as you're in pain.  You must undergo a successful surgery or suffer the consequences.  Once a Maimed Foot has undergone a failed surgery, you lose 1d6-1 toes.  For every toe lost, you permanently lose 1% Brawn.  If you lose all toes, you gain the Veteran's Boot Drawback.  If you already have this Drawback, you permanently lose 9% Brawn.")
    , ("Mangled Organ", "Until fully Recuperated, you remain Incapacitated!. You must undergo a successful surgery or suffer the consequences.  Once a Mangled Organ has undergone a failed surgery, you permanetnly gain the Eunuch Drawback.  If you already have this Drawback, you permanently lose 9% Fellowship.")
    , ("Mutilated Hand", "Until fully Recuperated, you cannot use your primary hand as you're in pain.  You must undergo a successful surgery or suffer the consequences.  Once a Mutilated Hand has undergone a failed surgery, you lose 1d6-1 fingers.  For every finger lost, you permanently lose 1% Agility.  If you lose all fingers, you gain the Veteran's Hand Drawback.  If you already have this Drawback, you permanently lose 9% Agility.")
    , ("Mutilated Nose", "Until fully Recuperated, you cannot smell as you're in pain.  You must undergo a successful surgery or suffer the consequences.  Once a Mutiliated Nose has undergone a failed surgery, you permanently must flip the results to fail all Skill Tests which rely on smell and taste.  If you already have this Drawback, you permanently lose 9% Perception.")
    , ("Punctured Lung", "Until fully Recuperated, you remain unconscious.  You must undergo a successful surgery or suffer the consequences.  Once a Punctured Lung has undergone a failed surgery, you permanently gain the Weak Lungs Drawback.  If you already have this Drawback, you permanently lose 9% Willpower.")
    , ("Splintered Elbow", "Until fully Recuperated, you cannot use one of your arms as you're in pain.  You must undergo a successful surgery or suffer the consequences.  Once a Splintered Elbow has undergone a failed surgery, you can no longer use two-handed ranged weapons.  If you already have this Drawback, you permannetly lose 9% Combat.")
    , ("Vitreous Hemmorrhage", "Until fully Recuperated, you cannot see as you're blinded.  You must undergo a successful surgery or suffer the consequences.  Once a Vitreous Hemmorrhage has undergone a failed surgery, you gain the Black Cataract Drawback.  If you already have this Drawback, you permanently lose 9% Perception.")
    , ("Severed Artery", "An arterial spray of blood marks your doom; you are instantly Slain!") ]

rollModerateInjury :: Integer -> String 
rollModerateInjury n 
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

rollSeriousInjury :: Integer -> String
rollSeriousInjury n 
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

rollGrievousInjury :: Integer -> String
rollGrievousInjury n 
    | elem n [1..8]    = "Fortune's Mercy! - Ignore Injury, keep your Fate Point and continue fighting onwards!"
    | elem n [9..17]   = printGrievousInjury "Butchered Leg"
    | elem n [17..25]  = printGrievousInjury "Cerebral Contusion"
    | elem n [26..33]  = printGrievousInjury "Detached Ear"
    | elem n [34..41]  = printGrievousInjury "Maimed Foot"
    | elem n [42..49]  = printGrievousInjury "Mangled Organ"
    | elem n [50..58]  = printGrievousInjury "Mutilated Hand"
    | elem n [59..67]  = printGrievousInjury "Mutilated Nose"
    | elem n [68..76]  = printGrievousInjury "Punctured Lung"
    | elem n [77..85]  = printGrievousInjury "Splintered Elbow"
    | elem n [86..92]  = printGrievousInjury "Vitreous Hemmorrhage"
    | elem n [93..100] = printGrievousInjury "Severed Artery"
    | otherwise = error ("out of range die roll: " ++ (show n))
    where printGrievousInjury = printMapEntry grievousInjuries

rollArchetype :: Integer -> String
rollArchetype n 
    | elem n [1..15]    = "Academic"
    | elem n [16..32]   = "Commoner"
    | elem n [33..49]   = "Knave"
    | elem n [50..66]   = "Ranger"
    | elem n [67..83]   = "Socialite"
    | elem n [84..100]  = "Warrior"
    | otherwise = error ("out of range die roll: " ++ (show n))

rollAcademic :: Integer -> String
rollAcademic n 
    | elem n [1..8]    = "Adherent"
    | elem n [9..17]   = "Anchorite"
    | elem n [17..25]  = "Antiquarian"
    | elem n [26..33]  = "Apothecary"
    | elem n [34..41]  = "Astrologer"
    | elem n [42..49]  = "Diabolist"
    | elem n [50..58]  = "Engineer"
    | elem n [59..67]  = "Informer"
    | elem n [68..76]  = "Investigator"
    | elem n [77..85]  = "Monk"
    | elem n [86..92]  = "Preacher"
    | elem n [93..100] = "Scribe"
    | otherwise = error ("out of range die roll: " ++ (show n))

rollCommoner :: Integer -> String
rollCommoner n 
    | elem n [1..8]    = "Artisan"
    | elem n [9..17]   = "Barber Surgeon"
    | elem n [17..25]  = "Boatman"
    | elem n [26..33]  = "Camp Follower"
    | elem n [34..41]  = "Cheapjack"
    | elem n [42..49]  = "Coachman"
    | elem n [50..58]  = "Doomsayer"
    | elem n [59..67]  = "Jailer"
    | elem n [68..76]  = "Laborer"
    | elem n [77..85]  = "Peasant"
    | elem n [86..92]  = "Rat Catcher"
    | elem n [93..100] = "Servant"
    | otherwise = error ("out of range die roll: " ++ (show n))

rollKnave :: Integer -> String
rollKnave n 
    | elem n [1..8]    = "Beggar"
    | elem n [9..17]   = "Burglar"
    | elem n [17..25]  = "Charlatan"
    | elem n [26..33]  = "Footpad"
    | elem n [34..41]  = "Gambler"
    | elem n [42..49]  = "Graverobber"
    | elem n [50..58]  = "Guttersnipe"
    | elem n [59..67]  = "Highwayman"
    | elem n [68..76]  = "Prostitute"
    | elem n [77..85]  = "Smuggler"
    | elem n [86..92]  = "Vagabond"
    | elem n [93..100] = "Vigilante"
    | otherwise = error ("out of range die roll: " ++ (show n))

rollRanger :: Integer -> String
rollRanger n 
    | elem n [1..8]    = "Animal Tamer"
    | elem n [9..17]   = "Bailiff"
    | elem n [17..25]  = "Bonepicker"
    | elem n [26..33]  = "Bounty Hunter"
    | elem n [34..41]  = "Gamekeeper"
    | elem n [42..49]  = "Hedgwise"
    | elem n [50..58]  = "Old Believer"
    | elem n [59..67]  = "Outrider"
    | elem n [68..76]  = "Pilgrim"
    | elem n [77..85]  = "Reeve"
    | elem n [86..92]  = "Slayer"
    | elem n [93..100] = "Trapper"
    | otherwise = error ("out of range die roll: " ++ (show n))

rollSocialite :: Integer -> String
rollSocialite n 
    | elem n [1..8]    = "Anarchist"
    | elem n [9..17]   = "Courtier"
    | elem n [17..25]  = "Cultist"
    | elem n [26..33]  = "Entertainer"
    | elem n [34..41]  = "Envoy"
    | elem n [42..49]  = "Fop"
    | elem n [50..58]  = "Jester"
    | elem n [59..67]  = "Provocateur"
    | elem n [68..76]  = "Racketeer"
    | elem n [77..85]  = "Raconteur"
    | elem n [86..92]  = "Rake"
    | elem n [93..100] = "Valet"
    | otherwise = error ("out of range die roll: " ++ (show n))

rollWarrior :: Integer -> String
rollWarrior n 
    | elem n [1..8]    = "Berserker"
    | elem n [9..17]   = "Bravo"
    | elem n [17..25]  = "Buccaneer"
    | elem n [26..33]  = "Dragoon"
    | elem n [34..41]  = "Hedge Knight"
    | elem n [42..49]  = "Man-At-Arms"
    | elem n [50..58]  = "Militiaman"
    | elem n [59..67]  = "Pit Fighter"
    | elem n [68..76]  = "Pugilist"
    | elem n [77..85]  = "Sellsword"
    | elem n [86..92]  = "Squire"
    | elem n [93..100] = "Watchman"
    | otherwise = error ("out of range die roll: " ++ (show n))
