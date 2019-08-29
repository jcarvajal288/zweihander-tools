module Qualities where

import qualified Data.Map.Strict as HashMap
import qualified Data.Maybe as Maybe

printMapEntry :: HashMap.Map String String -> String -> String
printMapEntry hashMap key =
    key ++ " - " ++ Maybe.fromJust (HashMap.lookup key hashMap)

weaponQualities :: HashMap.Map String String
weaponQualities = HashMap.fromList
     [ ("Adaptable", "Whenever weapons of this Quality are held in two hands instead of one for Attack Actions, add +1 to Damage.")
     , ("Defensive", "Shields and weapons of this Quality add a +10 Base Chance to Parry.")
     , ("Entangling", "Immediately after striking a foe, weapons fo this Quality force a foe to Resist either a Chokehold or Takedown -- you choose which.  Additionally, whenever a foe is threatened with a Chokehold or Takedown with this weapon, they must flip the results to fail when Resisting its effects.")
     , ("Fast", "Whenever a foe is struck by weapons of this Quality, they suffer a -10 Base Chance to Dodge or Parry.")
     , ("Fiery", "After a foe is struck with a weapon of this Quality, both the foe and one other combatant who is Engaged with them must succeed at a Coordination Test or be set On Fire (see Chapter 9: Hazards and Healing).")
     , ("Finesse", "Weapons of this Quality always reference [AB] whenever dealing Damage, instead of [CB]")
     , ("Gunpowder", "Weapons of this Quality can be loaded and fired while standing in an Engagement.  Furthermore, these weapons cannot be Dodged or Parried.  Finally, they explode on a face '1' or '6'.")
     , ("Immolate", "After a foe is struck with a weapon of this Qaulity, they must succeed at a Coordination Test or be set On Fire.  The weapon is immediately extinguished after a successful attack, until relit.")
     , ("Ineffective", "Weapons of this Quality cannot deal Damage or inflict Injuries.")
     , ("Light", "Whenever weapons or shields of this Quality are held in your off-hand when attacking with a melee weapon in your primary hand, add +1 to Total Damage.")
     , ("Powerful", "Immediately after striking an Engaged foe, weapons of this Quality force a foe to Resist with Toughness or be shoved out of the Engangement.")
     , ("Pummeling", "Weapons of this Quality always refer to [BB] whenever inflicting Damage, instead of [CB].  Pummeling weapons can only inflict Moderate Injuries, never Serious or Grievous Injuries.  Finally, Pummeling weapons cannot be used to cause Bleeding.")
     , ("Punishing", "Immediately after striking a foe, weapons of this Quality may add a 1D6 Fury Die to Total Damage in exchange for spending 1 additional Action Point on this Turn.")
     , ("Reach", "Weapons of this Quality may strike a foe you're Engaged with or standing one yard away from outside of anEngagement.  Additionally, foes who are armed with a Reach weapon can make an Opportunity Attack whenever someone Charges or Runs toward them.")
     , ("Repeating", "Ranged weapons of this Quality can be fired up to three times without having to spend Action Points to load.")
     , ("Shrapnel", "Whenever a weapon with this Quality is fired, it affects multiple targets in a Cone Template.")
     , ("Slow", "Whenever a foe is struck by weapons of this Quality, the gain a +10 Base Chance to Dodge or Parry its Damage.")
     , ("Throwing", "Weapons of this Quality do not have a Medium or Long Distance increment for ranged weapons.")
     , ("Vicious", "Weapons of this Quality grant an addtional 1D6 Chaos Die to determine whether you inflict an Injury upon a foe.")
     , ("Volatile", "When you Critically Fail an Attack Action or Perilous Stunt when using weapons of this Quality, roll a 1D6 Chaos Die.  On a result of 1 to 5, the weapon misfires, requiring an hour to clean and repair.  On a result of face '6', it explodes, dealing 2D10+2 Damage from fire to you and destroying the weapon.  This Damage cannot be Dodged, Parried, or Resisted.")
     , ("Weak", "Weapons of this Quality can only inflict Moderate or Serious Injuries, never Grievous Injuries.") ]

lookupWeaponQuality :: String -> String
lookupWeaponQuality name = printMapEntry weaponQualities name

armorQualities :: HashMap.Map String String
armorQualities = HashMap.fromList
     [ ("Dangerous", "Clothing or armor of this Quality cannot adequately protect you from harm.  Should you suffer an Injury during this time without wearing a suit of armor, you begin to Bleed.")
     , ("Heavy", "Armor of this Quality prohibits the use of the Incantation Skill to cast Magick and Coordination in order to Dodge attacks.")
     , ("Natural", "Armor of this Quality adds a +10 Base Chance to Dodge attacks.") ]

miscQualities :: HashMap.Map String String
miscQualities = HashMap.fromList
     [ ("Castle-forged", "This Quality is typically conferred by the best craftsmanship.  Armor, weapons and shields of this Quality cannot acquire the Ruined! Quality, outside of special exceptions by the GM.   All Castle-forged, trappings have an associated base cost of three times the listed price.")
     , ("Maker's Mark", "This Quality is conferred by the handiwork of Guild Masters.  Weapons of this Quality gain a +5 Base Chance to strike.  Armor of this Quality reduces its Encumbrance Value by 2 (to a minimum of 1).  Shields of this Quality add a passive +1 to Damage Threshold Modifier.  All Maker's Marked trappings hav ean associated base cost of six times the listed price.")
     , ("Ruined!", "Armor of this Quality does not add to Damage Threshold.  Shields of this Quality cannot be used to Parry.  Weapons of this Quality suffer a -3 penalty to Total Damage.  Fixing Ruined! trappings require an appropriate tradesman who can fix it, who must be paid six days' wage.  Alternatively, if you can make a successful Tradecraft Test after six days of repair, you may fix it yourself.  If already Ruined! trappings are once again Ruined! before they can be fixed, they are irrevocably destroyed.  Ruined! items have no sell or trade value whatsoever.") ]
