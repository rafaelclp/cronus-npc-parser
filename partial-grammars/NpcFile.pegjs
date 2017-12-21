/*
@import './ScriptCode.pegjs'
*/

NpcFile = (_ npc:Npc _ { return npc })*

Npc
  = Script
  / Shop
  / Mapflag
  / Duplicate
  / Warp
  / Monster

// ---------- Temporary - not implemented yet ----------

Monster = (!tab .)* tab "monster"i (!eol .)* eol
Warp = (!tab .)* tab "warp"i (!eol .)* eol / (!tab .)* tab "warp2"i (!eol .)* eol
Duplicate = (!tab .)* tab "duplicate("i (!eol .)* eol

// ---------- Mapflag ----------

Mapflag = w1:MapflagW1 w2: MapflagW2 w3:MapflagW3 w4:MapflagW4
    { return { ...w1, ...w2, ...w3, ...w4 } }

MapflagW1 = map:MapNameT
    { return { map } }
MapflagW2 = tab "mapflag"i
    { return { type: 'NPC', subtype: 'mapflag' } }
MapflagW3 = tab flag:StringParamT
    { return { flag } }
MapflagW4 = state:(tab state:StringParam? { return state })?
    { return { state } }

// ---------- Shop ----------

// TODO(lowpri): accept empty ShopItem list (as long as a comma is present)

Shop
  = w1:ShopW1 w2:ShopW2 w3:ShopW3 w4:ShopW4
    { return { ...w1, ...w2, ...w3, ...w4 } }
  / w1:ShopW1 w2:ItemshopW2 w3:ShopW3 w4:ItemshopW4
    { return { ...w1, ...w2, ...w3, ...w4 } }
  / w1:ShopW1 w2:PointshopW2 w3:ShopW3 w4:PointshopW4
    { return { ...w1, ...w2, ...w3, ...w4 } }

ItemshopW2 = tab "itemshop"i
    { return { type: 'NPC', subtype: "itemshop" } }
ItemshopW4 = tab sprite:Sprite cost:ItemshopCost items:ShopItemList
    { return { sprite, cost, items } }

PointshopW2 = tab "pointshop"i
    { return { type: 'NPC', subtype: "pointshop" } }
PointshopW4 = tab sprite:Sprite cost:PointshopCost items:ShopItemList
    { return { sprite, cost, items } }

ShopW1 = pos:NpcPosition
    { return { pos } }
ShopW2 = tab subtype:("shop"i / "cashshop"i / "marketshop"i)
    { return { type: 'NPC', subtype: subtype.toLowerCase() } }
ShopW3 = tab name:NpcName
    { return { name } }
ShopW4 = tab sprite:Sprite items:ShopItemList
    { return { sprite, items } }

ItemshopCost = "," id:NumberParamCC discount:ShopDiscount?
    { return { id, discount } }

PointshopCost = "," variable:$(!":" !"," .)+ discount:ShopDiscount?
    { return { variable, discount } }

ShopDiscount = ":" discount:NumberParamC
    { return discount }

ShopItemList = items:(ShopItem+ / "," _ { return [] })
    { return items }

ShopItem = "," id:NumberParamCC ":" price:NumberParamC
    { return { id, price } }

// ---------- Script ----------

Script
  = w1:ScriptFunctionW1 w2:ScriptW2 w3:ScriptW3 w4:ScriptFunctionW4
    { return { ...w1, ...w2, ...w3, ...w4 } }
  / w1:ScriptW1 w2:ScriptW2 w3:ScriptW3 w4:ScriptW4
    { return { ...w1, ...w2, ...w3, ...w4 } }

ScriptFunctionW1 = "function"i
    { return { isFunction: true } }
ScriptFunctionW4 = tab "{" _ code:ScriptCode _ "}"
    { return { code } }

ScriptW1 = pos:NpcPosition
    { return { pos } }
ScriptW2 = tab "script"i
    { return { type: 'NPC', subtype: 'script' } }
ScriptW3 = tab name:NpcName
    { return { name } }
ScriptW4 = tab sprite:Sprite trigger:ScriptTrigger? code:ScriptW4Code
    { return { sprite, trigger, code } }

ScriptW4Code = (!",{" .)* ",{" _ code:ScriptCode _ "}"
    { return code }

ScriptTrigger = "," x:ScriptTriggerCoord "," y:ScriptTriggerCoord
    { return { x, y } }

ScriptTriggerCoord = NumberParamC / $(!"{" StringParamC)

// ---------- Header utilities ----------

NpcPosition = pos:(NpcWithPosition / NpcNoPosition) (!tab .)*
    { return pos }

NpcNoPosition = "-"
    { return { map: '-' } }

NpcWithPosition = loc:Location facing:("," f:NumberParamCT { return f })?
    { return { ...loc, facing } }

NpcName = StringParamT

Location = map:MapNameCT "," x:NumberParamCT "," y:NumberParamCT
    { return { map, x, y } }

MapNameCT = StringParamCT
MapNameC = StringParamC
MapNameT = StringParamT
Sprite = NumberParamC

// It is better to handle numbers as strings in the headers
NumberParamCT = StringParamCT
NumberParamC = StringParamC
NumberParamCC = StringParamCC

StringParamCT = $(!tab !"," !eol .)+
StringParamT = $(!tab !eol .)+
StringParamC = $(!"," !eol .)+
StringParamCC = $(!":" !"," !eol .)+
StringParam = $(!eol .)+

tab = "\t"