/*
@import './ScriptCode.pegjs'
*/

NpcFile = (_ npc:Npc _ { return npc })*

Npc
  = Script
  / Shop

// ---------- Shop ----------

// TODO: accept empyt ShopItem list (as long as a comma is present)

Shop
  = pos:NpcPosition tab type:NormalShopType tab name:NpcName tab sprite:Sprite items:ShopItem+
    { return { type: '@Shop', subtype: type, pos, name, sprite, items } }
  / pos:NpcPosition tab "itemshop"i tab name:NpcName tab sprite:Sprite cost:ItemshopCost items:ShopItem+
    { return { type: '@Shop', subtype: "itemshop", pos, name, sprite, cost, items } }
  / pos:NpcPosition tab "pointshop"i tab name:NpcName tab sprite:Sprite cost:PointshopCost items:ShopItem+
    { return { type: '@Shop', subtype: "pointshop", pos, name, sprite, cost, items }}

ItemshopCost = "," id:NumberParamW4 discount:ShopDiscount?
    { return { id, discount } }

PointshopCost = "," variable:PointshopVariable discount:ShopDiscount?
    { return { variable, discount } }

ShopDiscount = ":" d:NumberParamW4? { return d }

PointshopVariable = $(!":" !"," .)+

NormalShopType = type:("shop"i / "cashshop"i / "marketshop"i)
    { return type.toLowerCase() }

ShopItem = "," id:NumberParamW4 ":" price:NumberParamW4
    { return { id, price } }

// ---------- Script ----------

Script
  = "function"i tab "script"i tab name:NpcName tab "{" _ code:ScriptCode _ "}"
    { return { type: '@Script', subtype: 'function', name, code } }
  / pos:NpcPosition tab "script"i tab name:NpcName tab w4:ScriptW4
    { return { type: '@Script', pos, name, ...w4 }}

ScriptW4
  = sprite:Sprite trigger:("," t:ScriptTrigger { return t })? (!",{" .)* ",{" _ code:ScriptCode _ "}"
    { return { sprite, trigger, code } }

ScriptTrigger = x:ScriptTriggerCoord "," y:ScriptTriggerCoord
    { return { x, y } }

ScriptTriggerCoord = NumberParamW4_ / $(!"{" StringParamW4)

// ---------- Header utilities ----------

NpcPosition = p:(NpcWithPosition / NpcNoPosition) (!tab .)*
    { return p }

NpcNoPosition = "-"
    { return { map: '-' } }

NpcWithPosition = loc:Location dir:("," f:NumberParam { return f })?
    { return { ...loc, dir } }

NpcName = StringParamW3

Location = map:MapName "," x:NumberParam "," y:NumberParam
    { return { map, x, y } }

MapName = StringParam
MapNameW4 = StringParamW4
Sprite = "-1" { return -1 } / NumberParamW4

NumberParam = NumberParam_ / StringParam
NumberParam_ = " "* n:number { return n }
NumberParamW4 = NumberParamW4_ / StringParamW4
NumberParamW4_ = [ \t]* n:number { return n }

StringParam = $(!tab !"," .)+
StringParamW3 = $(!tab .)+
StringParamW4 = $(!"," .)+

number = n:[0-9]+
    { return parseInt(n.join(''), 10) }

tab = "\t"