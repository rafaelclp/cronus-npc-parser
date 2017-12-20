/*
@import './ScriptCode.pegjs'
*/

NpcFile = (_ s:Script _ { return s })*

// ---------- Script ----------

Script
  = "function"i tab "script"i tab name:NpcName tab "{" _ code:ScriptCode _ "}"
    { return { type: 'NPC:ScriptFunction', name, code } }
  / pos:(NpcPosition / NpcNoPosition) rem tab "script"i tab name:NpcName tab w4:ScriptW4
    { return { type: 'NPC:Script', pos, name, ...w4 }}

ScriptW4
  = sprite:Sprite trigger:("," t:Trigger { return t })? (!",{" .)* ",{" _ code:ScriptCode _ "}"
    { return { sprite, trigger, code } }

// ---------- Header utilities ----------

NpcNoPosition = "-"
    { return { map: '-' } }

NpcPosition = loc:Location dir:("," f:FacingDirection { return f })?
    { return { ...loc, dir } }

NpcName = $text?

Location = map:MapName "," x:MapCoordinate "," y:MapCoordinate
    { return { map, x, y } }

Trigger = x:MapCoordinate "," y:MapCoordinate
    { return { x, y } }

MapName = $nctext?
Sprite = "-1" { return -1 } / number / $nctext?
MapCoordinate = number / $nctext?
FacingDirection = number / $nctext?

number = n:[0-9]+
    { return parseInt(n.join(''), 10) }
text = $(!tab .)+
nctext = $(!tab !"," .)+

rem "remaining" = (!tab .)*
tab = "\t"
