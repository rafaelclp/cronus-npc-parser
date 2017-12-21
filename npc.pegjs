{
  const constructBinaryExpression = (head, tail) => (
    tail.reduce((left, [_0, op, _1, right]) => (
      { type: 'BinaryExpression', left, op, right }
    ), head)
  )
}

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

// ---------- ScriptCode ----------

ScriptCode = _ stmts:StatementList _
    { return { type: 'StatementBlock', stmts } }

StatementList = stmts:(stmt:Statement _ { return stmt })*
    { return stmts.filter(stmt => stmt !== null) }

// ---------- Statement ----------

Statement
  = StatementBlock
  / ForStatement
  / WhileStatement
  / DoWhileStatement
  / IfElseStatement
  / FunctionStatement
  / SwitchStatement
  / LabelDeclaration
  / cmd:Command? _ ";"
    { return cmd }

StatementBlock = "{" _ stmts:StatementList _ "}"
    { return { type: 'StatementBlock', stmts } }

// ---------- Switch ----------

// In scripts, the switch works like this:
// 1. Execute any code before the first case
// 2. Find the proper case
// 3. Execute all remaining code starting from it

SwitchStatement = "switch"i _ "(" _ expr:Expression _ ")" _ "{" _ stmt:SwitchBlock _ "}"
    { return { type: 'Switch', expr, stmt } }

SwitchBlock = stmts:(stmt:(CaseDefinition / Statement) _ { return stmt })*
    { return { type: 'StatementBlock', stmts } }

CaseDefinition
  = "case"i __+ label:CaseLabel _ ":"
    { return { type: 'Case', label } }
  / "default"i _ ":"
    { return { type: 'Case', label: null } }

CaseLabel
    = IntegerLiteral
    / name:SimpleIdentifierName
      { return { type: 'Constant', name } }

// ---------- Function ----------

FunctionStatement
  = name:FunctionDeclaration _ "{" _ code:ScriptCode _ "}"
    { return { type: 'Function', name, code } }
  / name:FunctionDeclaration _ ";"
    { return { type: 'FunctionDeclaration', name } }

FunctionDeclaration = "function"i &ReservedWordSeparator _ name:CommandIdentifierName
    { return name }

// ---------- Condition ----------

IfElseStatement = ifStmt:IfStatement _ falseStmt:ElseDefinition?
    { return { ...ifStmt, falseStmt } }

IfStatement = "if"i _ "(" _ cond:Expression _ ")" _ trueStmt:Statement
    { return { type: 'Conditional', cond, trueStmt } }

ElseDefinition = "else"i &ReservedWordSeparator _ falseStmt:Statement
    { return falseStmt }

// ---------- Loop ----------

ForStatement
  = "for"i _ "(" _ init:Command _ ";" _ cond:Expression _ ";" _ inc:Command _ ")" _ stmt:Statement
    { return { type: 'Loop', init, cond, inc, stmt } }

DoWhileStatement
  = "do"i &ReservedWordSeparator _ stmt:Statement _ cond:WhileDefinition _ ";"
    { return { type: 'Loop', init: stmt, cond, stmt } }

WhileStatement
  = cond:WhileDefinition _ stmt:Statement
    { return { type: 'Loop', cond, stmt } }

WhileDefinition = "while"i _ "(" _ cond:Expression _ ")"
    { return cond }

// ---------- ReservedWordSeparator ----------

ReservedWordSeparator = !"$" (CommandToArgListSeparator / "{")

// ---------- Command ----------

Command
  = AssignmentCommand
  / name:CommandIdentifierName &CommandToArgListSeparator _ args:CommandArgList
    { return { type: 'Command', name, args } }

CommandToArgListSeparator = "-" / "+" / "~" / "!" / "(" / "\"" / ";" / __ / VariableScope

// menu("hi", -); fails, because '-' can only be used if no parentheses are used
// menu "hi", -; would work, with '-' being a special label (points to next statement)
CommandArgList
  = "(" _ ")"
    { return [] }
  / !("(" _ (Expression _ "," _)+ Expression _ ")") args:CommandCallArgList
  	{ return args }
  / "(" _ args:FunctionCallArgList _ ")"
    { return args }

CommandCallArgList
  = head:(arg:CommandArg _ "," _ { return arg })* tail:CommandArg
    { return head.concat(tail) }
  / ""
    { return [] }

CommandArg
  = Expression
  / "-"
    { return { type: 'Label', name: '-' } }

// ---------- AssignmentCommand ----------

AssignmentCommand = UnaryAssignmentCommand / BinaryAssignmentCommand

UnaryAssignmentCommand = left:CommandVariable _ op:("++" / "--")
    { return { type: 'Command', name: '=', args: [op, left] } }

BinaryAssignmentCommand = left:CommandVariable _ op:AssignmentOperator _ right:Expression
    { return { type: 'Command', name: '=', args: [op, left, right] } }

CommandVariable = name:CommandIdentifierName indexExpr:ArrayIndex?
    { return { type: 'Variable', name, index: indexExpr } }

// ---------- Label creation ----------

LabelDeclaration = name:CommandIdentifierName _ ":"
    { return { type: 'LabelDeclaration', name } }

// ---------- Expression ----------

Expression = ConditionalExpression

// ---------- Operations ----------

ConditionalExpression
  = cond:LogicalORExpression _ "?" _ trueExpr:Expression _ ":" _ falseExpr:Expression
    { return { type: 'ConditionalExpression', cond, trueExpr, falseExpr } }
  / LogicalORExpression

LogicalORExpression
  = head:LogicalANDExpression tail:(_ "||" _ LogicalANDExpression)*
    { return constructBinaryExpression(head, tail) }

LogicalANDExpression
  = head:BitwiseORExpression tail:(_ "&&" _ BitwiseORExpression)*
    { return constructBinaryExpression(head, tail) }

BitwiseORExpression
  = head:BitwiseXORExpression tail:(_ "|" _ BitwiseXORExpression)*
    { return constructBinaryExpression(head, tail) }

BitwiseXORExpression
  = head:BitwiseANDExpression tail:(_ "^" _ BitwiseANDExpression)*
    { return constructBinaryExpression(head, tail) }

BitwiseANDExpression
  = head:EqualityExpression tail:(_ "&" _ EqualityExpression)*
    { return constructBinaryExpression(head, tail) }

EqualityExpression
  = head:RelationalExpression tail:(_ ("==" / "!=") _ RelationalExpression)*
    { return constructBinaryExpression(head, tail) }

RelationalExpression
  = head:ShiftExpression tail:(_ RelationalOperator _ ShiftExpression)*
    { return constructBinaryExpression(head, tail) }

RelationalOperator = $("<=" / "<" !"<" / ">=" / ">" !">")

ShiftExpression
  = head:AdditiveExpression tail:(_ ("<<" / ">>") _ AdditiveExpression)*
    { return constructBinaryExpression(head, tail) }

AdditiveExpression
  = head:MultiplicativeExpression tail:(_ $("+" !"+" / "-") _ MultiplicativeExpression)*
    { return constructBinaryExpression(head, tail) }

MultiplicativeExpression
  = head:UnaryExpression tail:(_ ("*" / "/" / "%") _ UnaryExpression)*
    { return constructBinaryExpression(head, tail) }

UnaryExpression
  = op:("!" / "~") _ right:UnaryExpression
    { return { type: 'UnaryExpression', op, right } }
  / ArithmeticUnaryExpression

// ---------- Arithmetic negation ----------

// Scripts do not have a '+' unary operator. They do accept '+3', with '+' being
// part of the literal, but do not accept '+ 3' for example. Instead of multiple
// '-' here with collapse them into a single '-' (odd quantity) or '+' (even).
// We do need to keep an expression for '+' (meaning an even amount of '-') so
// we can report an error if it is applied to a string!
ArithmeticUnaryExpression
  = AssignmentExpression
  / sign:MultiNegationSign _ right:Operand
    { return { type: 'UnaryExpression', op: sign < 0 ? '-' : '+', right } }
  / Operand

// Scripts accept "- - 3" for example, but not "--3". "-+3" is acceptable; "-+3"
// just applies "-" to "+3"! Multiple "-" *MUST* be separated by whitespace(s)!
MultiNegationSign = head:"-" tail:(__+ "-" { return -1 })*
    { return tail.reduce((res, sign) => res * sign, -1) }

// ---------- Assignment ----------

AssignmentExpression = UnaryAssignmentExpression / BinaryAssignmentExpression

UnaryAssignmentExpression = left:Variable _ op:("++" / "--")
    { return { type: 'AssignmentExpression', left, op } }

BinaryAssignmentExpression = left:Variable _ op:AssignmentOperator _ right:Expression
    { return { type: 'AssignmentExpression', left, op, right } }

AssignmentOperator
  = "*=" / "/=" / "%=" / "+=" / "-=" / "<<=" / ">>=" / "&=" / "^=" / "|="
  / $("=" !"=")

// ---------- Operand ----------

Operand
  = "(" _ expr:Expression _ ")"
    { return expr }
  / int:IntegerLiteral !IdentifierName
    { return int }
  / StringLiteral
  / FunctionCall
  / Variable

// ---------- Function call ----------

FunctionCall
  = name:IdentifierName _ "(" _ args:FunctionCallArgList _ ")"
    { return { type: 'FunctionCall', name, args } }

FunctionCallArgList
  = head:(expr:Expression _ "," _ { return expr })* tail:Expression
    { return head.concat(tail) }
  / ""
    { return [] }

// ---------- Variable ----------

Variable = name:IdentifierName indexExpr:ArrayIndex?
    { return { type: 'Variable', name, index: indexExpr } }

ArrayIndex = "[" _ expr:Expression _ "]"
    { return expr }

// ---------- Identifier ----------

//  TODO(?): reject identifiers whose name is a reserved word

// NOTICE: inside expressions, identifiers can't be integers or (integer + '$').
// For example, '37' and '37$' aren't valid identifiers. However, '37_' is. They
// are valid for commands though. While '3 += 4' is invalid as an expression
// e.g. in 'mes 3 += 4;', it's valid as a whole command, e.g. just '3 += 4;'.
// Still, '3 += 4 += 5;' is invalid, since the right side, '4 += 5', is an
// expression, not a command. That's how bad our script language is.
IdentifierName
  = &(!IntegerLiteral / IntegerLiteral SimpleIdentifierName) ident:CommandIdentifierName
    { return ident }

// Variables such as '$', '$$', '$@', '@$', '$@$', '.', '3', '3$' are all valid...
// Weirdly enough, any variable name is also a valid label or function name...
CommandIdentifierName
  = scope:VariableScope name:SimpleIdentifierName? type:VariableType
    { return scope + (name || '') + type }
  / name:SimpleIdentifierName type:VariableType
    { return name + type }

SimpleIdentifierName = s:[0-9a-zA-Z_]+
    { return s.join('') }

VariableScope
  = "$@" // global
  / "$"  // globalPerm
  / "'"  // instance
  / ".@" // npcPlayer
  / "."  // npc
  / "##" // accountPerm2
  / "#"  // accountPerm
  / "@"  // player
/// ""   // playerPerm

VariableType
  = "$"  // string
  / ""   // integer

// ---------- Literals ----------

// Accepts 3, +3, but not -3. In scripts, this '-' is applied as an operator, but this '+' isn't.
IntegerLiteral
  = ("+" / "") "0x" n:[0-9]+
    { return { type: 'Literal', value: parseInt(n.join(''), 16) } }
  / ("+" / "") n:[0-9]+
    { return { type: 'Literal', value: parseInt(n.join(''), 10) } }

StringLiteral = "\"" s:StringCharacter* "\""
    { return { type: 'Literal', value: s.join('') } }

StringCharacter = StringEscapedCharacter / StringUnescapedCharacter

StringEscapedCharacter = "\\" !eol c:.
    { return '\\' + c }

StringUnescapedCharacter = !"\"" !eol c:.
    { return c }

// ---------- Whitespace and comments ----------

_ "whitespace" = __*
__ "whitespace character" = [ \t] / eol / comment
eol = "\n" / "\r" !"\n" / "\r\n"
comment = singlelineComment / multilineComment
singlelineComment = "//" (!eol .)* (!. / eol)
multilineComment = "/*" (!"*/" .)* "*/"
