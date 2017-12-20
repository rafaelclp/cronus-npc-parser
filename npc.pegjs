{
  const constructBinaryExpression = (head, tail) => (
    tail.reduce((left, [_0, op, _1, right]) => (
      { type: 'BinaryExpression', left, op, right }
    ), head)
  )
}

// @ScriptCode.pegjs
/*
  Handles npc script code.
*/

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
  / LabelCreation
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
    { return { type: 'Case' } }

CaseLabel
    = IntegerLiteral
    / name:SimpleIdentifierName
      { return { type: 'Constant', name } }

// ---------- Function ----------

FunctionStatement = name:FunctionDefinition _ "{" _ code:ScriptCode _ "}"
    { return { type: 'Function', name, code } }

FunctionDefinition = "function"i &ReservedWordSeparator _ name:CommandIdentifierName
    { return name }

// ---------- Condition ----------

IfElseStatement = ifStmt:IfStatement _ falseStmt:ElseDefinition?
    { return { ...ifStmt, falseStmt } }

IfStatement = "if"i _ "(" cond:Expression _ ")" _ trueStmt:Statement
    { return { type: 'Conditional', cond, trueStmt } }

ElseDefinition = "else"i &ReservedWordSeparator _ falseStmt:Statement
    { return falseStmt }

// ---------- Loop ----------

ForStatement
  = "for"i _ "(" _ init:Command _ ";" _ cond:Expression _ ";" _ inc:Command _ ")" _ stmt:Statement
    { return { type: 'Loop', init: [init], cond, inc, stmt } }

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

CommandArgList
  = !("(" _ (Expression _ ",")+ _ Expression _ ")") args:FunctionCallArgList
  	{ return args }
  / "(" _ args:FunctionCallArgList _ ")"
    { return args }

// ---------- AssignmentCommand ----------

AssignmentCommand = UnaryAssignmentCommand / BinaryAssignmentCommand

UnaryAssignmentCommand = left:CommandVariable _ op:("++" / "--")
    { return { type: 'Command', name: '=', args: [op, left] } }

BinaryAssignmentCommand = left:CommandVariable _ op:AssignmentOperator _ right:Expression
    { return { type: 'Command', name: '=', args: [op, left, right] } }

CommandVariable = name:CommandIdentifierName indexExpr:ArrayIndex?
    { return { type: 'Variable', name, index: indexExpr } }

// ---------- Label creation ----------

LabelCreation = name:CommandIdentifierName _ ":"
    { return { type: 'Label', name } }


// @Expression.pegjs
/*
  Handles npc expressions.
  e.g. in { mes "Hello world"; }, "Hello world" is an Expression.
  e.g. in { set .@a, 5 + .@b; }, both .@a and 5 + .@b are Expressions.
  e.g. in { set .@a, getd(.@s$); }, both .@a and get(.@s$) are Expressions.
  Notice how the main command itself is not an expression. However:
  e.g. in { .@a = 5; }, .@a = 5 is an Expression.
*/

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
  / sign:MultiNegationSign _ right:AssignmentExpression
    { return { type: 'UnaryExpression', op: sign < 0 ? '-' : '+', right } }
  / Operand

// Scripts accept "- - 3" for example, but not "--3". "-+3" is acceptable; "-+3"
// just applies "-" to "+3"! Multiple "-" *MUST* be separated by whitespace(s)!
MultiNegationSign = head:"-"? tail:(__+ "-" { return -1 })*
    { return tail.reduce((res, sign) => res * sign, head ? -1 : 1) }

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
  = head:(expr:Expression _ "," { return expr })* _ tail:Expression
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

// ---------- Whitespace ----------

_ "whitespace" = __*
__ "whitespace character" = [ \t] / eol
eol = "\n" / "\r" !"\n" / "\r\n"
