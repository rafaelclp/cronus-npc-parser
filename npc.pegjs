// This contains the full grammar (written so far)

{
  const constructBinaryExpression = (head, tail) => (
    tail.reduce((left, [_0, op, _1, right]) => (
      { type: 'BinaryExpression', left, op, right }
    ), head)
  )
}

/*
  Handles npc expressions.
  e.g. in { mes "Hello world"; }, "Hello world" is an Expression.
  e.g. in { set .@a, 5 + .@b; }, both .@a and 5 + .@b are Expressions.
  e.g. in { set .@a, getd(.@s$); }, both .@a and get(.@s$) are Expressions.
  Notice how the main command itself is not an expression. However:
  e.g. in { .@a = 5; }, .@a = 5 is an Expression.
*/

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
  = head:MultiplicativeExpression tail:(_ ("+" / "-") _ MultiplicativeExpression)*
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
  = UnaryAssignmentExpression
  / sign:MultiNegationSign _ right:UnaryAssignmentExpression
    { return { type: 'UnaryExpression', op: sign < 0 ? '-' : '+', right } }

// Scripts accept "- - 3" for example, but not "--3". "-+3" is acceptable; "-+3"
// just applies "-" to "+3"! Multiple "-" *MUST* be separated by whitespace(s)!
MultiNegationSign = head:"-"? tail:(__+ "-" { return -1 })*
    { return tail.reduce((res, sign) => res * sign, head ? -1 : 1) }

// ---------- Assignment ----------

UnaryAssignmentExpression
  = left:Variable _ op:("++" / "--")
    { return { type: 'AssignmentExpression', left, op } }
  / Operand

AssignmentExpression = left:Variable _ op:AssignmentOperator _ right:Expression
    { return { type: 'AssignmentExpression', left, op, right } }

AssignmentOperator
  = "*=" / "/=" / "%=" / "+=" / "-=" / "<<=" / ">>=" / "&=" / "^=" / "|="
  / $("=" !"=")

// ---------- Operand ----------

Operand
  = "(" _ expr:Expression _ ")"
    { return expr }
  / AssignmentExpression
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

// ---------- Label ----------

LabelCreation = name:LabelIdentifier _ ":"
    { return { type: 'Label', name } }

// Only matched when creating labels. In functions like 'goto', we handle the label as if it was a
// Variable, since there is no way to know what we are looking for unless we know what the function
// expects to receive. Adding that to the grammar would come with associated unnecessary complexity.
LabelIdentifier = ident:VarIdentifier
    { return ident.name }

// ---------- Variable ----------

//  TODO(?): reject variables whose name is a reserved word or a function

Variable = ident:VarIdentifier indexExpr:ArrayIndex?
    { return { ...ident, index: indexExpr } }

ArrayIndex = "[" _ expr:Expression _ "]"
    { return expr }

// Variables such as '$', '$$', '$@', '@$', '$@$', '.' are all valid...
VarIdentifier
  = scope:VariableScope name:IdentifierName? type:VariableType
    { return { type: 'Identifier', name: scope + (name || '') + type } }
  / name:IdentifierName type:VariableType
    { return { type: 'Identifier', name: name + type } }

VariableScope
  = "$@" // global
  / "$"  // globalPerm
  / "'"  // instance
  / ".@" // npcPlayer
  / "."  // npc
  / "##" // accountPerm2
  / "#"  // accountPerm
  / "@"  // player
// / ""   // playerPerm

VariableType
  = "$"  // string
  / ""   // integer

// ---------- Identifier name & Literals ----------

IdentifierName = s:[0-9a-zA-Z_]+
    { return s.join('') }

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
