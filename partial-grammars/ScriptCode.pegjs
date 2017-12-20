/*
  Handles npc script code. Can also be used to parse item scripts.
*/

/*
@import './Expression.pegjs'
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
    { return { type: 'Case' } }

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

// menu("hi", -); fails, because '-' can only be used if no parentheses are used
// menu "hi", -; would work, with '-' being a special label (points to next line)
CommandArgList
  = "(" _ ")"
    { return [] }
  / !("(" _ (Expression _ ",")+ _ Expression _ ")") args:CommandCallArgList
  	{ return args }
  / "(" _ args:FunctionCallArgList _ ")"
    { return args }

CommandCallArgList
  = head:(arg:CommandArg _ "," { return arg })* _ tail:CommandArg
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
