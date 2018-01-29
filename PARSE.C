/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */

//1
static TreeNode * program(void);
//2
static TreeNode * declarations(void);
//3
static TreeNode * decl(void);
//4
static TreeNode * type_specifier(void);
//5
static TreeNode * varlist(ExpType );
//6
static TreeNode * stmt_sequence(void);
//7
static TreeNode * statement(void);
//8
static TreeNode * while_stmt(void);
//9
static TreeNode * if_stmt(void);
//10
static TreeNode * repeat_stmt(void);
//11
static TreeNode * assign_stmt(void);
//12
static TreeNode * read_stmt(void);
//13
static TreeNode * write_stmt(void);
//14
static TreeNode * exp(void);
//15-16
static TreeNode * arithmetic_exp(void);
//17-18
static TreeNode * term(void);
//19
static TreeNode * factor(void);
//20
static TreeNode * bool_exp(void);
//21
static TreeNode * bterm(void);
//22
static TreeNode * bfactor(void);
//23
static TreeNode * comparison_exp(void);
//25
static TreeNode * string_exp(void);



static void syntaxError(char * message)
{ fprintf(listing,"\n>>> ");
  fprintf(listing,"Syntax error at line %d: %s",lineno,message);
  Error = TRUE;
}

static void match(TokenType expected)
{ if (token == expected) token = getToken();
  else {
    syntaxError("unexpected token -> ");
    printToken(token,tokenString);
    fprintf(listing,"      ");
  }
}
//4.3
//1
//program -> declarations stmt_sequence
TreeNode * program(void)
{
	TreeNode * t = declarations();
	TreeNode * t_tmp = t;
	while (t_tmp->sibling != NULL)
		t_tmp = t_tmp->sibling;
	if (t_tmp != NULL) t_tmp->sibling = stmt_sequence();
	t_tmp = NULL;
	return t;
}

//2
//declarations-> decl;declarations|$
//declarations-> (decl;)*,
TreeNode * declarations(void)
{
	TreeNode * t = decl();
	match(SEMI);
	TreeNode * p = t;
	while ((token != ENDFILE) && (token != END))
	{
		TreeNode * q = NULL;
		q = decl();
		if (q != NULL) {
			if (t == NULL) t = p = q;
			else 
			{
				p->sibling = q;
				p = q;
			}
	 		match(SEMI);
		}
		else {
			break;
		}
	}
	return t;
}

//3 
//decl-> type_specifier varlist
TreeNode * decl(void)
{
	TreeNode * t = type_specifier();
	if (t != NULL) {
		ExpType t_type_tmp = t->type;
		t->child[0] = varlist(t_type_tmp);
	}
	return t;
}
//4
//type_specifier-> int | bool | char
TreeNode * type_specifier(void)
{
	TreeNode * t = NULL;
	switch(token) {
		case INT:
			t = newDefineNode(IntD);
			t->attr.name = "int";
			t->type = Integer;
			match(INT);
			break;
		case CHAR:
			t = newDefineNode(CharD);
			t->attr.name = "char";
			t->type = Char;
			match(CHAR);
			break;
		case BOOL:
			t = newDefineNode(BoolD);
			t->attr.name = "bool";
			t->type = Boolean;
			match(BOOL);
			break;
		default:syntaxError("unexpected token -> ");
			printToken(token, tokenString);
			token = getToken();
			break;
	}
	return t;
}

//5   
//varlist -> identifier { , identifier }
TreeNode * varlist(ExpType t_type)
{
	TreeNode * t = NULL;
	if (token = ID)
	{
		t = newExpNode(IdK);
		t->attr.name = copyString(tokenString);
		t->type = t_type;
		match(ID);
	}
	TreeNode * p = t;
	while ((token != ENDFILE) && (token != END) &&
		(token != ELSE) && (token != UNTIL))
	{
		TreeNode * q;
		match(COMMA);
		q = newExpNode(IdK);
		match(ID);
		if (q != NULL) {
			if (t == NULL) t = p = q;
			else {
				p->sibling = q;
				p = q;
			}
		}
	}
	return t;
}

//6
//stmt_sequence -> statement { ; statement }
TreeNode * stmt_sequence(void)
{ 
  TreeNode * t = statement();
  TreeNode * p = t;
  while ((token!=ENDFILE) && (token!=END) &&
         (token!=ELSE) && (token!=UNTIL))
  { TreeNode * q;
    match(SEMI);
    q = statement();
    if (q!=NULL) {
      if (t==NULL) t = p = q;
      else /* now p cannot be NULL either */
      { p->sibling = q;
        p = q;
      }
    }
  }
  return t;
}
//7
//statement -> if-stmt | repeat-stmt | assign-stmt | read-stmt | write-stmt | while-stmt
TreeNode * statement(void)
{ TreeNode * t = NULL;
  switch (token) {
    case IF : t = if_stmt(); break;
    case REPEAT : t = repeat_stmt(); break;
    case ID : t = assign_stmt(); break;
    case READ : t = read_stmt(); break;
    case WRITE : t = write_stmt(); break;
	case WHILE: t = while_stmt(); break;
    default : syntaxError("unexpected token -> ");
              printToken(token,tokenString);
              token = getToken();
              break;
  } /* end case */
  return t;
}
//4.3
//8
//while-stmt -> while bool-exp do stmt-sequence end
TreeNode * while_stmt(void)
{
	TreeNode * t = newStmtNode(IfK);
	match(WHILE);
	if (t != NULL) t->child[0] = bool_exp();
	match(DO);
	if (t != NULL) t->child[1] = stmt_sequence();
	match(END);
	return t;

}
//4.3
//9
//if-stmt-> if  bool-exp then stmt-sequence [else stmt-sequence] end
TreeNode * if_stmt(void)
{
	TreeNode * t = newStmtNode(IfK);
	match(IF);
	if (t != NULL) t->child[0] = bool_exp();
	match(THEN);
	if (t != NULL) t->child[1] = stmt_sequence();
	if (token == ELSE) {
		match(ELSE);
		if (t != NULL) t->child[2] = stmt_sequence();
	}
	match(END);
	return t;
}
//10
//repeat-stmt-> repeat stmt_sequence until bool_exp
TreeNode * repeat_stmt(void)
{ TreeNode * t = newStmtNode(RepeatK);
  match(REPEAT);
  if (t!=NULL) t->child[0] = stmt_sequence();
  match(UNTIL);
  if (t!=NULL) t->child[1] = bool_exp();
  return t;
}

//11
//assign-stmt-> identifier:=exp

TreeNode * assign_stmt(void)
{ TreeNode * t = newStmtNode(AssignK);
  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  match(ID);
  match(ASSIGN);
  if (t!=NULL) t->child[0] = exp();
  return t;
}

//12
//read_stmt->read identifier
TreeNode * read_stmt(void)
{ TreeNode * t = newStmtNode(ReadK);
  match(READ);
  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  match(ID);
  return t;
}

//13
//write_stmt->write exp
TreeNode * write_stmt(void)
{ TreeNode * t = newStmtNode(WriteK);
  match(WRITE);
  if (t!=NULL) t->child[0] = exp();
  return t;
}

//14
//exp-> arithmetic_exp | bool_exp | string_exp
TreeNode * exp(void)
{ 
	TreeNode * t = NULL;
	switch (token) 
	{
	case STR :
		t = string_exp();
		break;
	case PLUS||MINUS:
		t = arithmetic_exp();
		break;
	default:
		t = bool_exp();
		break;
	}
	return;
}
//15-16
//arithmetic_exp-> term { addop term } 
TreeNode * arithmetic_exp(void)
{
	TreeNode * t = term();
	while ((token == PLUS) || (token == MINUS))
	{
		TreeNode * p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			t->child[1] = term();
		}
	}
	return t;
}

//17-18
//term-> factor { mulop factor }
TreeNode * term(void)
{ TreeNode * t = factor();
  while ((token==TIMES)||(token==OVER))
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      p->child[1] = factor();
    }
  }
  return t;
}


//19
//factor->  (arithmetic_exp) | number | identifier
TreeNode * factor(void)
{ TreeNode * t = NULL;
  switch (token) {
    case NUM :
      t = newExpNode(ConstK);
	  if ((t != NULL) && (token == NUM)) {
		  t->attr.val = atoi(tokenString);
		  t->type = Integer;
	  }
      match(NUM);
      break;
    case ID :
      t = newExpNode(IdK);
      if ((t!=NULL) && (token==ID))
        t->attr.name = copyString(tokenString);
      match(ID);
      break;
    case LPAREN :
      match(LPAREN);
      t = exp();
      match(RPAREN);
      break;
    default:
      syntaxError("unexpected token -> ");
      printToken(token,tokenString);
      token = getToken();
      break;
    }
  return t;
}
//20
//bool_exp-> bterm { or bterm }
TreeNode * bool_exp(void)
{
	TreeNode * t = bterm();
	while (token == OR)
	{
		TreeNode * p = newExpNode(LogicOpk);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			t->child[1] = bterm();
		}
	}
	return t;
}

//21
//bterm-> bfactor { and  bfactor}
TreeNode * bterm(void)
{
	TreeNode * t = bfactor();
	while (token == AND)
	{
		TreeNode * p = newExpNode(LogicOpk);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			t->child[1] = bfactor();
		}
	}
	return t;
}

//22
//bfactor-> comparison-exp
TreeNode * bfactor(void)
{
	TreeNode * t = comparison_exp();
	return t;
}

//23-24
//23comparison_exp-> arithmetic_exp comparison_op arithmetic-exp
TreeNode * comparison_exp(void)
{
	TreeNode * t = arithmetic_exp();
	if ((token == LT) || (token == LARGERT) || (token == EQ) || (token == LESSEQ) || (token == LARGEREQ)) {
		TreeNode * p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
		}
		match(token);
		if (t != NULL)
			t->child[1] = arithmetic_exp();
	}
	return t;
}
//25
//string_exp-> string 
TreeNode * string_exp(void)
{
	TreeNode * t = NULL;
	t = newExpNode(StringK);
	if ((t != NULL) && (token == STR))
		t->attr.name = copyString(tokenString);
	match(STR);
	return t;
}
/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly 
 * constructed syntax tree
 */
TreeNode * parse(void)
{ TreeNode * t;
  token = getToken();
  t = stmt_sequence();
  if (token!=ENDFILE)
    syntaxError("Code ends before file\n");
  return t;
}
