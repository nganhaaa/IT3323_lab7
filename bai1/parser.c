/* 
 * @copyright (c) 2008, Hedspi, Hanoi University of Technology
 * @author Huu-Duc Nguyen
 * @version 1.0
 */
#include <stdio.h>
#include <stdlib.h>

#include "reader.h"
#include "scanner.h"
#include "parser.h"
#include "semantics.h"
#include "error.h"
#include "debug.h"

Token *currentToken;
Token *lookAhead;

extern Type* intType;
extern Type* charType;
extern SymTab* symtab;

void scan(void) {
  Token* tmp = currentToken;
  currentToken = lookAhead;
  lookAhead = getValidToken();
  free(tmp);
}

void eat(TokenType tokenType) {
  if (lookAhead->tokenType == tokenType) {
    scan();
  } else missingToken(tokenType, lookAhead->lineNo, lookAhead->colNo);
}

void compileProgram(void) {
  Object* program;

  eat(KW_PROGRAM);
  eat(TK_IDENT);

  program = createProgramObject(currentToken->string);
  enterBlock(program->progAttrs->scope);

  eat(SB_SEMICOLON);

  compileBlock();
  eat(SB_PERIOD);

  exitBlock();
}

void compileBlock(void) {
  Object* constObj;
  ConstantValue* constValue;

  if (lookAhead->tokenType == KW_CONST) {
    eat(KW_CONST);

    do {
      eat(TK_IDENT);
      
      checkFreshIdent(currentToken->string);
      constObj = createConstantObject(currentToken->string);
      
      eat(SB_EQ);
      constValue = compileConstant();
      
      constObj->constAttrs->value = constValue;
      declareObject(constObj);
      
      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);

    compileBlock2();
  } 
  else compileBlock2();
}

void compileBlock2(void) {
  Object* typeObj;
  Type* actualType;

  if (lookAhead->tokenType == KW_TYPE) {
    eat(KW_TYPE);

    do {
      eat(TK_IDENT);
      
      checkFreshIdent(currentToken->string);
      typeObj = createTypeObject(currentToken->string);
      
      eat(SB_EQ);
      actualType = compileType();
      
      typeObj->typeAttrs->actualType = actualType;
      declareObject(typeObj);
      
      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);

    compileBlock3();
  } 
  else compileBlock3();
}

void compileBlock3(void) {
  Object* varObj;
  Type* varType;

  if (lookAhead->tokenType == KW_VAR) {
    eat(KW_VAR);

    do {
      eat(TK_IDENT);
      
      checkFreshIdent(currentToken->string);
      varObj = createVariableObject(currentToken->string);

      eat(SB_COLON);
      varType = compileType();
      
      varObj->varAttrs->type = varType;
      declareObject(varObj);
      
      eat(SB_SEMICOLON);
    } while (lookAhead->tokenType == TK_IDENT);

    compileBlock4();
  } 
  else compileBlock4();
}

void compileBlock4(void) {
  compileSubDecls();
  compileBlock5();
}

void compileBlock5(void) {
  eat(KW_BEGIN);
  compileStatements();
  eat(KW_END);
}

void compileSubDecls(void) {
  while ((lookAhead->tokenType == KW_FUNCTION) || (lookAhead->tokenType == KW_PROCEDURE)) {
    if (lookAhead->tokenType == KW_FUNCTION)
      compileFuncDecl();
    else compileProcDecl();
  }
}

void compileFuncDecl(void) {
  Object* funcObj;
  Type* returnType;

  eat(KW_FUNCTION);
  eat(TK_IDENT);

  checkFreshIdent(currentToken->string);
  funcObj = createFunctionObject(currentToken->string);
  declareObject(funcObj);

  enterBlock(funcObj->funcAttrs->scope);
  
  compileParams();

  eat(SB_COLON);
  returnType = compileBasicType();
  funcObj->funcAttrs->returnType = returnType;

  eat(SB_SEMICOLON);
  compileBlock();
  eat(SB_SEMICOLON);

  exitBlock();
}

void compileProcDecl(void) {
  Object* procObj;

  eat(KW_PROCEDURE);
  eat(TK_IDENT);

  checkFreshIdent(currentToken->string);
  procObj = createProcedureObject(currentToken->string);
  declareObject(procObj);

  enterBlock(procObj->procAttrs->scope);

  compileParams();

  eat(SB_SEMICOLON);
  compileBlock();
  eat(SB_SEMICOLON);

  exitBlock();
}

ConstantValue* compileUnsignedConstant(void) {
  ConstantValue* constValue = NULL;
  Object* obj;

  switch (lookAhead->tokenType) {
  case TK_NUMBER:
    eat(TK_NUMBER);
    constValue = makeIntConstant(currentToken->value);
    break;
  case TK_IDENT:
    eat(TK_IDENT);

    obj = checkDeclaredConstant(currentToken->string);
    constValue = duplicateConstantValue(obj->constAttrs->value);

    break;
  case TK_CHAR:
    eat(TK_CHAR);
    constValue = makeCharConstant(currentToken->string[0]);
    break;
  default:
    error(ERR_INVALID_CONSTANT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return constValue;
}

ConstantValue* compileConstant(void) {
  ConstantValue* constValue;

  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    constValue = compileConstant2();
    break;
  case SB_MINUS:
    eat(SB_MINUS);
    constValue = compileConstant2();
    constValue->intValue = - constValue->intValue;
    break;
  case TK_CHAR:
    eat(TK_CHAR);
    constValue = makeCharConstant(currentToken->string[0]);
    break;
  default:
    constValue = compileConstant2();
    break;
  }
  return constValue;
}

ConstantValue* compileConstant2(void) {
  ConstantValue* constValue = NULL;
  Object* obj;

  switch (lookAhead->tokenType) {
  case TK_NUMBER:
    eat(TK_NUMBER);
    constValue = makeIntConstant(currentToken->value);
    break;
  case TK_IDENT:
    eat(TK_IDENT);
    obj = checkDeclaredConstant(currentToken->string);
    if (obj->constAttrs->value->type == TP_INT)
      constValue = duplicateConstantValue(obj->constAttrs->value);
    else
      error(ERR_UNDECLARED_INT_CONSTANT,currentToken->lineNo, currentToken->colNo);
    break;
  default:
    error(ERR_INVALID_CONSTANT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return constValue;
}

Type* compileType(void) {
  Type* type = NULL;
  Type* elementType;
  int arraySize;
  Object* obj;

  switch (lookAhead->tokenType) {
  case KW_INTEGER: 
    eat(KW_INTEGER);
    type =  makeIntType();
    break;
  case KW_CHAR: 
    eat(KW_CHAR); 
    type = makeCharType();
    break;
  case KW_ARRAY:
    eat(KW_ARRAY);
    eat(SB_LSEL);
    eat(TK_NUMBER);

    arraySize = currentToken->value;

    eat(SB_RSEL);
    eat(KW_OF);
    elementType = compileType();
    type = makeArrayType(arraySize, elementType);
    break;
  case TK_IDENT:
    eat(TK_IDENT);
    obj = checkDeclaredType(currentToken->string);
    type = duplicateType(obj->typeAttrs->actualType);
    break;
  default:
    error(ERR_INVALID_TYPE, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return type;
}

Type* compileBasicType(void) {
  Type* type = NULL;

  switch (lookAhead->tokenType) {
  case KW_INTEGER: 
    eat(KW_INTEGER); 
    type = makeIntType();
    break;
  case KW_CHAR: 
    eat(KW_CHAR); 
    type = makeCharType();
    break;
  default:
    error(ERR_INVALID_BASICTYPE, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
  return type;
}

void compileParams(void) {
  if (lookAhead->tokenType == SB_LPAR) {
    eat(SB_LPAR);
    compileParam();
    while (lookAhead->tokenType == SB_SEMICOLON) {
      eat(SB_SEMICOLON);
      compileParam();
    }
    eat(SB_RPAR);
  }
}

void compileParam(void) {
  Object* param;
  Type* type;
  enum ParamKind paramKind = PARAM_VALUE;

  switch (lookAhead->tokenType) {
  case TK_IDENT:
    paramKind = PARAM_VALUE;
    break;
  case KW_VAR:
    eat(KW_VAR);
    paramKind = PARAM_REFERENCE;
    break;
  default:
    error(ERR_INVALID_PARAMETER, lookAhead->lineNo, lookAhead->colNo);
    break;
  }

  eat(TK_IDENT);
  checkFreshIdent(currentToken->string);
  param = createParameterObject(currentToken->string, paramKind, symtab->currentScope->owner);
  eat(SB_COLON);
  type = compileBasicType();
  param->paramAttrs->type = type;
  declareObject(param);
}

void compileStatements(void) {
  compileStatement();
  while (lookAhead->tokenType == SB_SEMICOLON) {
    eat(SB_SEMICOLON);
    compileStatement();
  }
}

void compileStatement(void) {
  switch (lookAhead->tokenType) {
  case TK_IDENT:
    compileAssignSt();
    break;
  case KW_CALL:
    compileCallSt();
    break;
  case KW_BEGIN:
    compileGroupSt();
    break;
  case KW_IF:
    compileIfSt();
    break;
  case KW_WHILE:
    compileWhileSt();
    break;
  case KW_FOR:
    compileForSt();
    break;
    // EmptySt needs to check FOLLOW tokens
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
    break;
    // Error occurs
  default:
    error(ERR_INVALID_STATEMENT, lookAhead->lineNo, lookAhead->colNo);
    break;
  }
}

Type* compileLValue(void) {
  Object* identifier;
  Type* resultType = NULL;

  eat(TK_IDENT);
  
  identifier = checkDeclaredLValueIdent(currentToken->string);
  
  // Validate that constants cannot be assigned
  if (identifier->kind == OBJ_CONSTANT)
    error(ERR_CONSTANT_ASSIGN, currentToken->lineNo, currentToken->colNo);

  // Determine type based on object kind
  switch(identifier->kind) {
    case OBJ_VARIABLE:
      resultType = identifier->varAttrs->type;
      // Handle array indexing if needed
      if (resultType->typeClass == TP_ARRAY)
        resultType = compileIndexes(resultType);
      break;
      
    case OBJ_PARAMETER:
      resultType = identifier->paramAttrs->type;
      break;
      
    case OBJ_FUNCTION:
      resultType = identifier->funcAttrs->returnType;
      break;
      
    default:
      error(ERR_INVALID_LVALUE, currentToken->lineNo, currentToken->colNo);
      break;
  }
  
  return resultType;
}

void compileLValueList(Type *lvalueTypes[], int *lvalueCount)
{
  Type *type;

  type = compileLValue();
  lvalueTypes[(*lvalueCount)++] = type;

  while (lookAhead->tokenType == SB_COMMA) {
    eat(SB_COMMA);
    type = compileLValue();
    lvalueTypes[(*lvalueCount)++] = type;
  }
}

void compileExpressionList(Type *expressionTypes[], int *expressionCount);

#define MAX_VARIABLES 100
void compileAssignSt(void) {
  Type *leftTypes[MAX_VARIABLES];
  Type *rightTypes[MAX_VARIABLES];
  int leftCount = 0, rightCount = 0;
  int idx;

  // Parse left-hand side values
  compileLValueList(leftTypes, &leftCount);

  eat(SB_ASSIGN);

  // Parse right-hand side expressions
  compileExpressionList(rightTypes, &rightCount);

  // Verify matching counts
  if(leftCount != rightCount) {
    error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
  }

  // Type checking for each assignment pair
  for (idx = 0; idx < leftCount; idx++) {
    checkTypeEquality(leftTypes[idx], rightTypes[idx]);
  }
}

void compileCallSt(void) {
  Object* proc;

  eat(KW_CALL);
  eat(TK_IDENT);

  proc = checkDeclaredProcedure(currentToken->string);

  compileArguments(proc->procAttrs->paramList);
}

void compileGroupSt(void) {
  eat(KW_BEGIN);
  compileStatements();
  eat(KW_END);
}

void compileIfSt(void) {
  eat(KW_IF);
  compileCondition();
  eat(KW_THEN);
  compileStatement();
  if (lookAhead->tokenType == KW_ELSE) 
    compileElseSt();
}

void compileElseSt(void) {
  eat(KW_ELSE);
  compileStatement();
}

void compileWhileSt(void) {
  eat(KW_WHILE);
  compileCondition();
  eat(KW_DO);
  compileStatement();
}

void compileForSt(void) {
  Object *var;
  Type *type1, *type2;

  eat(KW_FOR);
  eat(TK_IDENT);

  // check if the identifier is a variable
  var = checkDeclaredVariable(currentToken->string);

  eat(SB_ASSIGN);
  type1 = compileExpression();
  checkTypeEquality(var->varAttrs->type, type1);

  eat(KW_TO);
  type2 = compileExpression();
  checkTypeEquality(var->varAttrs->type, type2);

  eat(KW_DO);
  compileStatement();
}

void compileArgument(Object* param) {
  Type* argType;
  
  // Check if parameter requires reference (lvalue)
  if(param->paramAttrs->kind == PARAM_REFERENCE) {
    // For reference parameters, ensure argument is an lvalue
    if(lookAhead->tokenType != TK_IDENT) {
      error(ERR_TYPE_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
    }
    checkDeclaredLValueIdent(lookAhead->string);
  }
  
  // Compile expression and verify type compatibility
  argType = compileExpression();
  checkTypeEquality(argType, param->paramAttrs->type);
}

void compileArguments(ObjectNode* paramList) {
  ObjectNode *currentParam = paramList;

  if (lookAhead->tokenType == SB_LPAR) {
    eat(SB_LPAR);
    
    // Check if we have parameters to match
    if (currentParam == NULL)
      error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
    
    // Process first argument
    compileArgument(currentParam->object);
    currentParam = currentParam->next;
    
    // Process remaining arguments
    while (lookAhead->tokenType == SB_COMMA) {
      eat(SB_COMMA);
      
      if (currentParam == NULL)
        error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
      
      compileArgument(currentParam->object);
      currentParam = currentParam->next;
    }

    // Verify all parameters were provided
    if (currentParam != NULL)
      error(ERR_PARAMETERS_ARGUMENTS_INCONSISTENCY, currentToken->lineNo, currentToken->colNo);
    
    eat(SB_RPAR);
  } else {
    // Check FOLLOW set tokens
    switch(lookAhead->tokenType) {
      case SB_TIMES:
      case SB_SLASH:
      case SB_PLUS:
      case SB_MINUS:
      case KW_TO:
      case KW_DO:
      case SB_RPAR:
      case SB_COMMA:
      case SB_EQ:
      case SB_NEQ:
      case SB_LE:
      case SB_LT:
      case SB_GE:
      case SB_GT:
      case SB_RSEL:
      case SB_SEMICOLON:
      case KW_END:
      case KW_ELSE:
      case KW_THEN:
        break;
      default:
        error(ERR_INVALID_ARGUMENTS, lookAhead->lineNo, lookAhead->colNo);
    }
  }
}

void compileCondition(void) {
  Type *leftType, *rightType;

  // Evaluate left expression
  leftType = compileExpression();
  checkBasicType(leftType);

  // Parse comparison operator
  switch (lookAhead->tokenType) {
    case SB_EQ:
    case SB_NEQ:
    case SB_LE:
    case SB_LT:
    case SB_GE:
    case SB_GT:
      scan();
      break;
    default:
      error(ERR_INVALID_COMPARATOR, lookAhead->lineNo, lookAhead->colNo);
  }

  // Evaluate right expression
  rightType = compileExpression();
  
  // Verify type consistency
  checkTypeEquality(leftType, rightType);
}

void compileExpressionList(Type *expressionTypes[], int *expressionCount)
{
  Type *type;

  type = compileExpression();
  expressionTypes[(*expressionCount)++] = type;

  while (lookAhead->tokenType == SB_COMMA) {
    eat(SB_COMMA);
    type = compileExpression();
    expressionTypes[(*expressionCount)++] = type;
  }
}

Type* compileExpression(void) {
  Type* type;
  
  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    type = compileExpression2();
    checkIntType(type);
    break;
  case SB_MINUS:
    eat(SB_MINUS);
    type = compileExpression2();
    checkIntType(type);
    break;
  default:
    type = compileExpression2();
  }
  return type;
}

Type* compileExpression2(void) {
  Type* type;

  type = compileTerm();
  compileExpression3();

  return type;
}


void compileExpression3(void) {
  Type* type;

  switch (lookAhead->tokenType) {
  case SB_PLUS:
    eat(SB_PLUS);
    type = compileTerm();
    checkIntType(type);
    compileExpression3();
    break;
  case SB_MINUS:
    eat(SB_MINUS);
    type = compileTerm();
    checkIntType(type);
    compileExpression3();
    break;
    // check the FOLLOW set
  case KW_TO:
  case KW_DO:
  case SB_RPAR:
  case SB_COMMA:
  case SB_EQ:
  case SB_NEQ:
  case SB_LE:
  case SB_LT:
  case SB_GE:
  case SB_GT:
  case SB_RSEL:
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
  case KW_THEN:
    break;
  default:
    error(ERR_INVALID_EXPRESSION, lookAhead->lineNo, lookAhead->colNo);
  }
}

Type* compileTerm(void) {
  Type* type;

  type = compileFactor();
  compileTerm2();

  return type;
}

void compileTerm2(void) {
  Type* type;

  switch (lookAhead->tokenType) {
  case SB_TIMES:
    eat(SB_TIMES);
    type = compileFactor();
    checkIntType(type);
    compileTerm2();
    break;
  case SB_SLASH:
    eat(SB_SLASH);
    type = compileFactor();
    checkIntType(type);
    compileTerm2();
    break;
    // check the FOLLOW set
  case SB_PLUS:
  case SB_MINUS:
  case KW_TO:
  case KW_DO:
  case SB_RPAR:
  case SB_COMMA:
  case SB_EQ:
  case SB_NEQ:
  case SB_LE:
  case SB_LT:
  case SB_GE:
  case SB_GT:
  case SB_RSEL:
  case SB_SEMICOLON:
  case KW_END:
  case KW_ELSE:
  case KW_THEN:
    break;
  default:
    error(ERR_INVALID_TERM, lookAhead->lineNo, lookAhead->colNo);
  }
}

Type* compileFactor(void) {
  Object *identifier;
  Type *factorType = NULL;

  switch (lookAhead->tokenType) {
    case TK_NUMBER:
      eat(TK_NUMBER);
      factorType = intType;
      break;
      
    case TK_CHAR:
      eat(TK_CHAR);
      factorType = charType;
      break;
      
    case TK_IDENT:
      eat(TK_IDENT);
      identifier = checkDeclaredIdent(currentToken->string);

      // Handle different identifier kinds
      if (identifier->kind == OBJ_CONSTANT) {
        factorType = (identifier->constAttrs->value->type == TP_INT) ? intType : charType;
      } 
      else if (identifier->kind == OBJ_VARIABLE) {
        factorType = identifier->varAttrs->type;
        if (factorType->typeClass == TP_ARRAY)
          factorType = compileIndexes(factorType);
      } 
      else if (identifier->kind == OBJ_PARAMETER) {
        factorType = identifier->paramAttrs->type;
      } 
      else if (identifier->kind == OBJ_FUNCTION) {
        compileArguments(identifier->funcAttrs->paramList);
        factorType = identifier->funcAttrs->returnType;
      } 
      else {
        error(ERR_INVALID_FACTOR, currentToken->lineNo, currentToken->colNo);
      }
      break;
      
    default:
      error(ERR_INVALID_FACTOR, lookAhead->lineNo, lookAhead->colNo);
  }

  return factorType;
}

Type* compileIndexes(Type* arrayType) {
  Type *indexType;
  Type *currentType = arrayType;

  // Process each array index
  while (lookAhead->tokenType == SB_LSEL) {
    eat(SB_LSEL);
    
    // Compile and check index expression
    indexType = compileExpression();
    checkIntType(indexType);
    
    // Move to element type
    currentType = currentType->elementType;
    
    eat(SB_RSEL);
  }
  
  // Ensure final type is basic type
  checkBasicType(currentType);
  
  return currentType;
}

int compile(char *fileName) {
  if (openInputStream(fileName) == IO_ERROR)
    return IO_ERROR;

  currentToken = NULL;
  lookAhead = getValidToken();

  initSymTab();

  compileProgram();

  printObject(symtab->program,0);

  cleanSymTab();

  free(currentToken);
  free(lookAhead);
  closeInputStream();
  return IO_SUCCESS;

}