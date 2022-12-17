//
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
//
// Modified by:
//   Sean Kudrna
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //


  // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation:
    //
    // NOTE: identifier, int_literal and str_literal
    // are special cases because they are followed by
    // the name or literal value. In these cases exact
    // matching will not work, so we match the start 
    // of the token in these cases.
    //
    let next_token = List.head tokens

    if expected_token = "identifier" && next_token.StartsWith("identifier") then
      //
      // next_token starts with identifier, so we have a match:
      //
      List.tail tokens
    elif expected_token = "int_literal" && next_token.StartsWith("int_literal") then
      //
      // next_token starts with int_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = "real_literal" && next_token.StartsWith("real_literal") then
      //
      // next_token starts with int_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = "str_literal" && next_token.StartsWith("str_literal") then
      //
      // next_token starts with str_literal, so we have a match:
      //
      List.tail tokens
    elif expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


  //
  // <expr-value> -> identifier
  //               | int_literal
  //               | real_literal
  //               | str_literal
  //               | true
  //               | false
  //
  let rec private expr_value tokens symbolTable =
    let next_token = List.head tokens
    //
    if next_token = "false" then
      let T2 = matchToken "false" tokens
      (T2, "bool")
    elif next_token = "true" then
      let T2 = matchToken "true" tokens
      (T2, "bool")
    //
    // the others are trickier since we have to look 
    // at the start of the string for a match:
    //
    elif next_token.StartsWith("identifier") then
      let T2 = matchToken "identifier" tokens

      // Get variable name and check to see if it already exists 
      let varName = next_token.Substring(11)

      if not(List.contains (varName, "int") symbolTable || List.contains (varName, "real") symbolTable) then
        failwith ("variable '" + varName + "' undefined")

      // At this point, we know the variable exists, figure out what type it is, then return that type with tokens
      else 
        if (List.contains (varName, "int") symbolTable) then
          (T2, "int")
        else
          (T2, "real")

    elif next_token.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      (T2, "int")
    elif next_token.StartsWith("real_literal") then
      let T2 = matchToken "real_literal" tokens
      (T2, "real")
    elif next_token.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      (T2, "str")
    else
      failwith ("expecting identifier or literal, but found " + next_token)


  //
  // <expr-op> -> +
  //            | -
  //            | *
  //            | /
  //            | ^
  //            | <
  //            | <=
  //            | >
  //            | >=
  //            | ==
  //            | !=
  //
  let rec private expr_op tokens = 
    let next_token = List.head tokens
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T2 = matchToken next_token tokens
      T2
    else
      // error
      failwith ("expecting expression operator, but found " + next_token)


  //
  // <expr> -> <expr-value> <expr-op> <expr-value>
  //         | <expr-value>
  //
  let rec private expr tokens symbolTable = 
    //
    // first we have to match expr-value, since both
    // rules start with this:
    //
    let (T2, leftType) = expr_value tokens symbolTable

    //
    // now let's see if there's more to the expression:
    //
    let next_token = List.head T2
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then

      let T3 = expr_op T2

      let (T4, rightType) = expr_value T3 symbolTable

      // Arithemtic operation type-check
      if next_token = "+" ||
         next_token = "-" || 
         next_token = "*" ||
         next_token = "/" ||
         next_token = "^" then

        // Check if types are ints or reals
        if (rightType = "str") || (rightType = "bool") || (leftType = "str") || (leftType = "bool") then
          failwith ("operator " + next_token + " must involve 'int' or 'real'")
  
        if not(leftType = rightType) then
          failwith ("type mismatch '" + leftType + "' " + next_token + " '" + rightType + "'")


      // Comparison operation type-check
      elif next_token = "<"  ||
           next_token = "<=" || 
           next_token = ">"  ||
           next_token = ">=" ||
           next_token = "==" ||
           next_token = "!=" then
        
        if not(leftType = rightType) then
          failwith ("type mismatch '" + leftType + "' " + next_token + " '" + rightType + "'")

        if (leftType = "real" && next_token = "==") then
          printfn "warning: comparing real numbers with == may never be true"


      // Arithemtic operation type-check
      if next_token = "+" ||
         next_token = "-" || 
         next_token = "*" ||
         next_token = "/" ||
         next_token = "^" then
        (T4, rightType)

      else
        (T4, "bool")
    else
      // just expr_value, that's it
      (T2, leftType)


  //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    let T2 = matchToken ";" tokens
    T2


  //
  // <vardecl> -> int identifier ;
  //            | real identifier ;
  //
  let rec private vardecl tokens symbolTable = 
    let next_token = List.head tokens

    if next_token = "int" then
      let T2 = matchToken "int" tokens
      let T3 = matchToken "identifier" T2
      
      let T4 = matchToken ";" T3
      (T4, symbolTable)
    else 
      let T2 = matchToken "real" tokens
      let T3 = matchToken "identifier" T2

      let T4 = matchToken ";" T3
      (T4, symbolTable)


  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    T5


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens symbolTable = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      (T2, "nontype")
    else
      let (T2, varType) = expr_value tokens symbolTable
      (T2, varType)


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens symbolTable = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let (T4, varType)  = output_value T3 symbolTable
    let T5 = matchToken ";" T4
    T5


  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment tokens symbolTable = 
    let T2 = matchToken "identifier" tokens

    // Get variable name and check to see if exists 
    let varName = (List.head tokens).Substring(11)
    if not(List.contains (varName, "int") symbolTable || List.contains (varName, "real") symbolTable) then
        failwith ("variable '" + varName + "' undefined")
  
    let T3 = matchToken "=" T2
    let (T4, rightType) = expr T3 symbolTable
    let T5 = matchToken ";" T4

    // check for int match
    if (List.contains (varName, "int") symbolTable) then
      if not (rightType = "int") then
        failwith ("cannot assign '" + rightType + "' to variable of type 'int'")

    // check for real match
    if (List.contains (varName, "real") symbolTable) then
      if not (rightType = "real") then
        failwith ("cannot assign '" + rightType + "' to variable of type 'real'")
    T5


  //
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens symbolTable = 
    let next_token = List.head tokens
    //
    // use the next token to determine which rule
    // to call; if none match then it's a syntax
    // error:
    //
    if next_token = ";" then
      let T2 = empty tokens
      (T2, symbolTable)
    elif next_token = "int" || next_token = "real" then
      let (T2, ST2) = vardecl tokens symbolTable
      (T2, ST2) 
    elif next_token = "cin" then
      let T2 = input tokens
      (T2, symbolTable)
    elif next_token = "cout" then
      let T2 = output tokens symbolTable
      (T2, symbolTable)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens symbolTable
      (T2, symbolTable)
    elif next_token = "if" then
      let (T2, ST2) = ifstmt tokens symbolTable // symbolTable being updated
      (T2, ST2)
    else
      failwith ("expecting statement, but found " + next_token)
  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt tokens symbolTable = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3 symbolTable
    let T5 = matchToken ")" T4
    let (T6, ST2) = then_part T5 symbolTable // we just need to pass a symbolTable to these function in order to pass to stmt
    let (T7, ST3) = else_part T6 symbolTable // since the assignment says vardecl wont happen in if else, we dont need to return
    (T7, symbolTable)                        // the updated symbolTable
  //
  // <condition> -> <expr>
  //
  and private condition tokens symbolTable = 
    let (T2, exprType) = expr tokens symbolTable
    if not(exprType = "bool") then
      failwith ("if condition must be 'bool', but found '" + exprType + "'")
    T2
  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens symbolTable = 
    let (T2, ST2) = stmt tokens symbolTable
    (T2, symbolTable)
  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens symbolTable = 
    let next_token = List.head tokens
    //
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let (T3, ST2) = stmt T2 symbolTable
      (T3, symbolTable)
    else
      // EMPTY, do nothing but return tokens back
      (tokens, symbolTable)


  //
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts tokens symbolTable = 
    //
    // if the next token denotes the start of a stmt 
    // then process stmt and morestmts, otherwise apply
    // EMPTY
    //
    let next_token = List.head tokens
    //
    if next_token = ";"    ||
       next_token = "int"  ||
       next_token = "real" ||
       next_token = "cin"  ||
       next_token = "cout" ||
       next_token.StartsWith("identifier") ||
       next_token = "if" then
      //
      let (T2, ST2) = stmt tokens symbolTable
      let (T3, ST3) = morestmts T2 ST2
      (T3, ST3)
    else 
      // EMPTY => do nothing, just return tokens back
      (tokens, symbolTable)


  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens symbolTable = 
    let (T2, ST2) = stmt tokens symbolTable
    let (T3, ST3) = morestmts T2 ST2
    (T3, ST3)


  //
  // <simpleC> -> void main ( ) { <stmts> } $
  //
  let private simpleC tokens symbolTable = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7, ST2) = stmts   T6 symbolTable  // T7 is tokens ST2 is updated symbol table returned from stmts
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8  // $ => EOF, there should be no more tokens
    T9


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message

