# SimpleC-Compiler
F# Program to semantically check simple C programs.

# Lexer
Lexical analyzer for simple C programs. This component 
 translates the input into a list of lexical units or
 "tokens". Each token is a string, so the end result
 is a list of strings.

 Returns: a list of strings, for example:
   ["void";"main";"(";")";"{";"int";"identifier:x",...]
   
# Parser
Parser for simple C programs. This component checks 
 the input program to see if it meets the syntax rules
 of simple C. The parser returns the string "success" 
 if the input program is legal, otherwise the string 
 "syntax_error: ..." is returned denoting an invalid 
 simple C program.
 
# Analyzer
Analyzer for simple C programs.  This component performs
 semantic analysis, in particular collecting variable
 names and their types. The analysis also checks to ensure
 variable names are unique --- no duplicates.
 
# Checker
Checker for simple C programs.  This component performs
 type checking.  The analyzer returns a string denoting
 success or failure. The string "success" if the input 
 program is legal, otherwise the string "type_error: ..." 
 is returned denoting an invalid simple C program.
