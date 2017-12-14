{ open Math }

let num = ['0'-'9']+
let space = [' ' '\t']*

rule main = parse
  | space      { main lexbuf               }
  | ">"        { GREATER                   }
  | ">="       { GREATEREQ                 }
  | "<"        { LESS                      }
  | "<="       { LESSEQ                    }
  | "=="       { EQUAL                     }
  | "!="       { NE                        }
  | "if"       { IF                        }
  | "then"     { THEN                      }
  | "else"     { ELSE                      }
  | "?"        { QUESTION                  }
  | ":"        { COLON                     }
  | "("        { LP                        }
  | ")"        { RP                        }
  | "&&"       { AND                       }
  | "||"       { OR                        }
  | "%"        { MOD                       }
  | num as n   { LITERAL (int_of_string n) }
  | "n" | "N"  { N                         }
  | eof        { EOF                       }
