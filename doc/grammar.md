# Grammar
1. %start_parser -> Program
1. Program -> TopDefs
1. TopDefs -> TopDef
1. TopDefs -> TopDefs TopDef
1. TopDef -> proc GenId "(" Params0 ")" ":-" Insts "."
1. TopDef -> proc GenId "(" Params0 ")" "->" Type ":-" Insts "."
1. TopDef -> either GenId ":-" Conts "."
1. TopDef -> record GenId ":-" Conts "."
1. TopDef -> Type VarId "."
1. TopDef -> Type VarId is Exp "."
1. GenId -> genId
1. Params0 ->
1. Params0 -> Params
1. Params -> Param
1. Params -> Params "," Param
1. Param -> Type VarId
1. Conts -> Cont
1. Conts -> Conts "," Cont
1. Cont -> Type VarId
1. Insts -> Inst
1. Insts -> Insts "," Inst
1. Inst -> Declaration
1. Inst -> Initialization
1. Inst -> Assign
1. Inst -> Call
1. Inst -> If
1. Inst -> Case
1. Inst -> For
1. Inst -> While
1. Inst -> read Lval
1. Inst -> write Exp
1. Inst -> finish
1. Declaration -> Type VarId
1. Initialization -> Type VarId is Exp
1. Type -> GenId
1. Type -> GenId ArraySize
1. ArraySize -> "{" Int "]"
1. ArraySize -> "[" Int "}"
1. ArraySize -> ArraySize "{" Int "]"
1. ArraySize -> ArraySize "[" Int "}"
1. Assign -> Lval is Exp
1. Lval -> VarId
1. Lval -> Lval "_" VarId
1. Lval -> Lval "{" Exp "]"
1. Lval -> Lval "[" Exp "}"
1. VarId -> varId
1. Call -> GenId "(" Args ")"
1. Args ->
1. Args -> Args1
1. Args1 -> Exp
1. Args1 -> Args1 "," Exp
1. If -> if Guards end
1. Guards -> Guard
1. Guards -> Guards ";" Guard
1. Guard -> Exp "->" Insts
1. Case -> case Exp of Sets end
1. Sets -> Set
1. Sets -> Sets ";" Set
1. Elems -> Exp
1. Elems -> Elems "," Exp
1. Set -> Elems "->" Insts
1. For -> for VarId Ranges end
1. For -> for Declaration Ranges end
1. Ranges -> Range
1. Ranges -> Ranges ";" Range
1. Range -> from Exp to Exp "->" Insts
1. While -> while Guards end
1. Exp -> "(" Exp ")"
1. Exp -> Bool
1. Exp -> Char
1. Exp -> Int
1. Exp -> Float
1. Exp -> String
1. Exp -> otherwise
1. Exp -> Lval
1. Exp -> toBoolean Exp
1. Exp -> toCharacter Exp
1. Exp -> toFloat Exp
1. Exp -> toInteger Exp
1. Exp -> Exp and Exp
1. Exp -> Exp andalso Exp
1. Exp -> Exp or Exp
1. Exp -> Exp orelse Exp
1. Exp -> Exp xor Exp
1. Exp -> not Exp
1. Exp -> Exp band Exp
1. Exp -> Exp bor Exp
1. Exp -> Exp bsl Exp
1. Exp -> Exp bsr Exp
1. Exp -> Exp bxor Exp
1. Exp -> bnot Exp
1. Exp -> length Exp
1. Exp -> Exp "+" Exp
1. Exp -> Exp "-" Exp
1. Exp -> Exp "*" Exp
1. Exp -> Exp "/" Exp
1. Exp -> Exp div Exp
1. Exp -> Exp rem Exp
1. Exp -> "-" Exp
1. Exp -> Exp "<" Exp
1. Exp -> Exp "=<" Exp
1. Exp -> Exp ">" Exp
1. Exp -> Exp ">=" Exp
1. Exp -> Exp "=" Exp
1. Exp -> Exp "/=" Exp
1. Exp -> Exp "|" Exp
1. Exp -> Exp "!|" Exp
1. Bool -> boolLit
1. Char -> charLit
1. Int -> intLit
1. Float -> floatLit
1. String -> stringLit
