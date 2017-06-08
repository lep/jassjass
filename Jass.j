library Jass initializer init

public keyword List

globals
    public key LPAREN
    public key RPAREN
    public key RBRACKET
    public key LBRACKET
    public key NEQ
    public key EQ
    public key LEQ
    public key LT
    public key GEQ
    public key GT
    public key PLUS
    public key MINUS
    public key MUL
    public key DIV
    public key AND
    public key OR
    public key NOT
    public key CALL
    public key SET
    public key LOCAL
    public key GLOBALS
    public key ENDGLOBALS
    public key TYPE
    public key EXTENDS
    public key NATIVE
    public key CONSTANT
    public key FUNCTION
    public key TAKES
    public key RETURNS
    public key RETURN
    public key IF
    public key ELSEIF
    public key THEN
    public key ELSE
    public key ENDIF
    public key LOOP
    public key EXITWHEN
    public key ENDLOOP
    public key ENDFUNCTION
    public key NULL
    public key TrueTok
    public key FalseTok
    public key ARRAY
    public key NL
    public key COMMA
    public key NOTHING
    public key IDENT
    public key INTEGER
    public key REAL
    public key STRING
    public key CODE
    public key BOOL
    public key HEX
    public key OCT
    public key RAWCODE
    public key EqualTok
    
    public constant integer EOF = 0
    
    public string array NAMES
    public string array OPERATORS
    
    private integer array hex

    public List Nil = 0
endglobals

public struct List
    integer head = 0
    List tail = 0
endstruct

public function cons takes integer hd, List tl returns List
    local List l = List.create()
    set l.head = hd
    set l.tail = tl
    return l
endfunction

public keyword Statement
public keyword NormalDecl
public keyword Expr

private function B2S takes boolean b returns string
    if b then
        return "true"
    else
        return "false"
    endif
endfunction


private function print takes string s returns nothing
    call DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, 120, s)
endfunction

private function trace takes string s returns real
    call print(s)
    return 0.0
endfunction

private function error takes string s returns nothing
    local integer i
    call print(s)
    set i = 1/0
endfunction

private function assert takes boolean b, string s returns nothing
    if not b then
        call error("assert: "+ s)
    endif
endfunction




/*************************\
* * * * * * * * * * * * * *
*                         *
*                         *
*                         *
*        A S T            *
*                         *
*                         *
*                         *
* * * * * * * * * * * * * *
\*************************/

public struct Ast
    stub method toString takes nothing returns string
        return ""
    endmethod
endstruct

public struct Toplevel extends Ast
endstruct

public struct VarDecl extends Ast
endstruct

public struct Globals extends Toplevel
    List /*VarDecl*/ glbls
    
    static method create takes List defs returns thistype
        local thistype this = allocate()
        set .glbls = defs
        return this
    endmethod
    
    method toString takes nothing returns string
        local string ret = "(globals \n"
        local List l = glbls
        loop
        exitwhen l == Nil
            set ret = ret + VarDecl(l.head).toString() +"\n"
            set l = l.tail
        endloop
        return ret + ")\n"
    endmethod
endstruct

public struct TypeDef extends Toplevel
    string base
    string new
    
    static method create takes string base, string new returns thistype
        local thistype this = allocate()
        set .base = base
        set .new = new
        return this
    endmethod
    
    method toString takes nothing returns string
        return "(type "+ new +" "+ base +")\n"
    endmethod
endstruct

public struct TypeAndName extends Ast
    string type
    string name
    
    static method create takes string typ, string name returns thistype
        local thistype this = allocate()
        set .name = name
        set .type = typ
        return this
    endmethod
    
    method toString takes nothing returns string
        return .type +" : "+ .name
    endmethod
endstruct

public struct Native extends Toplevel
    boolean const
    string name
    List /*name, type*/ params
    string returntype
    
    static method create takes boolean const, string name, List paramlist, string returntype returns thistype
        local thistype this = allocate()
        set .const = const
        set .name = name
        set .params = paramlist
        set .returntype = returntype
        return this
    endmethod
    
    method toString takes nothing returns string
        local List l = params
        local string ret = "(native "+ name +" "
        loop
        exitwhen l == Nil
            set ret = ret + TypeAndName(l.head).toString() +" "
            set l = l.tail
        endloop
        
        set ret = ret + "-> "+ returntype +")\n"
        return ret
    endmethod
endstruct

public struct Function extends Toplevel
    boolean const
    string name
    List /*name, type*/ params
    string returntype
    List /*Statement*/ body
    
    static method create takes boolean const, string name, List paramlist, string returntype, List body returns thistype
        local thistype this = allocate()
        set .const = const
        set .name = name
        set .params = paramlist
        set .returntype = returntype
        set .body = body
        return this
    endmethod
    
    method toString takes nothing returns string
        local List l = params
        local string ret = "(function "+ name +" "
        loop
        exitwhen l == Nil
            set ret = ret + TypeAndName(l.head).toString() +" "
            set l = l.tail
        endloop
        
        set ret = ret + "-> "+ returntype +"\n"
        
        set l = body
        loop
        exitwhen l == Nil
            set ret = ret + Statement(l.head).toString()
            set l = l.tail
        endloop
        
        return ret + ")\n"
    endmethod
endstruct

public struct ArrayDecl extends VarDecl
    string name
    string typ
    
    static method create takes string t, string name returns thistype
        local thistype this = allocate()
        set .name = name
        set .typ = t
        return this
    endmethod
    
    method toString takes nothing returns string
        return "(def "+ name +" : "+ typ +"-array)"
    endmethod

endstruct

public struct NormalDecl extends VarDecl
    boolean const
    string name
    string typ
    Expr init
    
    static method create takes boolean const, string t, string name, Expr init returns thistype
        local thistype this = allocate()
        set .name = name
        set .typ = t
        set .init = init
        return this
    endmethod
    
    method toString takes nothing returns string
        if init == 0 then
            return "(def "+ name +" : "+ typ +")"
        else
            return "(def "+ name +" : "+ typ +" "+ init.toString() +")"
        endif
    endmethod
endstruct

public struct Statement extends Ast
endstruct

public struct Var extends Ast
endstruct

public struct Expr extends Ast
endstruct

public struct ArrayVar extends Var
    string name
    Expr idx

    static method create takes string name, Expr idx returns thistype
        local thistype this = allocate()
        set .name = name
        set .idx = idx
        return this
    endmethod
    
    method toString takes nothing returns string
        return "([] "+ name + " " + idx.toString() +")"
    endmethod
endstruct

public struct NormalVar extends Var
    string name
    
    static method create takes string name returns thistype
        local thistype this = allocate()
        set .name = name
        return this
    endmethod
    
    method toString takes nothing returns string
        return name
    endmethod
endstruct

public struct Set extends Statement
    Var var
    Expr value
    
    static method create takes Var v, Expr e returns thistype
        local thistype this = allocate()
        set .var = v
        set .value = e
        return this
    endmethod
    
    method toString takes nothing returns string
        return "(set "+ var.toString() +" "+ value.toString() +")\n"
    endmethod
endstruct

public struct Call extends Ast
    string name
    List /*Expr*/ args

    static method create takes string s, List /*Expr*/ l returns thistype
        local thistype this = allocate()
        set name = s
        set .args = l
        return this
    endmethod
    
    method toString takes nothing returns string
        local string ret="(" + .name
        
        local List l = args
        loop
        exitwhen l == 0
            set ret = ret +" "+ Expr(l.head).toString()
            set l = l.tail
        endloop
        return ret+")"
    endmethod
endstruct

public struct CallStmt extends Statement
   Call cll
   
   static method create takes string s, List /*Expr*/ l returns thistype
        local thistype this = allocate()
        set .cll = Call.create(s, l)
        return this
    endmethod
    
    method toString takes nothing returns string
        return "(call "+ .cll.toString() +")\n"
    endmethod
endstruct

public struct Local extends Statement
    VarDecl vd
   
    static method create takes VarDecl vd returns thistype
        local thistype this = allocate()
        set .vd = vd
        return this
    endmethod
    
    method toString takes nothing returns string
        return "(local "+ .vd.toString() +")\n"
    endmethod
endstruct

public struct Loop extends Statement
    List /*Statement*/ body
    
    static method create takes List l returns thistype
        local thistype this = allocate()
        set .body = l
        return this
    endmethod
    
    method toString takes nothing returns string
        local string ret = "(loop \n"
        local List l = body
        loop
        exitwhen l == Nil
            set ret = ret + Statement(l.head).toString()
            set l = l.tail
        endloop
        
        return ret +")\n"
    endmethod
endstruct

public struct If extends Statement
    Expr cond
    List /*Statement*/ thenBody   
    List /*Statement*/ elseBody
    
    static method create takes Expr cond, List thenBody, List elseBody returns thistype
        local thistype this = allocate()
        set .cond = cond
        set .thenBody = thenBody
        set .elseBody = elseBody
        return this
    endmethod
    
    method toString takes nothing returns string
        local string ret
        local List it
        local List tmp
        
        set ret = "(if " + cond.toString() +" (\n"
        
        set it = thenBody
        loop
        exitwhen it == Nil
            set ret = ret + Statement(it.head).toString()
            set it=it.tail
        endloop
        
        set ret = ret + ")("

        
        set it = elseBody
        loop
        exitwhen it == Nil
            set ret = ret + Statement(it.head).toString()
            set it=it.tail
        endloop

        set ret = ret + "))\n"
        
        return ret
        
    endmethod
endstruct

public struct Exitwhen extends Statement
    Expr cond
    
    static method create takes Expr r returns thistype
        local thistype this = allocate()
        set .cond = r
        return this
    endmethod
    
    method toString takes nothing returns string
        return "(exitwhen "+ cond.toString() +")\n"
    endmethod
endstruct

public struct Return extends Statement
    Expr value

    static method create takes Expr r returns thistype
        local thistype this = allocate()
        set .value = r
        return this
    endmethod
    
    method toString takes nothing returns string
        if value == 0 then
            return "(return)\n"
        else
            return "(return "+ value.toString() +")\n"
        endif
    endmethod
endstruct

public struct Integer extends Expr
    integer val
   
    static method create takes integer s returns thistype
        local thistype this = allocate()
        set .val = s
        return this
    endmethod
    
    method toString takes nothing returns string
        return I2S(.val)
    endmethod
endstruct

public struct Rawcode extends Expr
    string val
    
    static method create takes string s returns thistype
        local thistype this = allocate()
        set .val = s
        return this
    endmethod

    method toString takes nothing returns string
        return "'"+ .val +"'"
    endmethod
endstruct

public struct Real extends Expr
   real val
   
  static method create takes real s returns thistype
        local thistype this = allocate()
        set .val = s
        return this
    endmethod
    
    method toString takes nothing returns string
        return R2SW(.val, 0, 5)
    endmethod
endstruct

public struct String extends Expr
   string val
   
   static method create takes string s returns thistype
        local thistype this = allocate()
        set .val = s
        return this
    endmethod
    
    method toString takes nothing returns string
        return "\""+ .val +"\""
    endmethod
endstruct

public struct Code extends Expr
   string name
   
   static method create takes string s returns thistype
        local thistype this = allocate()
        set .name = s
        return this
    endmethod
    
    method toString takes nothing returns string
        return "(function "+name+")"
    endmethod
endstruct

public struct Null extends Expr
    method toString takes nothing returns string
        return "null"
    endmethod
endstruct

public struct Bool extends Expr
    boolean val

    static method create takes boolean s returns thistype
        local thistype this = allocate()
        set .val = s
        return this
    endmethod
    
    method toString takes nothing returns string
        return B2S(.val)
    endmethod
endstruct

public struct CallExpr extends Expr
    Call cll

    static method create takes string s, List /*Expr*/ l returns thistype
        local thistype this = allocate()
        set .cll = Call.create(s, l)
        return this
    endmethod
    
    method toString takes nothing returns string
        return .cll.toString()
    endmethod
endstruct

public struct VarExpr extends Expr
   Var var
       
    static method create takes Var var returns thistype
        local thistype this = allocate()
        set .var = var
        return this
    endmethod
    
    method toString takes nothing returns string
        return .var.toString()
    endmethod
endstruct


/*************************\
* * * * * * * * * * * * * *
*                         *
*                         *
*                         *
*        L E X E R        *
*                         *
*                         *
*                         *
* * * * * * * * * * * * * *
\*************************/


public struct Token
    integer type
    string value
    
    static method create takes integer k, string v returns thistype
        local thistype this = allocate()
        set .type = k
        set .value = v
        return this
    endmethod
    
    method toString takes nothing returns string
        return NAMES[.type]+" "+.value
    endmethod
endstruct

private function startsWith takes string input, string t returns boolean
    return SubString(input, 0, StringLength(t)) == t
endfunction

private function isDelim takes string s returns boolean
    return s == " " or s == "\t" or s =="\n" or s=="\r" or s=="[" or s=="]" /*
        */ or s == "(" or s == ")" or s == "," or s =="." or s== "<" or s==">" /*
        */ or s == "/" or s=="*" or s== "+" or s =="-" or s=="$" or s== "=" /*
        */ or s == "\"" or s =="'" or s == "!"
endfunction

private function isDigit takes string s returns boolean
    return s == "0" or s =="1" or s=="2" or s=="3" or s=="4" or s=="5" /*
        */ or s=="6" or s=="7" or s=="8" or s=="9"
endfunction

private function isLetter takes string s returns boolean
    set s = StringCase(s, false)
    return    s=="a" or s=="b" or s=="c" or s=="d" or s=="e" or s=="f" /*
        */ or s=="g" or s=="h" or s=="i" or s=="j" or s=="k" or s=="l" /*
        */ or s=="m" or s=="n" or s=="o" or s=="p" or s=="q" or s=="r" /*
        */ or s=="s" or s=="t" or s=="u" or s=="v" or s=="w" or s=="x" /*
        */ or s=="y" or s=="z"
endfunction

private function isHex takes string s returns boolean
    set s = StringCase(s, false)
    return isDigit(s) or s=="a" or s=="b" or s=="c" or s=="d" /*
        */ or s == "e" or s=="f"
endfunction

private function isOct takes string s returns boolean
    return s == "0" or s =="1" or s=="2" or s=="3" or s=="4" or s=="5" /*
        */ or s=="6" or s=="7"
endfunction


private function token takes string input, string t returns boolean
    local integer i = StringLength(t)
    return SubString(input, 0, i) == t and (isDelim(SubString(input, i, i+1)) or i==StringLength(input))
endfunction

private function op takes string input, string t returns boolean
    return startsWith(input, t)
endfunction

private function hash takes string s returns integer
    return StringHash(s) / 3183177 + 641
endfunction

private function hex2int takes string h returns integer
    local integer i = 0
    local integer sum = 0
    local integer len = StringLength(h)
    loop
    exitwhen i >= len
        set sum = sum*16 + hex[hash(SubString(h, i, i+1))]
        set i = i +1
    endloop
    return sum
endfunction

private function oct2int takes string h returns integer
    local integer i = 0
    local integer sum = 0
    local integer len = StringLength(h)
    loop
    exitwhen i >= len
        set sum = sum*8 + hex[hash(SubString(h, i, i+1))]
        set i = i +1
    endloop
    return sum
endfunction

public struct Lexer
    private string input
    private integer pos
    private integer length
    
    static method create takes string input returns thistype
        local thistype this = allocate()
        set .pos = 0
        set .input = input
        set .length = StringLength(.input)
        return this
    endmethod
    
    private method slurpWhitespace takes nothing returns nothing
        local string c = SubString(.input, .pos, .pos+1)

        loop
        exitwhen c != " " and c != "\t"
            set .pos = .pos +1
            set c = SubString(.input, .pos, .pos+1)
        endloop
    endmethod
    
    private method parseIdent takes nothing returns Token
        local integer p = .pos
        local string c = SubString(.input, p, p +1)
        local Token r
        
        loop
        exitwhen isDelim(c) or p >=.length
            set p = p +1
            set c = SubString(.input, p, p+1)
        endloop
        
        set r = Token.create(IDENT, SubString(.input, .pos, p))
        set .pos = p
        return r
    endmethod
    
    private method parseDotFloat takes nothing returns Token
        local integer start = .pos
        loop
        exitwhen not isDigit(SubString(.input, .pos, .pos+1))
            set .pos = .pos +1
        endloop
        return Token.create(REAL, "0." + SubString(.input, start, .pos))
    endmethod
    
    private method parseHex takes nothing returns Token
        local integer start = .pos
        loop
        exitwhen not isHex(SubString(.input, .pos, .pos+1))
            set .pos = .pos +1
        endloop
        return Token.create(INTEGER, I2S(hex2int(SubString(.input, start, .pos))))
    endmethod
    
    private method parseNumber takes nothing returns Token
        local integer start = .pos
        local boolean isReal = false
        
        loop
        exitwhen not isDigit(SubString(.input, .pos, .pos+1))
            set .pos = .pos +1
        endloop
        
        if SubString(.input, .pos, .pos+1) == "." then
            set isReal = true
            set .pos = .pos +1
            loop
            exitwhen not isDigit(SubString(.input, .pos, .pos+1))
                set .pos = .pos +1
            endloop
        endif
        
        if isReal then
            return Token.create(REAL, SubString(.input, start, .pos))
        else
            return Token.create(INTEGER, SubString(.input, start, .pos))
        endif

    endmethod
    
    private method parseOct takes nothing returns Token
        local integer start = .pos
        local boolean isReal = false
        
        loop
        exitwhen not isDigit(SubString(.input, .pos, .pos+1))
            set .pos = .pos +1
        endloop
        
        if SubString(.input, .pos, .pos+1) == "." then
            set isReal = true
            set .pos = .pos +1
            loop
            exitwhen not isDigit(SubString(.input, .pos, .pos+1))
                set .pos = .pos +1
            endloop
        endif
        
        if isReal then
            return Token.create(REAL, SubString(.input, start, .pos))
        else
            // we may have parsed non-oct digits. for now we'll just ignore this possibility
            return Token.create(INTEGER, I2S(oct2int(SubString(.input, start, .pos))))
        endif
    endmethod
    
    private method parseRawcode takes nothing returns Token
        local integer start = .pos
        local string c
        local Token t
        loop
            set c = SubString(.input, .pos, .pos +1)
            if c == "\\" then
                set .pos = .pos +1
            elseif c == "'" then
                exitwhen true
            elseif .pos >= .length then
                call error("Unclosed rawcode")
            endif
            set .pos = .pos +1
        endloop
        set t = Token.create(RAWCODE, SubString(.input, start, .pos))
        set .pos = .pos +1
        return t
    endmethod
    
    private method parseString takes nothing returns Token
        local integer start = .pos
        local string c
        local Token t
        loop
            set c = SubString(.input, .pos, .pos +1)
            if c == "\\" then
                set .pos = .pos +1
            elseif c == "\"" then
                exitwhen true
            elseif .pos >= .length then
                call error("Unclosed string")
            endif
            set .pos = .pos +1
        endloop
        set t = Token.create(STRING, SubString(.input, start, .pos))
        set .pos = .pos +1
        return t
    endmethod
    
    method next takes nothing returns Token
        local string c
        local string cur
        
        call .slurpWhitespace()
        
        set c = SubString(.input, .pos, .pos +1)
        set cur = SubString(.input, .pos, .length)
        
        // TODO: comments
        
        if c == "\r" or c == "\n" then
            loop
            exitwhen c != "\r" and c!= "\n" and c!=" " and c!="\t"
                set .pos = .pos +1
                set c = SubString(.input, .pos, .pos+1)
            endloop
            return Token.create(NL, "")
        elseif c == "\"" then
            set .pos = .pos +1
            return .parseString()
        elseif c == "'" then
            set .pos = .pos +1
            return .parseRawcode()
        elseif token(cur, "call") then
            set .pos = .pos +4
            return Token.create(CALL, "")
        elseif token(cur, "set") then
            set .pos = .pos +3
            return Token.create(SET, "")
        elseif token(cur, "return") then
            set .pos = .pos +6
            return Token.create(RETURN, "")
        elseif token(cur, "loop") then
            set .pos = .pos +4
            return Token.create(LOOP, "")
        elseif token(cur, "exitwhen") then
            set .pos = .pos +8
            return Token.create(EXITWHEN, "")
        elseif token(cur, "endloop") then
            set .pos = .pos +7
            return Token.create(ENDLOOP, "")
        elseif token(cur, "if") then
            set .pos = .pos +2
            return Token.create(IF, "")
        elseif token(cur, "then") then
            set .pos = .pos +4
            return Token.create(THEN, "")
        elseif token(cur, "elseif") then
            set .pos = .pos +6
            return Token.create(ELSEIF, "")
        elseif token(cur, "else") then
            set .pos = .pos +4
            return Token.create(ELSE, "")
        elseif token(cur, "endif") then
            set .pos = .pos +5
            return Token.create(ENDIF, "")
        elseif token(cur, "function") then
            set .pos = .pos +8
            return Token.create(FUNCTION, "")
        elseif token(cur, "takes") then
            set .pos = .pos +5
            return Token.create(TAKES, "")
        elseif token(cur, "returns") then
            set .pos = .pos +7
            return Token.create(RETURNS, "")
        elseif token(cur, "nothing") then
            set .pos = .pos +7
            return Token.create(NOTHING, "")
        elseif token(cur, "endfunction") then
            set .pos = .pos +11
            return Token.create(ENDFUNCTION, "")
        elseif token(cur, "constant") then
            set .pos = .pos +8
            return Token.create(CONSTANT, "")
        elseif token(cur, "native") then
            set .pos = .pos +6
            return Token.create(NATIVE, "")
        elseif token(cur, "globals") then
            set .pos = .pos +7
            return Token.create(GLOBALS, "")
        elseif token(cur, "endglobals") then
            set .pos = .pos +10
            return Token.create(ENDGLOBALS, "")
        elseif token(cur, "type") then
            set .pos = .pos +4
            return Token.create(TYPE, "")
        elseif token(cur, "extends") then
            set .pos = .pos +7
            return Token.create(EXTENDS, "")
        elseif token(cur, "local") then
            set .pos = .pos +5
            return Token.create(LOCAL, "")
        elseif token(cur, "array") then
            set .pos = .pos +5
            return Token.create(ARRAY, "")
        elseif token(cur, "null") then
            set .pos = .pos +4
            return Token.create(NULL, "")
        elseif token(cur, "true") then
            set .pos = .pos +4
            return Token.create(TrueTok, "")
        elseif token(cur, "false") then
            set .pos = .pos +5
            return Token.create(FalseTok, "")
        elseif token(cur, "and") then
            set .pos = .pos +3
            return Token.create(AND, "")
        elseif token(cur, "or") then
            set .pos = .pos +2
            return Token.create(OR, "")
        elseif token(cur, "not") then
            set .pos = .pos +3
            return Token.create(NOT, "")
        elseif op(cur, "<=") then
            set .pos = .pos +2
            return Token.create(LEQ, "")
        elseif op(cur, ">=") then
            set .pos = .pos +2
            return Token.create(GEQ, "")
        elseif op(cur, "==") then
            set .pos = .pos +2
            return Token.create(EQ, "")
        elseif op(cur, "!=") then
            set .pos = .pos +2
            return Token.create(NEQ, "")
        elseif (c=="<") then
            set .pos = .pos +1
            return Token.create(LT, "")
        elseif (c==">") then
            set .pos = .pos +1
            return Token.create(GT, "")
        elseif (c=="+") then
            set .pos = .pos +1
            return Token.create(PLUS, "")
        elseif (c=="-") then
            set .pos = .pos +1
            return Token.create(MINUS, "")
        elseif (c=="*") then
            set .pos = .pos +1
            return Token.create(MUL, "")
        elseif (c=="/") then
            set .pos = .pos +1
            return Token.create(DIV, "")
        elseif (c==",") then
            set .pos = .pos +1
            return Token.create(COMMA, "")
        elseif (c=="(") then
            set .pos = .pos +1
            return Token.create(LPAREN, "")
        elseif (c==")") then
            set .pos = .pos +1
            return Token.create(RPAREN, "")
        elseif (c=="[") then
            set .pos = .pos +1
            return Token.create(LBRACKET, "")
        elseif (c=="]") then
            set .pos = .pos +1
            return Token.create(RBRACKET, "")
        elseif (c=="=") then
            set .pos = .pos +1
            return Token.create(EqualTok, "")
        elseif isLetter(c) then
            return .parseIdent()
        elseif startsWith(cur, "0x") or startsWith(cur, "0X") then
            set .pos = .pos +2
            return .parseHex()
        elseif c == "$" then
            set .pos = .pos +1
            return .parseHex()
        elseif c == "0" then
            return .parseOct()
        elseif isDigit(c) then
            return .parseNumber()
        elseif c == "." then
            set .pos = .pos +1
            return .parseDotFloat()
        endif
        call error("lex error: unexpected "+ c)
        return 0
    endmethod
    
    method hasNext takes nothing returns boolean
        call .slurpWhitespace()
        return .pos < .length
    endmethod
endstruct

/*************************\
* * * * * * * * * * * * * *
*                         *
*                         *
*                         *
*        P A R S E R      *
*                         *
*                         *
*                         *
* * * * * * * * * * * * * *
\*************************/

private function mkOp takes integer t, Expr lhs, Expr rhs returns Ast
    local List l = cons(lhs, cons(rhs, Nil)) // [lhs, rhs]
    local Ast ret = CallExpr.create(OPERATORS[t], l)
    //call print("mkOp: "+ ret.toString())
    return ret
endfunction

public struct Parser
    private Lexer l
    private Token cur
    
    private static If LastElseif
    
    private method operator sym takes nothing returns integer
        return cur.type
    endmethod
    
   static method create takes string s returns thistype
        local thistype this = allocate()
        set .l = Lexer.create(s)
        return this
    endmethod
    
    method nextsym takes nothing returns integer
        if l.hasNext() then
            if cur != 0 then
                call cur.destroy()
            endif
            set cur = l.next()
            //call print("nextsym: "+NAMES[cur.type])
            return cur.type
        else
            //call print("nextsym error: lexer empty")
            set cur.type = EOF
            return EOF
        endif
    endmethod
    
    private method accept takes integer t returns boolean
        if sym == t then
            call nextsym()
            return true
        else
            return false
        endif
    endmethod
    
    private method expect takes integer t returns nothing
        if sym != t then
            call error("Parser error: Expected "+ NAMES[t] +" but got "+ NAMES[sym])
        endif
        call nextsym()
    endmethod
    
    /*
    AND
    OR
    LT GT EQ NEQ LEQ GEQ
    NOT
    MINUS PLUS
    MUL DIV
    */
    
    private method parseArgs takes nothing returns List
        local Expr e
        
        if accept(RPAREN) then
            return Nil
        else
            set e = .p1()
            if accept(COMMA) then
                return cons(e, parseArgs())
            else
                call expect(RPAREN)
                return cons(e, Nil)
            endif
        endif
    endmethod

    
    private method p6 takes nothing returns Ast
        local Ast r
        local string v
        local Ast tmp
        local List l
        
        loop
        exitwhen not accept(PLUS)
        endloop
        
        if accept(MINUS) then
            return CallExpr.create("-", cons(.p6(), Nil))
        endif
        
        if sym == IDENT then
            set v = cur.value
            call nextsym()

            if accept(LPAREN) then
                return CallExpr.create(v, .parseArgs())
            elseif accept(LBRACKET) then
                set tmp = .p1()
                call expect(RBRACKET)
                return VarExpr.create(ArrayVar.create(v, Expr(tmp)))
            else
                return VarExpr.create(NormalVar.create(v))
            endif
            
        elseif sym == INTEGER then
            set v = cur.value
            call nextsym()
            return Integer.create(S2I(v))
        elseif sym == RAWCODE then
            set v = cur.value
            call nextsym()
            return Rawcode.create(v)
        elseif sym == REAL then
            set v = cur.value
            call nextsym()
            return Real.create(S2R(v))
        elseif sym == STRING then
            set v = cur.value
            call nextsym()
            return String.create(v)
        elseif accept(FUNCTION) then
            set v = cur.value
            call expect(IDENT)
            
            return Code.create(v)
        elseif accept(TrueTok) then
            
            return (Bool.create(true))
        elseif accept(FalseTok) then
            
            return (Bool.create(false))
        elseif accept(NULL) then
            
            return Null.create()
        elseif accept(LPAREN) then
            set r = .p1()
            call expect(RPAREN)
            
            return r
        endif
        
        call error("Expected an expression")
        
        return 0
    endmethod
    
    private method p5 takes nothing returns Ast
        local Ast lhs = .p6()
        local integer t
        
        loop
        exitwhen sym != MUL and sym != DIV
            set t = sym
            call nextsym()
            set lhs = mkOp(t, lhs, .p6())
        endloop
        
        return (lhs)
    endmethod
    
    private method p4 takes nothing returns Ast
        local Ast lhs
        local integer t
        
        if accept(NOT) then
            set lhs = CallExpr.create("not", cons(.p4(), Nil))
        else
            set lhs = .p5()
        endif
        
        loop
        exitwhen sym != MINUS and sym != PLUS
            set t = sym
            call nextsym()
            set lhs = mkOp(t, lhs, .p5())
        endloop
        
        
        return (lhs)
    endmethod
    
    private method p3 takes nothing returns Ast
        local Ast lhs = .p4()
        local integer t

        loop
        exitwhen sym != LT and sym != GT and sym != EQ and sym != NEQ /*
            */ and sym != LEQ and sym != GEQ
            set t = sym
            call nextsym()
            set lhs = mkOp(t, lhs, .p4())
        endloop
        
        
        return (lhs)
    endmethod
    

    private method p2 takes nothing returns Ast
        local Ast lhs = .p3()
        
        loop
        exitwhen sym != OR
            call nextsym()
            set lhs = mkOp(OR, lhs, .p3())
        endloop
        
        
        return (lhs)
    endmethod
    

    private method p1 takes nothing returns Ast
        local Ast lhs = .p2()
        
        loop
        exitwhen sym != AND
            call nextsym()
            set lhs = mkOp(AND, lhs, .p2())
        endloop
        
        
        return (lhs)
    endmethod
    
    private method parseLVar takes nothing returns Var
        local string v = cur.value
        local Expr tmp
        call expect(IDENT)
        if accept(LBRACKET) then
            set tmp = .p1()
            call expect(RBRACKET)
            return VarExpr.create(ArrayVar.create(v, Expr(tmp)))
        else
            return VarExpr.create(NormalVar.create(v))
        endif
    endmethod
        
    private method parseStatements takes nothing returns List
        local Statement t = pStmt()
        if t == 0 then
            return Nil
        else
            call expect(NL)
            return cons(t, parseStatements())
        endif
    endmethod
    
    private method parseElseIfs takes nothing returns List
        local Expr c
        local If e
        if accept(ELSEIF) then
            set c = p1()
            call expect(THEN)
            call expect(NL)
            set e = If.create(c, parseStatements(), parseElseIfs())
            if e.elseBody == Nil then
                set LastElseif = e
            endif
            return cons(e, Nil)
        else
            return Nil
        endif
    endmethod
    
    private method parseIf takes nothing returns Statement
        local Expr cond = .p1()
        local List thenBody
        local List elseBody = 0
        local List elseifs

        call expect(THEN)
        call expect(NL)
        
        set thenBody = parseStatements()
        
        set LastElseif = 0
        set elseifs = parseElseIfs()
        
        call assert(LastElseif.elseBody == 0, "last elseif body zero")

        if accept(ELSE) then
            call expect(NL)
            set elseBody = parseStatements()
        endif
        
        call expect(ENDIF)
        
        if elseifs == Nil then
            return If.create(cond, thenBody, elseBody)
        else
            set LastElseif.elseBody = elseBody
            return If.create(cond, thenBody, elseifs)
        endif
    endmethod
    
    private method pStmt takes nothing returns Statement
        local string n
        local List l
        local Ast tmp

        if accept(CALL) then
            set n = cur.value
            call expect(IDENT)
            call expect(LPAREN)
            set l = .parseArgs()
            return CallStmt.create(n, l)
        elseif accept(SET) then
            set tmp = parseLVar()
            call expect(EqualTok)
            return Set.create(Var(tmp), .p1())
        elseif accept(LOCAL) then
            return Local.create(parseVarDecl(false))
        elseif accept(RETURN) then
            if sym == NL then
                return Return.create(0)
            else
                return Return.create(.p1())
            endif
        elseif accept(EXITWHEN) then
            return Exitwhen.create(.p1())
        elseif accept(LOOP) then
            call expect(NL)
            set tmp = Loop.create(parseStatements())
            call expect(ENDLOOP)
            return tmp
        elseif accept(IF) then
            return parseIf()
        endif
        
        return 0
    endmethod
    
    private method parseVarDecl takes boolean const returns VarDecl
        local string name
        local string t
        local Expr init = 0
        
        set t = cur.value
        call expect(IDENT)
        
        if accept(ARRAY) then
            set name = cur.value
            call expect(IDENT)
            
            return ArrayDecl.create(t, name)
        else
            set name = cur.value
            call expect(IDENT)
            if accept(EqualTok) then
                set init = .p1()
            endif
        endif
            
        return NormalDecl.create(const, t, name, init)
    endmethod
    
    private method parseGlobalVarDecls takes nothing returns List
        local VarDecl vd
        
        if accept(CONSTANT) then
            set vd = parseVarDecl(true)
            call expect(NL)
            return cons(vd, parseGlobalVarDecls())
        elseif accept(ENDGLOBALS) then
            return Nil
        elseif accept(NL) then
            return parseGlobalVarDecls()
        else
            set vd = parseVarDecl(false)
            call expect(NL)
            return cons(vd, parseGlobalVarDecls())
        endif
    endmethod
    
    private method pTypeAndName takes nothing returns TypeAndName
        local string t
        local string n
        
        set t = cur.value
        call expect(IDENT)
        set n = cur.value
        call expect(IDENT)
        
        return TypeAndName.create(t, n)
    endmethod
    
    private method parseParamlist2 takes nothing returns List
        local TypeAndName e
        
        if accept(RETURNS) then
            return Nil
        else
            set e = pTypeAndName()
            if accept(COMMA) then
                return cons(e, parseParamlist2())
            else
                call expect(RETURNS)
                return cons(e, Nil)
            endif
        endif
    endmethod
    
    private method parseParamlist takes nothing returns List
        if accept(NOTHING) then
            call expect(RETURNS)
            return Nil
        else
            return parseParamlist2()
        endif
    endmethod
    
    private method parseReturntype takes nothing returns string
        local string t
        if accept(NOTHING) then
            return "nothing"
        else
            set t = cur.value
            call expect(IDENT)
            return t
        endif
    endmethod
    
    private method pNative takes boolean const returns Native
        local string name
        local string returntype
        local List paramlist

        set name = cur.value
        call expect(IDENT)
        call expect(TAKES)
        set paramlist = parseParamlist()
        set returntype = parseReturntype()
        
        return Native.create(const, name, paramlist, returntype)
    endmethod
    
    private method pFunction takes boolean const returns Function
        local string name
        local string returntype
        local List paramlist
        local List body

        set name = cur.value
        call expect(IDENT)
        call expect(TAKES)
        set paramlist = parseParamlist()
        set returntype = parseReturntype()
        call expect(NL)
        set body = parseStatements()
        call expect(ENDFUNCTION)
        return Function.create(const, name, paramlist, returntype, body)
    endmethod
    
    private method pNativeOrFunction takes boolean const returns Toplevel
        if accept(NATIVE) then
            return pNative(const)
        else
            return pFunction(const)
        endif
    endmethod
    
    method pToplevel takes nothing returns Toplevel
        local Toplevel t
        
        local string n
        local string b
        
        if accept(GLOBALS) then
            call expect(NL)
            set t = Globals.create(parseGlobalVarDecls())
            call expect(NL)
            return t
            
        elseif accept(TYPE) then
            set n = cur.value
            call expect(IDENT)
            call expect(EXTENDS)
            set b = cur.value
            call expect(IDENT)
            set t = TypeDef.create(b, n)
            call expect(NL)
            return t
            
        elseif accept(CONSTANT) then
            call pNativeOrFunction(true)
            
        elseif accept(NATIVE) then
            set t = pNative(false)
            call expect(NL)
            return t
            
        elseif accept(FUNCTION) then
            set t =  pFunction(false)
            call expect(NL)
            return t
        endif
        
        call error("Not a toplevel decleration")
        
        return 0
    endmethod
    
    method parseToplevel takes nothing returns Toplevel
        call nextsym()
        return pToplevel()
    endmethod
    
    private method pProgram takes nothing returns List /*Toplevel*/        
        if accept(NL) then
            return pProgram()
        elseif accept(EOF) then
            return Nil
        else
            return cons(pToplevel(), pProgram())
        endif
    endmethod


    method parseExpr takes nothing returns Expr
        call nextsym()
        return Expr(.p1())
    endmethod
    
    method parseStatement takes nothing returns Statement
        local Statement t 
        call nextsym()
        set t = pStmt() 
        
        if t == 0 then
            call error("Not a statement")
        endif
        
        call expect(NL)
        return t
    endmethod
    
    method parseProgram takes nothing returns List /*Toplevel*/
        call nextsym()
        return pProgram()
    endmethod
endstruct

private function init takes nothing returns nothing
    local string charmap = "0123456789abcdef"
    local integer i = 0
    local integer len = StringLength(charmap)

    loop
    exitwhen i == len
        set hex[hash(SubString(charmap, i, i+1))] = i
        set i = i +1
    endloop
    
    
    set NAMES[LPAREN] = "LPAREN"
    set NAMES[RPAREN] = "RPAREN"
    set NAMES[RBRACKET] = "RBRACKET"
    set NAMES[LBRACKET] = "LBRACKET"
    set NAMES[NEQ] = "NEQ"
    set NAMES[EQ] = "EQ"
    set NAMES[LEQ] = "LEQ"
    set NAMES[LT] = "LT"
    set NAMES[GEQ] = "GEQ"
    set NAMES[GT] = "GT"
    set NAMES[PLUS] = "PLUS"
    set NAMES[MINUS] = "MINUS"
    set NAMES[MUL] = "MUL"
    set NAMES[DIV] = "DIV"
    set NAMES[AND] = "AND"
    set NAMES[OR] = "OR"
    set NAMES[NOT] = "NOT"
    set NAMES[CALL] = "CALL"
    set NAMES[SET] = "SET"
    set NAMES[LOCAL] = "LOCAL"
    set NAMES[GLOBALS] = "GLOBALS"
    set NAMES[ENDGLOBALS] = "ENDGLOBALS"
    set NAMES[TYPE] = "TYPE"
    set NAMES[EXTENDS] = "EXTENDS"
    set NAMES[NATIVE] = "NATIVE"
    set NAMES[CONSTANT] = "CONSTANT"
    set NAMES[FUNCTION] = "FUNCTION"
    set NAMES[TAKES] = "TAKES"
    set NAMES[RETURNS] = "RETURNS"
    set NAMES[RETURN] = "RETURN"
    set NAMES[IF] = "IF"
    set NAMES[ELSEIF] = "ELSEIF"
    set NAMES[THEN] = "THEN"
    set NAMES[ELSE] = "ELSE"
    set NAMES[ENDIF] = "ENDIF"
    set NAMES[LOOP] = "LOOP"
    set NAMES[EXITWHEN] = "EXITWHEN"
    set NAMES[ENDLOOP] = "ENDLOOP"
    set NAMES[ENDFUNCTION] = "ENDFUNCTION"
    set NAMES[NULL] = "NULL"
    set NAMES[TrueTok] = "TRUE"
    set NAMES[FalseTok] = "FALSE"
    set NAMES[ARRAY] = "ARRAY"
    set NAMES[NL] = "NL"
    set NAMES[COMMA] = "COMMA"
    set NAMES[NOTHING] = "NOTHING"
    set NAMES[IDENT] = "IDENT"
    set NAMES[RAWCODE] = "RAWCODE"
    set NAMES[INTEGER] = "INTEGER"
    set NAMES[STRING] = "STRING"
    set NAMES[REAL] = "REAL"
    set NAMES[EqualTok] = "EQUAL"
    
    set NAMES[EOF] = "EOF"
    
    set OPERATORS[MINUS] = "-"
    set OPERATORS[PLUS] = "+"
    set OPERATORS[MUL] = "*"
    set OPERATORS[DIV] = "/"
    set OPERATORS[EQ] = "=="
    set OPERATORS[NEQ] = "!="
    set OPERATORS[LEQ] = "<="
    set OPERATORS[GEQ] = ">="
    set OPERATORS[LT] = "<"
    set OPERATORS[GT] = ">"
    set OPERATORS[AND] = "and"
    set OPERATORS[OR] = "or"
    set OPERATORS[NOT] = "not"
endfunction

endlibrary
