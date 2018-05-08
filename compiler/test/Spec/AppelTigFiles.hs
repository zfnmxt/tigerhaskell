module Spec.AppelTigFiles where 
test1="/* an array type and an array variable */\nlet\n\ttype  arrtype = array of int\n\tvar arr1:arrtype := arrtype [10] of 0\nin\n\tarr1\nend\n"

test2="/* arr1 is valid since expression 0 is int = myint */\nlet\n\ttype myint = int\n\ttype  arrtype = array of myint\n\n\tvar arr1:arrtype := arrtype [10] of 0\nin\n\tarr1\nend\n"

test3="/* a record type and a record variable */\nlet\n\ttype  rectype = {name:string, age:int}\n\tvar rec1:rectype := rectype {name=\"Nobody\", age=1000}\nin\n\trec1.name := \"Somebody\";\n\trec1\nend\n"

test4="/* define a recursive function */\nlet\n\n/* calculate n! */\nfunction nfactor(n: int): int =\n\t\tif  n = 0 \n\t\t\tthen 1\n\t\t\telse n * nfactor(n-1)\n\nin\n\tnfactor(10)\nend\n\n"

test5="/* define valid recursive types */\nlet\n/* define a list */\ntype intlist = {hd: int, tl: intlist} \n\n/* define a tree */\ntype tree ={key: int, children: treelist}\ntype treelist = {hd: tree, tl: treelist}\n\nvar lis:intlist := intlist { hd=0, tl= nil } \n\nin\n\tlis\nend\n"

test6="/* define valid mutually recursive procedures */\nlet\n\nfunction do_nothing1(a: int, b: string)=\n\t\tdo_nothing2(a+1)\n\nfunction do_nothing2(d: int) =\n\t\tdo_nothing1(d, \"str\")\n\nin\n\tdo_nothing1(0, \"str2\")\nend\n\n"

test7="/* define valid mutually recursive functions */\nlet\n\nfunction do_nothing1(a: int, b: string):int=\n\t\t(do_nothing2(a+1);0)\n\nfunction do_nothing2(d: int):string =\n\t\t(do_nothing1(d, \"str\");\" \")\n\nin\n\tdo_nothing1(0, \"str2\")\nend\n\n"

test8="/* correct if */\nif (10 > 20) then 30 else 40\t\n"

test9="/* error : types of then - else differ */\n\nif (5>4) then 13 else  \" \"\n"

test10="/* error : body of while not unit */\nwhile(10 > 5) do 5+6\n"

test11="/* error hi expr is not int, and index variable erroneously assigned to.  */\nfor i:=10 to \" \" do \n\ti := i - 1\n"

test12="/* valid for and let */\n\nlet\n\tvar a:= 0\nin \n\tfor i:=0 to 100 do (a:=a+1;())\nend\n"

test13="/* error: comparison of incompatible types */\n\n3 > \"df\"\n"

test14="/* error : compare rec with array */\n\nlet\n\n\ttype arrtype = array of int\n\ttype rectype = {name:string, id: int}\n\n\tvar rec := rectype {name=\"aname\", id=0}\n\tvar arr := arrtype [3] of 0\n\nin\n\tif rec <> arr then 3 else 4\nend\n"

test15="/* error : if-then returns non unit */\n\nif 20 then 3\n"

test16="/* error: mutually recursive types thet do not pass through record or array */\nlet \n\ntype a=c\ntype b=a\ntype c=d\ntype d=a\n\nin\n \"\"\nend\n"

test17="/* error: definition of recursive types is interrupted */\nlet\n/* define a tree */\ntype tree ={key: int, children: treelist}\nvar d:int :=0\ntype treelist = {hd: tree, tl: treelist}\n\nin\n\td\nend\n"

test18="/* error : definition of recursive functions is interrupted */\nlet\n\nfunction do_nothing1(a: int, b: string):int=\n\t\t(do_nothing2(a+1);0)\n\nvar d:=0\n\nfunction do_nothing2(d: int):string =\n\t\t(do_nothing1(d, \"str\");\" \")\n\nin\n\tdo_nothing1(0, \"str2\")\nend\n\n"

test19="/* error : second function uses variables local to the first one, undeclared variable */\nlet\n\nfunction do_nothing1(a: int, b: string):int=\n\t\t(do_nothing2(a+1);0)\n\nfunction do_nothing2(d: int):string =\n\t\t(do_nothing1(a, \"str\");\" \")\n\nin\n\tdo_nothing1(0, \"str2\")\nend\n\n"

test20="/* error: undeclared variable i */\n\nwhile 10 > 5 do (i+1;())\n"

test21="/* error : procedure returns value  and procedure is used in arexpr */\nlet\n\n/* calculate n! */\nfunction nfactor(n: int) =\n\t\tif  n = 0 \n\t\t\tthen 1\n\t\t\telse n * nfactor(n-1)\n\nin\n\tnfactor(10)\nend\n\n"

test22="/* error : field not in record type */\n\nlet \n\ttype rectype = {name:string , id:int}\n\tvar rec1 := rectype {name=\"Name\", id=0}\nin\n\trec1.nam := \"asd\"\nend\n"

test23="/* error : type mismatch */\n\nlet \n\ttype rectype = {name:string , id:int}\n\tvar rec1 := rectype {name=\"aname\", id=0}\nin\n\trec1.name := 3;\n\trec1.id := \"\" \nend\n"

test24="/* error : variable not array */\nlet \n\tvar d:=0\nin\n\td[3]\nend\n\n"

test25="/* error : variable not record */\nlet \n\tvar d:=0\nin\n\td.f \nend\n\n"

test26="/* error : integer required */\n\n3 + \"var\"\n"

test27="/* locals hide globals */\nlet\n\tvar a:=0\n\n\tfunction g(a:int):int = a \nin\n g(2)\nend\n"

test28="/* error : different record types */\n\nlet\n\ttype rectype1 = {name:string , id:int}\n\ttype rectype2 = {name:string , id:int}\n\n\tvar rec1: rectype1 := rectype2 {name=\"Name\", id=0}\nin\n\trec1\nend\n"

test29="/* error : different array types */\n\nlet\n\ttype arrtype1 = array of int\n\ttype arrtype2 = array of int\n\n\tvar arr1: arrtype1 := arrtype2 [10] of 0\nin\n\tarr1\nend\n"

test30="/* synonyms are fine */\n\nlet \n\t\ttype a = array of int\n\t\ttype b = a\n\n\t\tvar arr1:a := b [10] of 0\nin\n\t\tarr1[2]\nend\n"

test31="/* error : type constraint and init value differ */\nlet \n\tvar a:int := \" \"\nin\n\ta\nend\n"

test32="/* error : initializing exp and array type differ */\n\nlet\n\ttype arrayty = array of int\n\n\tvar a := arrayty [10] of \" \"\nin\n\t0\nend\n"

test33="/* error : unknown type */\nlet\n\tvar a:= rectype {}\nin\n\t0\nend\n"

test34="/* error : formals and actuals have different types */\nlet\n\tfunction g (a:int , b:string):int = a\nin\n\tg(\"one\", \"two\")\nend\n"

test35="/* error : formals are more then actuals */\nlet\n\tfunction g (a:int , b:string):int = a\nin\n\tg(\"one\")\nend\n"

test36="/* error : formals are fewer then actuals */\nlet\n\tfunction g (a:int , b:string):int = a\nin\n\tg(3,\"one\",5)\nend\n"

test37="/* redeclaration of variable; this is legal, there are two different\n   variables with the same name.  The second one hides the first.  */\nlet\n\tvar a := 0\n\tvar a := \" \"\nin\n\t0\nend\n"

test38="/* This is illegal, since there are two types with the same name\n    in the same (consecutive) batch of mutually recursive types. \n    See also test47  */\nlet\n\ttype a = int\n\ttype a = string\nin\n\t0\nend\n"

test39="/* This is illegal, since there are two functions with the same name\n    in the same (consecutive) batch of mutually recursive functions.\n   See also test48 */\nlet\n\tfunction g(a:int):int = a\n\tfunction g(a:int):int = a\nin\n\t0\nend\n"

test40="/* error : procedure returns value */\nlet\n\tfunction g(a:int) = a\nin \n\tg(2)\nend\n\n"

test41="/* local types hide global */\nlet\n\ttype a = int\nin\n\tlet\n\t\ttype a = string\n\tin\n\t\t0\n\tend\nend\n"

test42="/* correct declarations */\nlet \n\ntype arrtype1 = array of int\ntype rectype1 = {name:string, address:string, id: int , age: int}\ntype arrtype2 = array of rectype1\ntype rectype2 = {name : string, dates: arrtype1}\n\ntype arrtype3 = array of string\n\nvar arr1 := arrtype1 [10] of 0\nvar arr2  := arrtype2 [5] of rectype1 {name=\"aname\", address=\"somewhere\", id=0, age=0}\nvar arr3:arrtype3 := arrtype3 [100] of \"\"\n\nvar rec1 := rectype1 {name=\"Kapoios\", address=\"Kapou\", id=02432, age=44}\nvar rec2 := rectype2 {name=\"Allos\", dates= arrtype1 [3] of 1900}\n\nin\n\narr1[0] := 1; \narr1[9] := 3;\narr2[3].name := \"kati\";\narr2[1].age := 23;\narr3[34] := \"sfd\";\n\nrec1.name := \"sdf\";\nrec2.dates[0] := 2323;\nrec2.dates[2] := 2323\n\nend\n"

test43="/* initialize with unit and causing type mismatch in addition */\n\nlet \n\tvar a := ()\nin\n\ta + 3\nend\n"

test44="/* valid nil initialization and assignment */\nlet \n\n\ttype rectype = {name:string, id:int}\n\tvar b:rectype := nil\n\nin\n\n\tb := nil\n\nend\n"

test45="/* error: initializing nil expressions not constrained by record type */\nlet \n\ttype rectype = {name:string, id:int}\n\n\tvar a:= nil\nin\n\ta\nend\n"

test46="/* valid rec comparisons */\nlet \n\ttype rectype = {name:string, id:int}\n\tvar b:rectype := nil\nin\n\tb = nil;\n\tb <> nil\nend\n"

test47="/* This is legal.  The second type \"a\" simply hides the first one.\n   Because of the intervening variable declaration, the two \"a\" types\n   are not in the same  batch of mutually recursive types.\n   See also test38 */\nlet\n\ttype a = int\n\tvar b := 4\n\ttype a = string\nin\n\t0\nend\n"

test48="/* This is legal.  The second function \"g\" simply hides the first one.\n   Because of the intervening variable declaration, the two \"g\" functions\n   are not in the same  batch of mutually recursive functions. \n   See also test39 */\nlet\n\tfunction g(a:int):int = a\n\ttype t = int\n\tfunction g(a:int):int = a\nin\n\t0\nend\n"

test49="/* error: syntax error, nil should not be preceded by type-id.  */\nlet \n\ttype rectype = {name:string, id:int}\n\n\tvar a:= rectype nil\nin\n\ta\nend\n"