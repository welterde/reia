%
% reia_parse: Yecc grammar for the Reia language
% Copyright (C)2008-09 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

Nonterminals
  grammar
  expr_list
  exprs
  expr
  match_expr
  send_expr
  bool_expr
  bool_op
  comp_expr
  comp_op
  range_expr
  add_expr
  add_op
  mult_expr
  mult_op
  pow_expr
  pow_op
  declaration
  module_decl
  class_decl
  functions
  function
  funref_expr
  funref
  funcall_expr
  funcall
  function_identifier
  function_params
  block
  class_inst_expr
  class_inst
  erl_funcall_expr
  erl_funcall
  unary_expr
  unary_op  
  max_expr
  ivar
  clauses
  clause
  case_expr
  receive_expr
  after_clause
  if_expr
  inline_if_expr
  if_clause
  elseif_clauses
  elseif_clause
  else_clause
  for_expr
  try_expr
  catch_clauses
  catch_clause
  throw_expr
  number
  list
  tuple
  dict
  dict_entries
  binary
  lambda
  list_comprehension
  lc_exprs
  lc_expr
  .
  
Terminals
  true false nil float integer string regexp atom
  identifier punctuated_identifier constant module class
  eol def fun do 'end' 'case' when 'if' elseif else unless 
  'and' 'or' 'not' 'try' 'catch' throw for in 'receive' 'after'
  '(' ')' '[' ']' '{' '}' '|' '<<' '>>'
  '+' '-' '*' '/' '%' '**' '!'
  '.' '..' ',' '::' '@' '&'
  '=' '==' '===' '!=' '>' '<' '=>' '<=' '>=' '<-'
  .

Rootsymbol grammar.

grammar -> expr_list : '$1'.

%% Expression lists (eol delimited)
expr_list -> expr : ['$1'].
expr_list -> expr eol : ['$1'].
expr_list -> eol expr_list : '$2'.
expr_list -> expr eol expr_list : ['$1'|'$3'].

%% Expressions (comma delimited)
exprs -> expr : ['$1'].
exprs -> expr eol : ['$1'].
exprs -> eol exprs : '$2'.
exprs -> expr ',' exprs : ['$1'|'$3'].

expr -> inline_if_expr : '$1'.

inline_if_expr -> match_expr 'if' match_expr : {'if', line('$2'), [{clause, line('$2'), '$3', ['$1']}]}.
inline_if_expr -> match_expr 'unless' match_expr : {'if', line('$2'), [{clause, line('$2'), {op, line('$2'), 'not', '$3'}, ['$1']}]}.
inline_if_expr -> match_expr : '$1'.

match_expr -> send_expr '=' match_expr : {match, line('$2'), '$1', '$3'}.
match_expr -> send_expr : '$1'.

send_expr -> bool_expr '!' send_expr : {send, line('$2'), '$1', '$3'}.
send_expr -> bool_expr : '$1'.

bool_expr -> comp_expr bool_op bool_expr : {op, line('$1'), op('$2'), '$1', '$3'}.
bool_expr -> comp_expr : '$1'.

comp_expr -> range_expr comp_op range_expr : {op, line('$1'), op('$2'), '$1', '$3'}.
comp_expr -> range_expr : '$1'.

range_expr -> add_expr '..' range_expr : {range, line('$2'), '$1', '$3'}.
range_expr -> add_expr : '$1'.

add_expr -> mult_expr add_op add_expr : {op, line('$1'), op('$2'), '$1', '$3'}.
add_expr -> mult_expr : '$1'.

mult_expr -> pow_expr mult_op mult_expr : {op, line('$1'), op('$2'), '$1', '$3'}.
mult_expr -> pow_expr : '$1'.

pow_expr -> throw_expr pow_op pow_expr : {op, line('$1'), op('$2'), '$1', '$3'}.
pow_expr -> throw_expr : '$1'.

throw_expr -> throw unary_expr : {throw, line('$1'), '$2'}.
throw_expr -> unary_expr : '$1'.

unary_expr -> unary_op unary_expr : {op, line('$1'), op('$1'), '$2'}.
unary_expr -> '!' unary_expr : {op, line('$1'), 'not', '$2'}.
unary_expr -> funref_expr : '$1'.

funref_expr -> funref : '$1'.
funref_expr -> funcall_expr : '$1'.

funcall_expr -> funcall : '$1'.
funcall_expr -> class_inst_expr : '$1'.

class_inst_expr -> class_inst : '$1'.
class_inst_expr -> erl_funcall_expr : '$1'.

erl_funcall_expr -> erl_funcall : '$1'.
erl_funcall_expr -> max_expr : '$1'.

max_expr -> ivar : '$1'.
max_expr -> identifier : '$1'.
max_expr -> nil        : '$1'.
max_expr -> true       : '$1'.
max_expr -> false      : '$1'.
max_expr -> number     : '$1'.
max_expr -> regexp     : '$1'.
max_expr -> list       : '$1'.
max_expr -> tuple      : '$1'.
max_expr -> dict       : '$1'.
max_expr -> binary     : '$1'.
max_expr -> atom       : '$1'.
max_expr -> constant   : '$1'.
max_expr -> lambda     : '$1'.
max_expr -> case_expr  : '$1'.
max_expr -> if_expr    : '$1'.
max_expr -> for_expr   : '$1'.
max_expr -> try_expr   : '$1'.
max_expr -> receive_expr : '$1'.
max_expr -> declaration  : '$1'.
max_expr -> list_comprehension : '$1'.
max_expr -> '(' expr ')' : '$2'.
max_expr -> string     : interpolate_string('$1').

%% Instance variables
ivar -> '@' identifier : {ivar, line('$1'), identifier_atom('$2')}.

%% Clauses
clauses -> clause clauses : ['$1'|'$2'].
clauses -> clause : ['$1'].

clause -> when expr eol expr_list : {clause, line('$1'), '$2', '$4'}.

%% Case expressions
case_expr -> 'case' expr eol clauses 'end': {'case', line('$1'), '$2', '$4'}.
  
%% Receive expressions
receive_expr -> 'receive' eol clauses 'end': {'receive', line('$1'), '$3'}.
receive_expr -> 'receive' eol clauses after_clause 'end': {'receive', line('$1'), '$3', '$4'}.
receive_expr -> 'receive' eol after_clause 'end' : {'receive', line('$1'), [], '$3'}.
  
after_clause -> 'after' expr eol expr_list : {'after', line('$1'), '$2', '$4'}.

%% If expressions
if_expr -> if_clause end : {'if', line('$1'), ['$1']}.
if_expr -> if_clause else_clause end : {'if', line('$1'), ['$1','$2']}.
if_expr -> if_clause elseif_clauses end : {'if', line('$1'), ['$1'|'$2']}.
if_expr -> if_clause elseif_clauses else_clause end : {'if', line('$1'), lists:flatten(['$1','$2','$3'])}.

if_clause -> 'if' expr eol expr_list : {clause, line('$1'), '$2', '$4'}.
if_clause -> 'unless' expr eol expr_list : {clause, line('$1'), {op, line('$1'), 'not', '$2'}, '$4'}.

elseif_clauses -> elseif_clause elseif_clauses : ['$1'|'$2'].
elseif_clauses -> elseif_clause : ['$1'].
elseif_clause -> elseif expr eol expr_list : {clause, line('$1'), '$2', '$4'}.

else_clause -> else expr_list : {clause, line('$1'), {true, line('$1')}, '$2'}.

%% For loops
for_expr -> for match_expr in expr eol expr_list end : {for, line('$1'), '$2', '$4', '$6'}.

%% Try expressions
try_expr -> 'try' expr_list catch_clauses 'end' : {'try', line('$1'), '$2', '$3'}.

catch_clauses -> catch_clause catch_clauses : ['$1'|'$2'].
catch_clauses -> catch_clause : ['$1'].

catch_clause -> 'catch' expr eol expr_list : {'catch', line('$1'), '$2', '$4'}.

%% Boolean operators
bool_op -> 'and' : '$1'.
bool_op -> 'or'  : '$1'.

%% Comparison operators
comp_op -> '===' : '$1'.
comp_op -> '==' : '$1'.
comp_op -> '!=' : '$1'.
comp_op -> '>'  : '$1'.
comp_op -> '<'  : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '<=' : '$1'.

%% Addition operators
add_op -> '+' : '$1'.
add_op -> '-' : '$1'.

%% Multiplication operators
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
mult_op -> '%' : '$1'.

%% Exponentation operator
pow_op -> '**' : '$1'.

%% Unary operators
unary_op -> '+'   : '$1'.
unary_op -> '-'   : '$1'.
unary_op -> 'not' : '$1'.

%% Declarations
declaration -> module_decl : '$1'.
declaration -> class_decl : '$1'.

%% Module declaration
module_decl -> module constant eol functions 'end' : {module, line('$1'), '$2', '$4'}.

%% Class declaration
class_decl -> class constant eol functions 'end' : {class, line('$1'), '$2', '$4'}.
class_decl -> class constant '<' constant eol functions 'end' : {class, line('$1'), '$2', '$4', '$6'}.

%% Functions
functions -> function : ['$1'].
functions -> function eol : ['$1'].
functions -> eol functions : '$2'.
functions -> function eol functions : ['$1'|'$3'].

%% Function definitions
function -> def function_identifier eol expr_list 'end' : {function, line('$1'), '$2', [], {nil, line('$2')}, '$4'}.
function -> def function_identifier '(' ')' eol expr_list 'end' : {function, line('$1'), '$2', [], {nil, line('$2')}, '$6'}.
function -> def function_identifier '(' exprs ')' eol expr_list 'end' : {function, line('$1'), '$2', '$4', {nil, line('$2')}, '$7'}.

%% Function definitions with block arguments
function -> def function_identifier '(' '&' identifier ')' eol expr_list 'end' : {function, line('$1'), '$2', [], '$5', '$8'}.
function -> def function_identifier '(' function_params '&' identifier ')' eol expr_list 'end' : {function, line('$1'), '$2', '$4', '$6', '$9'}.

%% FIXME: workaround for shift/reduce conflicts involving ',' and block args
%% Doesn't handle newlines properly
function_params -> expr ',' : ['$1'].
function_params -> expr ',' function_params : ['$1'|'$3'].

%% Function identifiers
function_identifier -> identifier : function_identifier('$1').
function_identifier -> punctuated_identifier : function_identifier('$1').
function_identifier -> class : function_identifier('$1').

%% Function references
funref -> funcall_expr '.' function_identifier : {funref, line('$2'), '$1', '$3'}.

%% Local function calls
funcall -> function_identifier '(' ')' : {funcall, line('$2'), '$1', [], {atom, line('$2'), nil}}.
funcall -> function_identifier '(' exprs ')' : {funcall, line('$2'), '$1', '$3', {atom, line('$2'), nil}}.

%% Remote function calls
funcall -> funcall_expr '.' function_identifier '(' ')' : {funcall, line('$2'), '$1', '$3', [], {atom, line('$2'), nil}}.
funcall -> funcall_expr '.' function_identifier '(' exprs ')' : {funcall, line('$2'), '$1', '$3', '$5', {atom, line('$2'), nil}}.
funcall -> funcall_expr '[' expr ']' : {funcall, line('$2'), '$1', {identifier, line('$2'), '[]'}, ['$3'], {atom, line('$2'), nil}}.
funcall -> funcall_expr '.' function_identifier '[' expr ']' : {funcall, line('$2'), {funcall, line('$2'), '$1', '$3', [], {atom, line('$2'), nil}}, {identifier, line('$2'), '[]'}, ['$5'], {atom, line('$2'), nil}}.

%% Async function calls
funcall -> funcall_expr '<-' function_identifier '(' ')' : {cast, line('$2'), '$1', '$3', [], {atom, line('$2'), nil}}.
funcall -> funcall_expr '<-' function_identifier '(' exprs ')' : {cast, line('$2'), '$1', '$3', '$5', {atom, line('$2'), nil}}.

%% Local function calls with blocks
funcall -> function_identifier block : {funcall, line('$2'), '$1', [], '$2'}.
funcall -> function_identifier '(' ')' block : {funcall, line('$2'), '$1', [], '$4'}.
funcall -> function_identifier '(' exprs ')' block : {funcall, line('$2'), '$1', '$3', '$5'}.

%% Remote function calls with blocks
funcall -> funcall_expr '.' function_identifier block : {funcall, line('$2'), '$1', '$3', [], '$4'}.
funcall -> funcall_expr '.' function_identifier '(' ')' block : {funcall, line('$2'), '$1', '$3', [], '$6'}.
funcall -> funcall_expr '.' function_identifier '(' exprs ')' block : {funcall, line('$2'), '$1', '$3', '$5', '$7'}.

% Async function calls with blocks
funcall -> funcall_expr '<-' function_identifier block : {cast, line('$2'), '$1', '$3', [], '$4'}.
funcall -> funcall_expr '<-' function_identifier '(' ')' block : {cast, line('$2'), '$1', '$3', [], '$6'}.
funcall -> funcall_expr '<-' function_identifier '(' exprs ')' block : {cast, line('$2'), '$1', '$3', '$5', '$7'}.

%% Local function calls with lambdas passed as blocks
funcall -> function_identifier '(' '&' identifier ')' : {funcall, line('$2'), '$1', [], '$4'}.
funcall -> function_identifier '(' function_params '&' identifier ')' : {funcall, line('$2'), '$1', '$3', '$5'}.

% Remote function calls with lambdas passed as blocks
funcall -> funcall_expr '.' function_identifier '(' '&' identifier ')' : {funcall, line('$2'), '$1', '$3', [], '$6'}.
funcall -> funcall_expr '.' function_identifier '(' function_params '&' identifier ')' : {funcall, line('$2'), '$1', '$3', '$5', '$7'}.

% Async function calls with lambdas passed as blocks
funcall -> funcall_expr '<-' function_identifier '(' '&' identifier ')' : {cast, line('$2'), '$1', '$3', [], '$6'}.
funcall -> funcall_expr '<-' function_identifier '(' function_params '&' identifier ')' : {cast, line('$2'), '$1', '$3', '$5', '$7'}.

%% Class instantiations
class_inst -> constant '(' ')' : {class_inst, line('$2'), '$1', []}.
class_inst -> constant '(' exprs ')' : {class_inst, line('$2'), '$1', '$3'}.

%% Class instantiations with blocks
class_inst -> constant block : {funcall, line('$2'), '$1', [], '$2'}.
class_inst -> constant '(' ')' block : {funcall, line('$2'), '$1', [], '$4'}.
class_inst -> constant '(' exprs ')' block : {funcall, line('$2'), '$1', '$3', '$5'}.

%% Blocks

block -> '{' expr_list '}' : {lambda, line('$1'), [], '$2'}.
block -> '{' '|' exprs '|' expr_list '}' : {lambda, line('$1'), '$3', '$5'}.

block -> do expr_list 'end' : {lambda, line('$1'), [], '$2'}.
block -> do '|' exprs '|' expr_list 'end' : {lambda, line('$1'), '$3', '$5'}.

%% Erlang function calls
erl_funcall -> identifier '::' identifier '(' ')' : {erl_funcall, line('$2'), '$1', '$3', []}.
erl_funcall -> identifier '::' identifier '(' exprs ')' : {erl_funcall, line('$2'), '$1', '$3', '$5'}.

%% Numbers
number -> float : '$1'.
number -> integer : '$1'.

%% Lists
list -> '[' ']' : {list, line('$1'), []}.
list -> '[' exprs ']' : {list, line('$1'), '$2'}.

%% Tuples
tuple -> '(' ')' : {tuple, line('$1'), []}.
tuple -> '(' expr ',' ')' : {tuple, line('$1'), ['$2']}.
tuple -> '(' expr ',' exprs ')': {tuple, line('$1'), ['$2'|'$4']}.

%% Dicts
dict -> '{' '}' : {dict, line('$1'), []}.
dict -> '{' dict_entries '}' : {dict, line('$1'), '$2'}.

dict_entries -> bool_expr '=>' expr : [{'$1','$3'}].
dict_entries -> bool_expr '=>' expr ',' dict_entries : [{'$1','$3'}|'$5'].

%% Binaries
binary -> '<<' string '>>' : {binary, line('$1'), '$2'}.

%% Lambdas
lambda -> fun '{' expr_list '}' : {lambda, line('$1'), [], '$3'}.
lambda -> fun '(' ')' '{' expr_list '}' : {lambda, line('$1'), [], '$5'}.
lambda -> fun '(' exprs ')' '{' expr_list '}' : {lambda, line('$1'), '$3', '$6'}.
lambda -> fun do expr_list 'end' : {lambda, line('$1'), [], '$3'}.
lambda -> fun '(' ')' do expr_list 'end' : {lambda, line('$1'), [], '$5'}.
lambda -> fun '(' exprs ')' do expr_list 'end' : {lambda, line('$1'), '$3', '$6'}.
  
%% List comprehensions
list_comprehension -> '[' expr '|' lc_exprs ']' : {lc, line('$1'), '$2', '$4'}.

lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs: ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr 'in' expr : {generate, line('$2'), '$1', '$3'}.

Erlang code.

-export([string/1]).

%% Easy interface for parsing a given string with nicely formatted errors
string(String) ->
  case reia_scan:string(String) of
    {ok, Tokens, _} -> 
      case reia_parse:parse(Tokens) of
        {ok, Exprs} ->
          {ok, Exprs};
        {error, {Line, _, [Message, Token]}} ->
          {error, {Line, lists:flatten(io_lib:format("~s~s", [Message, Token]))}}
      end;
    {error, {Line, _, {Message, Token}}, _} ->
      {error, {Line, lists:flatten(io_lib:format("~p ~p", [Message, Token]))}}
  end.

%% Keep track of line info in tokens
line(Tup) -> element(2, Tup).

%% Extract operators from op tokens
op({Op, _Line}) ->
  Op.

%% Extract the atom name from an identifier
identifier_atom({identifier, _Line, Atom}) ->
  Atom.

%% Generate proper forms for function identifiers
function_identifier({identifier, _Line, _Atom} = Identifier) ->
  Identifier;
function_identifier({punctuated_identifier, Line, Atom}) ->
  {identifier, Line, Atom};
function_identifier({class, Line}) ->
  {identifier, Line, class}.
    
%% Interpolate strings, parsing the contents of #{...} tags
interpolate_string({string, Line, String}) ->
  interpolate_string(String, Line, [], []).
  
interpolate_string([], Line, CharAcc, ExprAcc) ->
  Result = case CharAcc of
    [] -> lists:reverse(ExprAcc);
    _  -> lists:reverse([{string, Line, lists:reverse(CharAcc)}|ExprAcc])
  end,
  case Result of
    [{string, Line, _} = Res] -> Res;
    _ -> {dstring, Line, Result}
  end;
interpolate_string("#{" ++ String, Line, CharAcc, ExprAcc) ->
  {String2, Expr} = extract_fragment([], String, Line),
  ExprAcc2 = case CharAcc of
    [] -> ExprAcc;
    _  -> [{string, Line, lists:reverse(CharAcc)}|ExprAcc]
  end,
  interpolate_string(String2, Line, [], [Expr|ExprAcc2]);
interpolate_string([Char|Rest], Line, CharAcc, ExprAcc) ->
  interpolate_string(Rest, Line, [Char|CharAcc], ExprAcc).
  
extract_fragment(_Continuation, [], Line) ->
  throw({error, {Line, "unexpected end of interpolated string"}});
extract_fragment(_Continuation, [$"|_], Line) ->
  throw({error, {Line, "invalid quote within interpolated string"}});
extract_fragment(Continuation, [$}|String], Line) ->
  {more, Continuation2} = reia_scan:tokens(Continuation, [$}], Line),
  case Continuation2 of
    {tokens, _, _, _, _, [{'}', _}|Tokens], _, _} ->
      case reia_parse:parse(lists:reverse(Tokens)) of
        {ok, [Expr]} ->
          {String, Expr};
        %% Need more tokens
        {error, {999999, _}} ->
          extract_fragment(Continuation2, String, Line);
        Error ->
          throw(Error)
      end;
    {skip_tokens, _, _, _, _, {_, _, Error}, _, _} ->
      throw({error, {Line, Error}})
  end;
extract_fragment(Continuation, [Char|String], Line) ->
  {more, Continuation2} = reia_scan:tokens(Continuation, [Char], Line),
  extract_fragment(Continuation2, String, Line).
