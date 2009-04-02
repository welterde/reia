%
% reia_calls: Copy on update support for calls to Reia builtins
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_calls).
-export([ast/1]).

%-define(msg(Str, Xs), io:format(Str, Xs)).
-define(msg(Str, Xs), ok).

ast(Ast) ->
  ?msg("Input: ~p~n", [Ast]),
  {ok, _, Ast2} = reia_visitor:transform(Ast, void, fun transform/2),
  ?msg("Output: ~p~n", [Ast2]),
  Ast2.
  
transform(_, {funcall, Line, Receiver, Name, Arguments, Block}) ->
  % io:format("Input: ~p~n", [Node]),
  {ok, void, Receiver2} = reia_visitor:transform(Receiver, void, fun transform/2),
  Nonce = {identifier, Line, list_to_atom("__receiver_" ++ reia_compiler:nonce())},
  Node = {block, Line, [
    {match, Line, Nonce, Receiver2},
    case_node(Line, Nonce, Name, Arguments, Block)
  ]},
  
  % io:format("Output: ~p~n", [Node2]),
  {stop, void, Node};
  
% Walk unrecognized nodes without transforming them
transform(_, Node) ->
  {walk, void, Node}.

% Generate case node for dispatching function calls
case_node(Line, Receiver, Name, Arguments, Block) ->
  Call = {funcall, Line, Receiver, Name, Arguments, Block},
  {'case', Line, Receiver, [
    object_call_clause(Line, Call), 
    builtin_call_clause(Line, Call)
  ]}.
  
% Clause which matches calls to objects and dispatches them
object_call_clause(Line, Node) ->
  {clause, Line, 
    {erl_forms, Line,
      {tuple, Line, [{atom, Line, object}, {tuple, Line, [{var, Line, '_'}, {var, Line ,'_'}]}]}
    },
    [Node]
  }.
  
% Clause which matches calls to builtins and dispatches them
builtin_call_clause(Line, Node) ->
  Nonce = {identifier, Line, list_to_atom("__return_value_" ++ reia_compiler:nonce())},
  {clause, Line, {identifier, Line, '_'}, [
    {match, Line, Nonce, Node},
    Nonce
  ]}.