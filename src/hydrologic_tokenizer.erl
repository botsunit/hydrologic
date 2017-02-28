% @hidden
-module(hydrologic_tokenizer).
-include("../include/hydrologic_tokens.hrl").

-export([tokenize/1]).

tokenize(String) ->
  tokenize(String, 1, 1, []).

tokenize([], Line, Column, Tokens) ->
  {ok, Line, Column, lists:reverse(Tokens)};

% Indent and space
tokenize([H|_] = String, Line, Column, Tokens) when ?is_space(H), Column == 1 ->
  {Rest, Size} = build_ident(String, 0),
  tokenize(Rest, Line, Column + Size, [{indent, {Line, Column, Column + Size}, Size}|Tokens]);
tokenize([H|Rest], Line, Column, Tokens) when ?is_space(H) ->
  tokenize(Rest, Line, Column + 1, Tokens);

% Integer and floats
tokenize([H|_] = String, Line, Column, Tokens) when ?is_digit(H) ->
  {Rest, Number, Size, Type} = build_number(String, [], integer),
  tokenize(Rest, Line, Column + Size, [{Type, {Line, Column, Column + Size}, Number}|Tokens]);

% Identifier and keywords
tokenize([H|_] = String, Line, Column, Tokens) when ?is_identifier(H) ->
  case build_identifier(String, 0, []) of
    {Rest, Identifier, Size, true} ->
      tokenize(Rest, Line, Column + Size, [{keyword, {Line, Column, Column + Size}, Identifier}|Tokens]);
    {Rest, Identifier, Size, false} ->
      tokenize(Rest, Line, Column + Size, [{identifier, {Line, Column, Column + Size}, Identifier}|Tokens])
  end;

tokenize([H|_] = String, Line, Column, Tokens) when ?is_op(H) ->
  {Rest, Operator, Size} = build_operator(String, 0, []),
  tokenize(Rest, Line, Column + Size, [{operator, {Line, Column, Column + Size}, Operator}|Tokens]);

% End of line
tokenize("\n" ++ Rest, Line, Column, Tokens) ->
  {Line1, Tokens1} = eol(Line, Column, Tokens),
  tokenize(Rest, Line1, 1, Tokens1);
tokenize("\r\n" ++ Rest, Line, Column, Tokens) ->
  {Line1, Tokens1} = eol(Line, Column, Tokens),
  tokenize(Rest, Line1, 1, Tokens1).

% Private

build_ident([N|R], Size) when ?is_space(N) ->
  build_ident(R, Size + 1);
build_ident(Rest, Size) ->
  {Rest, Size}.

build_number([$., N|R], Acc, integer) when ?is_digit(N) ->
  build_number([N|R], [$.|Acc], float);
build_number([N|R], Acc, Number) when ?is_digit(N) ->
  build_number(R, [N|Acc], Number);
build_number(Rest, Acc, float) ->
  {Rest, list_to_float(lists:reverse(Acc)), length(Acc), float};
build_number(Rest, Acc, integer) ->
  {Rest, list_to_integer(lists:reverse(Acc)), length(Acc), integer}.

build_identifier([H|Rest], Len, Current) when ?is_identifier(H) ->
  build_identifier(Rest, Len + 1, [H|Current]);
build_identifier(Rest, Len, Current) ->
  Identifier = list_to_atom(lists:reverse(Current)),
  {Rest, Identifier, Len, reserved_word(Identifier)}.

build_operator([H|Rest], Len, Current) when ?is_op(H) ->
  build_operator(Rest, Len + 1, [H|Current]);
build_operator(Rest, Len, Current) ->
  {Rest, lists:reverse(Current), Len}.


% eol(_Line, _Column, [{';', _}|_] = Tokens) -> Tokens;
% eol(_Line, _Column, [{',', _}|_] = Tokens) -> Tokens;
eol(Line, _Column, [{eol, _}|_] = Tokens) -> {Line + 1, Tokens};
eol(Line, Column, Tokens) -> {Line + 1, [{eol, {Line, Column, Column + 1}}|Tokens]}.

reserved_word(_) -> false.
