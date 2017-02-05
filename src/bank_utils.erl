% @hidden
-module(bank_utils).

-export([fake_alpha/1, fake_num/1, fake_alphanum/1, fake/2]).

fake_alpha(Size) ->
  bucs:to_binary(fake(Size, "AZERTYUIOPQSDFGHJKLMWXCVBN")).

fake_num(Size) ->
  bucs:to_binary(fake(Size, "1234567890")).

fake_alphanum(Size) ->
  bucs:to_binary(fake(Size, "AZERTYUIOPQSDFGHJKLMWXCVBN1234567890")).

fake(Length, AllowedChars) ->
  bucs:to_binary(bucrandom:randstr(Length, AllowedChars)).

