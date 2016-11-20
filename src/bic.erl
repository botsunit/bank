% @doc
% A simple module for <a href="http://en.wikipedia.org/wiki/ISO_9362">BIC</a> manipulation.
% @end
-module(bic).

-export([is_valid/1, fake/1]).

% @doc
% Validate a BIC code
% @end
-spec is_valid(string()|binary()) -> true | false.
is_valid(BIC) when is_list(BIC) ->
  is_valid(list_to_binary(BIC));
is_valid(BIC) when is_binary(BIC) ->
  case re:run(BIC, "\\A[A-Z]{6,6}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3,3}){0,1}\\z") of
    nomatch -> false;
    {match, _} -> true
  end.

% @doc
% Generate a fake BIC code
% @end
-spec fake(binary()) -> binary().
fake(Country) ->
  <<(bank_utils:fake_alpha(4))/binary,
    Country/binary,
    (bank_utils:fake(1, "AZERTYUIOPQSDFGHJKLMWXCVBN23456789"))/binary,
    (bank_utils:fake(1, "AZERTYUIPQSDFGHJKLMWXCVBN1234567890"))/binary,
    (bank_utils:fake_alphanum(3))/binary>>.

