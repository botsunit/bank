-module(bic_tests).

-include_lib("eunit/include/eunit.hrl").

bic_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_valid())
    , ?_test(t_fake())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_valid() ->
  ?assertEqual(true, bic:is_valid("DEUTDEDBDUE")),
  ?assertEqual(true, bic:is_valid("COBADEFF")),
  ?assertEqual(true, bic:is_valid("GKCCBEBB")),
  ?assertEqual(true, bic:is_valid("ATCICIAB")),
  ?assertEqual(true, bic:is_valid("GEBABEBB")),
  ?assertEqual(true, bic:is_valid("PSSTFRPPNTE")),
  ?assertEqual(true, bic:is_valid("PSSTFRPPMON")),
  ?assertEqual(true, bic:is_valid("CCDQCAMM")),
  ?assertEqual(false, bic:is_valid("000000AA")),
  ?assertEqual(false, bic:is_valid("XXXXXX00")),
  ?assertEqual(false, bic:is_valid("XXXXXX10")),
  ?assertEqual(false, bic:is_valid("XXXXXX2O")).

t_fake() ->
  ?assertEqual(true, bic:is_valid(bic:fake(<<"AD">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"AE">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"AL">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"AT">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"AZ">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"BA">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"BE">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"BG">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"BH">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"CH">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"CY">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"CZ">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"DE">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"DK">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"DO">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"EE">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"ES">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"FI">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"FO">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"FR">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"GB">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"GE">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"GI">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"GL">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"GR">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"HR">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"HU">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"IE">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"IL">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"IS">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"IT">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"JO">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"KW">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"KZ">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"LB">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"LI">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"LT">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"LU">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"LV">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"MC">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"MD">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"ME">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"MK">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"MR">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"MT">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"MU">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"NL">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"NO">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"PK">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"PL">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"PT">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"QA">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"RO">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"RS">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"SA">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"SE">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"SI">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"SK">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"SM">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"TN">>))),
  ?assertEqual(true, bic:is_valid(bic:fake(<<"TR">>))).

