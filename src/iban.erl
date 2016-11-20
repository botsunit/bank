% @doc
% A simple module for <a href="http://en.wikipedia.org/wiki/International_Bank_Account_Number">IBAN</a> manipulation.
% @end
-module(iban).

-export([flatten/1, is_valid/1, fake/1]).

% @doc
% Flatten the given IBAN
% @end
-spec flatten(binary()) -> binary().
flatten(IBAN) when is_binary(IBAN) ->
  binary:replace(IBAN, <<" ">>, <<"">>, [global]).

% @doc
% Validate an IBAN
% @end
-spec is_valid(string()|binary()) -> true | false.
is_valid(IBAN) when is_list(IBAN) ->
  is_valid(list_to_binary(IBAN));
is_valid(<<_:5/binary, _/binary>> = IBAN) ->
  IBAN1 = flatten(IBAN),
  case is_format_valid(IBAN1) of
    true ->
      <<Country:2/binary,
        Key:2/binary,
        BBAN/binary>> = IBAN1,
      Country1 = letter_to_num(Country, <<"">>),
      BBAN1 = letter_to_num(BBAN, <<"">>),
      binary_to_integer(<<BBAN1/binary, Country1/binary, Key/binary>>) rem 97 =:= 1;
    false -> false
  end;
is_valid(_) -> false.

% @doc
% Generate a fake IBAN
% @end
-spec fake(binary()) -> binary() | nomatch.
fake(Country) ->
  generate_fake_iban(Country).


% @hidden
letter_to_num(<<"">>, Result) -> Result;
letter_to_num(<<"A", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "10">>);
letter_to_num(<<"B", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "11">>);
letter_to_num(<<"C", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "12">>);
letter_to_num(<<"D", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "13">>);
letter_to_num(<<"E", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "14">>);
letter_to_num(<<"F", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "15">>);
letter_to_num(<<"G", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "16">>);
letter_to_num(<<"H", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "17">>);
letter_to_num(<<"I", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "18">>);
letter_to_num(<<"J", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "19">>);
letter_to_num(<<"K", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "20">>);
letter_to_num(<<"L", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "21">>);
letter_to_num(<<"M", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "22">>);
letter_to_num(<<"N", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "23">>);
letter_to_num(<<"O", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "24">>);
letter_to_num(<<"P", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "25">>);
letter_to_num(<<"Q", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "26">>);
letter_to_num(<<"R", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "27">>);
letter_to_num(<<"S", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "28">>);
letter_to_num(<<"T", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "29">>);
letter_to_num(<<"U", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "30">>);
letter_to_num(<<"V", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "31">>);
letter_to_num(<<"W", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "32">>);
letter_to_num(<<"X", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "33">>);
letter_to_num(<<"Y", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "34">>);
letter_to_num(<<"Z", Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, "35">>);
letter_to_num(<<V:1/binary, Rest/binary>>, Result) ->
  letter_to_num(Rest, <<Result/binary, V/binary>>).

is_format_valid(IBAN) ->
  case validate_country_format(IBAN) of
    nomatch -> false;
    {match, _} -> true
  end.

validate_country_format(<<"AD", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^\\d{8}[A-Z0-9]{12}$");
validate_country_format(<<"AE", _:2/binary, BBAN:19/binary>>) ->
  re:run(BBAN, "^\\d{19}$");
validate_country_format(<<"AL", _:2/binary, BBAN:24/binary>>) ->
  re:run(BBAN, "^\\d{8}[A-Z0-9]{16}$");
validate_country_format(<<"AT", _:2/binary, BBAN:16/binary>>) ->
  re:run(BBAN, "^\\d{16}$");
validate_country_format(<<"AZ", _:2/binary, BBAN:24/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}[A-Z0-9]{20}$");
validate_country_format(<<"BA", _:2/binary, BBAN:16/binary>>) ->
  re:run(BBAN, "^\\d{16}$");
validate_country_format(<<"BE", _:2/binary, BBAN:12/binary>>) ->
  re:run(BBAN, "^\\d{12}$");
validate_country_format(<<"BG", _:2/binary, BBAN:18/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{6}[A-Z0-9]{8}$");
validate_country_format(<<"BH", _:2/binary, BBAN:18/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}[A-Z0-9]{14}$");
validate_country_format(<<"CH", _:2/binary, BBAN:17/binary>>) ->
  re:run(BBAN, "^\\d{5}[A-Z0-9]{12}$");
validate_country_format(<<"CY", _:2/binary, BBAN:24/binary>>) ->
  re:run(BBAN, "^\\d{8}[A-Z0-9]{16}$");
validate_country_format(<<"CZ", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^\\d{20}$");
validate_country_format(<<"DE", _:2/binary, BBAN:18/binary>>) ->
  re:run(BBAN, "^\\d{18}$");
validate_country_format(<<"DK", _:2/binary, BBAN:14/binary>>) ->
  re:run(BBAN, "^\\d{14}$");
validate_country_format(<<"DO", _:2/binary, BBAN:24/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{20}$");
validate_country_format(<<"EE", _:2/binary, BBAN:16/binary>>) ->
  re:run(BBAN, "^\\d{16}$");
validate_country_format(<<"ES", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^\\d{20}$");
validate_country_format(<<"FI", _:2/binary, BBAN:14/binary>>) ->
  re:run(BBAN, "^\\d{14}$");
validate_country_format(<<"FO", _:2/binary, BBAN:14/binary>>) ->
  re:run(BBAN, "^\\d{14}$");
validate_country_format(<<"FR", _:2/binary, BBAN:23/binary>>) ->
  re:run(BBAN, "^\\d{10}[A-Z0-9]{11}\\d{2}$");
validate_country_format(<<"GB", _:2/binary, BBAN:18/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{14}$");
validate_country_format(<<"GE", _:2/binary, BBAN:18/binary>>) ->
  re:run(BBAN, "^[A-Z]{2}\\d{16}$");
validate_country_format(<<"GI", _:2/binary, BBAN:19/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}[A-Z0-9]{15}$");
validate_country_format(<<"GL", _:2/binary, BBAN:14/binary>>) ->
  re:run(BBAN, "^\\d{14}$");
validate_country_format(<<"GR", _:2/binary, BBAN:23/binary>>) ->
  re:run(BBAN, "^\\d{7}[A-Z0-9]{16}$");
validate_country_format(<<"HR", _:2/binary, BBAN:17/binary>>) ->
  re:run(BBAN, "^\\d{17}$");
validate_country_format(<<"HU", _:2/binary, BBAN:24/binary>>) ->
  re:run(BBAN, "^\\d{24}$");
validate_country_format(<<"IE", _:2/binary, BBAN:18/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{14}$");
validate_country_format(<<"IL", _:2/binary, BBAN:19/binary>>) ->
  re:run(BBAN, "^\\d{19}$");
validate_country_format(<<"IS", _:2/binary, BBAN:22/binary>>) ->
  re:run(BBAN, "^\\d{22}$");
validate_country_format(<<"IT", _:2/binary, BBAN:23/binary>>) ->
  re:run(BBAN, "^[A-Z]\\d{10}[A-Z0-9]{12}$");
validate_country_format(<<"JO", _:2/binary, BBAN:26/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{4}[A-Z0-9]{18}$");
validate_country_format(<<"KW", _:2/binary, BBAN:26/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{22}$");
validate_country_format(<<"KZ", _:2/binary, BBAN:16/binary>>) ->
  re:run(BBAN, "^\\d{3}[A-Z0-9]{13}$");
validate_country_format(<<"LB", _:2/binary, BBAN:24/binary>>) ->
  re:run(BBAN, "^\\d{4}[A-Z0-9]{20}$");
validate_country_format(<<"LI", _:2/binary, BBAN:17/binary>>) ->
  re:run(BBAN, "^\\d{5}[A-Z0-9]{12}$");
validate_country_format(<<"LT", _:2/binary, BBAN:16/binary>>) ->
  re:run(BBAN, "^\\d{16}$");
validate_country_format(<<"LU", _:2/binary, BBAN:16/binary>>) ->
  re:run(BBAN, "^\\d{3}[A-Z0-9]{13}$");
validate_country_format(<<"LV", _:2/binary, BBAN:17/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}[A-Z0-9]{13}$");
validate_country_format(<<"MC", _:2/binary, BBAN:23/binary>>) ->
  re:run(BBAN, "^\\d{10}[A-Z0-9]{11}\\d{2}$");
validate_country_format(<<"MD", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^[A-Z0-9]{20}$");
validate_country_format(<<"ME", _:2/binary, BBAN:18/binary>>) ->
  re:run(BBAN, "^\\d{18}$");
validate_country_format(<<"MK", _:2/binary, BBAN:15/binary>>) ->
  re:run(BBAN, "^\\d{3}[A-Z0-9]{10}\\d{2}$");
validate_country_format(<<"MR", _:2/binary, BBAN:23/binary>>) ->
  re:run(BBAN, "^\\d{23}$");
validate_country_format(<<"MT", _:2/binary, BBAN:27/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{5}[A-Z0-9]{18}$");
validate_country_format(<<"MU", _:2/binary, BBAN:26/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{19}[A-Z]{3}$");
validate_country_format(<<"NL", _:2/binary, BBAN:14/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}\\d{10}$");
validate_country_format(<<"NO", _:2/binary, BBAN:11/binary>>) ->
  re:run(BBAN, "^\\d{11}$");
validate_country_format(<<"PK", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}[A-Z0-9]{16}$");
validate_country_format(<<"PL", _:2/binary, BBAN:24/binary>>) ->
  re:run(BBAN, "^\\d{8}[A-Z0-9]{16}$");
validate_country_format(<<"PT", _:2/binary, BBAN:21/binary>>) ->
  re:run(BBAN, "^\\d{21}$");
validate_country_format(<<"QA", _:2/binary, BBAN:25/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}[A-Z0-9]{21}$");
validate_country_format(<<"RO", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^[A-Z]{4}[A-Z0-9]{16}$");
validate_country_format(<<"RS", _:2/binary, BBAN:18/binary>>) ->
  re:run(BBAN, "^\\d{18}$");
validate_country_format(<<"SA", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^\\d{2}[A-Z0-9]{18}$");
validate_country_format(<<"SE", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^\\d{20}$");
validate_country_format(<<"SI", _:2/binary, BBAN:15/binary>>) ->
  re:run(BBAN, "^\\d{15}$");
validate_country_format(<<"SK", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^\\d{20}$");
validate_country_format(<<"SM", _:2/binary, BBAN:23/binary>>) ->
  re:run(BBAN, "^[A-Z]\\d{10}[A-Z0-9]{12}$");
validate_country_format(<<"TN", _:2/binary, BBAN:20/binary>>) ->
  re:run(BBAN, "^\\d{20}$");
validate_country_format(<<"TR", _:2/binary, BBAN:22/binary>>) ->
  re:run(BBAN, "^\\d{5}[A-Z0-9]{17}$");
validate_country_format(_) -> nomatch.

generate_fake_iban(<<"AD">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(8))/binary, (bank_utils:fake_alphanum(12))/binary>>);
generate_fake_iban(<<"AE">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(19))/binary>>);
generate_fake_iban(<<"AL">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(8))/binary, (bank_utils:fake_alphanum(16))/binary>>);
generate_fake_iban(<<"AT">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(16))/binary>>);
generate_fake_iban(<<"AZ">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_alphanum(20))/binary>>);
generate_fake_iban(<<"BA">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(16))/binary>>);
generate_fake_iban(<<"BE">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(12))/binary>>);
generate_fake_iban(<<"BG">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(6))/binary, (bank_utils:fake_alphanum(8))/binary>>);
generate_fake_iban(<<"BH">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_alphanum(14))/binary>>);
generate_fake_iban(<<"CH">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(5))/binary, (bank_utils:fake_alphanum(12))/binary>>);
generate_fake_iban(<<"CY">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(8))/binary, (bank_utils:fake_alphanum(16))/binary>>);
generate_fake_iban(<<"CZ">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(20))/binary>>);
generate_fake_iban(<<"DE">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(18))/binary>>);
generate_fake_iban(<<"DK">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(14))/binary>>);
generate_fake_iban(<<"DO">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(20))/binary>>);
generate_fake_iban(<<"EE">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(16))/binary>>);
generate_fake_iban(<<"ES">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(20))/binary>>);
generate_fake_iban(<<"FI">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(14))/binary>>);
generate_fake_iban(<<"FO">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(14))/binary>>);
generate_fake_iban(<<"FR">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(10))/binary, (bank_utils:fake_alphanum(11))/binary, (bank_utils:fake_num(2))/binary>>);
generate_fake_iban(<<"GB">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(14))/binary>>);
generate_fake_iban(<<"GE">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(2))/binary, (bank_utils:fake_num(16))/binary>>);
generate_fake_iban(<<"GI">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_alphanum(15))/binary>>);
generate_fake_iban(<<"GL">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(14))/binary>>);
generate_fake_iban(<<"GR">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(7))/binary, (bank_utils:fake_alphanum(16))/binary>>);
generate_fake_iban(<<"HR">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(17))/binary>>);
generate_fake_iban(<<"HU">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(24))/binary>>);
generate_fake_iban(<<"IE">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(14))/binary>>);
generate_fake_iban(<<"IL">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(19))/binary>>);
generate_fake_iban(<<"IS">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(22))/binary>>);
generate_fake_iban(<<"IT">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(1))/binary, (bank_utils:fake_num(10))/binary, (bank_utils:fake_alphanum(12))/binary>>);
generate_fake_iban(<<"JO">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(4))/binary, (bank_utils:fake_alphanum(18))/binary>>);
generate_fake_iban(<<"KW">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(22))/binary>>);
generate_fake_iban(<<"KZ">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(3))/binary, (bank_utils:fake_alphanum(13))/binary>>);
generate_fake_iban(<<"LB">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(4))/binary, (bank_utils:fake_alphanum(20))/binary>>);
generate_fake_iban(<<"LI">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(5))/binary, (bank_utils:fake_alphanum(12))/binary>>);
generate_fake_iban(<<"LT">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(16))/binary>>);
generate_fake_iban(<<"LU">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(3))/binary, (bank_utils:fake_alphanum(13))/binary>>);
generate_fake_iban(<<"LV">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_alphanum(13))/binary>>);
generate_fake_iban(<<"MC">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(10))/binary, (bank_utils:fake_alphanum(11))/binary, (bank_utils:fake_num(2))/binary>>);
generate_fake_iban(<<"MD">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alphanum(20))/binary>>);
generate_fake_iban(<<"ME">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(18))/binary>>);
generate_fake_iban(<<"MK">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(3))/binary, (bank_utils:fake_alphanum(10))/binary, (bank_utils:fake_num(2))/binary>>);
generate_fake_iban(<<"MR">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(23))/binary>>);
generate_fake_iban(<<"MT">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(5))/binary, (bank_utils:fake_alphanum(18))/binary>>);
generate_fake_iban(<<"MU">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(19))/binary, (bank_utils:fake_alpha(3))/binary>>);
generate_fake_iban(<<"NL">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_num(10))/binary>>);
generate_fake_iban(<<"NO">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(11))/binary>>);
generate_fake_iban(<<"PK">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_alphanum(16))/binary>>);
generate_fake_iban(<<"PL">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(8))/binary, (bank_utils:fake_alphanum(16))/binary>>);
generate_fake_iban(<<"PT">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(21))/binary>>);
generate_fake_iban(<<"QA">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_alphanum(21))/binary>>);
generate_fake_iban(<<"RO">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(4))/binary, (bank_utils:fake_alphanum(16))/binary>>);
generate_fake_iban(<<"RS">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(18))/binary>>);
generate_fake_iban(<<"SA">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(2))/binary, (bank_utils:fake_alphanum(18))/binary>>);
generate_fake_iban(<<"SE">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(20))/binary>>);
generate_fake_iban(<<"SI">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(15))/binary>>);
generate_fake_iban(<<"SK">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(20))/binary>>);
generate_fake_iban(<<"SM">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_alpha(1))/binary, (bank_utils:fake_num(10))/binary, (bank_utils:fake_alphanum(12))/binary>>);
generate_fake_iban(<<"TN">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(20))/binary>>);
generate_fake_iban(<<"TR">> = Country) ->
  generate_fake_with_key(Country, <<(bank_utils:fake_num(5))/binary, (bank_utils:fake_alphanum(17))/binary>>);
generate_fake_iban(_) -> nomatch.

generate_fake_with_key(Country, BBAN) ->
  Key = bucs:to_binary(
          lists:flatten(io_lib:format(
                          "~2.10.0B",
                          [98 - (binary_to_integer(letter_to_num(<<BBAN/binary, Country/binary, "00">>, <<"">>)) rem 97)]))),
  <<Country/binary, Key/binary, BBAN/binary>>.

