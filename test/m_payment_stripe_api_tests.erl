%% @copyright 2026 Marc Worrell
%% @doc Tests for Stripe amounts and payment normalization.
%% @end

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_payment_stripe_api_tests).
-moduledoc("
EUnit tests for Stripe API amount conversion.
").

-include_lib("eunit/include/eunit.hrl").

amount_minor_units_test() ->
    ?assertEqual(1234, m_payment_stripe_api:amount_minor_units(12.34, <<"EUR">>)),
    ?assertEqual(1234, m_payment_stripe_api:amount_minor_units(1234, <<"JPY">>)),
    ?assertEqual(123400, m_payment_stripe_api:amount_minor_units(1234, <<"HUF">>)),
    ?assertEqual(123400, m_payment_stripe_api:amount_minor_units(1234, <<"TWD">>)).

external_checkout_test() ->
    Session = #{<<"id">> => <<"cs_external">>, <<"mode">> => <<"payment">>,
        <<"status">> => <<"complete">>, <<"payment_status">> => <<"paid">>,
        <<"amount_total">> => 1234, <<"currency">> => <<"eur">>, <<"metadata">> => null},
    Event = #{<<"id">> => <<"evt_1">>, <<"created">> => 1793268000,
              <<"type">> => <<"checkout.session.completed">>},
    {ok, Data} = m_payment_stripe_api:webhook_payment(Session, Event),
    ?assertEqual(undefined, maps:get(payment_nr, Data)),
    ?assertEqual(paid, maps:get(status, Data)),
    ?assertEqual(12.34, maps:get(amount, Data)),
    ?assertEqual(<<"EUR">>, maps:get(currency, Data)),
    {ok, Failed} = m_payment_stripe_api:webhook_payment(Session#{<<"payment_status">> => <<"unpaid">>},
        Event#{<<"type">> => <<"checkout.session.async_payment_failed">>}),
    ?assertEqual(failed, maps:get(status, Failed)),
    {ok, Yen} = m_payment_stripe_api:webhook_payment(Session#{<<"currency">> => <<"jpy">>}, Event),
    ?assertEqual(1234.0, maps:get(amount, Yen)),
    {ok, Won} = m_payment_stripe_api:webhook_payment(Session#{<<"currency">> => <<"krw">>}, Event),
    ?assertEqual(1234.0, maps:get(amount, Won)),
    ?assertEqual({error, session_data}, m_payment_stripe_api:webhook_payment(
        Session#{<<"mode">> => <<"subscription">>}, Event)).
