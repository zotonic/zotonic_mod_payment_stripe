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
