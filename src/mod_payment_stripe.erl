%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2026 Marc Worrell
%% @doc Payment PSP module for Stripe.
%%
%% Configuration:
%%
%% <ul>
%%   <li><code>mod_payment_stripe.secret_key</code>:
%%       Stripe secret API key, used for Stripe API requests.</li>
%%   <li><code>mod_payment_stripe.webhook_secret</code>:
%%       Stripe webhook signing secret, used to verify incoming webhook requests.</li>
%% </ul>
%%
%% The Stripe webhook URL is <code>/stripe/webhook</code>. Configure it in Stripe
%% to send the Checkout events (<code>checkout.session.*</code>).
%% @end

%% Copyright 2021-2026 Marc Worrell
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

-module(mod_payment_stripe).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "site_administrator", "module",
        "payments", "configuration", "api_and_integration"
    ]
}).
-moduledoc("
Provide Stripe Checkout payments through `mod_payment`.

Enable this module together with `mod_payment`. Configure `secret_key` and
`webhook_secret` under **System → Payment configuration**, requiring
`use.mod_admin_config`. Use credentials and a signing secret for the intended
Stripe environment. Set
`#payment_request{preferred_psp_module = mod_payment_stripe}` to select Stripe.
This adapter creates payment-mode Checkout Sessions and does not create recurring
subscriptions.

Configure Stripe to deliver Checkout Session events to `/stripe/webhook`.
Incoming requests are verified with `webhook_secret` before their session data is
passed to the shared payment receive logic. The browser returns through
`/stripe/ready/:payment_nr`; status is synchronized with the Stripe API.

When `mod_payment.accept_unknown_payments` is enabled, verified webhooks may import
unknown payments. Four-hour discovery additionally requires that
`mod_payment.disable_unknown_payment_polling` is false. Discovery covers
payment-mode Checkout Sessions within the shared retention window; direct Charges,
PaymentIntents and subscriptions are not discovered.
").


-mod_title("Payments using Stripe").
-mod_description("Payments using Payment Service Provider Stripe").
-mod_author("Driebit").
-mod_depends([ mod_payment ]).
-mod_config([
    #{
        key => secret_key,
        is_secret => true,
        type => binary,
        default => <<>>,
        description => "Stripe secret key."
    },
    #{
        key => webhook_secret,
        is_secret => true,
        type => binary,
        default => <<>>,
        description => "Stripe webhook signing secret."
    }
]).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,

    observe_payment_psp_request/2,
    observe_payment_psp_view_url/2,
    observe_payment_psp_status_sync/2,
    observe_payment_psp_poll/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("zotonic_mod_payment/include/payment.hrl").

init(Context) ->
    lists:foreach(
        fun({K, V}) ->
            case m_config:get_value(?MODULE, K, Context) of
                undefined ->
                    m_config:set_value(?MODULE, K, V, Context);
                _ ->
                    ok
            end
        end,
        [
            {secret_key, <<>>},
            {webhook_secret, <<>>}
        ]).

%% @doc Payment request, make new payment with Stripe, return payment details and a
%% redirect uri for the user to handle the payment.
observe_payment_psp_request(#payment_psp_request{
        payment_id = PaymentId,
        preferred_psp_module = PreferredPspModule
    }, Context) when PreferredPspModule =:= undefined;
                     PreferredPspModule =:= ?MODULE ->
    m_payment_stripe_api:create(PaymentId, Context);
observe_payment_psp_request(#payment_psp_request{}, _Context) ->
    undefined.

%% @doc Return the URL where the given payment can be viewed on the Stripe website.
observe_payment_psp_view_url(#payment_psp_view_url{ psp_module = ?MODULE, psp_data = Data }, Context) ->
    m_payment_stripe_api:payment_url(Data, Context);
observe_payment_psp_view_url(#payment_psp_view_url{}, _Context) ->
    undefined.

%% @doc Synchronize the payment status from Strip to the local payment tables.
observe_payment_psp_status_sync(#payment_psp_status_sync{
        payment_id = PaymentId,
        psp_module = ?MODULE,
        psp_external_id = StripeSessionId
    }, Context) ->
    case m_payment_stripe_api:sync_payment_session_status(StripeSessionId, Context) of
        {ok, _} ->
            ok;
        {error, 404} = Error ->
            ?LOG_WARNING(#{
                in => zotonic_mod_payment_stripe,
                text => <<"Stripe payment session not found">>,
                result => error,
                reason => not_found,
                payment_id => PaymentId,
                stripe_session_id => StripeSessionId
            }),
            Error;
        {error, _} = Error ->
            Error
    end;
observe_payment_psp_status_sync(#payment_psp_status_sync{}, _Context) ->
    undefined.

%% Discovery pages are imported and checkpointed by mod_payment.
observe_payment_psp_poll(#payment_psp_poll{psp_module = undefined}, _Context) ->
    ?MODULE;
observe_payment_psp_poll(#payment_psp_poll{psp_module = ?MODULE} = Request, Context) ->
    payment_stripe_poll:page(Request, Context);
observe_payment_psp_poll(#payment_psp_poll{}, _Context) ->
    undefined.
