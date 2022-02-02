%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Payment PSP module for Stripe

%% Copyright 2021 Marc Worrell
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

-mod_title("Payments using Stripe").
-mod_description("Payments using Payment Service Provider Stripe").
-mod_author("Driebit").
-mod_depends([ mod_payment ]).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,

    observe_payment_psp_request/2,
    observe_payment_psp_view_url/2,
    observe_payment_psp_status_sync/2
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
            {secret_key, <<>>}
        ]).

%% @doc Payment request, make new payment with Stripe, return payment details and a
%% redirect uri for the user to handle the payment.
observe_payment_psp_request(#payment_psp_request{ payment_id = PaymentId }, Context) ->
    m_payment_stripe_api:create(PaymentId, Context).

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
            ?LOG_WARNING("[stripe] unknown payment id ~p (~p)", [ PaymentId, StripeSessionId ]),
            Error;
        {error, _} = Error ->
            Error
    end;
observe_payment_psp_status_sync(#payment_psp_status_sync{}, _Context) ->
    undefined.
