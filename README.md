Stripe payments for Zotonic
===========================

This is a Payment Service Provider (PSP) module for [zotonic_mod_payment](https://github.com/zotonic/zotonic_mod_payment).

This module interfaces mod_payment to the PSP Stripe (https://stripe.com/)


Configuration
-------------

The following configuration keys can be set:

 * `mod_payment_stripe.secret_key` Set this to the secret API key.
 * `mod_payment_stripe.webhook_secret` Set this to the secret used for webhook requests.

You can find all keys at: https://dashboard.stripe.com/developers

The URL for the webhook is https://example.com/stripe/webhook
You will need to activate the "checkout" (`checkout.session.*`) events.


Subscriptions
-------------

Periodic payments are not yet supported.


Receiving external payments
---------------------------

With `mod_payment.accept_unknown_payments = true`, verified Checkout Session
webhooks can import payments without Zotonic metadata. Imports use the shared
`mod_payment:receive_payment/3` API. This covers the existing Checkout Session
events, not PaymentIntent, subscription, or charge event families. Redirects
cannot import payments, and no polling is added.

Four-hour discovery is enabled by `mod_payment.accept_unknown_payments`, unless
`mod_payment.disable_unknown_payment_polling` is set. It lists payment-mode
Checkout Sessions only; direct Charges/PaymentIntents and subscriptions are not
discovered by this module. Scheduling and import policy belong to `mod_payment`.

Discovery also scans [Events](https://docs.stripe.com/api/events/list) by event date
for Checkout Session updates and full charge refunds. Refunds resolve to their
Checkout Session identity. Recent events can import or update older payments;
Stripe retains events for 30 days. Partial refunds are not represented by this
module.
