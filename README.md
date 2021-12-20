Stripe payments for Zotonic
===========================

This is a Payment Service Provider (PSP) module for mod_payment:

    https://github.com/zotonic/zotonic_mod_payment

This module interfaces mod_payment to the PSP Sripe (https://stripe.com/)


Configuration
-------------

The following configuration keys can be set:

 * `mod_payment_stripe.secret_key` Set this to the secret API key.
 * `mod_payment_stripe.webhook_secret` Set this to the secret used for webhook requests.

You can find all keys at: https://dashboard.stripe.com/developers

The URL for the webhook is https://example.com/stripe/webhook


Subscriptions
-------------

Periodic payments are not yet supported.
