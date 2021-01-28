# zotonic_mod_mailgun

Use [mailgun](https://mailgun.com/) for relaying emails.

## Functionality

This modules supports handling the webhook results from mailgun, when email is
sent via mailgun as the relay.

If the webhook is configured then the email results from mailgun are reported back
to the Zotonic system. This is done by calling `z_email_server:delivery_report/4`,
the `#email_failed` notification, or `m_email_status:mark_read/2`

`mod_mailgun` integrates into the email status module, copying clear and block
actions to mailgun.

## Configuration

There are four configurarion keys:

 * `mod_mailgun.api_url` endpoint for mailgun API requests
 * `mod_mailgun.domain` domain as configured in mailgun
 * `mod_mailgun.api_key` key for signing API requests
 * `mod_mailgun.webhook_secret` key for signing calls from mailgun to our webhook

Besides this the relay options for email sending need to be configured.

These can be configured in the admin via System > Mail settings. There will be
an additional panel to provide the Mailgun configurations.

## Webhook

Mailgun reports sending progress back to a webhook.

The webhook, to be configured at mailgun, is at: `/mailgun-webhook`

The config key `mod_mailgun.webhook_secret` MUST be configured for this to work.

If mailgun reports that the email is sent to the next MTA then in the email log it is shown as `relayed`.

## Dependencies

This module needs `mod_email_status`.
