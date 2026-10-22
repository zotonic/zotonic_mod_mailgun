%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2024 Maximonster Interactive Things
%% @doc Integration of mailgun as email relay.
%% @end

%% Copyright 2020-2024 Maximonster Interactive Things
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

-module(mod_mailgun).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "site_administrator", "backend_developer", "module",
        "email_delivery", "configuration", "api_and_integration", "send_and_receive"
    ]
}).
-moduledoc("
Relay outgoing email through the Mailgun HTTP API and receive delivery and
engagement events through a signed webhook. Requires `mod_email_status`.

## Setup and configuration

Enable `mod_mailgun` on the site. Configure its per-site `m_config` values in
the Mailgun panel under **System > Mail settings**. Saving these settings
requires an administrator with configuration-edit permission:

| Key | Meaning |
| --- | --- |
| `mod_mailgun.domain` | Sending domain configured in Mailgun. |
| `mod_mailgun.api_url` | Full API base URL including the domain, without a trailing slash. Defaults to `https://api.mailgun.net/v3/` followed by `domain`. |
| `mod_mailgun.api_key` | API key used for HTTP Basic authentication with username `api`. |
| `mod_mailgun.webhook_secret` | Signing key used to verify incoming webhook requests. |

For example, a domain of `mg.example.com` uses the default API base URL
`https://api.mailgun.net/v3/mg.example.com`. The administration panel also
provides a regional endpoint example. Configure both the sending domain (or a
complete API URL) and the API key before sending mail.

## Outgoing mail and address status

The `email_send_encoded` observer posts the encoded MIME message to
`<api_url>/messages.mime`. This path uses the HTTP API and does not require
SMTP relay settings. If both `api_url` and `api_key` are empty, the observer
returns `undefined`, leaving delivery to other handlers. Setting either value
activates the API path; this presence check does not verify the credentials or
service availability. A domain alone does not activate it.

An HTTP 200 response means Mailgun accepted the message. Other HTTP responses
are returned as temporary failures; delivery to the recipient is reported
separately by the webhook.

Manual `email_status` changes synchronize the address with Mailgun's bounce
list: blocking adds it and clearing removes it. Non-manual status changes are
ignored. The exported `mailgun_block/2` and `mailgun_clear/2` helpers return the
HTTP client result, or `ok` when the API is unconfigured. These Erlang helpers
do not perform an ACL check; callers must authorize manual changes.

## Webhook

Configure Mailgun to post events to the site's absolute `/mailgun-webhook`
URL and set `webhook_secret` to the matching signing key. The dispatch name
is `mailgun_webhook`; in templates use `{% url mailgun_webhook absolute_url %}`.
The endpoint accepts JSON POST requests authenticated by an HMAC signature,
without requiring a Zotonic user session.

Delivered messages are reported as `relayed`, opened and clicked events mark
the address as read, and failed events update delivery status. Spam complaints
emit a final `email_failed` notification. Unsubscribe events are logged and
ignored. See `controller_mailgun_webhook` for the event mapping.
").

-mod_title("Mailgun email relay").
-mod_description("Integrate mailgun as email relay.").
-mod_depends([ mod_email_status ]).

-export([
    observe_email_status/2,
    observe_email_send_encoded/2
    ]).

% Testing
-export([
    mailgun_block/2,
    mailgun_clear/2
    ]).

-define(MAILGUN_API_URL, "https://api.mailgun.net/v3/").

-include_lib("zotonic_core/include/zotonic.hrl").


observe_email_status(#email_status{ is_manual = true, is_valid = false, recipient = Email }, Context) ->
    mailgun_block(Email, Context);
observe_email_status(#email_status{ is_manual = true, is_valid = true, recipient = Email }, Context) ->
    mailgun_clear(Email, Context);
observe_email_status(#email_status{}, _Context) ->
    ok.


observe_email_send_encoded(#email_send_encoded{
            message_nr = MsgId,
            from = _From,
            to = To,
            encoded = EncodedEmail,
            options = _Options
        },
        Context) ->
    case is_mailgun_configured(Context) of
        true ->
            send_email(MsgId, To, EncodedEmail, Context);
        false ->
            undefined
    end.

send_email(MsgId, To, EncodedEmail, Context) ->
    ApiUrl = mailgun_api_url(Context),
    ?LOG_INFO(#{
        in => zotonic_mod_mailgun,
        text => <<"Relaying email via mailgun">>,
        recipient => To,
        message_id => MsgId,
        api_url => ApiUrl
    }),
    PostData = [
        #{
            name => <<"to">>,
            value => To
        },
        #{
            name => <<"message">>,
            data => EncodedEmail,
            filename => <<"message.eml">>,
            mime => <<"message/rfc822">>
        }
    ],
    {Body, CT} = z_multipart_encode:encode(PostData),
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    Auth = base64:encode_to_string( "api:" ++ z_convert:to_list(ApiKey) ),
    Hs = [
        {"Content-Length", integer_to_list(size(Body))},
        {"Authorization", "Basic " ++ Auth}
    ],
    case httpc:request(
        post,
        {ApiUrl ++ "/messages.mime", Hs, z_convert:to_list(CT), Body},
        httpc_http_options(),
        httpc_options())
    of
        {ok, {{_, 200, _}, _Headers, Payload}} ->
            #{
                <<"id">> := MailgunMsgId,
                <<"message">> := Message
            } = z_json:decode(Payload),
            {ok, <<"200 [Mailgun-API] ", Message/binary, " ", MailgunMsgId/binary>>};
        {ok, {{_, EAccess, _}, _Headers, _Payload}}
            when EAccess =:= 401;
                 EAccess =:= 403 ->
            {error, eacces, {temporary_failure, "mailgun.com", <<"401 error on Mailgun API - check the API key">>}};
        {ok, {{_, Err, _}, _Headers, _Payload}}
            when Err >= 500 ->
            ErrB = z_convert:to_binary(Err),
            {error, internal_error, {temporary_failure, "mailgun.com", <<ErrB/binary, " error on Mailgun API">>}};
        {ok, {{_, Err, _}, _Headers, _Payload}} ->
            ErrB = z_convert:to_binary(Err),
            {error, unknown, {temporary_failure, "mailgun.com", <<ErrB/binary, " error on Mailgun API">>}};
        {error, _} = Error ->
            Error
    end.

mailgun_clear(Email, Context) ->
    case is_mailgun_configured(Context) of
        true ->
            Url = iolist_to_binary([ "/bounces/", z_url:url_encode(Email) ]),
            Payload = <<>>,
            mailgun_api(delete, Url, Payload, Context);
        false ->
            ok
    end.

mailgun_block(Email, Context) ->
    case is_mailgun_configured(Context) of
        true ->
            Url = iolist_to_binary([ "/bounces" ]),
            Payload = iolist_to_binary([
                    "address=", z_url:url_encode(Email)
                ]),
            mailgun_api(post, Url, Payload, Context);
        false ->
            ok
    end.

is_mailgun_configured(Context) ->
    ApiUrl = z_convert:to_binary( m_config:get_value(mod_mailgun, api_url, Context) ),
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    case {ApiUrl, ApiKey} of
        {<<>>, <<>>} ->
            false;
        _ ->
            true
    end.

mailgun_api(delete, Url, _Payload, Context) ->
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    Auth = base64:encode_to_string( "api:" ++ z_convert:to_list(ApiKey) ),
    Hs = [
        {"Authorization", "Basic " ++ Auth}
    ],
    httpc:request(
        delete,
        {mailgun_api_url(Context) ++ z_convert:to_list(Url), Hs},
        httpc_http_options(),
        httpc_options());
mailgun_api(post, Url, Payload, Context) ->
    ApiKey = z_convert:to_binary( m_config:get_value(mod_mailgun, api_key, Context) ),
    Auth = base64:encode_to_string( "api:" ++ z_convert:to_list(ApiKey) ),
    Hs = [
        {"Authorization", "Basic " ++ Auth}
    ],
    httpc:request(
        post,
        {mailgun_api_url(Context) ++ z_convert:to_list(Url), Hs, "application/x-www-form-urlencoded", Payload},
        httpc_http_options(),
        httpc_options()).


mailgun_api_url(Context) ->
    URL = case z_convert:to_binary( m_config:get_value(mod_mailgun, api_url, Context) ) of
        <<>> ->
            lists:flatten([
                ?MAILGUN_API_URL,
                z_convert:to_list( m_config:get_value(mod_mailgun, domain, Context) )
            ]);
        Api ->
            Api
    end,
    z_convert:to_list(URL).


httpc_http_options() ->
    [
        {autoredirect, true},
        {relaxed, true},
        {timeout, 10000},
        {connect_timeout, 2000}
    ].

httpc_options() ->
    [
        {sync, true},
        {body_format, binary}
    ].


