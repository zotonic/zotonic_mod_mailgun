{#
 # Shown on the mod_admin_config email configuration page
 #}
<div class="widget">
    <div class="widget-header">
        {_ Mailgun _}
        <span class="text-muted pull-right">mod_mailgun</span>
    </div>
    <div class="widget-content">
        <p>
            <a href="https://mailgun.com/">Mailgun</a> {_ is a relay for email. _}
            {_ Configure the general proxy settings to enable Mailgun as an email relay. _}
            {_ To receive Mailgun status updates and synchronize email blocks, add the Mailgun API secrets below. _}
        </p>

        {% wire type="submit"
                id="mailgun_settings"
                postback={config_save module=`mod_mailgun`}
                delegate=`mod_admin_config`
        %}
        <form id="mailgun_settings" class="form" action="postback">
            <div class="form-group label-floating">
                <input type="text" class="form-control" name="domain" value="{{ m.config.mod_mailgun.domain.value|escape }}" placeholder="{_ Domain _}">
                <label class="control-label">{_ Domain _}</label>
                <p class="help-block">{_ Domain as configured in Mailgun. _}</p>
            </div>
            <div class="form-group label-floating">
                <input type="text" class="form-control" name="api_url" value="{{ m.config.mod_mailgun.api_url.value|escape }}"
                placeholder="{_ API URL _}">
                <label class="control-label">{_ API URL _}</label>
                <p class="help-block">
                    {_ Endpoint for Mailgun API requests. _}<br>
                    {_ Default _}: <tt>https://api.mailgun.net/v3/[domain]</tt><br>
                    {% trans "For Europe, use: <tt>{api}</tt>" api="https://api.eu.mailgun.net/v3/[domain]" %}
                </p>
            </div>
            <div class="form-group label-floating">
                <input type="text" class="form-control" name="api_key" value="{{ m.config.mod_mailgun.api_key.value }}" placeholder="{_ API Key _}">
                <label class="control-label">{_ API Key _}</label>
                <p class="help-block">{_ Key for signing API requests. _}</p>
            </div>
            <div class="form-group label-floating">
                <input type="text" class="form-control" name="webhook_secret" value="{{ m.config.mod_mailgun.webhook_secret.value }}" placeholder="{_ Webhook Secret _}">
                <label class="control-label">{_ Webhook Secret _}</label>
                <p class="help-block">{_ Key for signing calls from Mailgun to the Zotonic webhook. _}</p>
            </div>
            <div class="form-actions">
                <button type="submit" class="btn btn-primary">{_ Save _}</button>
            </div>
        </form>

        <p>
            <br>
            <br>
            <span class="glyphicon glyphicon-info-sign"></span> {_ The webhook URL is: _}
            <tt>{% url mailgun_webhook absolute_url %}</tt>
        </p>
    </div>
</div>
