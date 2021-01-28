<div class="panel panel-default">
    <div class="panel-heading">
        <div class="panel-title">{_ Mailgun _}</div>
    </div>
    <div class="panel-body">
        <p>
            {_ Mailgun can be used as a relay for email. _}
            {_ Configure the general proxy settings to enable Mailgun as an email relay. _}
            {_ To receive Mailgun mailing status updates and synchronize email blocks, add the mailgun API secrets below. _}
        </p>

        {% wire type="submit"
                id="mailgun_settings"
                postback={config_save module=`mod_mailgun`}
                delegate=`mod_admin_config`
        %}
        <form id="mailgun_settings" class="form" action="postback">
            <div class="form-group row ">
                <label class="control-label col-md-3">{_ Domain _}</label>
                <div class="col-md-6">
                    <input type="text" class="form-control" name="domain" value="{{ m.config.mod_mailgun.domain.value|escape }}">
                    <p class="help-block">{_ Domain as configured in mailgun. _}</p>
                </div>
            </div>
            <div class="form-group row ">
                <label class="control-label col-md-3">{_ API URL _}</label>
                <div class="col-md-9">
                    <input type="text" class="form-control" name="api_url" value="{{ m.config.mod_mailgun.api_url.value|escape }}">
                    <p class="help-block">{_ Endpoint for mailgun API requests. _}</p>
                </div>
            </div>
            <div class="form-group row ">
                <label class="control-label col-md-3">{_ API Key _}</label>
                <div class="col-md-6">
                    <input type="text" class="form-control" name="api_key" value="{{ m.config.mod_mailgun.api_key.value }}">
                    <p class="help-block">{_ Key for signing API requests. _}</p>
                </div>
            </div>
            <div class="form-group row ">
                <label class="control-label col-md-3">{_ Webhook Secret _}</label>
                <div class="col-md-6">
                    <input type="text" class="form-control" name="webhook_secret" value="{{ m.config.mod_mailgun.webhook_secret.value }}">
                    <p class="help-block">{_ Key for signing calls from mailgun to the Zotonic webhook. _}</p>
                </div>
            </div>
            <div class="form-actions row">
                <div class="col-md-offset-3 col-md-9">
                    <button type="submit" class="btn btn-primary">{_ Save _}</button>
                </div>
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
