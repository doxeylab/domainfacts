# `shiny-server` Open Source Edition Setup

These instructions were written with Ubuntu 16.04.6 LTS in mind. Follow these instructions to host the `DomainStats` app on a server with SSL support.

## Requirements

First install the following programs:

- [`shiny-server`](https://rstudio.com/products/shiny/download-server/ubuntu/)
- [`nginx`](https://www.nginx.com/resources/wiki/start/topics/tutorials/install/)
- [`certbot`](https://certbot.eff.org/docs/install.html)

## Configuration

There are two sample configuration files included in this directory: one for `shiny-server`, and one for `nginx`. The `shiny-server.conf` file (on Ubuntu) should be copied to `/etc/shiny-server/shiny-server.conf`, and the `nginx.conf` file to `/etc/nginx/sites-available/default`. Before copying these over hwoever, first edit these as discussed below.

### `shiny-server`

The included configuration file is merely a template: you must change the `$USER`, `$LOGS` and `$HOME` variables to match your setup. The `$LOGS` folder will be used by `shiny-server` to dump logs.

### `nginx`

The included configuration file will need to be edited. Currently, it points to our `domainstats.uwaterloo.ca` domain.

## SSL

Run the following command: `sudo certbot --nginx`. This will walk you through creating certificates for your domain. To renew the certificate, run `sudo certbot renew`.

## Launching the server

Run the following commands to load the new configuration files:

```
sudo systemctl restart nginx
sudo systemctl restart shiny-server
```
