#!/usr/bin/env bash
set -euo pipefail

domain=tswirly.duckdns.org
port=8000
owner=${SUDO_USER:-tom}
owner_home=$(dscl . -read "/Users/$owner" NFSHomeDirectory | awk '{print $2}')
site_dir="$owner_home/Sites/ok"
cert_dir="$site_dir/certs"
plist="$owner_home/Library/LaunchAgents/com.tom.ok-page.plist"
uid=$(id -u "$owner")

if [[ $(id -u) -ne 0 ]]; then
  echo "usage: sudo certs.sh" >&2
  exit 2
fi

if ! command -v certbot >/dev/null; then
  echo "certbot is not installed. Install it with: brew install certbot" >&2
  exit 1
fi

mkdir -p "$cert_dir" "$owner_home/Library/LaunchAgents"
printf 'ok\n' > "$site_dir/index.html"

cat > "$site_dir/serve_https.py" <<PY
from __future__ import annotations

import os
import ssl
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

root = Path("$site_dir")
os.chdir(root)

server = ThreadingHTTPServer(("0.0.0.0", $port), SimpleHTTPRequestHandler)
context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(root / "certs" / "fullchain.pem", root / "certs" / "privkey.pem")
server.socket = context.wrap_socket(server.socket, server_side=True)
server.serve_forever()
PY

echo "Make sure router TCP port 80 temporarily forwards to this Mac before continuing."
echo "Press return to request/renew the certificate for $domain."
read -r

certbot certonly --standalone -d "$domain"

cp "/etc/letsencrypt/live/$domain/fullchain.pem" "$cert_dir/fullchain.pem"
cp "/etc/letsencrypt/live/$domain/privkey.pem" "$cert_dir/privkey.pem"
chown -R "$owner":staff "$site_dir" "$owner_home/Library/LaunchAgents"
chmod 600 "$cert_dir/privkey.pem"

cat > "$plist" <<XML
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>com.tom.ok-page</string>

  <key>ProgramArguments</key>
  <array>
    <string>/usr/bin/python3</string>
    <string>$site_dir/serve_https.py</string>
  </array>

  <key>RunAtLoad</key>
  <true/>

  <key>KeepAlive</key>
  <true/>

  <key>StandardOutPath</key>
  <string>/tmp/ok-page.log</string>
  <key>StandardErrorPath</key>
  <string>/tmp/ok-page.err</string>
</dict>
</plist>
XML

chown "$owner":staff "$plist"

launchctl bootout "gui/$uid" "$plist" 2>/dev/null || true
launchctl bootstrap "gui/$uid" "$plist"
launchctl enable "gui/$uid/com.tom.ok-page"

echo "Ready: https://$domain:$port/"
