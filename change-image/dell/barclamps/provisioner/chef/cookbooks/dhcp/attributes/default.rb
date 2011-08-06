
default[:dhcp][:interfaces] = [ "eth0" ]
default[:dhcp][:options] = [
    "ddns-update-style ad-hoc"
    "option dhcp-client-state code 225 = unsigned integer 16",
    "option dhcp-client-state 0",
    "option dhcp-client-debug code 226 = unsigned integer 16",
    "option dhcp-client-debug 0"
]

