
default[:dhcp][:interfaces] = [ "eth0" ]
default[:dhcp][:options] = [
    "ddns-update-style none",
    "allow booting",
    "option option-128 code 128 = string",
    "option option-129 code 129 = text",
    "next-server 192.168.124.10",
    "option dhcp-client-state code 225 = unsigned integer 16",
    "option dhcp-client-state 0",
    "option dhcp-client-debug code 226 = unsigned integer 16",
    "option dhcp-client-debug 0"
]

