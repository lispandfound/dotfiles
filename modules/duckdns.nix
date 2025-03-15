{ config, pkgs, ... }: {
  services.duckdns.enable = true;
  services.duckdns.tokenFile = "/etc/duckdns/duckdns-token.txt";
  services.duckdns.domainsFile = "/etc/duckdns/duckdns-domains.txt";
}

