{ config, pkgs, ... }: {
  environment.systemPackages = with pkgs; [ wireguard wireguard-tools ];
  networking.wg-quick.interfaces.wg0 = {
    # This is the client, so it doesn't listen on a port unless it also acts as a server for other peers.
    # ListenPort = 51820; # Usually not needed for the client side initiating the connection

    privateKeyFile =
      "/etc/wireguard/privatekey"; # Path to your NixOS private key file
    address = [
      "10.0.0.2/24"
    ]; # IP address of the NixOS server within the WireGuard tunnel

    peers = [{
      # This is your Ubuntu VPS
      publicKey = "TXYU5/q9r2lWYrR18kCfmVKdHH3vEHYjbA6iAbWAylg=";
      allowedIPs =
        [ "10.0.0.0/24" ]; # Can route all VPN subnet traffic through the VPS
      endpoint = "139.99.195.31:51820"; # Public IP and port of your VPS
      persistentKeepalive =
        25; # Sends a packet every 25 seconds to keep NAT open
    }];
  };
}
