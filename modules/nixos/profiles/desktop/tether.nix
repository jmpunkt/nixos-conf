{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.desktop.tether;
in
{
  options.profiles.desktop.tether = {
    enable = lib.mkEnableOption "Network settings for tethering.";
  };

  config = lib.mkIf cfg.enable {
    boot.kernelModules = [
      "ifb"
    ];

    boot.kernel.sysctl = {
      "net.ipv4.tcp_notsent_lowat" = 131072; # 128 KiB
      # Enable PLPMTU Probing (prevents cellular MTU blackholes)
      "net.ipv4.tcp_mtu_probing" = 1;
    };

    # Validate with: `tc -s qdisc show`
    systemd.services."cake-tether@" = {
      description = "Apply CAKE Traffic Shaping to tethered interface %I";
      after = [ "sys-subsystem-net-devices-%i.device" ];
      bindsTo = [ "sys-subsystem-net-devices-%i.device" ];
      path = [
        pkgs.iproute2
        pkgs.iputils
      ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.writeShellScript "cake-tether-start" ''
          IFACE="$1"

          # Ensure ifb0 interface is created and up
          ${pkgs.iproute2}/bin/ip link add name ifb0 type ifb 2>/dev/null || true
          ${pkgs.iproute2}/bin/ip link set dev ifb0 up

          # Flush any pre-existing qdiscs to avoid collision errors
          ${pkgs.iproute2}/bin/tc qdisc del dev "$IFACE" ingress 2>/dev/null || true
          ${pkgs.iproute2}/bin/tc qdisc del dev "$IFACE" root 2>/dev/null || true
          ${pkgs.iproute2}/bin/tc qdisc del dev ifb0 root 2>/dev/null || true

          # 1. EGRESS (Upload): Apply CAKE directly on physical interface
          # ${pkgs.iproute2}/bin/tc qdisc replace dev "$IFACE" root cake bandwidth 5mbit diffserv3 triple-isolate nonat wash ack-filter-aggressive rtt 60ms
          ${pkgs.iproute2}/bin/tc qdisc replace dev "$IFACE" root cake bandwidth 5mbit diffserv3 triple-isolate nonat ack-filter-aggressive rtt 60ms fwmark 7

          # 2. INGRESS (Download): Redirect ingress packets to ifb0 and apply autorate-ingress
          ${pkgs.iproute2}/bin/tc qdisc add dev "$IFACE" handle ffff: ingress
          ${pkgs.iproute2}/bin/tc filter add dev "$IFACE" parent ffff: matchall action mirred egress redirect dev ifb0
          ${pkgs.iproute2}/bin/tc qdisc replace dev ifb0 root cake bandwidth 10mbit autorate-ingress ingress diffserv3 triple-isolate nonat wash rtt 60ms
        ''} %i";

        ExecStop = "${pkgs.writeShellScript "cake-tether-stop" ''
          IFACE="$1"
          ${pkgs.iproute2}/bin/tc qdisc del dev "$IFACE" ingress 2>/dev/null || true
          ${pkgs.iproute2}/bin/tc qdisc del dev "$IFACE" root 2>/dev/null || true
          ${pkgs.iproute2}/bin/tc qdisc del dev ifb0 root 2>/dev/null || true
        ''} %i";
      };
    };

    services.udev.extraRules = ''
      # Trigger cake-tether@ ONLY after udev has assigned the final predictable name
      ACTION=="move", SUBSYSTEM=="net", ENV{INTERFACE}=="enp*", TAG+="systemd", ENV{SYSTEMD_WANTS}+="cake-tether@%E{INTERFACE}.service"

      # Stop it immediately when the device is unplugged so a replug always
      # starts from a clean state (kills any leftover qdiscs/filters).
      # ACTION=="remove", SUBSYSTEM=="net", KERNEL=="enp*", RUN+="${pkgs.systemd}/bin/systemctl stop 'cake-tether@%k.service'"
    '';
  };
}
