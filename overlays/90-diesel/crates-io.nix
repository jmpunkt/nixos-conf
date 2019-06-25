{ lib, buildRustCrate, buildRustCrateHelpers }:
with buildRustCrateHelpers;
let inherit (lib.lists) fold;
    inherit (lib.attrsets) recursiveUpdate;
in
rec {

# aho-corasick-0.5.3

  crates.aho_corasick."0.5.3" = deps: { features?(features_.aho_corasick."0.5.3" deps {}) }: buildRustCrate {
    crateName = "aho-corasick";
    version = "0.5.3";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" ];
    sha256 = "1igab46mvgknga3sxkqc917yfff0wsjxjzabdigmh240p5qxqlnn";
    libName = "aho_corasick";
    crateBin =
      [{  name = "aho-corasick-dot"; }];
    dependencies = mapFeatures features ([
      (crates."memchr"."${deps."aho_corasick"."0.5.3"."memchr"}" deps)
    ]);
  };
  features_.aho_corasick."0.5.3" = deps: f: updateFeatures f (rec {
    aho_corasick."0.5.3".default = (f.aho_corasick."0.5.3".default or true);
    memchr."${deps.aho_corasick."0.5.3".memchr}".default = true;
  }) [
    (features_.memchr."${deps."aho_corasick"."0.5.3"."memchr"}" deps)
  ];


# end
# aho-corasick-0.6.10

  crates.aho_corasick."0.6.10" = deps: { features?(features_.aho_corasick."0.6.10" deps {}) }: buildRustCrate {
    crateName = "aho-corasick";
    version = "0.6.10";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" ];
    sha256 = "0bhasxfpmfmz1460chwsx59vdld05axvmk1nbp3sd48xav3d108p";
    libName = "aho_corasick";
    crateBin =
      [{  name = "aho-corasick-dot";  path = "src/main.rs"; }];
    dependencies = mapFeatures features ([
      (crates."memchr"."${deps."aho_corasick"."0.6.10"."memchr"}" deps)
    ]);
  };
  features_.aho_corasick."0.6.10" = deps: f: updateFeatures f (rec {
    aho_corasick."0.6.10".default = (f.aho_corasick."0.6.10".default or true);
    memchr."${deps.aho_corasick."0.6.10".memchr}".default = true;
  }) [
    (features_.memchr."${deps."aho_corasick"."0.6.10"."memchr"}" deps)
  ];


# end
# ansi_term-0.11.0

  crates.ansi_term."0.11.0" = deps: { features?(features_.ansi_term."0.11.0" deps {}) }: buildRustCrate {
    crateName = "ansi_term";
    version = "0.11.0";
    authors = [ "ogham@bsago.me" "Ryan Scheel (Havvy) <ryan.havvy@gmail.com>" "Josh Triplett <josh@joshtriplett.org>" ];
    sha256 = "08fk0p2xvkqpmz3zlrwnf6l8sj2vngw464rvzspzp31sbgxbwm4v";
    dependencies = (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."ansi_term"."0.11.0"."winapi"}" deps)
    ]) else []);
  };
  features_.ansi_term."0.11.0" = deps: f: updateFeatures f (rec {
    ansi_term."0.11.0".default = (f.ansi_term."0.11.0".default or true);
    winapi = fold recursiveUpdate {} [
      { "${deps.ansi_term."0.11.0".winapi}"."consoleapi" = true; }
      { "${deps.ansi_term."0.11.0".winapi}"."errhandlingapi" = true; }
      { "${deps.ansi_term."0.11.0".winapi}"."processenv" = true; }
      { "${deps.ansi_term."0.11.0".winapi}".default = true; }
    ];
  }) [
    (features_.winapi."${deps."ansi_term"."0.11.0"."winapi"}" deps)
  ];


# end
# atty-0.2.11

  crates.atty."0.2.11" = deps: { features?(features_.atty."0.2.11" deps {}) }: buildRustCrate {
    crateName = "atty";
    version = "0.2.11";
    authors = [ "softprops <d.tangren@gmail.com>" ];
    sha256 = "0by1bj2km9jxi4i4g76zzi76fc2rcm9934jpnyrqd95zw344pb20";
    dependencies = (if kernel == "redox" then mapFeatures features ([
      (crates."termion"."${deps."atty"."0.2.11"."termion"}" deps)
    ]) else [])
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
      (crates."libc"."${deps."atty"."0.2.11"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."atty"."0.2.11"."winapi"}" deps)
    ]) else []);
  };
  features_.atty."0.2.11" = deps: f: updateFeatures f (rec {
    atty."0.2.11".default = (f.atty."0.2.11".default or true);
    libc."${deps.atty."0.2.11".libc}".default = (f.libc."${deps.atty."0.2.11".libc}".default or false);
    termion."${deps.atty."0.2.11".termion}".default = true;
    winapi = fold recursiveUpdate {} [
      { "${deps.atty."0.2.11".winapi}"."consoleapi" = true; }
      { "${deps.atty."0.2.11".winapi}"."minwinbase" = true; }
      { "${deps.atty."0.2.11".winapi}"."minwindef" = true; }
      { "${deps.atty."0.2.11".winapi}"."processenv" = true; }
      { "${deps.atty."0.2.11".winapi}"."winbase" = true; }
      { "${deps.atty."0.2.11".winapi}".default = true; }
    ];
  }) [
    (features_.termion."${deps."atty"."0.2.11"."termion"}" deps)
    (features_.libc."${deps."atty"."0.2.11"."libc"}" deps)
    (features_.winapi."${deps."atty"."0.2.11"."winapi"}" deps)
  ];


# end
# autocfg-0.1.4

  crates.autocfg."0.1.4" = deps: { features?(features_.autocfg."0.1.4" deps {}) }: buildRustCrate {
    crateName = "autocfg";
    version = "0.1.4";
    authors = [ "Josh Stone <cuviper@gmail.com>" ];
    sha256 = "1xhpq1h2rqhqx95rc20x3wxi5yhv4a62jr269b8dqyhp8r84ss9i";
  };
  features_.autocfg."0.1.4" = deps: f: updateFeatures f (rec {
    autocfg."0.1.4".default = (f.autocfg."0.1.4".default or true);
  }) [];


# end
# backtrace-0.3.31

  crates.backtrace."0.3.31" = deps: { features?(features_.backtrace."0.3.31" deps {}) }: buildRustCrate {
    crateName = "backtrace";
    version = "0.3.31";
    authors = [ "The Rust Project Developers" ];
    sha256 = "02yzw7j0016agv6r7az2ax0r5nxdf2fhpdd7iqn4awhj3vg7fvmx";
    dependencies = mapFeatures features ([
      (crates."cfg_if"."${deps."backtrace"."0.3.31"."cfg_if"}" deps)
      (crates."libc"."${deps."backtrace"."0.3.31"."libc"}" deps)
      (crates."rustc_demangle"."${deps."backtrace"."0.3.31"."rustc_demangle"}" deps)
    ]
      ++ (if features.backtrace."0.3.31".backtrace-sys or false then [ (crates.backtrace_sys."${deps."backtrace"."0.3.31".backtrace_sys}" deps) ] else []))
      ++ (if kernel == "windows" then mapFeatures features ([
]) else []);

    buildDependencies = mapFeatures features ([
      (crates."autocfg"."${deps."backtrace"."0.3.31"."autocfg"}" deps)
    ]);
    features = mkFeatures (features."backtrace"."0.3.31" or {});
  };
  features_.backtrace."0.3.31" = deps: f: updateFeatures f (rec {
    autocfg."${deps.backtrace."0.3.31".autocfg}".default = true;
    backtrace = fold recursiveUpdate {} [
      { "0.3.31".addr2line =
        (f.backtrace."0.3.31".addr2line or false) ||
        (f.backtrace."0.3.31".gimli-symbolize or false) ||
        (backtrace."0.3.31"."gimli-symbolize" or false); }
      { "0.3.31".backtrace-sys =
        (f.backtrace."0.3.31".backtrace-sys or false) ||
        (f.backtrace."0.3.31".libbacktrace or false) ||
        (backtrace."0.3.31"."libbacktrace" or false); }
      { "0.3.31".compiler_builtins =
        (f.backtrace."0.3.31".compiler_builtins or false) ||
        (f.backtrace."0.3.31".rustc-dep-of-std or false) ||
        (backtrace."0.3.31"."rustc-dep-of-std" or false); }
      { "0.3.31".core =
        (f.backtrace."0.3.31".core or false) ||
        (f.backtrace."0.3.31".rustc-dep-of-std or false) ||
        (backtrace."0.3.31"."rustc-dep-of-std" or false); }
      { "0.3.31".dbghelp =
        (f.backtrace."0.3.31".dbghelp or false) ||
        (f.backtrace."0.3.31".default or false) ||
        (backtrace."0.3.31"."default" or false); }
      { "0.3.31".default = (f.backtrace."0.3.31".default or true); }
      { "0.3.31".dladdr =
        (f.backtrace."0.3.31".dladdr or false) ||
        (f.backtrace."0.3.31".default or false) ||
        (backtrace."0.3.31"."default" or false); }
      { "0.3.31".findshlibs =
        (f.backtrace."0.3.31".findshlibs or false) ||
        (f.backtrace."0.3.31".gimli-symbolize or false) ||
        (backtrace."0.3.31"."gimli-symbolize" or false); }
      { "0.3.31".libbacktrace =
        (f.backtrace."0.3.31".libbacktrace or false) ||
        (f.backtrace."0.3.31".default or false) ||
        (backtrace."0.3.31"."default" or false); }
      { "0.3.31".libunwind =
        (f.backtrace."0.3.31".libunwind or false) ||
        (f.backtrace."0.3.31".default or false) ||
        (backtrace."0.3.31"."default" or false); }
      { "0.3.31".memmap =
        (f.backtrace."0.3.31".memmap or false) ||
        (f.backtrace."0.3.31".gimli-symbolize or false) ||
        (backtrace."0.3.31"."gimli-symbolize" or false); }
      { "0.3.31".rustc-serialize =
        (f.backtrace."0.3.31".rustc-serialize or false) ||
        (f.backtrace."0.3.31".serialize-rustc or false) ||
        (backtrace."0.3.31"."serialize-rustc" or false); }
      { "0.3.31".serde =
        (f.backtrace."0.3.31".serde or false) ||
        (f.backtrace."0.3.31".serialize-serde or false) ||
        (backtrace."0.3.31"."serialize-serde" or false); }
      { "0.3.31".serde_derive =
        (f.backtrace."0.3.31".serde_derive or false) ||
        (f.backtrace."0.3.31".serialize-serde or false) ||
        (backtrace."0.3.31"."serialize-serde" or false); }
      { "0.3.31".std =
        (f.backtrace."0.3.31".std or false) ||
        (f.backtrace."0.3.31".default or false) ||
        (backtrace."0.3.31"."default" or false); }
    ];
    backtrace_sys = fold recursiveUpdate {} [
      { "${deps.backtrace."0.3.31".backtrace_sys}"."rustc-dep-of-std" =
        (f.backtrace_sys."${deps.backtrace."0.3.31".backtrace_sys}"."rustc-dep-of-std" or false) ||
        (backtrace."0.3.31"."rustc-dep-of-std" or false) ||
        (f."backtrace"."0.3.31"."rustc-dep-of-std" or false); }
      { "${deps.backtrace."0.3.31".backtrace_sys}".default = true; }
    ];
    cfg_if = fold recursiveUpdate {} [
      { "${deps.backtrace."0.3.31".cfg_if}"."rustc-dep-of-std" =
        (f.cfg_if."${deps.backtrace."0.3.31".cfg_if}"."rustc-dep-of-std" or false) ||
        (backtrace."0.3.31"."rustc-dep-of-std" or false) ||
        (f."backtrace"."0.3.31"."rustc-dep-of-std" or false); }
      { "${deps.backtrace."0.3.31".cfg_if}".default = true; }
    ];
    libc = fold recursiveUpdate {} [
      { "${deps.backtrace."0.3.31".libc}"."rustc-dep-of-std" =
        (f.libc."${deps.backtrace."0.3.31".libc}"."rustc-dep-of-std" or false) ||
        (backtrace."0.3.31"."rustc-dep-of-std" or false) ||
        (f."backtrace"."0.3.31"."rustc-dep-of-std" or false); }
      { "${deps.backtrace."0.3.31".libc}".default = (f.libc."${deps.backtrace."0.3.31".libc}".default or false); }
    ];
    rustc_demangle = fold recursiveUpdate {} [
      { "${deps.backtrace."0.3.31".rustc_demangle}"."rustc-dep-of-std" =
        (f.rustc_demangle."${deps.backtrace."0.3.31".rustc_demangle}"."rustc-dep-of-std" or false) ||
        (backtrace."0.3.31"."rustc-dep-of-std" or false) ||
        (f."backtrace"."0.3.31"."rustc-dep-of-std" or false); }
      { "${deps.backtrace."0.3.31".rustc_demangle}".default = true; }
    ];
  }) [
    (features_.backtrace_sys."${deps."backtrace"."0.3.31"."backtrace_sys"}" deps)
    (features_.cfg_if."${deps."backtrace"."0.3.31"."cfg_if"}" deps)
    (features_.libc."${deps."backtrace"."0.3.31"."libc"}" deps)
    (features_.rustc_demangle."${deps."backtrace"."0.3.31"."rustc_demangle"}" deps)
    (features_.autocfg."${deps."backtrace"."0.3.31"."autocfg"}" deps)
  ];


# end
# backtrace-sys-0.1.28

  crates.backtrace_sys."0.1.28" = deps: { features?(features_.backtrace_sys."0.1.28" deps {}) }: buildRustCrate {
    crateName = "backtrace-sys";
    version = "0.1.28";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "1bbw8chs0wskxwzz7f3yy7mjqhyqj8lslq8pcjw1rbd2g23c34xl";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."backtrace_sys"."0.1.28"."libc"}" deps)
    ]);

    buildDependencies = mapFeatures features ([
      (crates."cc"."${deps."backtrace_sys"."0.1.28"."cc"}" deps)
    ]);
  };
  features_.backtrace_sys."0.1.28" = deps: f: updateFeatures f (rec {
    backtrace_sys."0.1.28".default = (f.backtrace_sys."0.1.28".default or true);
    cc."${deps.backtrace_sys."0.1.28".cc}".default = true;
    libc."${deps.backtrace_sys."0.1.28".libc}".default = (f.libc."${deps.backtrace_sys."0.1.28".libc}".default or false);
  }) [
    (features_.libc."${deps."backtrace_sys"."0.1.28"."libc"}" deps)
    (features_.cc."${deps."backtrace_sys"."0.1.28"."cc"}" deps)
  ];


# end
# barrel-0.6.2

  crates.barrel."0.6.2" = deps: { features?(features_.barrel."0.6.2" deps {}) }: buildRustCrate {
    crateName = "barrel";
    version = "0.6.2";
    authors = [ "Katharina Fey <kookie@spacekookie.de>" "Rob Rowe <rippinrobr@gmail.com>" ];
    edition = "2018";
    sha256 = "0kad7gvlffj97vnzri0fjzagck0j89fjkdr13icrmrzsnfb977v8";
    dependencies = mapFeatures features ([
    ]
      ++ (if features.barrel."0.6.2".tempdir or false then [ (crates.tempdir."${deps."barrel"."0.6.2".tempdir}" deps) ] else []));
    features = mkFeatures (features."barrel"."0.6.2" or {});
  };
  features_.barrel."0.6.2" = deps: f: updateFeatures f (rec {
    barrel = fold recursiveUpdate {} [
      { "0.6.2".default = (f.barrel."0.6.2".default or true); }
      { "0.6.2".diesel_rs =
        (f.barrel."0.6.2".diesel_rs or false) ||
        (f.barrel."0.6.2".diesel or false) ||
        (barrel."0.6.2"."diesel" or false); }
      { "0.6.2".tempdir =
        (f.barrel."0.6.2".tempdir or false) ||
        (f.barrel."0.6.2".diesel or false) ||
        (barrel."0.6.2"."diesel" or false); }
    ];
    tempdir."${deps.barrel."0.6.2".tempdir}".default = true;
  }) [
    (features_.tempdir."${deps."barrel"."0.6.2"."tempdir"}" deps)
  ];


# end
# bigdecimal-0.1.0

  crates.bigdecimal."0.1.0" = deps: { features?(features_.bigdecimal."0.1.0" deps {}) }: buildRustCrate {
    crateName = "bigdecimal";
    version = "0.1.0";
    authors = [ "Andrew Kubera" ];
    sha256 = "09a51nww7saqs3c16srd5p2xpb5llwsczwxc8ayi7avpj15m7rz4";
    dependencies = mapFeatures features ([
      (crates."num_bigint"."${deps."bigdecimal"."0.1.0"."num_bigint"}" deps)
      (crates."num_integer"."${deps."bigdecimal"."0.1.0"."num_integer"}" deps)
      (crates."num_traits"."${deps."bigdecimal"."0.1.0"."num_traits"}" deps)
    ]);
  };
  features_.bigdecimal."0.1.0" = deps: f: updateFeatures f (rec {
    bigdecimal."0.1.0".default = (f.bigdecimal."0.1.0".default or true);
    num_bigint."${deps.bigdecimal."0.1.0".num_bigint}".default = true;
    num_integer."${deps.bigdecimal."0.1.0".num_integer}".default = true;
    num_traits."${deps.bigdecimal."0.1.0".num_traits}".default = true;
  }) [
    (features_.num_bigint."${deps."bigdecimal"."0.1.0"."num_bigint"}" deps)
    (features_.num_integer."${deps."bigdecimal"."0.1.0"."num_integer"}" deps)
    (features_.num_traits."${deps."bigdecimal"."0.1.0"."num_traits"}" deps)
  ];


# end
# bitflags-1.1.0

  crates.bitflags."1.1.0" = deps: { features?(features_.bitflags."1.1.0" deps {}) }: buildRustCrate {
    crateName = "bitflags";
    version = "1.1.0";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1iwa4jrqcf4lnbwl562a3lx3r0jkh1j88b219bsqvbm4sni67dyv";
    build = "build.rs";
    features = mkFeatures (features."bitflags"."1.1.0" or {});
  };
  features_.bitflags."1.1.0" = deps: f: updateFeatures f (rec {
    bitflags."1.1.0".default = (f.bitflags."1.1.0".default or true);
  }) [];


# end
# byteorder-1.3.2

  crates.byteorder."1.3.2" = deps: { features?(features_.byteorder."1.3.2" deps {}) }: buildRustCrate {
    crateName = "byteorder";
    version = "1.3.2";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" ];
    sha256 = "099fxwc79ncpcl8dgg9hql8gznz11a3sjs7pai0mg6w8r05khvdx";
    build = "build.rs";
    features = mkFeatures (features."byteorder"."1.3.2" or {});
  };
  features_.byteorder."1.3.2" = deps: f: updateFeatures f (rec {
    byteorder = fold recursiveUpdate {} [
      { "1.3.2".default = (f.byteorder."1.3.2".default or true); }
      { "1.3.2".std =
        (f.byteorder."1.3.2".std or false) ||
        (f.byteorder."1.3.2".default or false) ||
        (byteorder."1.3.2"."default" or false); }
    ];
  }) [];


# end
# cc-1.0.37

  crates.cc."1.0.37" = deps: { features?(features_.cc."1.0.37" deps {}) }: buildRustCrate {
    crateName = "cc";
    version = "1.0.37";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "1m5s357yi2amgd0kd8chxdcbnscyxwxifmf5hgv92x5xj56b3shj";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."cc"."1.0.37" or {});
  };
  features_.cc."1.0.37" = deps: f: updateFeatures f (rec {
    cc = fold recursiveUpdate {} [
      { "1.0.37".default = (f.cc."1.0.37".default or true); }
      { "1.0.37".rayon =
        (f.cc."1.0.37".rayon or false) ||
        (f.cc."1.0.37".parallel or false) ||
        (cc."1.0.37"."parallel" or false); }
    ];
  }) [];


# end
# cfg-if-0.1.9

  crates.cfg_if."0.1.9" = deps: { features?(features_.cfg_if."0.1.9" deps {}) }: buildRustCrate {
    crateName = "cfg-if";
    version = "0.1.9";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "13g9p2mc5b2b5wn716fwvilzib376ycpkgk868yxfp16jzix57p7";
  };
  features_.cfg_if."0.1.9" = deps: f: updateFeatures f (rec {
    cfg_if."0.1.9".default = (f.cfg_if."0.1.9".default or true);
  }) [];


# end
# chrono-0.4.7

  crates.chrono."0.4.7" = deps: { features?(features_.chrono."0.4.7" deps {}) }: buildRustCrate {
    crateName = "chrono";
    version = "0.4.7";
    authors = [ "Kang Seonghoon <public+rust@mearie.org>" "Brandon W Maister <quodlibetor@gmail.com>" ];
    sha256 = "1f5r3h2vyr8g42fncp0g55qzaq2cxkchd59sjdlda1bl7m4wxnb5";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."chrono"."0.4.7"."libc"}" deps)
      (crates."num_integer"."${deps."chrono"."0.4.7"."num_integer"}" deps)
      (crates."num_traits"."${deps."chrono"."0.4.7"."num_traits"}" deps)
    ]
      ++ (if features.chrono."0.4.7".time or false then [ (crates.time."${deps."chrono"."0.4.7".time}" deps) ] else []));
    features = mkFeatures (features."chrono"."0.4.7" or {});
  };
  features_.chrono."0.4.7" = deps: f: updateFeatures f (rec {
    chrono = fold recursiveUpdate {} [
      { "0.4.7".clock =
        (f.chrono."0.4.7".clock or false) ||
        (f.chrono."0.4.7".default or false) ||
        (chrono."0.4.7"."default" or false); }
      { "0.4.7".default = (f.chrono."0.4.7".default or true); }
      { "0.4.7".time =
        (f.chrono."0.4.7".time or false) ||
        (f.chrono."0.4.7".clock or false) ||
        (chrono."0.4.7"."clock" or false); }
    ];
    libc."${deps.chrono."0.4.7".libc}".default = (f.libc."${deps.chrono."0.4.7".libc}".default or false);
    num_integer."${deps.chrono."0.4.7".num_integer}".default = (f.num_integer."${deps.chrono."0.4.7".num_integer}".default or false);
    num_traits."${deps.chrono."0.4.7".num_traits}".default = (f.num_traits."${deps.chrono."0.4.7".num_traits}".default or false);
    time."${deps.chrono."0.4.7".time}".default = true;
  }) [
    (features_.libc."${deps."chrono"."0.4.7"."libc"}" deps)
    (features_.num_integer."${deps."chrono"."0.4.7"."num_integer"}" deps)
    (features_.num_traits."${deps."chrono"."0.4.7"."num_traits"}" deps)
    (features_.time."${deps."chrono"."0.4.7"."time"}" deps)
  ];


# end
# clap-2.33.0

  crates.clap."2.33.0" = deps: { features?(features_.clap."2.33.0" deps {}) }: buildRustCrate {
    crateName = "clap";
    version = "2.33.0";
    authors = [ "Kevin K. <kbknapp@gmail.com>" ];
    sha256 = "054n9ngh6pkknpmd4acgdsp40iw6f5jzq8a4h2b76gnbvk6p5xjh";
    dependencies = mapFeatures features ([
      (crates."bitflags"."${deps."clap"."2.33.0"."bitflags"}" deps)
      (crates."textwrap"."${deps."clap"."2.33.0"."textwrap"}" deps)
      (crates."unicode_width"."${deps."clap"."2.33.0"."unicode_width"}" deps)
    ]
      ++ (if features.clap."2.33.0".atty or false then [ (crates.atty."${deps."clap"."2.33.0".atty}" deps) ] else [])
      ++ (if features.clap."2.33.0".strsim or false then [ (crates.strsim."${deps."clap"."2.33.0".strsim}" deps) ] else [])
      ++ (if features.clap."2.33.0".vec_map or false then [ (crates.vec_map."${deps."clap"."2.33.0".vec_map}" deps) ] else []))
      ++ (if !(kernel == "windows") then mapFeatures features ([
    ]
      ++ (if features.clap."2.33.0".ansi_term or false then [ (crates.ansi_term."${deps."clap"."2.33.0".ansi_term}" deps) ] else [])) else []);
    features = mkFeatures (features."clap"."2.33.0" or {});
  };
  features_.clap."2.33.0" = deps: f: updateFeatures f (rec {
    ansi_term."${deps.clap."2.33.0".ansi_term}".default = true;
    atty."${deps.clap."2.33.0".atty}".default = true;
    bitflags."${deps.clap."2.33.0".bitflags}".default = true;
    clap = fold recursiveUpdate {} [
      { "2.33.0".ansi_term =
        (f.clap."2.33.0".ansi_term or false) ||
        (f.clap."2.33.0".color or false) ||
        (clap."2.33.0"."color" or false); }
      { "2.33.0".atty =
        (f.clap."2.33.0".atty or false) ||
        (f.clap."2.33.0".color or false) ||
        (clap."2.33.0"."color" or false); }
      { "2.33.0".clippy =
        (f.clap."2.33.0".clippy or false) ||
        (f.clap."2.33.0".lints or false) ||
        (clap."2.33.0"."lints" or false); }
      { "2.33.0".color =
        (f.clap."2.33.0".color or false) ||
        (f.clap."2.33.0".default or false) ||
        (clap."2.33.0"."default" or false); }
      { "2.33.0".default = (f.clap."2.33.0".default or true); }
      { "2.33.0".strsim =
        (f.clap."2.33.0".strsim or false) ||
        (f.clap."2.33.0".suggestions or false) ||
        (clap."2.33.0"."suggestions" or false); }
      { "2.33.0".suggestions =
        (f.clap."2.33.0".suggestions or false) ||
        (f.clap."2.33.0".default or false) ||
        (clap."2.33.0"."default" or false); }
      { "2.33.0".term_size =
        (f.clap."2.33.0".term_size or false) ||
        (f.clap."2.33.0".wrap_help or false) ||
        (clap."2.33.0"."wrap_help" or false); }
      { "2.33.0".vec_map =
        (f.clap."2.33.0".vec_map or false) ||
        (f.clap."2.33.0".default or false) ||
        (clap."2.33.0"."default" or false); }
      { "2.33.0".yaml =
        (f.clap."2.33.0".yaml or false) ||
        (f.clap."2.33.0".doc or false) ||
        (clap."2.33.0"."doc" or false); }
      { "2.33.0".yaml-rust =
        (f.clap."2.33.0".yaml-rust or false) ||
        (f.clap."2.33.0".yaml or false) ||
        (clap."2.33.0"."yaml" or false); }
    ];
    strsim."${deps.clap."2.33.0".strsim}".default = true;
    textwrap = fold recursiveUpdate {} [
      { "${deps.clap."2.33.0".textwrap}"."term_size" =
        (f.textwrap."${deps.clap."2.33.0".textwrap}"."term_size" or false) ||
        (clap."2.33.0"."wrap_help" or false) ||
        (f."clap"."2.33.0"."wrap_help" or false); }
      { "${deps.clap."2.33.0".textwrap}".default = true; }
    ];
    unicode_width."${deps.clap."2.33.0".unicode_width}".default = true;
    vec_map."${deps.clap."2.33.0".vec_map}".default = true;
  }) [
    (features_.atty."${deps."clap"."2.33.0"."atty"}" deps)
    (features_.bitflags."${deps."clap"."2.33.0"."bitflags"}" deps)
    (features_.strsim."${deps."clap"."2.33.0"."strsim"}" deps)
    (features_.textwrap."${deps."clap"."2.33.0"."textwrap"}" deps)
    (features_.unicode_width."${deps."clap"."2.33.0"."unicode_width"}" deps)
    (features_.vec_map."${deps."clap"."2.33.0"."vec_map"}" deps)
    (features_.ansi_term."${deps."clap"."2.33.0"."ansi_term"}" deps)
  ];


# end
# cloudabi-0.0.3

  crates.cloudabi."0.0.3" = deps: { features?(features_.cloudabi."0.0.3" deps {}) }: buildRustCrate {
    crateName = "cloudabi";
    version = "0.0.3";
    authors = [ "Nuxi (https://nuxi.nl/) and contributors" ];
    sha256 = "1z9lby5sr6vslfd14d6igk03s7awf91mxpsfmsp3prxbxlk0x7h5";
    libPath = "cloudabi.rs";
    dependencies = mapFeatures features ([
    ]
      ++ (if features.cloudabi."0.0.3".bitflags or false then [ (crates.bitflags."${deps."cloudabi"."0.0.3".bitflags}" deps) ] else []));
    features = mkFeatures (features."cloudabi"."0.0.3" or {});
  };
  features_.cloudabi."0.0.3" = deps: f: updateFeatures f (rec {
    bitflags."${deps.cloudabi."0.0.3".bitflags}".default = true;
    cloudabi = fold recursiveUpdate {} [
      { "0.0.3".bitflags =
        (f.cloudabi."0.0.3".bitflags or false) ||
        (f.cloudabi."0.0.3".default or false) ||
        (cloudabi."0.0.3"."default" or false); }
      { "0.0.3".default = (f.cloudabi."0.0.3".default or true); }
    ];
  }) [
    (features_.bitflags."${deps."cloudabi"."0.0.3"."bitflags"}" deps)
  ];


# end
# derive-error-chain-0.10.1

  crates.derive_error_chain."0.10.1" = deps: { features?(features_.derive_error_chain."0.10.1" deps {}) }: buildRustCrate {
    crateName = "derive-error-chain";
    version = "0.10.1";
    authors = [ "Brian Anderson <banderson@mozilla.com>" "Paul Colomiets <paul@colomiets.name>" "Colin Kiegel <kiegel@gmx.de>" "Yamakaky <yamakaky@yamaworld.fr>" "Arnav Singh <arnavion@gmail.com>" ];
    sha256 = "1m0wplzfb6cir43ay1lbfa0xg9avv1vcwnkm3kwk6b6sqialxwcr";
    procMacro = true;
    dependencies = mapFeatures features ([
      (crates."quote"."${deps."derive_error_chain"."0.10.1"."quote"}" deps)
      (crates."syn"."${deps."derive_error_chain"."0.10.1"."syn"}" deps)
    ]);
  };
  features_.derive_error_chain."0.10.1" = deps: f: updateFeatures f (rec {
    derive_error_chain."0.10.1".default = (f.derive_error_chain."0.10.1".default or true);
    quote."${deps.derive_error_chain."0.10.1".quote}".default = true;
    syn = fold recursiveUpdate {} [
      { "${deps.derive_error_chain."0.10.1".syn}"."full" = true; }
      { "${deps.derive_error_chain."0.10.1".syn}"."parsing" = true; }
      { "${deps.derive_error_chain."0.10.1".syn}".default = true; }
    ];
  }) [
    (features_.quote."${deps."derive_error_chain"."0.10.1"."quote"}" deps)
    (features_.syn."${deps."derive_error_chain"."0.10.1"."syn"}" deps)
  ];


# end
# diesel-1.4.2

  crates.diesel."1.4.2" = deps: { features?(features_.diesel."1.4.2" deps {}) }: buildRustCrate {
    crateName = "diesel";
    version = "1.4.2";
    authors = [ "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "06c6gbz3p8hsidmz3h7j3hlm3034yzq788z8jficc07ggmdf5hq3";
    dependencies = mapFeatures features ([
      (crates."byteorder"."${deps."diesel"."1.4.2"."byteorder"}" deps)
      (crates."diesel_derives"."${deps."diesel"."1.4.2"."diesel_derives"}" deps)
    ]
      ++ (if features.diesel."1.4.2".bigdecimal or false then [ (crates.bigdecimal."${deps."diesel"."1.4.2".bigdecimal}" deps) ] else [])
      ++ (if features.diesel."1.4.2".bitflags or false then [ (crates.bitflags."${deps."diesel"."1.4.2".bitflags}" deps) ] else [])
      ++ (if features.diesel."1.4.2".chrono or false then [ (crates.chrono."${deps."diesel"."1.4.2".chrono}" deps) ] else [])
      ++ (if features.diesel."1.4.2".ipnetwork or false then [ (crates.ipnetwork."${deps."diesel"."1.4.2".ipnetwork}" deps) ] else [])
      ++ (if features.diesel."1.4.2".libc or false then [ (crates.libc."${deps."diesel"."1.4.2".libc}" deps) ] else [])
      ++ (if features.diesel."1.4.2".libsqlite3-sys or false then [ (crates.libsqlite3_sys."${deps."diesel"."1.4.2".libsqlite3_sys}" deps) ] else [])
      ++ (if features.diesel."1.4.2".mysqlclient-sys or false then [ (crates.mysqlclient_sys."${deps."diesel"."1.4.2".mysqlclient_sys}" deps) ] else [])
      ++ (if features.diesel."1.4.2".num-bigint or false then [ (crates.num_bigint."${deps."diesel"."1.4.2".num_bigint}" deps) ] else [])
      ++ (if features.diesel."1.4.2".num-integer or false then [ (crates.num_integer."${deps."diesel"."1.4.2".num_integer}" deps) ] else [])
      ++ (if features.diesel."1.4.2".num-traits or false then [ (crates.num_traits."${deps."diesel"."1.4.2".num_traits}" deps) ] else [])
      ++ (if features.diesel."1.4.2".pq-sys or false then [ (crates.pq_sys."${deps."diesel"."1.4.2".pq_sys}" deps) ] else [])
      ++ (if features.diesel."1.4.2".quickcheck or false then [ (crates.quickcheck."${deps."diesel"."1.4.2".quickcheck}" deps) ] else [])
      ++ (if features.diesel."1.4.2".r2d2 or false then [ (crates.r2d2."${deps."diesel"."1.4.2".r2d2}" deps) ] else [])
      ++ (if features.diesel."1.4.2".serde_json or false then [ (crates.serde_json."${deps."diesel"."1.4.2".serde_json}" deps) ] else [])
      ++ (if features.diesel."1.4.2".time or false then [ (crates.time."${deps."diesel"."1.4.2".time}" deps) ] else [])
      ++ (if features.diesel."1.4.2".url or false then [ (crates.url."${deps."diesel"."1.4.2".url}" deps) ] else [])
      ++ (if features.diesel."1.4.2".uuid or false then [ (crates.uuid."${deps."diesel"."1.4.2".uuid}" deps) ] else []));
    features = mkFeatures (features."diesel"."1.4.2" or {});
  };
  features_.diesel."1.4.2" = deps: f: updateFeatures f (rec {
    bigdecimal."${deps.diesel."1.4.2".bigdecimal}".default = true;
    bitflags."${deps.diesel."1.4.2".bitflags}".default = true;
    byteorder."${deps.diesel."1.4.2".byteorder}".default = true;
    chrono."${deps.diesel."1.4.2".chrono}".default = true;
    diesel = fold recursiveUpdate {} [
      { "1.4.2"."128-column-tables" =
        (f.diesel."1.4.2"."128-column-tables" or false) ||
        (f.diesel."1.4.2"."x128-column-tables" or false) ||
        (diesel."1.4.2"."x128-column-tables" or false); }
      { "1.4.2"."32-column-tables" =
        (f.diesel."1.4.2"."32-column-tables" or false) ||
        (f.diesel."1.4.2"."64-column-tables" or false) ||
        (diesel."1.4.2"."64-column-tables" or false) ||
        (f.diesel."1.4.2".default or false) ||
        (diesel."1.4.2"."default" or false) ||
        (f.diesel."1.4.2".large-tables or false) ||
        (diesel."1.4.2"."large-tables" or false) ||
        (f.diesel."1.4.2"."x32-column-tables" or false) ||
        (diesel."1.4.2"."x32-column-tables" or false); }
      { "1.4.2"."64-column-tables" =
        (f.diesel."1.4.2"."64-column-tables" or false) ||
        (f.diesel."1.4.2"."128-column-tables" or false) ||
        (diesel."1.4.2"."128-column-tables" or false) ||
        (f.diesel."1.4.2".huge-tables or false) ||
        (diesel."1.4.2"."huge-tables" or false) ||
        (f.diesel."1.4.2".x64-column-tables or false) ||
        (diesel."1.4.2"."x64-column-tables" or false); }
      { "1.4.2".bigdecimal =
        (f.diesel."1.4.2".bigdecimal or false) ||
        (f.diesel."1.4.2".numeric or false) ||
        (diesel."1.4.2"."numeric" or false); }
      { "1.4.2".bitflags =
        (f.diesel."1.4.2".bitflags or false) ||
        (f.diesel."1.4.2".postgres or false) ||
        (diesel."1.4.2"."postgres" or false); }
      { "1.4.2".chrono =
        (f.diesel."1.4.2".chrono or false) ||
        (f.diesel."1.4.2".extras or false) ||
        (diesel."1.4.2"."extras" or false); }
      { "1.4.2".default = (f.diesel."1.4.2".default or true); }
      { "1.4.2".deprecated-time =
        (f.diesel."1.4.2".deprecated-time or false) ||
        (f.diesel."1.4.2".extras or false) ||
        (diesel."1.4.2"."extras" or false); }
      { "1.4.2".ipnetwork =
        (f.diesel."1.4.2".ipnetwork or false) ||
        (f.diesel."1.4.2".network-address or false) ||
        (diesel."1.4.2"."network-address" or false); }
      { "1.4.2".libc =
        (f.diesel."1.4.2".libc or false) ||
        (f.diesel."1.4.2".network-address or false) ||
        (diesel."1.4.2"."network-address" or false); }
      { "1.4.2".libsqlite3-sys =
        (f.diesel."1.4.2".libsqlite3-sys or false) ||
        (f.diesel."1.4.2".sqlite or false) ||
        (diesel."1.4.2"."sqlite" or false); }
      { "1.4.2".mysqlclient-sys =
        (f.diesel."1.4.2".mysqlclient-sys or false) ||
        (f.diesel."1.4.2".mysql or false) ||
        (diesel."1.4.2"."mysql" or false); }
      { "1.4.2".network-address =
        (f.diesel."1.4.2".network-address or false) ||
        (f.diesel."1.4.2".extras or false) ||
        (diesel."1.4.2"."extras" or false); }
      { "1.4.2".num-bigint =
        (f.diesel."1.4.2".num-bigint or false) ||
        (f.diesel."1.4.2".numeric or false) ||
        (diesel."1.4.2"."numeric" or false); }
      { "1.4.2".num-integer =
        (f.diesel."1.4.2".num-integer or false) ||
        (f.diesel."1.4.2".numeric or false) ||
        (diesel."1.4.2"."numeric" or false); }
      { "1.4.2".num-traits =
        (f.diesel."1.4.2".num-traits or false) ||
        (f.diesel."1.4.2".numeric or false) ||
        (diesel."1.4.2"."numeric" or false); }
      { "1.4.2".numeric =
        (f.diesel."1.4.2".numeric or false) ||
        (f.diesel."1.4.2".extras or false) ||
        (diesel."1.4.2"."extras" or false); }
      { "1.4.2".pq-sys =
        (f.diesel."1.4.2".pq-sys or false) ||
        (f.diesel."1.4.2".postgres or false) ||
        (diesel."1.4.2"."postgres" or false); }
      { "1.4.2".r2d2 =
        (f.diesel."1.4.2".r2d2 or false) ||
        (f.diesel."1.4.2".extras or false) ||
        (diesel."1.4.2"."extras" or false); }
      { "1.4.2".serde_json =
        (f.diesel."1.4.2".serde_json or false) ||
        (f.diesel."1.4.2".extras or false) ||
        (diesel."1.4.2"."extras" or false); }
      { "1.4.2".time =
        (f.diesel."1.4.2".time or false) ||
        (f.diesel."1.4.2".deprecated-time or false) ||
        (diesel."1.4.2"."deprecated-time" or false); }
      { "1.4.2".url =
        (f.diesel."1.4.2".url or false) ||
        (f.diesel."1.4.2".mysql or false) ||
        (diesel."1.4.2"."mysql" or false); }
      { "1.4.2".uuid =
        (f.diesel."1.4.2".uuid or false) ||
        (f.diesel."1.4.2".extras or false) ||
        (diesel."1.4.2"."extras" or false); }
      { "1.4.2".with-deprecated =
        (f.diesel."1.4.2".with-deprecated or false) ||
        (f.diesel."1.4.2".default or false) ||
        (diesel."1.4.2"."default" or false); }
    ];
    diesel_derives = fold recursiveUpdate {} [
      { "${deps.diesel."1.4.2".diesel_derives}"."mysql" =
        (f.diesel_derives."${deps.diesel."1.4.2".diesel_derives}"."mysql" or false) ||
        (diesel."1.4.2"."mysql" or false) ||
        (f."diesel"."1.4.2"."mysql" or false); }
      { "${deps.diesel."1.4.2".diesel_derives}"."nightly" =
        (f.diesel_derives."${deps.diesel."1.4.2".diesel_derives}"."nightly" or false) ||
        (diesel."1.4.2"."unstable" or false) ||
        (f."diesel"."1.4.2"."unstable" or false); }
      { "${deps.diesel."1.4.2".diesel_derives}"."postgres" =
        (f.diesel_derives."${deps.diesel."1.4.2".diesel_derives}"."postgres" or false) ||
        (diesel."1.4.2"."postgres" or false) ||
        (f."diesel"."1.4.2"."postgres" or false); }
      { "${deps.diesel."1.4.2".diesel_derives}"."sqlite" =
        (f.diesel_derives."${deps.diesel."1.4.2".diesel_derives}"."sqlite" or false) ||
        (diesel."1.4.2"."sqlite" or false) ||
        (f."diesel"."1.4.2"."sqlite" or false); }
      { "${deps.diesel."1.4.2".diesel_derives}".default = true; }
    ];
    ipnetwork."${deps.diesel."1.4.2".ipnetwork}".default = true;
    libc."${deps.diesel."1.4.2".libc}".default = true;
    libsqlite3_sys = fold recursiveUpdate {} [
      { "${deps.diesel."1.4.2".libsqlite3_sys}"."min_sqlite_version_3_7_16" = true; }
      { "${deps.diesel."1.4.2".libsqlite3_sys}".default = true; }
    ];
    mysqlclient_sys."${deps.diesel."1.4.2".mysqlclient_sys}".default = true;
    num_bigint."${deps.diesel."1.4.2".num_bigint}".default = true;
    num_integer."${deps.diesel."1.4.2".num_integer}".default = true;
    num_traits."${deps.diesel."1.4.2".num_traits}".default = true;
    pq_sys."${deps.diesel."1.4.2".pq_sys}".default = true;
    quickcheck."${deps.diesel."1.4.2".quickcheck}".default = true;
    r2d2."${deps.diesel."1.4.2".r2d2}".default = true;
    serde_json."${deps.diesel."1.4.2".serde_json}".default = true;
    time."${deps.diesel."1.4.2".time}".default = true;
    url."${deps.diesel."1.4.2".url}".default = true;
    uuid = fold recursiveUpdate {} [
      { "${deps.diesel."1.4.2".uuid}"."use_std" = true; }
      { "${deps.diesel."1.4.2".uuid}".default = true; }
    ];
  }) [
    (features_.bigdecimal."${deps."diesel"."1.4.2"."bigdecimal"}" deps)
    (features_.bitflags."${deps."diesel"."1.4.2"."bitflags"}" deps)
    (features_.byteorder."${deps."diesel"."1.4.2"."byteorder"}" deps)
    (features_.chrono."${deps."diesel"."1.4.2"."chrono"}" deps)
    (features_.diesel_derives."${deps."diesel"."1.4.2"."diesel_derives"}" deps)
    (features_.ipnetwork."${deps."diesel"."1.4.2"."ipnetwork"}" deps)
    (features_.libc."${deps."diesel"."1.4.2"."libc"}" deps)
    (features_.libsqlite3_sys."${deps."diesel"."1.4.2"."libsqlite3_sys"}" deps)
    (features_.mysqlclient_sys."${deps."diesel"."1.4.2"."mysqlclient_sys"}" deps)
    (features_.num_bigint."${deps."diesel"."1.4.2"."num_bigint"}" deps)
    (features_.num_integer."${deps."diesel"."1.4.2"."num_integer"}" deps)
    (features_.num_traits."${deps."diesel"."1.4.2"."num_traits"}" deps)
    (features_.pq_sys."${deps."diesel"."1.4.2"."pq_sys"}" deps)
    (features_.quickcheck."${deps."diesel"."1.4.2"."quickcheck"}" deps)
    (features_.r2d2."${deps."diesel"."1.4.2"."r2d2"}" deps)
    (features_.serde_json."${deps."diesel"."1.4.2"."serde_json"}" deps)
    (features_.time."${deps."diesel"."1.4.2"."time"}" deps)
    (features_.url."${deps."diesel"."1.4.2"."url"}" deps)
    (features_.uuid."${deps."diesel"."1.4.2"."uuid"}" deps)
  ];


# end
# diesel_cli-1.4.0

  crates.diesel_cli."1.4.0" = deps: { features?(features_.diesel_cli."1.4.0" deps {}) }: buildRustCrate {
    crateName = "diesel_cli";
    version = "1.4.0";
    authors = [ "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "00wa6jsdvkfp4csj92plfcymav9hgwv6zkj1cpl1l1w78n1xldbl";
    crateBin =
      [{  name = "diesel";  path = "src/main.rs"; }];
    dependencies = mapFeatures features ([
      (crates."chrono"."${deps."diesel_cli"."1.4.0"."chrono"}" deps)
      (crates."clap"."${deps."diesel_cli"."1.4.0"."clap"}" deps)
      (crates."diesel"."${deps."diesel_cli"."1.4.0"."diesel"}" deps)
      (crates."dotenv"."${deps."diesel_cli"."1.4.0"."dotenv"}" deps)
      (crates."migrations_internals"."${deps."diesel_cli"."1.4.0"."migrations_internals"}" deps)
      (crates."serde"."${deps."diesel_cli"."1.4.0"."serde"}" deps)
      (crates."tempfile"."${deps."diesel_cli"."1.4.0"."tempfile"}" deps)
      (crates."toml"."${deps."diesel_cli"."1.4.0"."toml"}" deps)
    ]
      ++ (if features.diesel_cli."1.4.0".barrel or false then [ (crates.barrel."${deps."diesel_cli"."1.4.0".barrel}" deps) ] else [])
      ++ (if features.diesel_cli."1.4.0".libsqlite3-sys or false then [ (crates.libsqlite3_sys."${deps."diesel_cli"."1.4.0".libsqlite3_sys}" deps) ] else [])
      ++ (if features.diesel_cli."1.4.0".url or false then [ (crates.url."${deps."diesel_cli"."1.4.0".url}" deps) ] else []));
    features = mkFeatures (features."diesel_cli"."1.4.0" or {});
  };
  features_.diesel_cli."1.4.0" = deps: f: updateFeatures f (rec {
    barrel = fold recursiveUpdate {} [
      { "${deps.diesel_cli."1.4.0".barrel}"."diesel-filled" = true; }
      { "${deps.diesel_cli."1.4.0".barrel}".default = true; }
    ];
    chrono."${deps.diesel_cli."1.4.0".chrono}".default = true;
    clap."${deps.diesel_cli."1.4.0".clap}".default = true;
    diesel = fold recursiveUpdate {} [
      { "${deps.diesel_cli."1.4.0".diesel}"."mysql" = false; }
      { "${deps.diesel_cli."1.4.0".diesel}"."postgres" =
        (f.diesel."${deps.diesel_cli."1.4.0".diesel}"."postgres" or false) ||
        (diesel_cli."1.4.0"."postgres" or false) ||
        (f."diesel_cli"."1.4.0"."postgres" or false); }
      { "${deps.diesel_cli."1.4.0".diesel}"."sqlite" =
        (f.diesel."${deps.diesel_cli."1.4.0".diesel}"."sqlite" or false) ||
        (diesel_cli."1.4.0"."sqlite" or false) ||
        (f."diesel_cli"."1.4.0"."sqlite" or false); }
      { "${deps.diesel_cli."1.4.0".diesel}".default = (f.diesel."${deps.diesel_cli."1.4.0".diesel}".default or false); }
    ];
    diesel_cli = fold recursiveUpdate {} [
      { "1.4.0".barrel =
        (f.diesel_cli."1.4.0".barrel or false) ||
        (f.diesel_cli."1.4.0".barrel-migrations or false) ||
        (diesel_cli."1.4.0"."barrel-migrations" or false); }
      { "1.4.0".default = (f.diesel_cli."1.4.0".default or true); }
      { "1.4.0".mysql = false; }
      { "1.4.0".postgres =
        (f.diesel_cli."1.4.0".postgres or false) ||
        (f.diesel_cli."1.4.0".default or false) ||
        (diesel_cli."1.4.0"."default" or false); }
      { "1.4.0".sqlite =
        (f.diesel_cli."1.4.0".sqlite or false) ||
        (f.diesel_cli."1.4.0".default or false) ||
        (diesel_cli."1.4.0"."default" or false) ||
        (f.diesel_cli."1.4.0".sqlite-bundled or false) ||
        (diesel_cli."1.4.0"."sqlite-bundled" or false); }
      { "1.4.0".url =
        (f.diesel_cli."1.4.0".url or false) ||
        (f.diesel_cli."1.4.0".mysql or false) ||
        (diesel_cli."1.4.0"."mysql" or false) ||
        (f.diesel_cli."1.4.0".postgres or false) ||
        (diesel_cli."1.4.0"."postgres" or false); }
      { "1.4.0".uses_information_schema =
        (f.diesel_cli."1.4.0".uses_information_schema or false) ||
        (f.diesel_cli."1.4.0".mysql or false) ||
        (diesel_cli."1.4.0"."mysql" or false) ||
        (f.diesel_cli."1.4.0".postgres or false) ||
        (diesel_cli."1.4.0"."postgres" or false); }
    ];
    dotenv."${deps.diesel_cli."1.4.0".dotenv}".default = true;
    libsqlite3_sys = fold recursiveUpdate {} [
      { "${deps.diesel_cli."1.4.0".libsqlite3_sys}"."bundled" =
        (f.libsqlite3_sys."${deps.diesel_cli."1.4.0".libsqlite3_sys}"."bundled" or false) ||
        (diesel_cli."1.4.0"."sqlite-bundled" or false) ||
        (f."diesel_cli"."1.4.0"."sqlite-bundled" or false); }
      { "${deps.diesel_cli."1.4.0".libsqlite3_sys}"."min_sqlite_version_3_7_16" = true; }
      { "${deps.diesel_cli."1.4.0".libsqlite3_sys}".default = true; }
    ];
    migrations_internals = fold recursiveUpdate {} [
      { "${deps.diesel_cli."1.4.0".migrations_internals}"."barrel" =
        (f.migrations_internals."${deps.diesel_cli."1.4.0".migrations_internals}"."barrel" or false) ||
        (diesel_cli."1.4.0"."barrel-migrations" or false) ||
        (f."diesel_cli"."1.4.0"."barrel-migrations" or false); }
      { "${deps.diesel_cli."1.4.0".migrations_internals}".default = true; }
    ];
    serde = fold recursiveUpdate {} [
      { "${deps.diesel_cli."1.4.0".serde}"."derive" = true; }
      { "${deps.diesel_cli."1.4.0".serde}".default = true; }
    ];
    tempfile."${deps.diesel_cli."1.4.0".tempfile}".default = true;
    toml."${deps.diesel_cli."1.4.0".toml}".default = true;
    url."${deps.diesel_cli."1.4.0".url}".default = true;
  }) [
    (features_.barrel."${deps."diesel_cli"."1.4.0"."barrel"}" deps)
    (features_.chrono."${deps."diesel_cli"."1.4.0"."chrono"}" deps)
    (features_.clap."${deps."diesel_cli"."1.4.0"."clap"}" deps)
    (features_.diesel."${deps."diesel_cli"."1.4.0"."diesel"}" deps)
    (features_.dotenv."${deps."diesel_cli"."1.4.0"."dotenv"}" deps)
    (features_.libsqlite3_sys."${deps."diesel_cli"."1.4.0"."libsqlite3_sys"}" deps)
    (features_.migrations_internals."${deps."diesel_cli"."1.4.0"."migrations_internals"}" deps)
    (features_.serde."${deps."diesel_cli"."1.4.0"."serde"}" deps)
    (features_.tempfile."${deps."diesel_cli"."1.4.0"."tempfile"}" deps)
    (features_.toml."${deps."diesel_cli"."1.4.0"."toml"}" deps)
    (features_.url."${deps."diesel_cli"."1.4.0"."url"}" deps)
  ];


# end
# diesel_derives-1.4.0

  crates.diesel_derives."1.4.0" = deps: { features?(features_.diesel_derives."1.4.0" deps {}) }: buildRustCrate {
    crateName = "diesel_derives";
    version = "1.4.0";
    authors = [ "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "0glcvgn8vl60nw6xm0q13ljfcjlhg7j9pb8lnc06zfik8qvy27lz";
    procMacro = true;
    dependencies = mapFeatures features ([
      (crates."proc_macro2"."${deps."diesel_derives"."1.4.0"."proc_macro2"}" deps)
      (crates."quote"."${deps."diesel_derives"."1.4.0"."quote"}" deps)
      (crates."syn"."${deps."diesel_derives"."1.4.0"."syn"}" deps)
    ]);
    features = mkFeatures (features."diesel_derives"."1.4.0" or {});
  };
  features_.diesel_derives."1.4.0" = deps: f: updateFeatures f (rec {
    diesel_derives."1.4.0".default = (f.diesel_derives."1.4.0".default or true);
    proc_macro2 = fold recursiveUpdate {} [
      { "${deps.diesel_derives."1.4.0".proc_macro2}"."nightly" =
        (f.proc_macro2."${deps.diesel_derives."1.4.0".proc_macro2}"."nightly" or false) ||
        (diesel_derives."1.4.0"."nightly" or false) ||
        (f."diesel_derives"."1.4.0"."nightly" or false); }
      { "${deps.diesel_derives."1.4.0".proc_macro2}".default = true; }
    ];
    quote."${deps.diesel_derives."1.4.0".quote}".default = true;
    syn = fold recursiveUpdate {} [
      { "${deps.diesel_derives."1.4.0".syn}"."fold" = true; }
      { "${deps.diesel_derives."1.4.0".syn}"."full" = true; }
      { "${deps.diesel_derives."1.4.0".syn}".default = true; }
    ];
  }) [
    (features_.proc_macro2."${deps."diesel_derives"."1.4.0"."proc_macro2"}" deps)
    (features_.quote."${deps."diesel_derives"."1.4.0"."quote"}" deps)
    (features_.syn."${deps."diesel_derives"."1.4.0"."syn"}" deps)
  ];


# end
# diesel_migrations-1.4.0

  crates.diesel_migrations."1.4.0" = deps: { features?(features_.diesel_migrations."1.4.0" deps {}) }: buildRustCrate {
    crateName = "diesel_migrations";
    version = "1.4.0";
    authors = [ "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "0z2qwwzbgkrhh9fm29cqiv09jmcvxa55w1p0hiap2dd9wzwxf2c4";
    dependencies = mapFeatures features ([
      (crates."migrations_internals"."${deps."diesel_migrations"."1.4.0"."migrations_internals"}" deps)
      (crates."migrations_macros"."${deps."diesel_migrations"."1.4.0"."migrations_macros"}" deps)
    ]);
    features = mkFeatures (features."diesel_migrations"."1.4.0" or {});
  };
  features_.diesel_migrations."1.4.0" = deps: f: updateFeatures f (rec {
    diesel_migrations."1.4.0".default = (f.diesel_migrations."1.4.0".default or true);
    migrations_internals."${deps.diesel_migrations."1.4.0".migrations_internals}".default = true;
    migrations_macros."${deps.diesel_migrations."1.4.0".migrations_macros}".default = true;
  }) [
    (features_.migrations_internals."${deps."diesel_migrations"."1.4.0"."migrations_internals"}" deps)
    (features_.migrations_macros."${deps."diesel_migrations"."1.4.0"."migrations_macros"}" deps)
  ];


# end
# difference-1.0.0

  crates.difference."1.0.0" = deps: { features?(features_.difference."1.0.0" deps {}) }: buildRustCrate {
    crateName = "difference";
    version = "1.0.0";
    authors = [ "Johann Hofmann <mail@johann-hofmann.com>" ];
    sha256 = "0r1p2diin8zykfiifv6v9i3ajimdb1rg6qzxkrfw2n2iy57846qn";
    crateBin =
      [{  name = "difference"; }];
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."difference"."1.0.0" or {});
  };
  features_.difference."1.0.0" = deps: f: updateFeatures f (rec {
    difference = fold recursiveUpdate {} [
      { "1.0.0".default = (f.difference."1.0.0".default or true); }
      { "1.0.0".getopts =
        (f.difference."1.0.0".getopts or false) ||
        (f.difference."1.0.0".bin or false) ||
        (difference."1.0.0"."bin" or false); }
    ];
  }) [];


# end
# dotenv-0.10.1

  crates.dotenv."0.10.1" = deps: { features?(features_.dotenv."0.10.1" deps {}) }: buildRustCrate {
    crateName = "dotenv";
    version = "0.10.1";
    authors = [ "Santiago Lapresta <santiago.lapresta@gmail.com>" "Craig Hills <chills@gmail.com>" "Mike Piccolo <mfpiccolo@gmail.com>" "Alice Maz <alice@alicemaz.com>" "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "1dhkc41yqchvyxpb9k711pqgfpv0ly44ghmqqfi31mjpirdlg09v";
    dependencies = mapFeatures features ([
      (crates."derive_error_chain"."${deps."dotenv"."0.10.1"."derive_error_chain"}" deps)
      (crates."error_chain"."${deps."dotenv"."0.10.1"."error_chain"}" deps)
      (crates."regex"."${deps."dotenv"."0.10.1"."regex"}" deps)
    ]);
    features = mkFeatures (features."dotenv"."0.10.1" or {});
  };
  features_.dotenv."0.10.1" = deps: f: updateFeatures f (rec {
    derive_error_chain."${deps.dotenv."0.10.1".derive_error_chain}".default = true;
    dotenv = fold recursiveUpdate {} [
      { "0.10.1".backtrace =
        (f.dotenv."0.10.1".backtrace or false) ||
        (f.dotenv."0.10.1".default or false) ||
        (dotenv."0.10.1"."default" or false); }
      { "0.10.1".default = (f.dotenv."0.10.1".default or true); }
    ];
    error_chain = fold recursiveUpdate {} [
      { "${deps.dotenv."0.10.1".error_chain}"."backtrace" =
        (f.error_chain."${deps.dotenv."0.10.1".error_chain}"."backtrace" or false) ||
        (dotenv."0.10.1"."backtrace" or false) ||
        (f."dotenv"."0.10.1"."backtrace" or false); }
      { "${deps.dotenv."0.10.1".error_chain}".default = (f.error_chain."${deps.dotenv."0.10.1".error_chain}".default or false); }
    ];
    regex."${deps.dotenv."0.10.1".regex}".default = true;
  }) [
    (features_.derive_error_chain."${deps."dotenv"."0.10.1"."derive_error_chain"}" deps)
    (features_.error_chain."${deps."dotenv"."0.10.1"."error_chain"}" deps)
    (features_.regex."${deps."dotenv"."0.10.1"."regex"}" deps)
  ];


# end
# env_logger-0.3.5

  crates.env_logger."0.3.5" = deps: { features?(features_.env_logger."0.3.5" deps {}) }: buildRustCrate {
    crateName = "env_logger";
    version = "0.3.5";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1mvxiaaqsyjliv1mm1qaagjqiccw11mdyi3n9h9rf8y6wj15zycw";
    dependencies = mapFeatures features ([
      (crates."log"."${deps."env_logger"."0.3.5"."log"}" deps)
    ]
      ++ (if features.env_logger."0.3.5".regex or false then [ (crates.regex."${deps."env_logger"."0.3.5".regex}" deps) ] else []));
    features = mkFeatures (features."env_logger"."0.3.5" or {});
  };
  features_.env_logger."0.3.5" = deps: f: updateFeatures f (rec {
    env_logger = fold recursiveUpdate {} [
      { "0.3.5".default = (f.env_logger."0.3.5".default or true); }
      { "0.3.5".regex =
        (f.env_logger."0.3.5".regex or false) ||
        (f.env_logger."0.3.5".default or false) ||
        (env_logger."0.3.5"."default" or false); }
    ];
    log."${deps.env_logger."0.3.5".log}".default = true;
    regex."${deps.env_logger."0.3.5".regex}".default = true;
  }) [
    (features_.log."${deps."env_logger"."0.3.5"."log"}" deps)
    (features_.regex."${deps."env_logger"."0.3.5"."regex"}" deps)
  ];


# end
# error-chain-0.10.0

  crates.error_chain."0.10.0" = deps: { features?(features_.error_chain."0.10.0" deps {}) }: buildRustCrate {
    crateName = "error-chain";
    version = "0.10.0";
    authors = [ "Brian Anderson <banderson@mozilla.com>" "Paul Colomiets <paul@colomiets.name>" "Colin Kiegel <kiegel@gmx.de>" "Yamakaky <yamakaky@yamaworld.fr>" ];
    sha256 = "1xxbzd8cjlpzsb9fsih7mdnndhzrvykj0w77yg90qc85az1xwy5z";
    dependencies = mapFeatures features ([
    ]
      ++ (if features.error_chain."0.10.0".backtrace or false then [ (crates.backtrace."${deps."error_chain"."0.10.0".backtrace}" deps) ] else []));
    features = mkFeatures (features."error_chain"."0.10.0" or {});
  };
  features_.error_chain."0.10.0" = deps: f: updateFeatures f (rec {
    backtrace."${deps.error_chain."0.10.0".backtrace}".default = true;
    error_chain = fold recursiveUpdate {} [
      { "0.10.0".backtrace =
        (f.error_chain."0.10.0".backtrace or false) ||
        (f.error_chain."0.10.0".default or false) ||
        (error_chain."0.10.0"."default" or false); }
      { "0.10.0".default = (f.error_chain."0.10.0".default or true); }
      { "0.10.0".example_generated =
        (f.error_chain."0.10.0".example_generated or false) ||
        (f.error_chain."0.10.0".default or false) ||
        (error_chain."0.10.0"."default" or false); }
    ];
  }) [
    (features_.backtrace."${deps."error_chain"."0.10.0"."backtrace"}" deps)
  ];


# end
# fuchsia-cprng-0.1.1

  crates.fuchsia_cprng."0.1.1" = deps: { features?(features_.fuchsia_cprng."0.1.1" deps {}) }: buildRustCrate {
    crateName = "fuchsia-cprng";
    version = "0.1.1";
    authors = [ "Erick Tryzelaar <etryzelaar@google.com>" ];
    edition = "2018";
    sha256 = "07apwv9dj716yjlcj29p94vkqn5zmfh7hlrqvrjx3wzshphc95h9";
  };
  features_.fuchsia_cprng."0.1.1" = deps: f: updateFeatures f (rec {
    fuchsia_cprng."0.1.1".default = (f.fuchsia_cprng."0.1.1".default or true);
  }) [];


# end
# heck-0.3.1

  crates.heck."0.3.1" = deps: { features?(features_.heck."0.3.1" deps {}) }: buildRustCrate {
    crateName = "heck";
    version = "0.3.1";
    authors = [ "Without Boats <woboats@gmail.com>" ];
    sha256 = "1q7vmnlh62kls6cvkfhbcacxkawaznaqa5wwm9dg1xkcza846c3d";
    dependencies = mapFeatures features ([
      (crates."unicode_segmentation"."${deps."heck"."0.3.1"."unicode_segmentation"}" deps)
    ]);
  };
  features_.heck."0.3.1" = deps: f: updateFeatures f (rec {
    heck."0.3.1".default = (f.heck."0.3.1".default or true);
    unicode_segmentation."${deps.heck."0.3.1".unicode_segmentation}".default = true;
  }) [
    (features_.unicode_segmentation."${deps."heck"."0.3.1"."unicode_segmentation"}" deps)
  ];


# end
# idna-0.1.5

  crates.idna."0.1.5" = deps: { features?(features_.idna."0.1.5" deps {}) }: buildRustCrate {
    crateName = "idna";
    version = "0.1.5";
    authors = [ "The rust-url developers" ];
    sha256 = "1gwgl19rz5vzi67rrhamczhxy050f5ynx4ybabfapyalv7z1qmjy";
    dependencies = mapFeatures features ([
      (crates."matches"."${deps."idna"."0.1.5"."matches"}" deps)
      (crates."unicode_bidi"."${deps."idna"."0.1.5"."unicode_bidi"}" deps)
      (crates."unicode_normalization"."${deps."idna"."0.1.5"."unicode_normalization"}" deps)
    ]);
  };
  features_.idna."0.1.5" = deps: f: updateFeatures f (rec {
    idna."0.1.5".default = (f.idna."0.1.5".default or true);
    matches."${deps.idna."0.1.5".matches}".default = true;
    unicode_bidi."${deps.idna."0.1.5".unicode_bidi}".default = true;
    unicode_normalization."${deps.idna."0.1.5".unicode_normalization}".default = true;
  }) [
    (features_.matches."${deps."idna"."0.1.5"."matches"}" deps)
    (features_.unicode_bidi."${deps."idna"."0.1.5"."unicode_bidi"}" deps)
    (features_.unicode_normalization."${deps."idna"."0.1.5"."unicode_normalization"}" deps)
  ];


# end
# ipnetwork-0.13.1

  crates.ipnetwork."0.13.1" = deps: { features?(features_.ipnetwork."0.13.1" deps {}) }: buildRustCrate {
    crateName = "ipnetwork";
    version = "0.13.1";
    authors = [ "Abhishek Chanda <abhishek.becs@gmail.com>" "Linus Färnstrand <faern@faern.net>" ];
    sha256 = "0950ixh2dwbhx9kyn3fixyxhi5sss957acdrx88sdqjsh6pzmnck";
    dependencies = mapFeatures features ([
      (crates."serde"."${deps."ipnetwork"."0.13.1"."serde"}" deps)
      (crates."serde_derive"."${deps."ipnetwork"."0.13.1"."serde_derive"}" deps)
    ]);
    features = mkFeatures (features."ipnetwork"."0.13.1" or {});
  };
  features_.ipnetwork."0.13.1" = deps: f: updateFeatures f (rec {
    ipnetwork = fold recursiveUpdate {} [
      { "0.13.1".clippy =
        (f.ipnetwork."0.13.1".clippy or false) ||
        (f.ipnetwork."0.13.1".dev or false) ||
        (ipnetwork."0.13.1"."dev" or false); }
      { "0.13.1".default = (f.ipnetwork."0.13.1".default or true); }
    ];
    serde."${deps.ipnetwork."0.13.1".serde}".default = true;
    serde_derive."${deps.ipnetwork."0.13.1".serde_derive}".default = true;
  }) [
    (features_.serde."${deps."ipnetwork"."0.13.1"."serde"}" deps)
    (features_.serde_derive."${deps."ipnetwork"."0.13.1"."serde_derive"}" deps)
  ];


# end
# itoa-0.4.4

  crates.itoa."0.4.4" = deps: { features?(features_.itoa."0.4.4" deps {}) }: buildRustCrate {
    crateName = "itoa";
    version = "0.4.4";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "1fqc34xzzl2spfdawxd9awhzl0fwf1y6y4i94l8bq8rfrzd90awl";
    features = mkFeatures (features."itoa"."0.4.4" or {});
  };
  features_.itoa."0.4.4" = deps: f: updateFeatures f (rec {
    itoa = fold recursiveUpdate {} [
      { "0.4.4".default = (f.itoa."0.4.4".default or true); }
      { "0.4.4".std =
        (f.itoa."0.4.4".std or false) ||
        (f.itoa."0.4.4".default or false) ||
        (itoa."0.4.4"."default" or false); }
    ];
  }) [];


# end
# kernel32-sys-0.2.2

  crates.kernel32_sys."0.2.2" = deps: { features?(features_.kernel32_sys."0.2.2" deps {}) }: buildRustCrate {
    crateName = "kernel32-sys";
    version = "0.2.2";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "1lrw1hbinyvr6cp28g60z97w32w8vsk6pahk64pmrv2fmby8srfj";
    libName = "kernel32";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."winapi"."${deps."kernel32_sys"."0.2.2"."winapi"}" deps)
    ]);

    buildDependencies = mapFeatures features ([
      (crates."winapi_build"."${deps."kernel32_sys"."0.2.2"."winapi_build"}" deps)
    ]);
  };
  features_.kernel32_sys."0.2.2" = deps: f: updateFeatures f (rec {
    kernel32_sys."0.2.2".default = (f.kernel32_sys."0.2.2".default or true);
    winapi."${deps.kernel32_sys."0.2.2".winapi}".default = true;
    winapi_build."${deps.kernel32_sys."0.2.2".winapi_build}".default = true;
  }) [
    (features_.winapi."${deps."kernel32_sys"."0.2.2"."winapi"}" deps)
    (features_.winapi_build."${deps."kernel32_sys"."0.2.2"."winapi_build"}" deps)
  ];


# end
# lazy_static-1.3.0

  crates.lazy_static."1.3.0" = deps: { features?(features_.lazy_static."1.3.0" deps {}) }: buildRustCrate {
    crateName = "lazy_static";
    version = "1.3.0";
    authors = [ "Marvin Löbel <loebel.marvin@gmail.com>" ];
    sha256 = "1vv47va18ydk7dx5paz88g3jy1d3lwbx6qpxkbj8gyfv770i4b1y";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."lazy_static"."1.3.0" or {});
  };
  features_.lazy_static."1.3.0" = deps: f: updateFeatures f (rec {
    lazy_static = fold recursiveUpdate {} [
      { "1.3.0".default = (f.lazy_static."1.3.0".default or true); }
      { "1.3.0".spin =
        (f.lazy_static."1.3.0".spin or false) ||
        (f.lazy_static."1.3.0".spin_no_std or false) ||
        (lazy_static."1.3.0"."spin_no_std" or false); }
    ];
  }) [];


# end
# libc-0.2.58

  crates.libc."0.2.58" = deps: { features?(features_.libc."0.2.58" deps {}) }: buildRustCrate {
    crateName = "libc";
    version = "0.2.58";
    authors = [ "The Rust Project Developers" ];
    sha256 = "06yk3c0qlcn925ap1rrhikpzgwf504ydkcadj41kib7a06f66k7c";
    build = "build.rs";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."libc"."0.2.58" or {});
  };
  features_.libc."0.2.58" = deps: f: updateFeatures f (rec {
    libc = fold recursiveUpdate {} [
      { "0.2.58".align =
        (f.libc."0.2.58".align or false) ||
        (f.libc."0.2.58".rustc-dep-of-std or false) ||
        (libc."0.2.58"."rustc-dep-of-std" or false); }
      { "0.2.58".default = (f.libc."0.2.58".default or true); }
      { "0.2.58".rustc-std-workspace-core =
        (f.libc."0.2.58".rustc-std-workspace-core or false) ||
        (f.libc."0.2.58".rustc-dep-of-std or false) ||
        (libc."0.2.58"."rustc-dep-of-std" or false); }
      { "0.2.58".std =
        (f.libc."0.2.58".std or false) ||
        (f.libc."0.2.58".default or false) ||
        (libc."0.2.58"."default" or false) ||
        (f.libc."0.2.58".use_std or false) ||
        (libc."0.2.58"."use_std" or false); }
    ];
  }) [];


# end
# libsqlite3-sys-0.12.0

  crates.libsqlite3_sys."0.12.0" = deps: { features?(features_.libsqlite3_sys."0.12.0" deps {}) }: buildRustCrate {
    crateName = "libsqlite3-sys";
    version = "0.12.0";
    authors = [ "John Gallagher <jgallagher@bignerdranch.com>" ];
    edition = "2018";
    sha256 = "01ws9vrks20axk6ghvs7ahhn8lixah4a3q39c32bf0711rz93013";
    build = "build.rs";
    dependencies = (if abi == "msvc" then mapFeatures features ([
]) else []);

    buildDependencies = mapFeatures features ([
    ]
      ++ (if features.libsqlite3_sys."0.12.0".cc or false then [ (crates.cc."${deps."libsqlite3_sys"."0.12.0".cc}" deps) ] else [])
      ++ (if features.libsqlite3_sys."0.12.0".pkg-config or false then [ (crates.pkg_config."${deps."libsqlite3_sys"."0.12.0".pkg_config}" deps) ] else []));
    features = mkFeatures (features."libsqlite3_sys"."0.12.0" or {});
  };
  features_.libsqlite3_sys."0.12.0" = deps: f: updateFeatures f (rec {
    cc."${deps.libsqlite3_sys."0.12.0".cc}".default = true;
    libsqlite3_sys = fold recursiveUpdate {} [
      { "0.12.0".bindgen =
        (f.libsqlite3_sys."0.12.0".bindgen or false) ||
        (f.libsqlite3_sys."0.12.0".buildtime_bindgen or false) ||
        (libsqlite3_sys."0.12.0"."buildtime_bindgen" or false); }
      { "0.12.0".cc =
        (f.libsqlite3_sys."0.12.0".cc or false) ||
        (f.libsqlite3_sys."0.12.0".bundled or false) ||
        (libsqlite3_sys."0.12.0"."bundled" or false); }
      { "0.12.0".default = (f.libsqlite3_sys."0.12.0".default or true); }
      { "0.12.0".min_sqlite_version_3_6_8 =
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_6_8 or false) ||
        (f.libsqlite3_sys."0.12.0".default or false) ||
        (libsqlite3_sys."0.12.0"."default" or false); }
      { "0.12.0".pkg-config =
        (f.libsqlite3_sys."0.12.0".pkg-config or false) ||
        (f.libsqlite3_sys."0.12.0".buildtime_bindgen or false) ||
        (libsqlite3_sys."0.12.0"."buildtime_bindgen" or false) ||
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_6_23 or false) ||
        (libsqlite3_sys."0.12.0"."min_sqlite_version_3_6_23" or false) ||
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_6_8 or false) ||
        (libsqlite3_sys."0.12.0"."min_sqlite_version_3_6_8" or false) ||
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_7_16 or false) ||
        (libsqlite3_sys."0.12.0"."min_sqlite_version_3_7_16" or false) ||
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_7_7 or false) ||
        (libsqlite3_sys."0.12.0"."min_sqlite_version_3_7_7" or false); }
      { "0.12.0".vcpkg =
        (f.libsqlite3_sys."0.12.0".vcpkg or false) ||
        (f.libsqlite3_sys."0.12.0".buildtime_bindgen or false) ||
        (libsqlite3_sys."0.12.0"."buildtime_bindgen" or false) ||
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_6_23 or false) ||
        (libsqlite3_sys."0.12.0"."min_sqlite_version_3_6_23" or false) ||
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_6_8 or false) ||
        (libsqlite3_sys."0.12.0"."min_sqlite_version_3_6_8" or false) ||
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_7_16 or false) ||
        (libsqlite3_sys."0.12.0"."min_sqlite_version_3_7_16" or false) ||
        (f.libsqlite3_sys."0.12.0".min_sqlite_version_3_7_7 or false) ||
        (libsqlite3_sys."0.12.0"."min_sqlite_version_3_7_7" or false); }
    ];
    pkg_config."${deps.libsqlite3_sys."0.12.0".pkg_config}".default = true;
  }) [
    (features_.cc."${deps."libsqlite3_sys"."0.12.0"."cc"}" deps)
    (features_.pkg_config."${deps."libsqlite3_sys"."0.12.0"."pkg_config"}" deps)
  ];


# end
# lock_api-0.2.0

  crates.lock_api."0.2.0" = deps: { features?(features_.lock_api."0.2.0" deps {}) }: buildRustCrate {
    crateName = "lock_api";
    version = "0.2.0";
    authors = [ "Amanieu d'Antras <amanieu@gmail.com>" ];
    edition = "2018";
    sha256 = "0395dw47pjqzwd2krhj8p5khm0dhsj3h03syjdvqb75zxwagi84k";
    dependencies = mapFeatures features ([
      (crates."scopeguard"."${deps."lock_api"."0.2.0"."scopeguard"}" deps)
    ]);
    features = mkFeatures (features."lock_api"."0.2.0" or {});
  };
  features_.lock_api."0.2.0" = deps: f: updateFeatures f (rec {
    lock_api."0.2.0".default = (f.lock_api."0.2.0".default or true);
    scopeguard."${deps.lock_api."0.2.0".scopeguard}".default = (f.scopeguard."${deps.lock_api."0.2.0".scopeguard}".default or false);
  }) [
    (features_.scopeguard."${deps."lock_api"."0.2.0"."scopeguard"}" deps)
  ];


# end
# log-0.3.9

  crates.log."0.3.9" = deps: { features?(features_.log."0.3.9" deps {}) }: buildRustCrate {
    crateName = "log";
    version = "0.3.9";
    authors = [ "The Rust Project Developers" ];
    sha256 = "19i9pwp7lhaqgzangcpw00kc3zsgcqcx84crv07xgz3v7d3kvfa2";
    dependencies = mapFeatures features ([
      (crates."log"."${deps."log"."0.3.9"."log"}" deps)
    ]);
    features = mkFeatures (features."log"."0.3.9" or {});
  };
  features_.log."0.3.9" = deps: f: updateFeatures f (rec {
    log = fold recursiveUpdate {} [
      { "${deps.log."0.3.9".log}"."max_level_debug" =
        (f.log."${deps.log."0.3.9".log}"."max_level_debug" or false) ||
        (log."0.3.9"."max_level_debug" or false) ||
        (f."log"."0.3.9"."max_level_debug" or false); }
      { "${deps.log."0.3.9".log}"."max_level_error" =
        (f.log."${deps.log."0.3.9".log}"."max_level_error" or false) ||
        (log."0.3.9"."max_level_error" or false) ||
        (f."log"."0.3.9"."max_level_error" or false); }
      { "${deps.log."0.3.9".log}"."max_level_info" =
        (f.log."${deps.log."0.3.9".log}"."max_level_info" or false) ||
        (log."0.3.9"."max_level_info" or false) ||
        (f."log"."0.3.9"."max_level_info" or false); }
      { "${deps.log."0.3.9".log}"."max_level_off" =
        (f.log."${deps.log."0.3.9".log}"."max_level_off" or false) ||
        (log."0.3.9"."max_level_off" or false) ||
        (f."log"."0.3.9"."max_level_off" or false); }
      { "${deps.log."0.3.9".log}"."max_level_trace" =
        (f.log."${deps.log."0.3.9".log}"."max_level_trace" or false) ||
        (log."0.3.9"."max_level_trace" or false) ||
        (f."log"."0.3.9"."max_level_trace" or false); }
      { "${deps.log."0.3.9".log}"."max_level_warn" =
        (f.log."${deps.log."0.3.9".log}"."max_level_warn" or false) ||
        (log."0.3.9"."max_level_warn" or false) ||
        (f."log"."0.3.9"."max_level_warn" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_debug" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_debug" or false) ||
        (log."0.3.9"."release_max_level_debug" or false) ||
        (f."log"."0.3.9"."release_max_level_debug" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_error" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_error" or false) ||
        (log."0.3.9"."release_max_level_error" or false) ||
        (f."log"."0.3.9"."release_max_level_error" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_info" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_info" or false) ||
        (log."0.3.9"."release_max_level_info" or false) ||
        (f."log"."0.3.9"."release_max_level_info" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_off" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_off" or false) ||
        (log."0.3.9"."release_max_level_off" or false) ||
        (f."log"."0.3.9"."release_max_level_off" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_trace" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_trace" or false) ||
        (log."0.3.9"."release_max_level_trace" or false) ||
        (f."log"."0.3.9"."release_max_level_trace" or false); }
      { "${deps.log."0.3.9".log}"."release_max_level_warn" =
        (f.log."${deps.log."0.3.9".log}"."release_max_level_warn" or false) ||
        (log."0.3.9"."release_max_level_warn" or false) ||
        (f."log"."0.3.9"."release_max_level_warn" or false); }
      { "${deps.log."0.3.9".log}"."std" =
        (f.log."${deps.log."0.3.9".log}"."std" or false) ||
        (log."0.3.9"."use_std" or false) ||
        (f."log"."0.3.9"."use_std" or false); }
      { "${deps.log."0.3.9".log}".default = true; }
      { "0.3.9".default = (f.log."0.3.9".default or true); }
      { "0.3.9".use_std =
        (f.log."0.3.9".use_std or false) ||
        (f.log."0.3.9".default or false) ||
        (log."0.3.9"."default" or false); }
    ];
  }) [
    (features_.log."${deps."log"."0.3.9"."log"}" deps)
  ];


# end
# log-0.4.6

  crates.log."0.4.6" = deps: { features?(features_.log."0.4.6" deps {}) }: buildRustCrate {
    crateName = "log";
    version = "0.4.6";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1nd8dl9mvc9vd6fks5d4gsxaz990xi6rzlb8ymllshmwi153vngr";
    dependencies = mapFeatures features ([
      (crates."cfg_if"."${deps."log"."0.4.6"."cfg_if"}" deps)
    ]);
    features = mkFeatures (features."log"."0.4.6" or {});
  };
  features_.log."0.4.6" = deps: f: updateFeatures f (rec {
    cfg_if."${deps.log."0.4.6".cfg_if}".default = true;
    log."0.4.6".default = (f.log."0.4.6".default or true);
  }) [
    (features_.cfg_if."${deps."log"."0.4.6"."cfg_if"}" deps)
  ];


# end
# matches-0.1.8

  crates.matches."0.1.8" = deps: { features?(features_.matches."0.1.8" deps {}) }: buildRustCrate {
    crateName = "matches";
    version = "0.1.8";
    authors = [ "Simon Sapin <simon.sapin@exyr.org>" ];
    sha256 = "03hl636fg6xggy0a26200xs74amk3k9n0908rga2szn68agyz3cv";
    libPath = "lib.rs";
  };
  features_.matches."0.1.8" = deps: f: updateFeatures f (rec {
    matches."0.1.8".default = (f.matches."0.1.8".default or true);
  }) [];


# end
# memchr-0.1.11

  crates.memchr."0.1.11" = deps: { features?(features_.memchr."0.1.11" deps {}) }: buildRustCrate {
    crateName = "memchr";
    version = "0.1.11";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" "bluss" ];
    sha256 = "0x73jghamvxxq5fsw9wb0shk5m6qp3q6fsf0nibn0i6bbqkw91s8";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."memchr"."0.1.11"."libc"}" deps)
    ]);
  };
  features_.memchr."0.1.11" = deps: f: updateFeatures f (rec {
    libc."${deps.memchr."0.1.11".libc}".default = true;
    memchr."0.1.11".default = (f.memchr."0.1.11".default or true);
  }) [
    (features_.libc."${deps."memchr"."0.1.11"."libc"}" deps)
  ];


# end
# memchr-2.2.0

  crates.memchr."2.2.0" = deps: { features?(features_.memchr."2.2.0" deps {}) }: buildRustCrate {
    crateName = "memchr";
    version = "2.2.0";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" "bluss" ];
    sha256 = "11vwg8iig9jyjxq3n1cq15g29ikzw5l7ar87md54k1aisjs0997p";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."memchr"."2.2.0" or {});
  };
  features_.memchr."2.2.0" = deps: f: updateFeatures f (rec {
    memchr = fold recursiveUpdate {} [
      { "2.2.0".default = (f.memchr."2.2.0".default or true); }
      { "2.2.0".use_std =
        (f.memchr."2.2.0".use_std or false) ||
        (f.memchr."2.2.0".default or false) ||
        (memchr."2.2.0"."default" or false); }
    ];
  }) [];


# end
# migrations_internals-1.4.0

  crates.migrations_internals."1.4.0" = deps: { features?(features_.migrations_internals."1.4.0" deps {}) }: buildRustCrate {
    crateName = "migrations_internals";
    version = "1.4.0";
    authors = [ "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "0k19bzh4i6vkckvjq9n895cr7wgaya9m2h6n622hh2q2swfkc5bp";
    dependencies = mapFeatures features ([
      (crates."diesel"."${deps."migrations_internals"."1.4.0"."diesel"}" deps)
    ]
      ++ (if features.migrations_internals."1.4.0".barrel or false then [ (crates.barrel."${deps."migrations_internals"."1.4.0".barrel}" deps) ] else []));
    features = mkFeatures (features."migrations_internals"."1.4.0" or {});
  };
  features_.migrations_internals."1.4.0" = deps: f: updateFeatures f (rec {
    barrel = fold recursiveUpdate {} [
      { "${deps.migrations_internals."1.4.0".barrel}"."diesel-filled" = true; }
      { "${deps.migrations_internals."1.4.0".barrel}".default = true; }
    ];
    diesel."${deps.migrations_internals."1.4.0".diesel}".default = (f.diesel."${deps.migrations_internals."1.4.0".diesel}".default or false);
    migrations_internals."1.4.0".default = (f.migrations_internals."1.4.0".default or true);
  }) [
    (features_.barrel."${deps."migrations_internals"."1.4.0"."barrel"}" deps)
    (features_.diesel."${deps."migrations_internals"."1.4.0"."diesel"}" deps)
  ];


# end
# migrations_macros-1.4.0

  crates.migrations_macros."1.4.0" = deps: { features?(features_.migrations_macros."1.4.0" deps {}) }: buildRustCrate {
    crateName = "migrations_macros";
    version = "1.4.0";
    authors = [ "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "0rl8jnxig4f79wxklckk9bb06h49f8xikgm86a9ysfg9wd4gfdfv";
    procMacro = true;
    dependencies = mapFeatures features ([
      (crates."migrations_internals"."${deps."migrations_macros"."1.4.0"."migrations_internals"}" deps)
      (crates."quote"."${deps."migrations_macros"."1.4.0"."quote"}" deps)
      (crates."syn"."${deps."migrations_macros"."1.4.0"."syn"}" deps)
    ]);
  };
  features_.migrations_macros."1.4.0" = deps: f: updateFeatures f (rec {
    migrations_internals."${deps.migrations_macros."1.4.0".migrations_internals}".default = true;
    migrations_macros."1.4.0".default = (f.migrations_macros."1.4.0".default or true);
    quote."${deps.migrations_macros."1.4.0".quote}".default = true;
    syn = fold recursiveUpdate {} [
      { "${deps.migrations_macros."1.4.0".syn}"."aster" = true; }
      { "${deps.migrations_macros."1.4.0".syn}".default = true; }
    ];
  }) [
    (features_.migrations_internals."${deps."migrations_macros"."1.4.0"."migrations_internals"}" deps)
    (features_.quote."${deps."migrations_macros"."1.4.0"."quote"}" deps)
    (features_.syn."${deps."migrations_macros"."1.4.0"."syn"}" deps)
  ];


# end
# mysqlclient-sys-0.2.4

  crates.mysqlclient_sys."0.2.4" = deps: { features?(features_.mysqlclient_sys."0.2.4" deps {}) }: buildRustCrate {
    crateName = "mysqlclient-sys";
    version = "0.2.4";
    authors = [ "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "0f0rdfqfx3083wv30kdi274pflpwvizqv7jav2q4i5hvdhsaj7v5";
    build = "build.rs";
    dependencies = (if abi == "msvc" then mapFeatures features ([
]) else []);

    buildDependencies = mapFeatures features ([
      (crates."pkg_config"."${deps."mysqlclient_sys"."0.2.4"."pkg_config"}" deps)
    ]);
  };
  features_.mysqlclient_sys."0.2.4" = deps: f: updateFeatures f (rec {
    mysqlclient_sys."0.2.4".default = (f.mysqlclient_sys."0.2.4".default or true);
    pkg_config."${deps.mysqlclient_sys."0.2.4".pkg_config}".default = true;
  }) [
    (features_.pkg_config."${deps."mysqlclient_sys"."0.2.4"."pkg_config"}" deps)
  ];


# end
# num-bigint-0.2.2

  crates.num_bigint."0.2.2" = deps: { features?(features_.num_bigint."0.2.2" deps {}) }: buildRustCrate {
    crateName = "num-bigint";
    version = "0.2.2";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0alza0afrwvhiilqvjazkxv94sir14jqfi50cpv40rgjl1rk7xf6";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."num_integer"."${deps."num_bigint"."0.2.2"."num_integer"}" deps)
      (crates."num_traits"."${deps."num_bigint"."0.2.2"."num_traits"}" deps)
    ]);
    features = mkFeatures (features."num_bigint"."0.2.2" or {});
  };
  features_.num_bigint."0.2.2" = deps: f: updateFeatures f (rec {
    num_bigint = fold recursiveUpdate {} [
      { "0.2.2".default = (f.num_bigint."0.2.2".default or true); }
      { "0.2.2".std =
        (f.num_bigint."0.2.2".std or false) ||
        (f.num_bigint."0.2.2".default or false) ||
        (num_bigint."0.2.2"."default" or false); }
    ];
    num_integer = fold recursiveUpdate {} [
      { "${deps.num_bigint."0.2.2".num_integer}"."i128" =
        (f.num_integer."${deps.num_bigint."0.2.2".num_integer}"."i128" or false) ||
        (num_bigint."0.2.2"."i128" or false) ||
        (f."num_bigint"."0.2.2"."i128" or false); }
      { "${deps.num_bigint."0.2.2".num_integer}"."std" =
        (f.num_integer."${deps.num_bigint."0.2.2".num_integer}"."std" or false) ||
        (num_bigint."0.2.2"."std" or false) ||
        (f."num_bigint"."0.2.2"."std" or false); }
      { "${deps.num_bigint."0.2.2".num_integer}".default = (f.num_integer."${deps.num_bigint."0.2.2".num_integer}".default or false); }
    ];
    num_traits = fold recursiveUpdate {} [
      { "${deps.num_bigint."0.2.2".num_traits}"."i128" =
        (f.num_traits."${deps.num_bigint."0.2.2".num_traits}"."i128" or false) ||
        (num_bigint."0.2.2"."i128" or false) ||
        (f."num_bigint"."0.2.2"."i128" or false); }
      { "${deps.num_bigint."0.2.2".num_traits}"."std" =
        (f.num_traits."${deps.num_bigint."0.2.2".num_traits}"."std" or false) ||
        (num_bigint."0.2.2"."std" or false) ||
        (f."num_bigint"."0.2.2"."std" or false); }
      { "${deps.num_bigint."0.2.2".num_traits}".default = (f.num_traits."${deps.num_bigint."0.2.2".num_traits}".default or false); }
    ];
  }) [
    (features_.num_integer."${deps."num_bigint"."0.2.2"."num_integer"}" deps)
    (features_.num_traits."${deps."num_bigint"."0.2.2"."num_traits"}" deps)
  ];


# end
# num-integer-0.1.41

  crates.num_integer."0.1.41" = deps: { features?(features_.num_integer."0.1.41" deps {}) }: buildRustCrate {
    crateName = "num-integer";
    version = "0.1.41";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1y45nh9xlp2dra9svb1wfsy65fysm3k1w4m8jynywccq645yixid";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."num_traits"."${deps."num_integer"."0.1.41"."num_traits"}" deps)
    ]);

    buildDependencies = mapFeatures features ([
      (crates."autocfg"."${deps."num_integer"."0.1.41"."autocfg"}" deps)
    ]);
    features = mkFeatures (features."num_integer"."0.1.41" or {});
  };
  features_.num_integer."0.1.41" = deps: f: updateFeatures f (rec {
    autocfg."${deps.num_integer."0.1.41".autocfg}".default = true;
    num_integer = fold recursiveUpdate {} [
      { "0.1.41".default = (f.num_integer."0.1.41".default or true); }
      { "0.1.41".std =
        (f.num_integer."0.1.41".std or false) ||
        (f.num_integer."0.1.41".default or false) ||
        (num_integer."0.1.41"."default" or false); }
    ];
    num_traits = fold recursiveUpdate {} [
      { "${deps.num_integer."0.1.41".num_traits}"."i128" =
        (f.num_traits."${deps.num_integer."0.1.41".num_traits}"."i128" or false) ||
        (num_integer."0.1.41"."i128" or false) ||
        (f."num_integer"."0.1.41"."i128" or false); }
      { "${deps.num_integer."0.1.41".num_traits}"."std" =
        (f.num_traits."${deps.num_integer."0.1.41".num_traits}"."std" or false) ||
        (num_integer."0.1.41"."std" or false) ||
        (f."num_integer"."0.1.41"."std" or false); }
      { "${deps.num_integer."0.1.41".num_traits}".default = (f.num_traits."${deps.num_integer."0.1.41".num_traits}".default or false); }
    ];
  }) [
    (features_.num_traits."${deps."num_integer"."0.1.41"."num_traits"}" deps)
    (features_.autocfg."${deps."num_integer"."0.1.41"."autocfg"}" deps)
  ];


# end
# num-traits-0.2.8

  crates.num_traits."0.2.8" = deps: { features?(features_.num_traits."0.2.8" deps {}) }: buildRustCrate {
    crateName = "num-traits";
    version = "0.2.8";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1mnlmy35n734n9xlq0qkfbgzz33x09a1s4rfj30p1976p09b862v";
    build = "build.rs";

    buildDependencies = mapFeatures features ([
      (crates."autocfg"."${deps."num_traits"."0.2.8"."autocfg"}" deps)
    ]);
    features = mkFeatures (features."num_traits"."0.2.8" or {});
  };
  features_.num_traits."0.2.8" = deps: f: updateFeatures f (rec {
    autocfg."${deps.num_traits."0.2.8".autocfg}".default = true;
    num_traits = fold recursiveUpdate {} [
      { "0.2.8".default = (f.num_traits."0.2.8".default or true); }
      { "0.2.8".std =
        (f.num_traits."0.2.8".std or false) ||
        (f.num_traits."0.2.8".default or false) ||
        (num_traits."0.2.8"."default" or false); }
    ];
  }) [
    (features_.autocfg."${deps."num_traits"."0.2.8"."autocfg"}" deps)
  ];


# end
# numtoa-0.1.0

  crates.numtoa."0.1.0" = deps: { features?(features_.numtoa."0.1.0" deps {}) }: buildRustCrate {
    crateName = "numtoa";
    version = "0.1.0";
    authors = [ "Michael Aaron Murphy <mmstickman@gmail.com>" ];
    sha256 = "1i2wxr96bb1rvax15z843126z3bnl2frpx69vxsp95r96wr24j08";
    features = mkFeatures (features."numtoa"."0.1.0" or {});
  };
  features_.numtoa."0.1.0" = deps: f: updateFeatures f (rec {
    numtoa."0.1.0".default = (f.numtoa."0.1.0".default or true);
  }) [];


# end
# parking_lot-0.8.0

  crates.parking_lot."0.8.0" = deps: { features?(features_.parking_lot."0.8.0" deps {}) }: buildRustCrate {
    crateName = "parking_lot";
    version = "0.8.0";
    authors = [ "Amanieu d'Antras <amanieu@gmail.com>" ];
    edition = "2018";
    sha256 = "176yk6b55qxz9h4hp01icpb7jsh4fki0ycp9gs6x3yqcxgfkp53z";
    dependencies = mapFeatures features ([
      (crates."lock_api"."${deps."parking_lot"."0.8.0"."lock_api"}" deps)
      (crates."parking_lot_core"."${deps."parking_lot"."0.8.0"."parking_lot_core"}" deps)
    ]);

    buildDependencies = mapFeatures features ([
      (crates."rustc_version"."${deps."parking_lot"."0.8.0"."rustc_version"}" deps)
    ]);
    features = mkFeatures (features."parking_lot"."0.8.0" or {});
  };
  features_.parking_lot."0.8.0" = deps: f: updateFeatures f (rec {
    lock_api = fold recursiveUpdate {} [
      { "${deps.parking_lot."0.8.0".lock_api}"."nightly" =
        (f.lock_api."${deps.parking_lot."0.8.0".lock_api}"."nightly" or false) ||
        (parking_lot."0.8.0"."nightly" or false) ||
        (f."parking_lot"."0.8.0"."nightly" or false); }
      { "${deps.parking_lot."0.8.0".lock_api}"."owning_ref" =
        (f.lock_api."${deps.parking_lot."0.8.0".lock_api}"."owning_ref" or false) ||
        (parking_lot."0.8.0"."owning_ref" or false) ||
        (f."parking_lot"."0.8.0"."owning_ref" or false); }
      { "${deps.parking_lot."0.8.0".lock_api}"."serde" =
        (f.lock_api."${deps.parking_lot."0.8.0".lock_api}"."serde" or false) ||
        (parking_lot."0.8.0"."serde" or false) ||
        (f."parking_lot"."0.8.0"."serde" or false); }
      { "${deps.parking_lot."0.8.0".lock_api}".default = true; }
    ];
    parking_lot."0.8.0".default = (f.parking_lot."0.8.0".default or true);
    parking_lot_core = fold recursiveUpdate {} [
      { "${deps.parking_lot."0.8.0".parking_lot_core}"."deadlock_detection" =
        (f.parking_lot_core."${deps.parking_lot."0.8.0".parking_lot_core}"."deadlock_detection" or false) ||
        (parking_lot."0.8.0"."deadlock_detection" or false) ||
        (f."parking_lot"."0.8.0"."deadlock_detection" or false); }
      { "${deps.parking_lot."0.8.0".parking_lot_core}"."nightly" =
        (f.parking_lot_core."${deps.parking_lot."0.8.0".parking_lot_core}"."nightly" or false) ||
        (parking_lot."0.8.0"."nightly" or false) ||
        (f."parking_lot"."0.8.0"."nightly" or false); }
      { "${deps.parking_lot."0.8.0".parking_lot_core}".default = true; }
    ];
    rustc_version."${deps.parking_lot."0.8.0".rustc_version}".default = true;
  }) [
    (features_.lock_api."${deps."parking_lot"."0.8.0"."lock_api"}" deps)
    (features_.parking_lot_core."${deps."parking_lot"."0.8.0"."parking_lot_core"}" deps)
    (features_.rustc_version."${deps."parking_lot"."0.8.0"."rustc_version"}" deps)
  ];


# end
# parking_lot_core-0.5.0

  crates.parking_lot_core."0.5.0" = deps: { features?(features_.parking_lot_core."0.5.0" deps {}) }: buildRustCrate {
    crateName = "parking_lot_core";
    version = "0.5.0";
    authors = [ "Amanieu d'Antras <amanieu@gmail.com>" ];
    edition = "2018";
    sha256 = "0a8v2jnm5r97qmihlpffxwr252n8i3ljywgbxzagnma55k3323zp";
    dependencies = mapFeatures features ([
      (crates."cfg_if"."${deps."parking_lot_core"."0.5.0"."cfg_if"}" deps)
      (crates."rand"."${deps."parking_lot_core"."0.5.0"."rand"}" deps)
      (crates."smallvec"."${deps."parking_lot_core"."0.5.0"."smallvec"}" deps)
    ])
      ++ (if kernel == "cloudabi" then mapFeatures features ([
      (crates."cloudabi"."${deps."parking_lot_core"."0.5.0"."cloudabi"}" deps)
    ]) else [])
      ++ (if kernel == "redox" then mapFeatures features ([
      (crates."redox_syscall"."${deps."parking_lot_core"."0.5.0"."redox_syscall"}" deps)
    ]) else [])
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
      (crates."libc"."${deps."parking_lot_core"."0.5.0"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."parking_lot_core"."0.5.0"."winapi"}" deps)
    ]) else []);

    buildDependencies = mapFeatures features ([
      (crates."rustc_version"."${deps."parking_lot_core"."0.5.0"."rustc_version"}" deps)
    ]);
    features = mkFeatures (features."parking_lot_core"."0.5.0" or {});
  };
  features_.parking_lot_core."0.5.0" = deps: f: updateFeatures f (rec {
    cfg_if."${deps.parking_lot_core."0.5.0".cfg_if}".default = true;
    cloudabi."${deps.parking_lot_core."0.5.0".cloudabi}".default = true;
    libc."${deps.parking_lot_core."0.5.0".libc}".default = true;
    parking_lot_core = fold recursiveUpdate {} [
      { "0.5.0".backtrace =
        (f.parking_lot_core."0.5.0".backtrace or false) ||
        (f.parking_lot_core."0.5.0".deadlock_detection or false) ||
        (parking_lot_core."0.5.0"."deadlock_detection" or false); }
      { "0.5.0".default = (f.parking_lot_core."0.5.0".default or true); }
      { "0.5.0".petgraph =
        (f.parking_lot_core."0.5.0".petgraph or false) ||
        (f.parking_lot_core."0.5.0".deadlock_detection or false) ||
        (parking_lot_core."0.5.0"."deadlock_detection" or false); }
      { "0.5.0".thread-id =
        (f.parking_lot_core."0.5.0".thread-id or false) ||
        (f.parking_lot_core."0.5.0".deadlock_detection or false) ||
        (parking_lot_core."0.5.0"."deadlock_detection" or false); }
    ];
    rand."${deps.parking_lot_core."0.5.0".rand}".default = true;
    redox_syscall."${deps.parking_lot_core."0.5.0".redox_syscall}".default = true;
    rustc_version."${deps.parking_lot_core."0.5.0".rustc_version}".default = true;
    smallvec."${deps.parking_lot_core."0.5.0".smallvec}".default = true;
    winapi = fold recursiveUpdate {} [
      { "${deps.parking_lot_core."0.5.0".winapi}"."errhandlingapi" = true; }
      { "${deps.parking_lot_core."0.5.0".winapi}"."handleapi" = true; }
      { "${deps.parking_lot_core."0.5.0".winapi}"."minwindef" = true; }
      { "${deps.parking_lot_core."0.5.0".winapi}"."ntstatus" = true; }
      { "${deps.parking_lot_core."0.5.0".winapi}"."winbase" = true; }
      { "${deps.parking_lot_core."0.5.0".winapi}"."winerror" = true; }
      { "${deps.parking_lot_core."0.5.0".winapi}"."winnt" = true; }
      { "${deps.parking_lot_core."0.5.0".winapi}".default = true; }
    ];
  }) [
    (features_.cfg_if."${deps."parking_lot_core"."0.5.0"."cfg_if"}" deps)
    (features_.rand."${deps."parking_lot_core"."0.5.0"."rand"}" deps)
    (features_.smallvec."${deps."parking_lot_core"."0.5.0"."smallvec"}" deps)
    (features_.rustc_version."${deps."parking_lot_core"."0.5.0"."rustc_version"}" deps)
    (features_.cloudabi."${deps."parking_lot_core"."0.5.0"."cloudabi"}" deps)
    (features_.redox_syscall."${deps."parking_lot_core"."0.5.0"."redox_syscall"}" deps)
    (features_.libc."${deps."parking_lot_core"."0.5.0"."libc"}" deps)
    (features_.winapi."${deps."parking_lot_core"."0.5.0"."winapi"}" deps)
  ];


# end
# percent-encoding-1.0.1

  crates.percent_encoding."1.0.1" = deps: { features?(features_.percent_encoding."1.0.1" deps {}) }: buildRustCrate {
    crateName = "percent-encoding";
    version = "1.0.1";
    authors = [ "The rust-url developers" ];
    sha256 = "04ahrp7aw4ip7fmadb0bknybmkfav0kk0gw4ps3ydq5w6hr0ib5i";
    libPath = "lib.rs";
  };
  features_.percent_encoding."1.0.1" = deps: f: updateFeatures f (rec {
    percent_encoding."1.0.1".default = (f.percent_encoding."1.0.1".default or true);
  }) [];


# end
# pkg-config-0.3.14

  crates.pkg_config."0.3.14" = deps: { features?(features_.pkg_config."0.3.14" deps {}) }: buildRustCrate {
    crateName = "pkg-config";
    version = "0.3.14";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "0207fsarrm412j0dh87lfcas72n8mxar7q3mgflsbsrqnb140sv6";
  };
  features_.pkg_config."0.3.14" = deps: f: updateFeatures f (rec {
    pkg_config."0.3.14".default = (f.pkg_config."0.3.14".default or true);
  }) [];


# end
# pq-sys-0.4.6

  crates.pq_sys."0.4.6" = deps: { features?(features_.pq_sys."0.4.6" deps {}) }: buildRustCrate {
    crateName = "pq-sys";
    version = "0.4.6";
    authors = [ "Sean Griffin <sean@seantheprogrammer.com>" ];
    sha256 = "0qsy79igizb54x34m1p7zf28ffvadsm3hyj52djq3rwb56kb95n6";
    libName = "pq_sys";
    build = "build.rs";
    dependencies = (if abi == "msvc" then mapFeatures features ([
]) else []);

    buildDependencies = mapFeatures features ([
]);
  };
  features_.pq_sys."0.4.6" = deps: f: updateFeatures f (rec {
    pq_sys."0.4.6".default = (f.pq_sys."0.4.6".default or true);
  }) [];


# end
# proc-macro2-0.4.30

  crates.proc_macro2."0.4.30" = deps: { features?(features_.proc_macro2."0.4.30" deps {}) }: buildRustCrate {
    crateName = "proc-macro2";
    version = "0.4.30";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "0iifv51wrm6r4r2gghw6rray3nv53zcap355bbz1nsmbhj5s09b9";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."unicode_xid"."${deps."proc_macro2"."0.4.30"."unicode_xid"}" deps)
    ]);
    features = mkFeatures (features."proc_macro2"."0.4.30" or {});
  };
  features_.proc_macro2."0.4.30" = deps: f: updateFeatures f (rec {
    proc_macro2 = fold recursiveUpdate {} [
      { "0.4.30".default = (f.proc_macro2."0.4.30".default or true); }
      { "0.4.30".proc-macro =
        (f.proc_macro2."0.4.30".proc-macro or false) ||
        (f.proc_macro2."0.4.30".default or false) ||
        (proc_macro2."0.4.30"."default" or false); }
    ];
    unicode_xid."${deps.proc_macro2."0.4.30".unicode_xid}".default = true;
  }) [
    (features_.unicode_xid."${deps."proc_macro2"."0.4.30"."unicode_xid"}" deps)
  ];


# end
# quickcheck-0.4.1

  crates.quickcheck."0.4.1" = deps: { features?(features_.quickcheck."0.4.1" deps {}) }: buildRustCrate {
    crateName = "quickcheck";
    version = "0.4.1";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" ];
    sha256 = "0d7hwwpzbwzy4gk4kkdxqf8yi9b1pfkwzb5lfvvrvp73hhlh2cgi";
    dependencies = mapFeatures features ([
      (crates."rand"."${deps."quickcheck"."0.4.1"."rand"}" deps)
    ]
      ++ (if features.quickcheck."0.4.1".env_logger or false then [ (crates.env_logger."${deps."quickcheck"."0.4.1".env_logger}" deps) ] else [])
      ++ (if features.quickcheck."0.4.1".log or false then [ (crates.log."${deps."quickcheck"."0.4.1".log}" deps) ] else []));
    features = mkFeatures (features."quickcheck"."0.4.1" or {});
  };
  features_.quickcheck."0.4.1" = deps: f: updateFeatures f (rec {
    env_logger."${deps.quickcheck."0.4.1".env_logger}".default = true;
    log."${deps.quickcheck."0.4.1".log}".default = true;
    quickcheck = fold recursiveUpdate {} [
      { "0.4.1".default = (f.quickcheck."0.4.1".default or true); }
      { "0.4.1".env_logger =
        (f.quickcheck."0.4.1".env_logger or false) ||
        (f.quickcheck."0.4.1".use_logging or false) ||
        (quickcheck."0.4.1"."use_logging" or false); }
      { "0.4.1".log =
        (f.quickcheck."0.4.1".log or false) ||
        (f.quickcheck."0.4.1".use_logging or false) ||
        (quickcheck."0.4.1"."use_logging" or false); }
      { "0.4.1".use_logging =
        (f.quickcheck."0.4.1".use_logging or false) ||
        (f.quickcheck."0.4.1".default or false) ||
        (quickcheck."0.4.1"."default" or false); }
    ];
    rand."${deps.quickcheck."0.4.1".rand}".default = true;
  }) [
    (features_.env_logger."${deps."quickcheck"."0.4.1"."env_logger"}" deps)
    (features_.log."${deps."quickcheck"."0.4.1"."log"}" deps)
    (features_.rand."${deps."quickcheck"."0.4.1"."rand"}" deps)
  ];


# end
# quote-0.3.15

  crates.quote."0.3.15" = deps: { features?(features_.quote."0.3.15" deps {}) }: buildRustCrate {
    crateName = "quote";
    version = "0.3.15";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "09il61jv4kd1360spaj46qwyl21fv1qz18fsv2jra8wdnlgl5jsg";
  };
  features_.quote."0.3.15" = deps: f: updateFeatures f (rec {
    quote."0.3.15".default = (f.quote."0.3.15".default or true);
  }) [];


# end
# quote-0.6.12

  crates.quote."0.6.12" = deps: { features?(features_.quote."0.6.12" deps {}) }: buildRustCrate {
    crateName = "quote";
    version = "0.6.12";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "1ckd2d2sy0hrwrqcr47dn0n3hyh7ygpc026l8xaycccyg27mihv9";
    dependencies = mapFeatures features ([
      (crates."proc_macro2"."${deps."quote"."0.6.12"."proc_macro2"}" deps)
    ]);
    features = mkFeatures (features."quote"."0.6.12" or {});
  };
  features_.quote."0.6.12" = deps: f: updateFeatures f (rec {
    proc_macro2 = fold recursiveUpdate {} [
      { "${deps.quote."0.6.12".proc_macro2}"."proc-macro" =
        (f.proc_macro2."${deps.quote."0.6.12".proc_macro2}"."proc-macro" or false) ||
        (quote."0.6.12"."proc-macro" or false) ||
        (f."quote"."0.6.12"."proc-macro" or false); }
      { "${deps.quote."0.6.12".proc_macro2}".default = (f.proc_macro2."${deps.quote."0.6.12".proc_macro2}".default or false); }
    ];
    quote = fold recursiveUpdate {} [
      { "0.6.12".default = (f.quote."0.6.12".default or true); }
      { "0.6.12".proc-macro =
        (f.quote."0.6.12".proc-macro or false) ||
        (f.quote."0.6.12".default or false) ||
        (quote."0.6.12"."default" or false); }
    ];
  }) [
    (features_.proc_macro2."${deps."quote"."0.6.12"."proc_macro2"}" deps)
  ];


# end
# r2d2-0.8.5

  crates.r2d2."0.8.5" = deps: { features?(features_.r2d2."0.8.5" deps {}) }: buildRustCrate {
    crateName = "r2d2";
    version = "0.8.5";
    authors = [ "Steven Fackler <sfackler@gmail.com>" ];
    edition = "2018";
    sha256 = "0fj6xkn9gkza81wlpysf773i8qd5368q27arb7mkyvv3iiw2pqyn";
    dependencies = mapFeatures features ([
      (crates."log"."${deps."r2d2"."0.8.5"."log"}" deps)
      (crates."parking_lot"."${deps."r2d2"."0.8.5"."parking_lot"}" deps)
      (crates."scheduled_thread_pool"."${deps."r2d2"."0.8.5"."scheduled_thread_pool"}" deps)
    ]);
  };
  features_.r2d2."0.8.5" = deps: f: updateFeatures f (rec {
    log."${deps.r2d2."0.8.5".log}".default = true;
    parking_lot."${deps.r2d2."0.8.5".parking_lot}".default = true;
    r2d2."0.8.5".default = (f.r2d2."0.8.5".default or true);
    scheduled_thread_pool."${deps.r2d2."0.8.5".scheduled_thread_pool}".default = true;
  }) [
    (features_.log."${deps."r2d2"."0.8.5"."log"}" deps)
    (features_.parking_lot."${deps."r2d2"."0.8.5"."parking_lot"}" deps)
    (features_.scheduled_thread_pool."${deps."r2d2"."0.8.5"."scheduled_thread_pool"}" deps)
  ];


# end
# rand-0.3.23

  crates.rand."0.3.23" = deps: { features?(features_.rand."0.3.23" deps {}) }: buildRustCrate {
    crateName = "rand";
    version = "0.3.23";
    authors = [ "The Rust Project Developers" ];
    sha256 = "118rairvv46npqqx7hmkf97kkimjrry9z31z4inxcv2vn0nj1s2g";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."rand"."0.3.23"."libc"}" deps)
      (crates."rand"."${deps."rand"."0.3.23"."rand"}" deps)
    ]);
    features = mkFeatures (features."rand"."0.3.23" or {});
  };
  features_.rand."0.3.23" = deps: f: updateFeatures f (rec {
    libc."${deps.rand."0.3.23".libc}".default = true;
    rand = fold recursiveUpdate {} [
      { "${deps.rand."0.3.23".rand}".default = true; }
      { "0.3.23".default = (f.rand."0.3.23".default or true); }
      { "0.3.23".i128_support =
        (f.rand."0.3.23".i128_support or false) ||
        (f.rand."0.3.23".nightly or false) ||
        (rand."0.3.23"."nightly" or false); }
    ];
  }) [
    (features_.libc."${deps."rand"."0.3.23"."libc"}" deps)
    (features_.rand."${deps."rand"."0.3.23"."rand"}" deps)
  ];


# end
# rand-0.4.6

  crates.rand."0.4.6" = deps: { features?(features_.rand."0.4.6" deps {}) }: buildRustCrate {
    crateName = "rand";
    version = "0.4.6";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0c3rmg5q7d6qdi7cbmg5py9alm70wd3xsg0mmcawrnl35qv37zfs";
    dependencies = (if abi == "sgx" then mapFeatures features ([
      (crates."rand_core"."${deps."rand"."0.4.6"."rand_core"}" deps)
      (crates."rdrand"."${deps."rand"."0.4.6"."rdrand"}" deps)
    ]) else [])
      ++ (if kernel == "fuchsia" then mapFeatures features ([
      (crates."fuchsia_cprng"."${deps."rand"."0.4.6"."fuchsia_cprng"}" deps)
    ]) else [])
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
    ]
      ++ (if features.rand."0.4.6".libc or false then [ (crates.libc."${deps."rand"."0.4.6".libc}" deps) ] else [])) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."rand"."0.4.6"."winapi"}" deps)
    ]) else []);
    features = mkFeatures (features."rand"."0.4.6" or {});
  };
  features_.rand."0.4.6" = deps: f: updateFeatures f (rec {
    fuchsia_cprng."${deps.rand."0.4.6".fuchsia_cprng}".default = true;
    libc."${deps.rand."0.4.6".libc}".default = true;
    rand = fold recursiveUpdate {} [
      { "0.4.6".default = (f.rand."0.4.6".default or true); }
      { "0.4.6".i128_support =
        (f.rand."0.4.6".i128_support or false) ||
        (f.rand."0.4.6".nightly or false) ||
        (rand."0.4.6"."nightly" or false); }
      { "0.4.6".libc =
        (f.rand."0.4.6".libc or false) ||
        (f.rand."0.4.6".std or false) ||
        (rand."0.4.6"."std" or false); }
      { "0.4.6".std =
        (f.rand."0.4.6".std or false) ||
        (f.rand."0.4.6".default or false) ||
        (rand."0.4.6"."default" or false); }
    ];
    rand_core."${deps.rand."0.4.6".rand_core}".default = (f.rand_core."${deps.rand."0.4.6".rand_core}".default or false);
    rdrand."${deps.rand."0.4.6".rdrand}".default = true;
    winapi = fold recursiveUpdate {} [
      { "${deps.rand."0.4.6".winapi}"."minwindef" = true; }
      { "${deps.rand."0.4.6".winapi}"."ntsecapi" = true; }
      { "${deps.rand."0.4.6".winapi}"."profileapi" = true; }
      { "${deps.rand."0.4.6".winapi}"."winnt" = true; }
      { "${deps.rand."0.4.6".winapi}".default = true; }
    ];
  }) [
    (features_.rand_core."${deps."rand"."0.4.6"."rand_core"}" deps)
    (features_.rdrand."${deps."rand"."0.4.6"."rdrand"}" deps)
    (features_.fuchsia_cprng."${deps."rand"."0.4.6"."fuchsia_cprng"}" deps)
    (features_.libc."${deps."rand"."0.4.6"."libc"}" deps)
    (features_.winapi."${deps."rand"."0.4.6"."winapi"}" deps)
  ];


# end
# rand-0.6.5

  crates.rand."0.6.5" = deps: { features?(features_.rand."0.6.5" deps {}) }: buildRustCrate {
    crateName = "rand";
    version = "0.6.5";
    authors = [ "The Rand Project Developers" "The Rust Project Developers" ];
    sha256 = "0zbck48159aj8zrwzf80sd9xxh96w4f4968nshwjpysjvflimvgb";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."rand_chacha"."${deps."rand"."0.6.5"."rand_chacha"}" deps)
      (crates."rand_core"."${deps."rand"."0.6.5"."rand_core"}" deps)
      (crates."rand_hc"."${deps."rand"."0.6.5"."rand_hc"}" deps)
      (crates."rand_isaac"."${deps."rand"."0.6.5"."rand_isaac"}" deps)
      (crates."rand_jitter"."${deps."rand"."0.6.5"."rand_jitter"}" deps)
      (crates."rand_pcg"."${deps."rand"."0.6.5"."rand_pcg"}" deps)
      (crates."rand_xorshift"."${deps."rand"."0.6.5"."rand_xorshift"}" deps)
    ]
      ++ (if features.rand."0.6.5".rand_os or false then [ (crates.rand_os."${deps."rand"."0.6.5".rand_os}" deps) ] else []))
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
      (crates."libc"."${deps."rand"."0.6.5"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."rand"."0.6.5"."winapi"}" deps)
    ]) else []);

    buildDependencies = mapFeatures features ([
      (crates."autocfg"."${deps."rand"."0.6.5"."autocfg"}" deps)
    ]);
    features = mkFeatures (features."rand"."0.6.5" or {});
  };
  features_.rand."0.6.5" = deps: f: updateFeatures f (rec {
    autocfg."${deps.rand."0.6.5".autocfg}".default = true;
    libc."${deps.rand."0.6.5".libc}".default = (f.libc."${deps.rand."0.6.5".libc}".default or false);
    rand = fold recursiveUpdate {} [
      { "0.6.5".alloc =
        (f.rand."0.6.5".alloc or false) ||
        (f.rand."0.6.5".std or false) ||
        (rand."0.6.5"."std" or false); }
      { "0.6.5".default = (f.rand."0.6.5".default or true); }
      { "0.6.5".packed_simd =
        (f.rand."0.6.5".packed_simd or false) ||
        (f.rand."0.6.5".simd_support or false) ||
        (rand."0.6.5"."simd_support" or false); }
      { "0.6.5".rand_os =
        (f.rand."0.6.5".rand_os or false) ||
        (f.rand."0.6.5".std or false) ||
        (rand."0.6.5"."std" or false); }
      { "0.6.5".simd_support =
        (f.rand."0.6.5".simd_support or false) ||
        (f.rand."0.6.5".nightly or false) ||
        (rand."0.6.5"."nightly" or false); }
      { "0.6.5".std =
        (f.rand."0.6.5".std or false) ||
        (f.rand."0.6.5".default or false) ||
        (rand."0.6.5"."default" or false); }
    ];
    rand_chacha."${deps.rand."0.6.5".rand_chacha}".default = true;
    rand_core = fold recursiveUpdate {} [
      { "${deps.rand."0.6.5".rand_core}"."alloc" =
        (f.rand_core."${deps.rand."0.6.5".rand_core}"."alloc" or false) ||
        (rand."0.6.5"."alloc" or false) ||
        (f."rand"."0.6.5"."alloc" or false); }
      { "${deps.rand."0.6.5".rand_core}"."serde1" =
        (f.rand_core."${deps.rand."0.6.5".rand_core}"."serde1" or false) ||
        (rand."0.6.5"."serde1" or false) ||
        (f."rand"."0.6.5"."serde1" or false); }
      { "${deps.rand."0.6.5".rand_core}"."std" =
        (f.rand_core."${deps.rand."0.6.5".rand_core}"."std" or false) ||
        (rand."0.6.5"."std" or false) ||
        (f."rand"."0.6.5"."std" or false); }
      { "${deps.rand."0.6.5".rand_core}".default = true; }
    ];
    rand_hc."${deps.rand."0.6.5".rand_hc}".default = true;
    rand_isaac = fold recursiveUpdate {} [
      { "${deps.rand."0.6.5".rand_isaac}"."serde1" =
        (f.rand_isaac."${deps.rand."0.6.5".rand_isaac}"."serde1" or false) ||
        (rand."0.6.5"."serde1" or false) ||
        (f."rand"."0.6.5"."serde1" or false); }
      { "${deps.rand."0.6.5".rand_isaac}".default = true; }
    ];
    rand_jitter = fold recursiveUpdate {} [
      { "${deps.rand."0.6.5".rand_jitter}"."std" =
        (f.rand_jitter."${deps.rand."0.6.5".rand_jitter}"."std" or false) ||
        (rand."0.6.5"."std" or false) ||
        (f."rand"."0.6.5"."std" or false); }
      { "${deps.rand."0.6.5".rand_jitter}".default = true; }
    ];
    rand_os = fold recursiveUpdate {} [
      { "${deps.rand."0.6.5".rand_os}"."stdweb" =
        (f.rand_os."${deps.rand."0.6.5".rand_os}"."stdweb" or false) ||
        (rand."0.6.5"."stdweb" or false) ||
        (f."rand"."0.6.5"."stdweb" or false); }
      { "${deps.rand."0.6.5".rand_os}"."wasm-bindgen" =
        (f.rand_os."${deps.rand."0.6.5".rand_os}"."wasm-bindgen" or false) ||
        (rand."0.6.5"."wasm-bindgen" or false) ||
        (f."rand"."0.6.5"."wasm-bindgen" or false); }
      { "${deps.rand."0.6.5".rand_os}".default = true; }
    ];
    rand_pcg."${deps.rand."0.6.5".rand_pcg}".default = true;
    rand_xorshift = fold recursiveUpdate {} [
      { "${deps.rand."0.6.5".rand_xorshift}"."serde1" =
        (f.rand_xorshift."${deps.rand."0.6.5".rand_xorshift}"."serde1" or false) ||
        (rand."0.6.5"."serde1" or false) ||
        (f."rand"."0.6.5"."serde1" or false); }
      { "${deps.rand."0.6.5".rand_xorshift}".default = true; }
    ];
    winapi = fold recursiveUpdate {} [
      { "${deps.rand."0.6.5".winapi}"."minwindef" = true; }
      { "${deps.rand."0.6.5".winapi}"."ntsecapi" = true; }
      { "${deps.rand."0.6.5".winapi}"."profileapi" = true; }
      { "${deps.rand."0.6.5".winapi}"."winnt" = true; }
      { "${deps.rand."0.6.5".winapi}".default = true; }
    ];
  }) [
    (features_.rand_chacha."${deps."rand"."0.6.5"."rand_chacha"}" deps)
    (features_.rand_core."${deps."rand"."0.6.5"."rand_core"}" deps)
    (features_.rand_hc."${deps."rand"."0.6.5"."rand_hc"}" deps)
    (features_.rand_isaac."${deps."rand"."0.6.5"."rand_isaac"}" deps)
    (features_.rand_jitter."${deps."rand"."0.6.5"."rand_jitter"}" deps)
    (features_.rand_os."${deps."rand"."0.6.5"."rand_os"}" deps)
    (features_.rand_pcg."${deps."rand"."0.6.5"."rand_pcg"}" deps)
    (features_.rand_xorshift."${deps."rand"."0.6.5"."rand_xorshift"}" deps)
    (features_.autocfg."${deps."rand"."0.6.5"."autocfg"}" deps)
    (features_.libc."${deps."rand"."0.6.5"."libc"}" deps)
    (features_.winapi."${deps."rand"."0.6.5"."winapi"}" deps)
  ];


# end
# rand_chacha-0.1.1

  crates.rand_chacha."0.1.1" = deps: { features?(features_.rand_chacha."0.1.1" deps {}) }: buildRustCrate {
    crateName = "rand_chacha";
    version = "0.1.1";
    authors = [ "The Rand Project Developers" "The Rust Project Developers" ];
    sha256 = "0xnxm4mjd7wjnh18zxc1yickw58axbycp35ciraplqdfwn1gffwi";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_chacha"."0.1.1"."rand_core"}" deps)
    ]);

    buildDependencies = mapFeatures features ([
      (crates."autocfg"."${deps."rand_chacha"."0.1.1"."autocfg"}" deps)
    ]);
  };
  features_.rand_chacha."0.1.1" = deps: f: updateFeatures f (rec {
    autocfg."${deps.rand_chacha."0.1.1".autocfg}".default = true;
    rand_chacha."0.1.1".default = (f.rand_chacha."0.1.1".default or true);
    rand_core."${deps.rand_chacha."0.1.1".rand_core}".default = (f.rand_core."${deps.rand_chacha."0.1.1".rand_core}".default or false);
  }) [
    (features_.rand_core."${deps."rand_chacha"."0.1.1"."rand_core"}" deps)
    (features_.autocfg."${deps."rand_chacha"."0.1.1"."autocfg"}" deps)
  ];


# end
# rand_core-0.3.1

  crates.rand_core."0.3.1" = deps: { features?(features_.rand_core."0.3.1" deps {}) }: buildRustCrate {
    crateName = "rand_core";
    version = "0.3.1";
    authors = [ "The Rand Project Developers" "The Rust Project Developers" ];
    sha256 = "0q0ssgpj9x5a6fda83nhmfydy7a6c0wvxm0jhncsmjx8qp8gw91m";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_core"."0.3.1"."rand_core"}" deps)
    ]);
    features = mkFeatures (features."rand_core"."0.3.1" or {});
  };
  features_.rand_core."0.3.1" = deps: f: updateFeatures f (rec {
    rand_core = fold recursiveUpdate {} [
      { "${deps.rand_core."0.3.1".rand_core}"."alloc" =
        (f.rand_core."${deps.rand_core."0.3.1".rand_core}"."alloc" or false) ||
        (rand_core."0.3.1"."alloc" or false) ||
        (f."rand_core"."0.3.1"."alloc" or false); }
      { "${deps.rand_core."0.3.1".rand_core}"."serde1" =
        (f.rand_core."${deps.rand_core."0.3.1".rand_core}"."serde1" or false) ||
        (rand_core."0.3.1"."serde1" or false) ||
        (f."rand_core"."0.3.1"."serde1" or false); }
      { "${deps.rand_core."0.3.1".rand_core}"."std" =
        (f.rand_core."${deps.rand_core."0.3.1".rand_core}"."std" or false) ||
        (rand_core."0.3.1"."std" or false) ||
        (f."rand_core"."0.3.1"."std" or false); }
      { "${deps.rand_core."0.3.1".rand_core}".default = true; }
      { "0.3.1".default = (f.rand_core."0.3.1".default or true); }
      { "0.3.1".std =
        (f.rand_core."0.3.1".std or false) ||
        (f.rand_core."0.3.1".default or false) ||
        (rand_core."0.3.1"."default" or false); }
    ];
  }) [
    (features_.rand_core."${deps."rand_core"."0.3.1"."rand_core"}" deps)
  ];


# end
# rand_core-0.4.0

  crates.rand_core."0.4.0" = deps: { features?(features_.rand_core."0.4.0" deps {}) }: buildRustCrate {
    crateName = "rand_core";
    version = "0.4.0";
    authors = [ "The Rand Project Developers" "The Rust Project Developers" ];
    sha256 = "0wb5iwhffibj0pnpznhv1g3i7h1fnhz64s3nz74fz6vsm3q6q3br";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."rand_core"."0.4.0" or {});
  };
  features_.rand_core."0.4.0" = deps: f: updateFeatures f (rec {
    rand_core = fold recursiveUpdate {} [
      { "0.4.0".alloc =
        (f.rand_core."0.4.0".alloc or false) ||
        (f.rand_core."0.4.0".std or false) ||
        (rand_core."0.4.0"."std" or false); }
      { "0.4.0".default = (f.rand_core."0.4.0".default or true); }
      { "0.4.0".serde =
        (f.rand_core."0.4.0".serde or false) ||
        (f.rand_core."0.4.0".serde1 or false) ||
        (rand_core."0.4.0"."serde1" or false); }
      { "0.4.0".serde_derive =
        (f.rand_core."0.4.0".serde_derive or false) ||
        (f.rand_core."0.4.0".serde1 or false) ||
        (rand_core."0.4.0"."serde1" or false); }
    ];
  }) [];


# end
# rand_hc-0.1.0

  crates.rand_hc."0.1.0" = deps: { features?(features_.rand_hc."0.1.0" deps {}) }: buildRustCrate {
    crateName = "rand_hc";
    version = "0.1.0";
    authors = [ "The Rand Project Developers" ];
    sha256 = "05agb75j87yp7y1zk8yf7bpm66hc0673r3dlypn0kazynr6fdgkz";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_hc"."0.1.0"."rand_core"}" deps)
    ]);
  };
  features_.rand_hc."0.1.0" = deps: f: updateFeatures f (rec {
    rand_core."${deps.rand_hc."0.1.0".rand_core}".default = (f.rand_core."${deps.rand_hc."0.1.0".rand_core}".default or false);
    rand_hc."0.1.0".default = (f.rand_hc."0.1.0".default or true);
  }) [
    (features_.rand_core."${deps."rand_hc"."0.1.0"."rand_core"}" deps)
  ];


# end
# rand_isaac-0.1.1

  crates.rand_isaac."0.1.1" = deps: { features?(features_.rand_isaac."0.1.1" deps {}) }: buildRustCrate {
    crateName = "rand_isaac";
    version = "0.1.1";
    authors = [ "The Rand Project Developers" "The Rust Project Developers" ];
    sha256 = "10hhdh5b5sa03s6b63y9bafm956jwilx41s71jbrzl63ccx8lxdq";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_isaac"."0.1.1"."rand_core"}" deps)
    ]);
    features = mkFeatures (features."rand_isaac"."0.1.1" or {});
  };
  features_.rand_isaac."0.1.1" = deps: f: updateFeatures f (rec {
    rand_core = fold recursiveUpdate {} [
      { "${deps.rand_isaac."0.1.1".rand_core}"."serde1" =
        (f.rand_core."${deps.rand_isaac."0.1.1".rand_core}"."serde1" or false) ||
        (rand_isaac."0.1.1"."serde1" or false) ||
        (f."rand_isaac"."0.1.1"."serde1" or false); }
      { "${deps.rand_isaac."0.1.1".rand_core}".default = (f.rand_core."${deps.rand_isaac."0.1.1".rand_core}".default or false); }
    ];
    rand_isaac = fold recursiveUpdate {} [
      { "0.1.1".default = (f.rand_isaac."0.1.1".default or true); }
      { "0.1.1".serde =
        (f.rand_isaac."0.1.1".serde or false) ||
        (f.rand_isaac."0.1.1".serde1 or false) ||
        (rand_isaac."0.1.1"."serde1" or false); }
      { "0.1.1".serde_derive =
        (f.rand_isaac."0.1.1".serde_derive or false) ||
        (f.rand_isaac."0.1.1".serde1 or false) ||
        (rand_isaac."0.1.1"."serde1" or false); }
    ];
  }) [
    (features_.rand_core."${deps."rand_isaac"."0.1.1"."rand_core"}" deps)
  ];


# end
# rand_jitter-0.1.4

  crates.rand_jitter."0.1.4" = deps: { features?(features_.rand_jitter."0.1.4" deps {}) }: buildRustCrate {
    crateName = "rand_jitter";
    version = "0.1.4";
    authors = [ "The Rand Project Developers" ];
    sha256 = "13nr4h042ab9l7qcv47bxrxw3gkf2pc3cni6c9pyi4nxla0mm7b6";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_jitter"."0.1.4"."rand_core"}" deps)
    ])
      ++ (if kernel == "darwin" || kernel == "ios" then mapFeatures features ([
      (crates."libc"."${deps."rand_jitter"."0.1.4"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."rand_jitter"."0.1.4"."winapi"}" deps)
    ]) else []);
    features = mkFeatures (features."rand_jitter"."0.1.4" or {});
  };
  features_.rand_jitter."0.1.4" = deps: f: updateFeatures f (rec {
    libc."${deps.rand_jitter."0.1.4".libc}".default = true;
    rand_core = fold recursiveUpdate {} [
      { "${deps.rand_jitter."0.1.4".rand_core}"."std" =
        (f.rand_core."${deps.rand_jitter."0.1.4".rand_core}"."std" or false) ||
        (rand_jitter."0.1.4"."std" or false) ||
        (f."rand_jitter"."0.1.4"."std" or false); }
      { "${deps.rand_jitter."0.1.4".rand_core}".default = true; }
    ];
    rand_jitter."0.1.4".default = (f.rand_jitter."0.1.4".default or true);
    winapi = fold recursiveUpdate {} [
      { "${deps.rand_jitter."0.1.4".winapi}"."profileapi" = true; }
      { "${deps.rand_jitter."0.1.4".winapi}".default = true; }
    ];
  }) [
    (features_.rand_core."${deps."rand_jitter"."0.1.4"."rand_core"}" deps)
    (features_.libc."${deps."rand_jitter"."0.1.4"."libc"}" deps)
    (features_.winapi."${deps."rand_jitter"."0.1.4"."winapi"}" deps)
  ];


# end
# rand_os-0.1.3

  crates.rand_os."0.1.3" = deps: { features?(features_.rand_os."0.1.3" deps {}) }: buildRustCrate {
    crateName = "rand_os";
    version = "0.1.3";
    authors = [ "The Rand Project Developers" ];
    sha256 = "0ywwspizgs9g8vzn6m5ix9yg36n15119d6n792h7mk4r5vs0ww4j";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_os"."0.1.3"."rand_core"}" deps)
    ])
      ++ (if abi == "sgx" then mapFeatures features ([
      (crates."rdrand"."${deps."rand_os"."0.1.3"."rdrand"}" deps)
    ]) else [])
      ++ (if kernel == "cloudabi" then mapFeatures features ([
      (crates."cloudabi"."${deps."rand_os"."0.1.3"."cloudabi"}" deps)
    ]) else [])
      ++ (if kernel == "fuchsia" then mapFeatures features ([
      (crates."fuchsia_cprng"."${deps."rand_os"."0.1.3"."fuchsia_cprng"}" deps)
    ]) else [])
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
      (crates."libc"."${deps."rand_os"."0.1.3"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."rand_os"."0.1.3"."winapi"}" deps)
    ]) else [])
      ++ (if kernel == "wasm32-unknown-unknown" then mapFeatures features ([
]) else []);
  };
  features_.rand_os."0.1.3" = deps: f: updateFeatures f (rec {
    cloudabi."${deps.rand_os."0.1.3".cloudabi}".default = true;
    fuchsia_cprng."${deps.rand_os."0.1.3".fuchsia_cprng}".default = true;
    libc."${deps.rand_os."0.1.3".libc}".default = true;
    rand_core = fold recursiveUpdate {} [
      { "${deps.rand_os."0.1.3".rand_core}"."std" = true; }
      { "${deps.rand_os."0.1.3".rand_core}".default = true; }
    ];
    rand_os."0.1.3".default = (f.rand_os."0.1.3".default or true);
    rdrand."${deps.rand_os."0.1.3".rdrand}".default = true;
    winapi = fold recursiveUpdate {} [
      { "${deps.rand_os."0.1.3".winapi}"."minwindef" = true; }
      { "${deps.rand_os."0.1.3".winapi}"."ntsecapi" = true; }
      { "${deps.rand_os."0.1.3".winapi}"."winnt" = true; }
      { "${deps.rand_os."0.1.3".winapi}".default = true; }
    ];
  }) [
    (features_.rand_core."${deps."rand_os"."0.1.3"."rand_core"}" deps)
    (features_.rdrand."${deps."rand_os"."0.1.3"."rdrand"}" deps)
    (features_.cloudabi."${deps."rand_os"."0.1.3"."cloudabi"}" deps)
    (features_.fuchsia_cprng."${deps."rand_os"."0.1.3"."fuchsia_cprng"}" deps)
    (features_.libc."${deps."rand_os"."0.1.3"."libc"}" deps)
    (features_.winapi."${deps."rand_os"."0.1.3"."winapi"}" deps)
  ];


# end
# rand_pcg-0.1.2

  crates.rand_pcg."0.1.2" = deps: { features?(features_.rand_pcg."0.1.2" deps {}) }: buildRustCrate {
    crateName = "rand_pcg";
    version = "0.1.2";
    authors = [ "The Rand Project Developers" ];
    sha256 = "04qgi2ai2z42li5h4aawvxbpnlqyjfnipz9d6k73mdnl6p1xq938";
    build = "build.rs";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_pcg"."0.1.2"."rand_core"}" deps)
    ]);

    buildDependencies = mapFeatures features ([
      (crates."autocfg"."${deps."rand_pcg"."0.1.2"."autocfg"}" deps)
    ]);
    features = mkFeatures (features."rand_pcg"."0.1.2" or {});
  };
  features_.rand_pcg."0.1.2" = deps: f: updateFeatures f (rec {
    autocfg."${deps.rand_pcg."0.1.2".autocfg}".default = true;
    rand_core."${deps.rand_pcg."0.1.2".rand_core}".default = true;
    rand_pcg = fold recursiveUpdate {} [
      { "0.1.2".default = (f.rand_pcg."0.1.2".default or true); }
      { "0.1.2".serde =
        (f.rand_pcg."0.1.2".serde or false) ||
        (f.rand_pcg."0.1.2".serde1 or false) ||
        (rand_pcg."0.1.2"."serde1" or false); }
      { "0.1.2".serde_derive =
        (f.rand_pcg."0.1.2".serde_derive or false) ||
        (f.rand_pcg."0.1.2".serde1 or false) ||
        (rand_pcg."0.1.2"."serde1" or false); }
    ];
  }) [
    (features_.rand_core."${deps."rand_pcg"."0.1.2"."rand_core"}" deps)
    (features_.autocfg."${deps."rand_pcg"."0.1.2"."autocfg"}" deps)
  ];


# end
# rand_xorshift-0.1.1

  crates.rand_xorshift."0.1.1" = deps: { features?(features_.rand_xorshift."0.1.1" deps {}) }: buildRustCrate {
    crateName = "rand_xorshift";
    version = "0.1.1";
    authors = [ "The Rand Project Developers" "The Rust Project Developers" ];
    sha256 = "0v365c4h4lzxwz5k5kp9m0661s0sss7ylv74if0xb4svis9sswnn";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rand_xorshift"."0.1.1"."rand_core"}" deps)
    ]);
    features = mkFeatures (features."rand_xorshift"."0.1.1" or {});
  };
  features_.rand_xorshift."0.1.1" = deps: f: updateFeatures f (rec {
    rand_core."${deps.rand_xorshift."0.1.1".rand_core}".default = (f.rand_core."${deps.rand_xorshift."0.1.1".rand_core}".default or false);
    rand_xorshift = fold recursiveUpdate {} [
      { "0.1.1".default = (f.rand_xorshift."0.1.1".default or true); }
      { "0.1.1".serde =
        (f.rand_xorshift."0.1.1".serde or false) ||
        (f.rand_xorshift."0.1.1".serde1 or false) ||
        (rand_xorshift."0.1.1"."serde1" or false); }
      { "0.1.1".serde_derive =
        (f.rand_xorshift."0.1.1".serde_derive or false) ||
        (f.rand_xorshift."0.1.1".serde1 or false) ||
        (rand_xorshift."0.1.1"."serde1" or false); }
    ];
  }) [
    (features_.rand_core."${deps."rand_xorshift"."0.1.1"."rand_core"}" deps)
  ];


# end
# rdrand-0.4.0

  crates.rdrand."0.4.0" = deps: { features?(features_.rdrand."0.4.0" deps {}) }: buildRustCrate {
    crateName = "rdrand";
    version = "0.4.0";
    authors = [ "Simonas Kazlauskas <rdrand@kazlauskas.me>" ];
    sha256 = "15hrcasn0v876wpkwab1dwbk9kvqwrb3iv4y4dibb6yxnfvzwajk";
    dependencies = mapFeatures features ([
      (crates."rand_core"."${deps."rdrand"."0.4.0"."rand_core"}" deps)
    ]);
    features = mkFeatures (features."rdrand"."0.4.0" or {});
  };
  features_.rdrand."0.4.0" = deps: f: updateFeatures f (rec {
    rand_core."${deps.rdrand."0.4.0".rand_core}".default = (f.rand_core."${deps.rdrand."0.4.0".rand_core}".default or false);
    rdrand = fold recursiveUpdate {} [
      { "0.4.0".default = (f.rdrand."0.4.0".default or true); }
      { "0.4.0".std =
        (f.rdrand."0.4.0".std or false) ||
        (f.rdrand."0.4.0".default or false) ||
        (rdrand."0.4.0"."default" or false); }
    ];
  }) [
    (features_.rand_core."${deps."rdrand"."0.4.0"."rand_core"}" deps)
  ];


# end
# redox_syscall-0.1.54

  crates.redox_syscall."0.1.54" = deps: { features?(features_.redox_syscall."0.1.54" deps {}) }: buildRustCrate {
    crateName = "redox_syscall";
    version = "0.1.54";
    authors = [ "Jeremy Soller <jackpot51@gmail.com>" ];
    sha256 = "1ndcp7brnvii87ndcd34fk846498r07iznphkslcy0shic9cp4rr";
    libName = "syscall";
  };
  features_.redox_syscall."0.1.54" = deps: f: updateFeatures f (rec {
    redox_syscall."0.1.54".default = (f.redox_syscall."0.1.54".default or true);
  }) [];


# end
# redox_termios-0.1.1

  crates.redox_termios."0.1.1" = deps: { features?(features_.redox_termios."0.1.1" deps {}) }: buildRustCrate {
    crateName = "redox_termios";
    version = "0.1.1";
    authors = [ "Jeremy Soller <jackpot51@gmail.com>" ];
    sha256 = "04s6yyzjca552hdaqlvqhp3vw0zqbc304md5czyd3axh56iry8wh";
    libPath = "src/lib.rs";
    dependencies = mapFeatures features ([
      (crates."redox_syscall"."${deps."redox_termios"."0.1.1"."redox_syscall"}" deps)
    ]);
  };
  features_.redox_termios."0.1.1" = deps: f: updateFeatures f (rec {
    redox_syscall."${deps.redox_termios."0.1.1".redox_syscall}".default = true;
    redox_termios."0.1.1".default = (f.redox_termios."0.1.1".default or true);
  }) [
    (features_.redox_syscall."${deps."redox_termios"."0.1.1"."redox_syscall"}" deps)
  ];


# end
# regex-0.1.80

  crates.regex."0.1.80" = deps: { features?(features_.regex."0.1.80" deps {}) }: buildRustCrate {
    crateName = "regex";
    version = "0.1.80";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0y4s8ghhx6sgzb35irwivm3w0l2hhqhmdcd2px9hirqnkagal9l6";
    dependencies = mapFeatures features ([
      (crates."aho_corasick"."${deps."regex"."0.1.80"."aho_corasick"}" deps)
      (crates."memchr"."${deps."regex"."0.1.80"."memchr"}" deps)
      (crates."regex_syntax"."${deps."regex"."0.1.80"."regex_syntax"}" deps)
      (crates."thread_local"."${deps."regex"."0.1.80"."thread_local"}" deps)
      (crates."utf8_ranges"."${deps."regex"."0.1.80"."utf8_ranges"}" deps)
    ]);
    features = mkFeatures (features."regex"."0.1.80" or {});
  };
  features_.regex."0.1.80" = deps: f: updateFeatures f (rec {
    aho_corasick."${deps.regex."0.1.80".aho_corasick}".default = true;
    memchr."${deps.regex."0.1.80".memchr}".default = true;
    regex = fold recursiveUpdate {} [
      { "0.1.80".default = (f.regex."0.1.80".default or true); }
      { "0.1.80".simd =
        (f.regex."0.1.80".simd or false) ||
        (f.regex."0.1.80".simd-accel or false) ||
        (regex."0.1.80"."simd-accel" or false); }
    ];
    regex_syntax."${deps.regex."0.1.80".regex_syntax}".default = true;
    thread_local."${deps.regex."0.1.80".thread_local}".default = true;
    utf8_ranges."${deps.regex."0.1.80".utf8_ranges}".default = true;
  }) [
    (features_.aho_corasick."${deps."regex"."0.1.80"."aho_corasick"}" deps)
    (features_.memchr."${deps."regex"."0.1.80"."memchr"}" deps)
    (features_.regex_syntax."${deps."regex"."0.1.80"."regex_syntax"}" deps)
    (features_.thread_local."${deps."regex"."0.1.80"."thread_local"}" deps)
    (features_.utf8_ranges."${deps."regex"."0.1.80"."utf8_ranges"}" deps)
  ];


# end
# regex-0.2.11

  crates.regex."0.2.11" = deps: { features?(features_.regex."0.2.11" deps {}) }: buildRustCrate {
    crateName = "regex";
    version = "0.2.11";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0r50cymxdqp0fv1dxd22mjr6y32q450nwacd279p9s7lh0cafijj";
    dependencies = mapFeatures features ([
      (crates."aho_corasick"."${deps."regex"."0.2.11"."aho_corasick"}" deps)
      (crates."memchr"."${deps."regex"."0.2.11"."memchr"}" deps)
      (crates."regex_syntax"."${deps."regex"."0.2.11"."regex_syntax"}" deps)
      (crates."thread_local"."${deps."regex"."0.2.11"."thread_local"}" deps)
      (crates."utf8_ranges"."${deps."regex"."0.2.11"."utf8_ranges"}" deps)
    ]);
    features = mkFeatures (features."regex"."0.2.11" or {});
  };
  features_.regex."0.2.11" = deps: f: updateFeatures f (rec {
    aho_corasick."${deps.regex."0.2.11".aho_corasick}".default = true;
    memchr."${deps.regex."0.2.11".memchr}".default = true;
    regex = fold recursiveUpdate {} [
      { "0.2.11".default = (f.regex."0.2.11".default or true); }
      { "0.2.11".pattern =
        (f.regex."0.2.11".pattern or false) ||
        (f.regex."0.2.11".unstable or false) ||
        (regex."0.2.11"."unstable" or false); }
    ];
    regex_syntax."${deps.regex."0.2.11".regex_syntax}".default = true;
    thread_local."${deps.regex."0.2.11".thread_local}".default = true;
    utf8_ranges."${deps.regex."0.2.11".utf8_ranges}".default = true;
  }) [
    (features_.aho_corasick."${deps."regex"."0.2.11"."aho_corasick"}" deps)
    (features_.memchr."${deps."regex"."0.2.11"."memchr"}" deps)
    (features_.regex_syntax."${deps."regex"."0.2.11"."regex_syntax"}" deps)
    (features_.thread_local."${deps."regex"."0.2.11"."thread_local"}" deps)
    (features_.utf8_ranges."${deps."regex"."0.2.11"."utf8_ranges"}" deps)
  ];


# end
# regex-syntax-0.3.9

  crates.regex_syntax."0.3.9" = deps: { features?(features_.regex_syntax."0.3.9" deps {}) }: buildRustCrate {
    crateName = "regex-syntax";
    version = "0.3.9";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1mzhphkbwppwd1zam2jkgjk550cqgf6506i87bw2yzrvcsraiw7m";
  };
  features_.regex_syntax."0.3.9" = deps: f: updateFeatures f (rec {
    regex_syntax."0.3.9".default = (f.regex_syntax."0.3.9".default or true);
  }) [];


# end
# regex-syntax-0.5.6

  crates.regex_syntax."0.5.6" = deps: { features?(features_.regex_syntax."0.5.6" deps {}) }: buildRustCrate {
    crateName = "regex-syntax";
    version = "0.5.6";
    authors = [ "The Rust Project Developers" ];
    sha256 = "10vf3r34bgjnbrnqd5aszn35bjvm8insw498l1vjy8zx5yms3427";
    dependencies = mapFeatures features ([
      (crates."ucd_util"."${deps."regex_syntax"."0.5.6"."ucd_util"}" deps)
    ]);
  };
  features_.regex_syntax."0.5.6" = deps: f: updateFeatures f (rec {
    regex_syntax."0.5.6".default = (f.regex_syntax."0.5.6".default or true);
    ucd_util."${deps.regex_syntax."0.5.6".ucd_util}".default = true;
  }) [
    (features_.ucd_util."${deps."regex_syntax"."0.5.6"."ucd_util"}" deps)
  ];


# end
# remove_dir_all-0.5.2

  crates.remove_dir_all."0.5.2" = deps: { features?(features_.remove_dir_all."0.5.2" deps {}) }: buildRustCrate {
    crateName = "remove_dir_all";
    version = "0.5.2";
    authors = [ "Aaronepower <theaaronepower@gmail.com>" ];
    sha256 = "04sxg2ppvxiljc2i13bwvpbi540rf9d2a89cq0wmqf9pjvr3a1wm";
    dependencies = (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."remove_dir_all"."0.5.2"."winapi"}" deps)
    ]) else []);
  };
  features_.remove_dir_all."0.5.2" = deps: f: updateFeatures f (rec {
    remove_dir_all."0.5.2".default = (f.remove_dir_all."0.5.2".default or true);
    winapi = fold recursiveUpdate {} [
      { "${deps.remove_dir_all."0.5.2".winapi}"."errhandlingapi" = true; }
      { "${deps.remove_dir_all."0.5.2".winapi}"."fileapi" = true; }
      { "${deps.remove_dir_all."0.5.2".winapi}"."std" = true; }
      { "${deps.remove_dir_all."0.5.2".winapi}"."winbase" = true; }
      { "${deps.remove_dir_all."0.5.2".winapi}"."winerror" = true; }
      { "${deps.remove_dir_all."0.5.2".winapi}".default = true; }
    ];
  }) [
    (features_.winapi."${deps."remove_dir_all"."0.5.2"."winapi"}" deps)
  ];


# end
# rustc-demangle-0.1.15

  crates.rustc_demangle."0.1.15" = deps: { features?(features_.rustc_demangle."0.1.15" deps {}) }: buildRustCrate {
    crateName = "rustc-demangle";
    version = "0.1.15";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "04rgsfzhz4k9s56vkczsdbvmvg9409xp0nw4cy99lb2i0aa0255s";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."rustc_demangle"."0.1.15" or {});
  };
  features_.rustc_demangle."0.1.15" = deps: f: updateFeatures f (rec {
    rustc_demangle = fold recursiveUpdate {} [
      { "0.1.15".compiler_builtins =
        (f.rustc_demangle."0.1.15".compiler_builtins or false) ||
        (f.rustc_demangle."0.1.15".rustc-dep-of-std or false) ||
        (rustc_demangle."0.1.15"."rustc-dep-of-std" or false); }
      { "0.1.15".core =
        (f.rustc_demangle."0.1.15".core or false) ||
        (f.rustc_demangle."0.1.15".rustc-dep-of-std or false) ||
        (rustc_demangle."0.1.15"."rustc-dep-of-std" or false); }
      { "0.1.15".default = (f.rustc_demangle."0.1.15".default or true); }
    ];
  }) [];


# end
# rustc_version-0.2.3

  crates.rustc_version."0.2.3" = deps: { features?(features_.rustc_version."0.2.3" deps {}) }: buildRustCrate {
    crateName = "rustc_version";
    version = "0.2.3";
    authors = [ "Marvin Löbel <loebel.marvin@gmail.com>" ];
    sha256 = "0rgwzbgs3i9fqjm1p4ra3n7frafmpwl29c8lw85kv1rxn7n2zaa7";
    dependencies = mapFeatures features ([
      (crates."semver"."${deps."rustc_version"."0.2.3"."semver"}" deps)
    ]);
  };
  features_.rustc_version."0.2.3" = deps: f: updateFeatures f (rec {
    rustc_version."0.2.3".default = (f.rustc_version."0.2.3".default or true);
    semver."${deps.rustc_version."0.2.3".semver}".default = true;
  }) [
    (features_.semver."${deps."rustc_version"."0.2.3"."semver"}" deps)
  ];


# end
# ryu-0.2.8

  crates.ryu."0.2.8" = deps: { features?(features_.ryu."0.2.8" deps {}) }: buildRustCrate {
    crateName = "ryu";
    version = "0.2.8";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "1qd0ni13w19a97y51vm31biyh2pvz8j9gi78rn5in912mi04xcnk";
    build = "build.rs";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."ryu"."0.2.8" or {});
  };
  features_.ryu."0.2.8" = deps: f: updateFeatures f (rec {
    ryu."0.2.8".default = (f.ryu."0.2.8".default or true);
  }) [];


# end
# scheduled-thread-pool-0.2.1

  crates.scheduled_thread_pool."0.2.1" = deps: { features?(features_.scheduled_thread_pool."0.2.1" deps {}) }: buildRustCrate {
    crateName = "scheduled-thread-pool";
    version = "0.2.1";
    authors = [ "Steven Fackler <sfackler@gmail.com>" ];
    edition = "2018";
    sha256 = "0mapxd7qc2g39prbibgban8gzw44rqil63l3d54kgz88x15imm6p";
    dependencies = mapFeatures features ([
      (crates."parking_lot"."${deps."scheduled_thread_pool"."0.2.1"."parking_lot"}" deps)
    ]);
  };
  features_.scheduled_thread_pool."0.2.1" = deps: f: updateFeatures f (rec {
    parking_lot."${deps.scheduled_thread_pool."0.2.1".parking_lot}".default = true;
    scheduled_thread_pool."0.2.1".default = (f.scheduled_thread_pool."0.2.1".default or true);
  }) [
    (features_.parking_lot."${deps."scheduled_thread_pool"."0.2.1"."parking_lot"}" deps)
  ];


# end
# scopeguard-1.0.0

  crates.scopeguard."1.0.0" = deps: { features?(features_.scopeguard."1.0.0" deps {}) }: buildRustCrate {
    crateName = "scopeguard";
    version = "1.0.0";
    authors = [ "bluss" ];
    sha256 = "15vrix0jx3i4naqnjswddzn4m036krrv71a8vkh3b1zq4hxmrb0q";
    features = mkFeatures (features."scopeguard"."1.0.0" or {});
  };
  features_.scopeguard."1.0.0" = deps: f: updateFeatures f (rec {
    scopeguard = fold recursiveUpdate {} [
      { "1.0.0".default = (f.scopeguard."1.0.0".default or true); }
      { "1.0.0".use_std =
        (f.scopeguard."1.0.0".use_std or false) ||
        (f.scopeguard."1.0.0".default or false) ||
        (scopeguard."1.0.0"."default" or false); }
    ];
  }) [];


# end
# semver-0.9.0

  crates.semver."0.9.0" = deps: { features?(features_.semver."0.9.0" deps {}) }: buildRustCrate {
    crateName = "semver";
    version = "0.9.0";
    authors = [ "Steve Klabnik <steve@steveklabnik.com>" "The Rust Project Developers" ];
    sha256 = "0azak2lb2wc36s3x15az886kck7rpnksrw14lalm157rg9sc9z63";
    dependencies = mapFeatures features ([
      (crates."semver_parser"."${deps."semver"."0.9.0"."semver_parser"}" deps)
    ]);
    features = mkFeatures (features."semver"."0.9.0" or {});
  };
  features_.semver."0.9.0" = deps: f: updateFeatures f (rec {
    semver = fold recursiveUpdate {} [
      { "0.9.0".default = (f.semver."0.9.0".default or true); }
      { "0.9.0".serde =
        (f.semver."0.9.0".serde or false) ||
        (f.semver."0.9.0".ci or false) ||
        (semver."0.9.0"."ci" or false); }
    ];
    semver_parser."${deps.semver."0.9.0".semver_parser}".default = true;
  }) [
    (features_.semver_parser."${deps."semver"."0.9.0"."semver_parser"}" deps)
  ];


# end
# semver-parser-0.7.0

  crates.semver_parser."0.7.0" = deps: { features?(features_.semver_parser."0.7.0" deps {}) }: buildRustCrate {
    crateName = "semver-parser";
    version = "0.7.0";
    authors = [ "Steve Klabnik <steve@steveklabnik.com>" ];
    sha256 = "1da66c8413yakx0y15k8c055yna5lyb6fr0fw9318kdwkrk5k12h";
  };
  features_.semver_parser."0.7.0" = deps: f: updateFeatures f (rec {
    semver_parser."0.7.0".default = (f.semver_parser."0.7.0".default or true);
  }) [];


# end
# serde-1.0.93

  crates.serde."1.0.93" = deps: { features?(features_.serde."1.0.93" deps {}) }: buildRustCrate {
    crateName = "serde";
    version = "1.0.93";
    authors = [ "Erick Tryzelaar <erick.tryzelaar@gmail.com>" "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "0ipi9cqi0h9bqmq7dh7k2gfdi7q6qfpx8xf6pk4vsg0sdjh4xgmd";
    build = "build.rs";
    dependencies = mapFeatures features ([
    ]
      ++ (if features.serde."1.0.93".serde_derive or false then [ (crates.serde_derive."${deps."serde"."1.0.93".serde_derive}" deps) ] else []));
    features = mkFeatures (features."serde"."1.0.93" or {});
  };
  features_.serde."1.0.93" = deps: f: updateFeatures f (rec {
    serde = fold recursiveUpdate {} [
      { "1.0.93".default = (f.serde."1.0.93".default or true); }
      { "1.0.93".serde_derive =
        (f.serde."1.0.93".serde_derive or false) ||
        (f.serde."1.0.93".derive or false) ||
        (serde."1.0.93"."derive" or false); }
      { "1.0.93".std =
        (f.serde."1.0.93".std or false) ||
        (f.serde."1.0.93".default or false) ||
        (serde."1.0.93"."default" or false); }
      { "1.0.93".unstable =
        (f.serde."1.0.93".unstable or false) ||
        (f.serde."1.0.93".alloc or false) ||
        (serde."1.0.93"."alloc" or false); }
    ];
    serde_derive."${deps.serde."1.0.93".serde_derive}".default = true;
  }) [
    (features_.serde_derive."${deps."serde"."1.0.93"."serde_derive"}" deps)
  ];


# end
# serde_derive-1.0.93

  crates.serde_derive."1.0.93" = deps: { features?(features_.serde_derive."1.0.93" deps {}) }: buildRustCrate {
    crateName = "serde_derive";
    version = "1.0.93";
    authors = [ "Erick Tryzelaar <erick.tryzelaar@gmail.com>" "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "09ygrvvs6n95jc64gvsm96zg7lis6qrxd8xk4bmh2zkq5wssm5dn";
    procMacro = true;
    dependencies = mapFeatures features ([
      (crates."proc_macro2"."${deps."serde_derive"."1.0.93"."proc_macro2"}" deps)
      (crates."quote"."${deps."serde_derive"."1.0.93"."quote"}" deps)
      (crates."syn"."${deps."serde_derive"."1.0.93"."syn"}" deps)
    ]);
    features = mkFeatures (features."serde_derive"."1.0.93" or {});
  };
  features_.serde_derive."1.0.93" = deps: f: updateFeatures f (rec {
    proc_macro2."${deps.serde_derive."1.0.93".proc_macro2}".default = true;
    quote."${deps.serde_derive."1.0.93".quote}".default = true;
    serde_derive."1.0.93".default = (f.serde_derive."1.0.93".default or true);
    syn = fold recursiveUpdate {} [
      { "${deps.serde_derive."1.0.93".syn}"."visit" = true; }
      { "${deps.serde_derive."1.0.93".syn}".default = true; }
    ];
  }) [
    (features_.proc_macro2."${deps."serde_derive"."1.0.93"."proc_macro2"}" deps)
    (features_.quote."${deps."serde_derive"."1.0.93"."quote"}" deps)
    (features_.syn."${deps."serde_derive"."1.0.93"."syn"}" deps)
  ];


# end
# serde_json-1.0.39

  crates.serde_json."1.0.39" = deps: { features?(features_.serde_json."1.0.39" deps {}) }: buildRustCrate {
    crateName = "serde_json";
    version = "1.0.39";
    authors = [ "Erick Tryzelaar <erick.tryzelaar@gmail.com>" "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "07ydv06hn8x0yl0rc94l2wl9r2xz1fqd97n1s6j3bgdc6gw406a8";
    dependencies = mapFeatures features ([
      (crates."itoa"."${deps."serde_json"."1.0.39"."itoa"}" deps)
      (crates."ryu"."${deps."serde_json"."1.0.39"."ryu"}" deps)
      (crates."serde"."${deps."serde_json"."1.0.39"."serde"}" deps)
    ]);
    features = mkFeatures (features."serde_json"."1.0.39" or {});
  };
  features_.serde_json."1.0.39" = deps: f: updateFeatures f (rec {
    itoa."${deps.serde_json."1.0.39".itoa}".default = true;
    ryu."${deps.serde_json."1.0.39".ryu}".default = true;
    serde."${deps.serde_json."1.0.39".serde}".default = true;
    serde_json = fold recursiveUpdate {} [
      { "1.0.39".default = (f.serde_json."1.0.39".default or true); }
      { "1.0.39".indexmap =
        (f.serde_json."1.0.39".indexmap or false) ||
        (f.serde_json."1.0.39".preserve_order or false) ||
        (serde_json."1.0.39"."preserve_order" or false); }
    ];
  }) [
    (features_.itoa."${deps."serde_json"."1.0.39"."itoa"}" deps)
    (features_.ryu."${deps."serde_json"."1.0.39"."ryu"}" deps)
    (features_.serde."${deps."serde_json"."1.0.39"."serde"}" deps)
  ];


# end
# smallvec-0.6.10

  crates.smallvec."0.6.10" = deps: { features?(features_.smallvec."0.6.10" deps {}) }: buildRustCrate {
    crateName = "smallvec";
    version = "0.6.10";
    authors = [ "Simon Sapin <simon.sapin@exyr.org>" ];
    sha256 = "01w7xd79q0bwn683gk4ryw50ad1zzxkny10f7gkbaaj1ax6f4q4h";
    libPath = "lib.rs";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."smallvec"."0.6.10" or {});
  };
  features_.smallvec."0.6.10" = deps: f: updateFeatures f (rec {
    smallvec = fold recursiveUpdate {} [
      { "0.6.10".default = (f.smallvec."0.6.10".default or true); }
      { "0.6.10".std =
        (f.smallvec."0.6.10".std or false) ||
        (f.smallvec."0.6.10".default or false) ||
        (smallvec."0.6.10"."default" or false); }
    ];
  }) [];


# end
# strsim-0.8.0

  crates.strsim."0.8.0" = deps: { features?(features_.strsim."0.8.0" deps {}) }: buildRustCrate {
    crateName = "strsim";
    version = "0.8.0";
    authors = [ "Danny Guo <dannyguo91@gmail.com>" ];
    sha256 = "0d3jsdz22wgjyxdakqnvdgmwjdvkximz50d9zfk4qlalw635qcvy";
  };
  features_.strsim."0.8.0" = deps: f: updateFeatures f (rec {
    strsim."0.8.0".default = (f.strsim."0.8.0".default or true);
  }) [];


# end
# syn-0.11.11

  crates.syn."0.11.11" = deps: { features?(features_.syn."0.11.11" deps {}) }: buildRustCrate {
    crateName = "syn";
    version = "0.11.11";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "0yw8ng7x1dn5a6ykg0ib49y7r9nhzgpiq2989rqdp7rdz3n85502";
    dependencies = mapFeatures features ([
    ]
      ++ (if features.syn."0.11.11".quote or false then [ (crates.quote."${deps."syn"."0.11.11".quote}" deps) ] else [])
      ++ (if features.syn."0.11.11".synom or false then [ (crates.synom."${deps."syn"."0.11.11".synom}" deps) ] else [])
      ++ (if features.syn."0.11.11".unicode-xid or false then [ (crates.unicode_xid."${deps."syn"."0.11.11".unicode_xid}" deps) ] else []));
    features = mkFeatures (features."syn"."0.11.11" or {});
  };
  features_.syn."0.11.11" = deps: f: updateFeatures f (rec {
    quote."${deps.syn."0.11.11".quote}".default = true;
    syn = fold recursiveUpdate {} [
      { "0.11.11".default = (f.syn."0.11.11".default or true); }
      { "0.11.11".parsing =
        (f.syn."0.11.11".parsing or false) ||
        (f.syn."0.11.11".default or false) ||
        (syn."0.11.11"."default" or false); }
      { "0.11.11".printing =
        (f.syn."0.11.11".printing or false) ||
        (f.syn."0.11.11".default or false) ||
        (syn."0.11.11"."default" or false); }
      { "0.11.11".quote =
        (f.syn."0.11.11".quote or false) ||
        (f.syn."0.11.11".printing or false) ||
        (syn."0.11.11"."printing" or false); }
      { "0.11.11".synom =
        (f.syn."0.11.11".synom or false) ||
        (f.syn."0.11.11".parsing or false) ||
        (syn."0.11.11"."parsing" or false); }
      { "0.11.11".unicode-xid =
        (f.syn."0.11.11".unicode-xid or false) ||
        (f.syn."0.11.11".parsing or false) ||
        (syn."0.11.11"."parsing" or false); }
    ];
    synom."${deps.syn."0.11.11".synom}".default = true;
    unicode_xid."${deps.syn."0.11.11".unicode_xid}".default = true;
  }) [
    (features_.quote."${deps."syn"."0.11.11"."quote"}" deps)
    (features_.synom."${deps."syn"."0.11.11"."synom"}" deps)
    (features_.unicode_xid."${deps."syn"."0.11.11"."unicode_xid"}" deps)
  ];


# end
# syn-0.15.38

  crates.syn."0.15.38" = deps: { features?(features_.syn."0.15.38" deps {}) }: buildRustCrate {
    crateName = "syn";
    version = "0.15.38";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "11kznyj3kakkvc3sny8snwfbzmvrgdjhq0d2i94wdkc60bff3g83";
    dependencies = mapFeatures features ([
      (crates."proc_macro2"."${deps."syn"."0.15.38"."proc_macro2"}" deps)
      (crates."unicode_xid"."${deps."syn"."0.15.38"."unicode_xid"}" deps)
    ]
      ++ (if features.syn."0.15.38".quote or false then [ (crates.quote."${deps."syn"."0.15.38".quote}" deps) ] else []));
    features = mkFeatures (features."syn"."0.15.38" or {});
  };
  features_.syn."0.15.38" = deps: f: updateFeatures f (rec {
    proc_macro2 = fold recursiveUpdate {} [
      { "${deps.syn."0.15.38".proc_macro2}"."proc-macro" =
        (f.proc_macro2."${deps.syn."0.15.38".proc_macro2}"."proc-macro" or false) ||
        (syn."0.15.38"."proc-macro" or false) ||
        (f."syn"."0.15.38"."proc-macro" or false); }
      { "${deps.syn."0.15.38".proc_macro2}".default = (f.proc_macro2."${deps.syn."0.15.38".proc_macro2}".default or false); }
    ];
    quote = fold recursiveUpdate {} [
      { "${deps.syn."0.15.38".quote}"."proc-macro" =
        (f.quote."${deps.syn."0.15.38".quote}"."proc-macro" or false) ||
        (syn."0.15.38"."proc-macro" or false) ||
        (f."syn"."0.15.38"."proc-macro" or false); }
      { "${deps.syn."0.15.38".quote}".default = (f.quote."${deps.syn."0.15.38".quote}".default or false); }
    ];
    syn = fold recursiveUpdate {} [
      { "0.15.38".clone-impls =
        (f.syn."0.15.38".clone-impls or false) ||
        (f.syn."0.15.38".default or false) ||
        (syn."0.15.38"."default" or false); }
      { "0.15.38".default = (f.syn."0.15.38".default or true); }
      { "0.15.38".derive =
        (f.syn."0.15.38".derive or false) ||
        (f.syn."0.15.38".default or false) ||
        (syn."0.15.38"."default" or false); }
      { "0.15.38".parsing =
        (f.syn."0.15.38".parsing or false) ||
        (f.syn."0.15.38".default or false) ||
        (syn."0.15.38"."default" or false); }
      { "0.15.38".printing =
        (f.syn."0.15.38".printing or false) ||
        (f.syn."0.15.38".default or false) ||
        (syn."0.15.38"."default" or false); }
      { "0.15.38".proc-macro =
        (f.syn."0.15.38".proc-macro or false) ||
        (f.syn."0.15.38".default or false) ||
        (syn."0.15.38"."default" or false); }
      { "0.15.38".quote =
        (f.syn."0.15.38".quote or false) ||
        (f.syn."0.15.38".printing or false) ||
        (syn."0.15.38"."printing" or false); }
    ];
    unicode_xid."${deps.syn."0.15.38".unicode_xid}".default = true;
  }) [
    (features_.proc_macro2."${deps."syn"."0.15.38"."proc_macro2"}" deps)
    (features_.quote."${deps."syn"."0.15.38"."quote"}" deps)
    (features_.unicode_xid."${deps."syn"."0.15.38"."unicode_xid"}" deps)
  ];


# end
# synom-0.11.3

  crates.synom."0.11.3" = deps: { features?(features_.synom."0.11.3" deps {}) }: buildRustCrate {
    crateName = "synom";
    version = "0.11.3";
    authors = [ "David Tolnay <dtolnay@gmail.com>" ];
    sha256 = "1l6d1s9qjfp6ng2s2z8219igvlv7gyk8gby97sdykqc1r93d8rhc";
    dependencies = mapFeatures features ([
      (crates."unicode_xid"."${deps."synom"."0.11.3"."unicode_xid"}" deps)
    ]);
  };
  features_.synom."0.11.3" = deps: f: updateFeatures f (rec {
    synom."0.11.3".default = (f.synom."0.11.3".default or true);
    unicode_xid."${deps.synom."0.11.3".unicode_xid}".default = true;
  }) [
    (features_.unicode_xid."${deps."synom"."0.11.3"."unicode_xid"}" deps)
  ];


# end
# tempdir-0.3.7

  crates.tempdir."0.3.7" = deps: { features?(features_.tempdir."0.3.7" deps {}) }: buildRustCrate {
    crateName = "tempdir";
    version = "0.3.7";
    authors = [ "The Rust Project Developers" ];
    sha256 = "0y53sxybyljrr7lh0x0ysrsa7p7cljmwv9v80acy3rc6n97g67vy";
    dependencies = mapFeatures features ([
      (crates."rand"."${deps."tempdir"."0.3.7"."rand"}" deps)
      (crates."remove_dir_all"."${deps."tempdir"."0.3.7"."remove_dir_all"}" deps)
    ]);
  };
  features_.tempdir."0.3.7" = deps: f: updateFeatures f (rec {
    rand."${deps.tempdir."0.3.7".rand}".default = true;
    remove_dir_all."${deps.tempdir."0.3.7".remove_dir_all}".default = true;
    tempdir."0.3.7".default = (f.tempdir."0.3.7".default or true);
  }) [
    (features_.rand."${deps."tempdir"."0.3.7"."rand"}" deps)
    (features_.remove_dir_all."${deps."tempdir"."0.3.7"."remove_dir_all"}" deps)
  ];


# end
# tempfile-3.0.8

  crates.tempfile."3.0.8" = deps: { features?(features_.tempfile."3.0.8" deps {}) }: buildRustCrate {
    crateName = "tempfile";
    version = "3.0.8";
    authors = [ "Steven Allen <steven@stebalien.com>" "The Rust Project Developers" "Ashley Mannix <ashleymannix@live.com.au>" "Jason White <jasonaw0@gmail.com>" ];
    sha256 = "0v16b7ksfrbll0kxx8m761rp39qfq44f36z1jpiw036znx9cis8s";
    dependencies = mapFeatures features ([
      (crates."cfg_if"."${deps."tempfile"."3.0.8"."cfg_if"}" deps)
      (crates."rand"."${deps."tempfile"."3.0.8"."rand"}" deps)
      (crates."remove_dir_all"."${deps."tempfile"."3.0.8"."remove_dir_all"}" deps)
    ])
      ++ (if kernel == "redox" then mapFeatures features ([
      (crates."redox_syscall"."${deps."tempfile"."3.0.8"."redox_syscall"}" deps)
    ]) else [])
      ++ (if (kernel == "linux" || kernel == "darwin") then mapFeatures features ([
      (crates."libc"."${deps."tempfile"."3.0.8"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."tempfile"."3.0.8"."winapi"}" deps)
    ]) else []);
  };
  features_.tempfile."3.0.8" = deps: f: updateFeatures f (rec {
    cfg_if."${deps.tempfile."3.0.8".cfg_if}".default = true;
    libc."${deps.tempfile."3.0.8".libc}".default = true;
    rand."${deps.tempfile."3.0.8".rand}".default = true;
    redox_syscall."${deps.tempfile."3.0.8".redox_syscall}".default = true;
    remove_dir_all."${deps.tempfile."3.0.8".remove_dir_all}".default = true;
    tempfile."3.0.8".default = (f.tempfile."3.0.8".default or true);
    winapi = fold recursiveUpdate {} [
      { "${deps.tempfile."3.0.8".winapi}"."fileapi" = true; }
      { "${deps.tempfile."3.0.8".winapi}"."handleapi" = true; }
      { "${deps.tempfile."3.0.8".winapi}"."winbase" = true; }
      { "${deps.tempfile."3.0.8".winapi}".default = true; }
    ];
  }) [
    (features_.cfg_if."${deps."tempfile"."3.0.8"."cfg_if"}" deps)
    (features_.rand."${deps."tempfile"."3.0.8"."rand"}" deps)
    (features_.remove_dir_all."${deps."tempfile"."3.0.8"."remove_dir_all"}" deps)
    (features_.redox_syscall."${deps."tempfile"."3.0.8"."redox_syscall"}" deps)
    (features_.libc."${deps."tempfile"."3.0.8"."libc"}" deps)
    (features_.winapi."${deps."tempfile"."3.0.8"."winapi"}" deps)
  ];


# end
# termion-1.5.3

  crates.termion."1.5.3" = deps: { features?(features_.termion."1.5.3" deps {}) }: buildRustCrate {
    crateName = "termion";
    version = "1.5.3";
    authors = [ "ticki <Ticki@users.noreply.github.com>" "gycos <alexandre.bury@gmail.com>" "IGI-111 <igi-111@protonmail.com>" ];
    sha256 = "0l47ppblj8d97ch100100w9fbv47c3fhnqxbvsajcz2pj7ci414k";
    dependencies = mapFeatures features ([
      (crates."numtoa"."${deps."termion"."1.5.3"."numtoa"}" deps)
    ])
      ++ (if !(kernel == "redox") then mapFeatures features ([
      (crates."libc"."${deps."termion"."1.5.3"."libc"}" deps)
    ]) else [])
      ++ (if kernel == "redox" then mapFeatures features ([
      (crates."redox_syscall"."${deps."termion"."1.5.3"."redox_syscall"}" deps)
      (crates."redox_termios"."${deps."termion"."1.5.3"."redox_termios"}" deps)
    ]) else []);
  };
  features_.termion."1.5.3" = deps: f: updateFeatures f (rec {
    libc."${deps.termion."1.5.3".libc}".default = true;
    numtoa = fold recursiveUpdate {} [
      { "${deps.termion."1.5.3".numtoa}"."std" = true; }
      { "${deps.termion."1.5.3".numtoa}".default = true; }
    ];
    redox_syscall."${deps.termion."1.5.3".redox_syscall}".default = true;
    redox_termios."${deps.termion."1.5.3".redox_termios}".default = true;
    termion."1.5.3".default = (f.termion."1.5.3".default or true);
  }) [
    (features_.numtoa."${deps."termion"."1.5.3"."numtoa"}" deps)
    (features_.libc."${deps."termion"."1.5.3"."libc"}" deps)
    (features_.redox_syscall."${deps."termion"."1.5.3"."redox_syscall"}" deps)
    (features_.redox_termios."${deps."termion"."1.5.3"."redox_termios"}" deps)
  ];


# end
# textwrap-0.11.0

  crates.textwrap."0.11.0" = deps: { features?(features_.textwrap."0.11.0" deps {}) }: buildRustCrate {
    crateName = "textwrap";
    version = "0.11.0";
    authors = [ "Martin Geisler <martin@geisler.net>" ];
    sha256 = "0s25qh49n7kjayrdj4q3v0jk0jc6vy88rdw0bvgfxqlscpqpxi7d";
    dependencies = mapFeatures features ([
      (crates."unicode_width"."${deps."textwrap"."0.11.0"."unicode_width"}" deps)
    ]);
  };
  features_.textwrap."0.11.0" = deps: f: updateFeatures f (rec {
    textwrap."0.11.0".default = (f.textwrap."0.11.0".default or true);
    unicode_width."${deps.textwrap."0.11.0".unicode_width}".default = true;
  }) [
    (features_.unicode_width."${deps."textwrap"."0.11.0"."unicode_width"}" deps)
  ];


# end
# thread-id-2.0.0

  crates.thread_id."2.0.0" = deps: { features?(features_.thread_id."2.0.0" deps {}) }: buildRustCrate {
    crateName = "thread-id";
    version = "2.0.0";
    authors = [ "Ruud van Asseldonk <dev@veniogames.com>" ];
    sha256 = "06i3c8ckn97i5rp16civ2vpqbknlkx66dkrl070iw60nawi0kjc3";
    dependencies = mapFeatures features ([
      (crates."kernel32_sys"."${deps."thread_id"."2.0.0"."kernel32_sys"}" deps)
      (crates."libc"."${deps."thread_id"."2.0.0"."libc"}" deps)
    ]);
  };
  features_.thread_id."2.0.0" = deps: f: updateFeatures f (rec {
    kernel32_sys."${deps.thread_id."2.0.0".kernel32_sys}".default = true;
    libc."${deps.thread_id."2.0.0".libc}".default = true;
    thread_id."2.0.0".default = (f.thread_id."2.0.0".default or true);
  }) [
    (features_.kernel32_sys."${deps."thread_id"."2.0.0"."kernel32_sys"}" deps)
    (features_.libc."${deps."thread_id"."2.0.0"."libc"}" deps)
  ];


# end
# thread_local-0.2.7

  crates.thread_local."0.2.7" = deps: { features?(features_.thread_local."0.2.7" deps {}) }: buildRustCrate {
    crateName = "thread_local";
    version = "0.2.7";
    authors = [ "Amanieu d'Antras <amanieu@gmail.com>" ];
    sha256 = "19p0zrs24rdwjvpi10jig5ms3sxj00pv8shkr9cpddri8cdghqp7";
    dependencies = mapFeatures features ([
      (crates."thread_id"."${deps."thread_local"."0.2.7"."thread_id"}" deps)
    ]);
  };
  features_.thread_local."0.2.7" = deps: f: updateFeatures f (rec {
    thread_id."${deps.thread_local."0.2.7".thread_id}".default = true;
    thread_local."0.2.7".default = (f.thread_local."0.2.7".default or true);
  }) [
    (features_.thread_id."${deps."thread_local"."0.2.7"."thread_id"}" deps)
  ];


# end
# thread_local-0.3.6

  crates.thread_local."0.3.6" = deps: { features?(features_.thread_local."0.3.6" deps {}) }: buildRustCrate {
    crateName = "thread_local";
    version = "0.3.6";
    authors = [ "Amanieu d'Antras <amanieu@gmail.com>" ];
    sha256 = "02rksdwjmz2pw9bmgbb4c0bgkbq5z6nvg510sq1s6y2j1gam0c7i";
    dependencies = mapFeatures features ([
      (crates."lazy_static"."${deps."thread_local"."0.3.6"."lazy_static"}" deps)
    ]);
  };
  features_.thread_local."0.3.6" = deps: f: updateFeatures f (rec {
    lazy_static."${deps.thread_local."0.3.6".lazy_static}".default = true;
    thread_local."0.3.6".default = (f.thread_local."0.3.6".default or true);
  }) [
    (features_.lazy_static."${deps."thread_local"."0.3.6"."lazy_static"}" deps)
  ];


# end
# time-0.1.42

  crates.time."0.1.42" = deps: { features?(features_.time."0.1.42" deps {}) }: buildRustCrate {
    crateName = "time";
    version = "0.1.42";
    authors = [ "The Rust Project Developers" ];
    sha256 = "1ny809kmdjwd4b478ipc33dz7q6nq7rxk766x8cnrg6zygcksmmx";
    dependencies = mapFeatures features ([
      (crates."libc"."${deps."time"."0.1.42"."libc"}" deps)
    ])
      ++ (if kernel == "redox" then mapFeatures features ([
      (crates."redox_syscall"."${deps."time"."0.1.42"."redox_syscall"}" deps)
    ]) else [])
      ++ (if kernel == "windows" then mapFeatures features ([
      (crates."winapi"."${deps."time"."0.1.42"."winapi"}" deps)
    ]) else []);
  };
  features_.time."0.1.42" = deps: f: updateFeatures f (rec {
    libc."${deps.time."0.1.42".libc}".default = true;
    redox_syscall."${deps.time."0.1.42".redox_syscall}".default = true;
    time."0.1.42".default = (f.time."0.1.42".default or true);
    winapi = fold recursiveUpdate {} [
      { "${deps.time."0.1.42".winapi}"."minwinbase" = true; }
      { "${deps.time."0.1.42".winapi}"."minwindef" = true; }
      { "${deps.time."0.1.42".winapi}"."ntdef" = true; }
      { "${deps.time."0.1.42".winapi}"."profileapi" = true; }
      { "${deps.time."0.1.42".winapi}"."std" = true; }
      { "${deps.time."0.1.42".winapi}"."sysinfoapi" = true; }
      { "${deps.time."0.1.42".winapi}"."timezoneapi" = true; }
      { "${deps.time."0.1.42".winapi}".default = true; }
    ];
  }) [
    (features_.libc."${deps."time"."0.1.42"."libc"}" deps)
    (features_.redox_syscall."${deps."time"."0.1.42"."redox_syscall"}" deps)
    (features_.winapi."${deps."time"."0.1.42"."winapi"}" deps)
  ];


# end
# toml-0.4.10

  crates.toml."0.4.10" = deps: { features?(features_.toml."0.4.10" deps {}) }: buildRustCrate {
    crateName = "toml";
    version = "0.4.10";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" ];
    sha256 = "0fs4kxl86w3kmgwcgcv23nk79zagayz1spg281r83w0ywf88d6f1";
    dependencies = mapFeatures features ([
      (crates."serde"."${deps."toml"."0.4.10"."serde"}" deps)
    ]);
  };
  features_.toml."0.4.10" = deps: f: updateFeatures f (rec {
    serde."${deps.toml."0.4.10".serde}".default = true;
    toml."0.4.10".default = (f.toml."0.4.10".default or true);
  }) [
    (features_.serde."${deps."toml"."0.4.10"."serde"}" deps)
  ];


# end
# ucd-util-0.1.3

  crates.ucd_util."0.1.3" = deps: { features?(features_.ucd_util."0.1.3" deps {}) }: buildRustCrate {
    crateName = "ucd-util";
    version = "0.1.3";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" ];
    sha256 = "1n1qi3jywq5syq90z9qd8qzbn58pcjgv1sx4sdmipm4jf9zanz15";
  };
  features_.ucd_util."0.1.3" = deps: f: updateFeatures f (rec {
    ucd_util."0.1.3".default = (f.ucd_util."0.1.3".default or true);
  }) [];


# end
# unicode-bidi-0.3.4

  crates.unicode_bidi."0.3.4" = deps: { features?(features_.unicode_bidi."0.3.4" deps {}) }: buildRustCrate {
    crateName = "unicode-bidi";
    version = "0.3.4";
    authors = [ "The Servo Project Developers" ];
    sha256 = "0lcd6jasrf8p9p0q20qyf10c6xhvw40m2c4rr105hbk6zy26nj1q";
    libName = "unicode_bidi";
    dependencies = mapFeatures features ([
      (crates."matches"."${deps."unicode_bidi"."0.3.4"."matches"}" deps)
    ]);
    features = mkFeatures (features."unicode_bidi"."0.3.4" or {});
  };
  features_.unicode_bidi."0.3.4" = deps: f: updateFeatures f (rec {
    matches."${deps.unicode_bidi."0.3.4".matches}".default = true;
    unicode_bidi = fold recursiveUpdate {} [
      { "0.3.4".default = (f.unicode_bidi."0.3.4".default or true); }
      { "0.3.4".flame =
        (f.unicode_bidi."0.3.4".flame or false) ||
        (f.unicode_bidi."0.3.4".flame_it or false) ||
        (unicode_bidi."0.3.4"."flame_it" or false); }
      { "0.3.4".flamer =
        (f.unicode_bidi."0.3.4".flamer or false) ||
        (f.unicode_bidi."0.3.4".flame_it or false) ||
        (unicode_bidi."0.3.4"."flame_it" or false); }
      { "0.3.4".serde =
        (f.unicode_bidi."0.3.4".serde or false) ||
        (f.unicode_bidi."0.3.4".with_serde or false) ||
        (unicode_bidi."0.3.4"."with_serde" or false); }
    ];
  }) [
    (features_.matches."${deps."unicode_bidi"."0.3.4"."matches"}" deps)
  ];


# end
# unicode-normalization-0.1.8

  crates.unicode_normalization."0.1.8" = deps: { features?(features_.unicode_normalization."0.1.8" deps {}) }: buildRustCrate {
    crateName = "unicode-normalization";
    version = "0.1.8";
    authors = [ "kwantam <kwantam@gmail.com>" ];
    sha256 = "1pb26i2xd5zz0icabyqahikpca0iwj2jd4145pczc4bb7p641dsz";
    dependencies = mapFeatures features ([
      (crates."smallvec"."${deps."unicode_normalization"."0.1.8"."smallvec"}" deps)
    ]);
  };
  features_.unicode_normalization."0.1.8" = deps: f: updateFeatures f (rec {
    smallvec."${deps.unicode_normalization."0.1.8".smallvec}".default = true;
    unicode_normalization."0.1.8".default = (f.unicode_normalization."0.1.8".default or true);
  }) [
    (features_.smallvec."${deps."unicode_normalization"."0.1.8"."smallvec"}" deps)
  ];


# end
# unicode-segmentation-1.3.0

  crates.unicode_segmentation."1.3.0" = deps: { features?(features_.unicode_segmentation."1.3.0" deps {}) }: buildRustCrate {
    crateName = "unicode-segmentation";
    version = "1.3.0";
    authors = [ "kwantam <kwantam@gmail.com>" ];
    sha256 = "0jnns99wpjjpqzdn9jiplsr003rr41i95c008jb4inccb3avypp0";
    features = mkFeatures (features."unicode_segmentation"."1.3.0" or {});
  };
  features_.unicode_segmentation."1.3.0" = deps: f: updateFeatures f (rec {
    unicode_segmentation."1.3.0".default = (f.unicode_segmentation."1.3.0".default or true);
  }) [];


# end
# unicode-width-0.1.5

  crates.unicode_width."0.1.5" = deps: { features?(features_.unicode_width."0.1.5" deps {}) }: buildRustCrate {
    crateName = "unicode-width";
    version = "0.1.5";
    authors = [ "kwantam <kwantam@gmail.com>" ];
    sha256 = "0886lc2aymwgy0lhavwn6s48ik3c61ykzzd3za6prgnw51j7bi4w";
    features = mkFeatures (features."unicode_width"."0.1.5" or {});
  };
  features_.unicode_width."0.1.5" = deps: f: updateFeatures f (rec {
    unicode_width."0.1.5".default = (f.unicode_width."0.1.5".default or true);
  }) [];


# end
# unicode-xid-0.0.4

  crates.unicode_xid."0.0.4" = deps: { features?(features_.unicode_xid."0.0.4" deps {}) }: buildRustCrate {
    crateName = "unicode-xid";
    version = "0.0.4";
    authors = [ "erick.tryzelaar <erick.tryzelaar@gmail.com>" "kwantam <kwantam@gmail.com>" ];
    sha256 = "1dc8wkkcd3s6534s5aw4lbjn8m67flkkbnajp5bl8408wdg8rh9v";
    features = mkFeatures (features."unicode_xid"."0.0.4" or {});
  };
  features_.unicode_xid."0.0.4" = deps: f: updateFeatures f (rec {
    unicode_xid."0.0.4".default = (f.unicode_xid."0.0.4".default or true);
  }) [];


# end
# unicode-xid-0.1.0

  crates.unicode_xid."0.1.0" = deps: { features?(features_.unicode_xid."0.1.0" deps {}) }: buildRustCrate {
    crateName = "unicode-xid";
    version = "0.1.0";
    authors = [ "erick.tryzelaar <erick.tryzelaar@gmail.com>" "kwantam <kwantam@gmail.com>" ];
    sha256 = "05wdmwlfzxhq3nhsxn6wx4q8dhxzzfb9szsz6wiw092m1rjj01zj";
    features = mkFeatures (features."unicode_xid"."0.1.0" or {});
  };
  features_.unicode_xid."0.1.0" = deps: f: updateFeatures f (rec {
    unicode_xid."0.1.0".default = (f.unicode_xid."0.1.0".default or true);
  }) [];


# end
# url-1.7.2

  crates.url."1.7.2" = deps: { features?(features_.url."1.7.2" deps {}) }: buildRustCrate {
    crateName = "url";
    version = "1.7.2";
    authors = [ "The rust-url developers" ];
    sha256 = "0qzrjzd9r1niv7037x4cgnv98fs1vj0k18lpxx890ipc47x5gc09";
    dependencies = mapFeatures features ([
      (crates."idna"."${deps."url"."1.7.2"."idna"}" deps)
      (crates."matches"."${deps."url"."1.7.2"."matches"}" deps)
      (crates."percent_encoding"."${deps."url"."1.7.2"."percent_encoding"}" deps)
    ]);
    features = mkFeatures (features."url"."1.7.2" or {});
  };
  features_.url."1.7.2" = deps: f: updateFeatures f (rec {
    idna."${deps.url."1.7.2".idna}".default = true;
    matches."${deps.url."1.7.2".matches}".default = true;
    percent_encoding."${deps.url."1.7.2".percent_encoding}".default = true;
    url = fold recursiveUpdate {} [
      { "1.7.2".default = (f.url."1.7.2".default or true); }
      { "1.7.2".encoding =
        (f.url."1.7.2".encoding or false) ||
        (f.url."1.7.2".query_encoding or false) ||
        (url."1.7.2"."query_encoding" or false); }
      { "1.7.2".heapsize =
        (f.url."1.7.2".heapsize or false) ||
        (f.url."1.7.2".heap_size or false) ||
        (url."1.7.2"."heap_size" or false); }
    ];
  }) [
    (features_.idna."${deps."url"."1.7.2"."idna"}" deps)
    (features_.matches."${deps."url"."1.7.2"."matches"}" deps)
    (features_.percent_encoding."${deps."url"."1.7.2"."percent_encoding"}" deps)
  ];


# end
# utf8-ranges-0.1.3

  crates.utf8_ranges."0.1.3" = deps: { features?(features_.utf8_ranges."0.1.3" deps {}) }: buildRustCrate {
    crateName = "utf8-ranges";
    version = "0.1.3";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" ];
    sha256 = "1cj548a91a93j8375p78qikaiam548xh84cb0ck8y119adbmsvbp";
  };
  features_.utf8_ranges."0.1.3" = deps: f: updateFeatures f (rec {
    utf8_ranges."0.1.3".default = (f.utf8_ranges."0.1.3".default or true);
  }) [];


# end
# utf8-ranges-1.0.3

  crates.utf8_ranges."1.0.3" = deps: { features?(features_.utf8_ranges."1.0.3" deps {}) }: buildRustCrate {
    crateName = "utf8-ranges";
    version = "1.0.3";
    authors = [ "Andrew Gallant <jamslam@gmail.com>" ];
    sha256 = "0nkh73y241czrxagm77qz20qcfn3h54a6v9cpvc7wjzwkaaqkswp";
  };
  features_.utf8_ranges."1.0.3" = deps: f: updateFeatures f (rec {
    utf8_ranges."1.0.3".default = (f.utf8_ranges."1.0.3".default or true);
  }) [];


# end
# uuid-0.6.5

  crates.uuid."0.6.5" = deps: { features?(features_.uuid."0.6.5" deps {}) }: buildRustCrate {
    crateName = "uuid";
    version = "0.6.5";
    authors = [ "Ashley Mannix<ashleymannix@live.com.au>" "Christopher Armstrong" "Dylan DPC<dylan.dpc@gmail.com>" "Hunar Roop Kahlon<hunar.roop@gmail.com>" ];
    sha256 = "1jy15m4yxxwma0jsy070garhbgfprky23i77rawjkk75vqhnnhlf";
    dependencies = mapFeatures features ([
      (crates."cfg_if"."${deps."uuid"."0.6.5"."cfg_if"}" deps)
    ]);
    features = mkFeatures (features."uuid"."0.6.5" or {});
  };
  features_.uuid."0.6.5" = deps: f: updateFeatures f (rec {
    cfg_if."${deps.uuid."0.6.5".cfg_if}".default = true;
    uuid = fold recursiveUpdate {} [
      { "0.6.5".byteorder =
        (f.uuid."0.6.5".byteorder or false) ||
        (f.uuid."0.6.5".u128 or false) ||
        (uuid."0.6.5"."u128" or false); }
      { "0.6.5".default = (f.uuid."0.6.5".default or true); }
      { "0.6.5".md5 =
        (f.uuid."0.6.5".md5 or false) ||
        (f.uuid."0.6.5".v3 or false) ||
        (uuid."0.6.5"."v3" or false); }
      { "0.6.5".nightly =
        (f.uuid."0.6.5".nightly or false) ||
        (f.uuid."0.6.5".const_fn or false) ||
        (uuid."0.6.5"."const_fn" or false); }
      { "0.6.5".rand =
        (f.uuid."0.6.5".rand or false) ||
        (f.uuid."0.6.5".v3 or false) ||
        (uuid."0.6.5"."v3" or false) ||
        (f.uuid."0.6.5".v4 or false) ||
        (uuid."0.6.5"."v4" or false) ||
        (f.uuid."0.6.5".v5 or false) ||
        (uuid."0.6.5"."v5" or false); }
      { "0.6.5".sha1 =
        (f.uuid."0.6.5".sha1 or false) ||
        (f.uuid."0.6.5".v5 or false) ||
        (uuid."0.6.5"."v5" or false); }
      { "0.6.5".std =
        (f.uuid."0.6.5".std or false) ||
        (f.uuid."0.6.5".default or false) ||
        (uuid."0.6.5"."default" or false) ||
        (f.uuid."0.6.5".use_std or false) ||
        (uuid."0.6.5"."use_std" or false); }
    ];
  }) [
    (features_.cfg_if."${deps."uuid"."0.6.5"."cfg_if"}" deps)
  ];


# end
# uuid-0.7.4

  crates.uuid."0.7.4" = deps: { features?(features_.uuid."0.7.4" deps {}) }: buildRustCrate {
    crateName = "uuid";
    version = "0.7.4";
    authors = [ "Ashley Mannix<ashleymannix@live.com.au>" "Christopher Armstrong" "Dylan DPC<dylan.dpc@gmail.com>" "Hunar Roop Kahlon<hunar.roop@gmail.com>" ];
    sha256 = "1kzjah6i8vf51hrla6qnplymaqx2fadhhlnbvgivgld311lqyz9m";
    dependencies = mapFeatures features ([
])
      ++ (if kernel == "windows" then mapFeatures features ([
]) else []);
    features = mkFeatures (features."uuid"."0.7.4" or {});
  };
  features_.uuid."0.7.4" = deps: f: updateFeatures f (rec {
    uuid = fold recursiveUpdate {} [
      { "0.7.4".byteorder =
        (f.uuid."0.7.4".byteorder or false) ||
        (f.uuid."0.7.4".u128 or false) ||
        (uuid."0.7.4"."u128" or false); }
      { "0.7.4".default = (f.uuid."0.7.4".default or true); }
      { "0.7.4".md5 =
        (f.uuid."0.7.4".md5 or false) ||
        (f.uuid."0.7.4".v3 or false) ||
        (uuid."0.7.4"."v3" or false); }
      { "0.7.4".nightly =
        (f.uuid."0.7.4".nightly or false) ||
        (f.uuid."0.7.4".const_fn or false) ||
        (uuid."0.7.4"."const_fn" or false); }
      { "0.7.4".rand =
        (f.uuid."0.7.4".rand or false) ||
        (f.uuid."0.7.4".v4 or false) ||
        (uuid."0.7.4"."v4" or false); }
      { "0.7.4".sha1 =
        (f.uuid."0.7.4".sha1 or false) ||
        (f.uuid."0.7.4".v5 or false) ||
        (uuid."0.7.4"."v5" or false); }
      { "0.7.4".std =
        (f.uuid."0.7.4".std or false) ||
        (f.uuid."0.7.4".default or false) ||
        (uuid."0.7.4"."default" or false); }
      { "0.7.4".winapi =
        (f.uuid."0.7.4".winapi or false) ||
        (f.uuid."0.7.4".guid or false) ||
        (uuid."0.7.4"."guid" or false); }
    ];
  }) [];


# end
# vcpkg-0.2.6

  crates.vcpkg."0.2.6" = deps: { features?(features_.vcpkg."0.2.6" deps {}) }: buildRustCrate {
    crateName = "vcpkg";
    version = "0.2.6";
    authors = [ "Jim McGrath <jimmc2@gmail.com>" ];
    sha256 = "1ig6jqpzzl1z9vk4qywgpfr4hfbd8ny8frqsgm3r449wkc4n1i5x";
  };
  features_.vcpkg."0.2.6" = deps: f: updateFeatures f (rec {
    vcpkg."0.2.6".default = (f.vcpkg."0.2.6".default or true);
  }) [];


# end
# vec_map-0.8.1

  crates.vec_map."0.8.1" = deps: { features?(features_.vec_map."0.8.1" deps {}) }: buildRustCrate {
    crateName = "vec_map";
    version = "0.8.1";
    authors = [ "Alex Crichton <alex@alexcrichton.com>" "Jorge Aparicio <japaricious@gmail.com>" "Alexis Beingessner <a.beingessner@gmail.com>" "Brian Anderson <>" "tbu- <>" "Manish Goregaokar <>" "Aaron Turon <aturon@mozilla.com>" "Adolfo Ochagavía <>" "Niko Matsakis <>" "Steven Fackler <>" "Chase Southwood <csouth3@illinois.edu>" "Eduard Burtescu <>" "Florian Wilkens <>" "Félix Raimundo <>" "Tibor Benke <>" "Markus Siemens <markus@m-siemens.de>" "Josh Branchaud <jbranchaud@gmail.com>" "Huon Wilson <dbau.pp@gmail.com>" "Corey Farwell <coref@rwell.org>" "Aaron Liblong <>" "Nick Cameron <nrc@ncameron.org>" "Patrick Walton <pcwalton@mimiga.net>" "Felix S Klock II <>" "Andrew Paseltiner <apaseltiner@gmail.com>" "Sean McArthur <sean.monstar@gmail.com>" "Vadim Petrochenkov <>" ];
    sha256 = "1jj2nrg8h3l53d43rwkpkikq5a5x15ms4rf1rw92hp5lrqhi8mpi";
    dependencies = mapFeatures features ([
]);
    features = mkFeatures (features."vec_map"."0.8.1" or {});
  };
  features_.vec_map."0.8.1" = deps: f: updateFeatures f (rec {
    vec_map = fold recursiveUpdate {} [
      { "0.8.1".default = (f.vec_map."0.8.1".default or true); }
      { "0.8.1".serde =
        (f.vec_map."0.8.1".serde or false) ||
        (f.vec_map."0.8.1".eders or false) ||
        (vec_map."0.8.1"."eders" or false); }
    ];
  }) [];


# end
# winapi-0.2.8

  crates.winapi."0.2.8" = deps: { features?(features_.winapi."0.2.8" deps {}) }: buildRustCrate {
    crateName = "winapi";
    version = "0.2.8";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "0a45b58ywf12vb7gvj6h3j264nydynmzyqz8d8rqxsj6icqv82as";
  };
  features_.winapi."0.2.8" = deps: f: updateFeatures f (rec {
    winapi."0.2.8".default = (f.winapi."0.2.8".default or true);
  }) [];


# end
# winapi-0.3.7

  crates.winapi."0.3.7" = deps: { features?(features_.winapi."0.3.7" deps {}) }: buildRustCrate {
    crateName = "winapi";
    version = "0.3.7";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "1k51gfkp0zqw7nj07y443mscs46icmdhld442s2073niap0kkdr8";
    build = "build.rs";
    dependencies = (if kernel == "i686-pc-windows-gnu" then mapFeatures features ([
      (crates."winapi_i686_pc_windows_gnu"."${deps."winapi"."0.3.7"."winapi_i686_pc_windows_gnu"}" deps)
    ]) else [])
      ++ (if kernel == "x86_64-pc-windows-gnu" then mapFeatures features ([
      (crates."winapi_x86_64_pc_windows_gnu"."${deps."winapi"."0.3.7"."winapi_x86_64_pc_windows_gnu"}" deps)
    ]) else []);
    features = mkFeatures (features."winapi"."0.3.7" or {});
  };
  features_.winapi."0.3.7" = deps: f: updateFeatures f (rec {
    winapi = fold recursiveUpdate {} [
      { "0.3.7".default = (f.winapi."0.3.7".default or true); }
      { "0.3.7".impl-debug =
        (f.winapi."0.3.7".impl-debug or false) ||
        (f.winapi."0.3.7".debug or false) ||
        (winapi."0.3.7"."debug" or false); }
    ];
    winapi_i686_pc_windows_gnu."${deps.winapi."0.3.7".winapi_i686_pc_windows_gnu}".default = true;
    winapi_x86_64_pc_windows_gnu."${deps.winapi."0.3.7".winapi_x86_64_pc_windows_gnu}".default = true;
  }) [
    (features_.winapi_i686_pc_windows_gnu."${deps."winapi"."0.3.7"."winapi_i686_pc_windows_gnu"}" deps)
    (features_.winapi_x86_64_pc_windows_gnu."${deps."winapi"."0.3.7"."winapi_x86_64_pc_windows_gnu"}" deps)
  ];


# end
# winapi-build-0.1.1

  crates.winapi_build."0.1.1" = deps: { features?(features_.winapi_build."0.1.1" deps {}) }: buildRustCrate {
    crateName = "winapi-build";
    version = "0.1.1";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "1lxlpi87rkhxcwp2ykf1ldw3p108hwm24nywf3jfrvmff4rjhqga";
    libName = "build";
  };
  features_.winapi_build."0.1.1" = deps: f: updateFeatures f (rec {
    winapi_build."0.1.1".default = (f.winapi_build."0.1.1".default or true);
  }) [];


# end
# winapi-i686-pc-windows-gnu-0.4.0

  crates.winapi_i686_pc_windows_gnu."0.4.0" = deps: { features?(features_.winapi_i686_pc_windows_gnu."0.4.0" deps {}) }: buildRustCrate {
    crateName = "winapi-i686-pc-windows-gnu";
    version = "0.4.0";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "05ihkij18r4gamjpxj4gra24514can762imjzlmak5wlzidplzrp";
    build = "build.rs";
  };
  features_.winapi_i686_pc_windows_gnu."0.4.0" = deps: f: updateFeatures f (rec {
    winapi_i686_pc_windows_gnu."0.4.0".default = (f.winapi_i686_pc_windows_gnu."0.4.0".default or true);
  }) [];


# end
# winapi-x86_64-pc-windows-gnu-0.4.0

  crates.winapi_x86_64_pc_windows_gnu."0.4.0" = deps: { features?(features_.winapi_x86_64_pc_windows_gnu."0.4.0" deps {}) }: buildRustCrate {
    crateName = "winapi-x86_64-pc-windows-gnu";
    version = "0.4.0";
    authors = [ "Peter Atashian <retep998@gmail.com>" ];
    sha256 = "0n1ylmlsb8yg1v583i4xy0qmqg42275flvbc51hdqjjfjcl9vlbj";
    build = "build.rs";
  };
  features_.winapi_x86_64_pc_windows_gnu."0.4.0" = deps: f: updateFeatures f (rec {
    winapi_x86_64_pc_windows_gnu."0.4.0".default = (f.winapi_x86_64_pc_windows_gnu."0.4.0".default or true);
  }) [];


# end
}
