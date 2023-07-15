{ config, ... }:

{
  nix = {
    settings = {
      # You should generally set this to the total number of logical cores in your system.
      # $ sysctl -n hw.ncpu
      max-jobs = 10;
      cores = 10;
    };
  };
}
