let zoxide_cache = "~/.cache/zoxide" | path expand
if not ($zoxide_cache | path exists) {
  mkdir $zoxide_cache
}
zoxide init nushell | save --force $"($zoxide_cache)/init.nu"

let oh_my_posh_cache = "~/.cache/oh-my-posh" | path expand
if not ($oh_my_posh_cache | path exists) {
  mkdir $oh_my_posh_cache
}
oh-my-posh init nu --print | save --force $"($oh_my_posh_cache)/init.nu"

load-env {}
