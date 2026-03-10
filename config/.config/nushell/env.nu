let zoxide_cache = "~/.cache/zoxide" | path expand
if not ($zoxide_cache | path exists) {
  mkdir $zoxide_cache
}
let zoxide_init = $"($zoxide_cache)/init.nu"
if not ($zoxide_init | path exists) {
  zoxide init nushell | save --force $zoxide_init
}

let oh_my_posh_cache = "~/.cache/oh-my-posh" | path expand
if not ($oh_my_posh_cache | path exists) {
  mkdir $oh_my_posh_cache
}
let oh_my_posh_init = $"($oh_my_posh_cache)/init.nu"
if not ($oh_my_posh_init | path exists) {
  oh-my-posh init nu --print | save --force $oh_my_posh_init
}

load-env {}
