if $env.config.hooks.env_change == null {
  $env.config.hooks.env_change = {}
}
if $env.config.hooks.env_change.PWD? == null {
  $env.config.hooks.env_change.PWD = []
}
if $env.config.hooks.pre_prompt? == null {
  $env.config.hooks.pre_prompt = []
}
let direnv_hook = (source ~/projects/others/nu_scripts/nu-hooks/nu-hooks/direnv/config.nu)
$env.config.hooks.pre_prompt = $env.config.hooks.pre_prompt | append $direnv_hook
use ~/projects/others/nu_scripts/nu-hooks/nu-hooks/nuenv/hook.nu [ "nuenv allow", "nuenv disallow" ]
$env.config.hooks.env_change.PWD = $env.config.hooks.env_change.PWD | append (use ~/projects/others/nu_scripts/nu-hooks/nu-hooks/nuenv/hook.nu; hook setup)
