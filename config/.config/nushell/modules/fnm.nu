### fnm begin ###
# Adding the fnm path must come before nix or brew to prevent it from being overwritten by them
let $fnm_all_vars = fnm env --shell bash | str replace -a "export " "" | str replace -a '"' "" | lines | split column "=" | rename name value | reduce -f {} {|it, acc| $acc | upsert $it.name $it.value }
let has_fnm_path = "PATH" in ($fnm_all_vars | columns)

if $has_fnm_path {
  let $fnm_path: string = $fnm_all_vars.PATH | str replace ":$PATH" ""
  $env.PATH = $env.PATH | append $fnm_path
}

# Add env vars
let $fnm_vars = if $has_fnm_path {
  $fnm_all_vars | reject PATH
} else {
  $fnm_all_vars
}
load-env $fnm_vars
### fnm end ###
