### fnm begin ###
# Adding the fnm path must come before nix or brew to prevent it from being overwritten by them
let $fnm_all_vars = fnm env --shell bash | str replace -a "export " "" | str replace -a '"' "" | lines | split column "=" | rename name value | reduce -f {} {|it, acc| $acc | upsert $it.name $it.value }

let $fnm_path: string = $fnm_all_vars.PATH | str replace ":$PATH" ""
print "Adding FNM to path: " $fnm_path
$env.PATH = $env.PATH | append $fnm_path

# Add env vars
let $fnm_vars = $fnm_all_vars | reject PATH
print "Adding FNM vars to shell env: " $fnm_vars
load-env $fnm_vars
### fnm end ###
