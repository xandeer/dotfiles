(( $+commands[npm] )) && {
    __NPM_COMPLETION_FILE="${ZSH_CACHE_DIR}/npm_completion"

    if [[ ! -f $__NPM_COMPLETION_FILE ]]; then
        npm completion >! $__NPM_COMPLETION_FILE 2>/dev/null
        [[ $? -ne 0 ]] && rm -f $__NPM_COMPLETION_FILE
    fi

    [[ -f $__NPM_COMPLETION_FILE ]] && source $__NPM_COMPLETION_FILE

    unset __NPM_COMPLETION_FILE
}

alias ni="npm i"
# Install dependencies globally
alias nig="npm i -g "

# npm package names are lowercase
# Thus, we've used camelCase for the following aliases:

# Install and save to dependencies in your package.json
# npms is used by https://www.npmjs.com/package/npms
alias nis="npm i -S "

# Install and save to dev-dependencies in your package.json
# npmd is used by https://github.com/dominictarr/npmd
alias nid="npm i -D "

# Run npm start
alias ns="npm start"

# Run npm test
alias nt="npm test"
alias nto="npm test --"

alias nr="npm run"
