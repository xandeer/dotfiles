map('J', 'D');
map('K', 'S');
map('t', 'T');
map('F', 'af');
map('h', 'E');
map('l', 'R');
map('j', 'd');
map('k', 'e');

settings.focusFirstCandidate = true;
settings.nextLinkRegex = /((>>|next|pre)+)/i;
settings.prevLinkRegex = /((<<|prev(ious)?)+)/;

addSearchAliasX('c', 'grep.app', 'https://grep.app/search?q=', 's', 'https://grep.app/search?q=', function(response) {
    var res = JSON.parse(response.text);
    return res.map(function(r){
        return r.phrase;
    });
});
mapkey('oc', 'grep.app', function() {
    Front.openOmnibar({type: "SearchEngine", extra: "c"});
});
addSearchAliasX('w', 'wikipedia en', 'https://en.wikipedia.org/wiki/', 's', 'https://en.wikipedia.org/w/api.php?action=opensearch&format=json&formatversion=2&namespace=0&limit=40&search=', function(response) {
    return JSON.parse(response.text)[1];
});
addSearchAliasX('f', 'wikipedia cn', 'https://zh.wikipedia.org/wiki/', 's', 'https://en.wikipedia.org/w/api.php?action=opensearch&format=json&formatversion=2&namespace=0&limit=40&search=', function(response) {
    return JSON.parse(response.text)[1];
});
mapkey('ow', 'wikipedia en', function() {
    Front.openOmnibar({type: "SearchEngine", extra: "w"});
});
mapkey('of', 'wikipedia cn', function() {
    Front.openOmnibar({type: "SearchEngine", extra: "f"});
});
map('oo', 'og');
mapkey('ou', 'github', function() {
    Front.openOmnibar({type: "SearchEngine", extra: "h"});
});
mapkey('oa', 'Open google translate to cn', () => {
    tabOpenLink("https://translate.google.com/?sl=auto&tl=zh-CN&text=")
});
mapkey('ob', 'Open google translate to en', () => {
    tabOpenLink("https://translate.google.com/?sl=auto&tl=en&text=")
});

// theme
settings.theme = `
.sk_theme {
	background: #100a14dd;
	color: #4f97d7;
}
.sk_theme tbody {
	color: #292d;
}
.sk_theme input {
	color: #d9dce0;
}
.sk_theme .url {
	color: #2d9574;
}
.sk_theme .annotation {
	color: #a31db1;
}
.sk_theme .omnibar_highlight {
	color: #333;
	background: #ffff00aa;
}
.sk_theme #sk_omnibarSearchResult ul>li:nth-child(odd) {
	background: #5d4d7a55;
}
.sk_theme #sk_omnibarSearchResult ul>li.focused {
	background: #5d4d7aaa;
}
.sk_theme #sk_omnibarSearchResult .omnibar_folder {
	color: #a31db1;
}
.sk_theme #sk_hints > div {
    color: white !important;
}
`;
