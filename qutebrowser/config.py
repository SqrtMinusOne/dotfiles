import dracula.draw

config.load_autoconfig()

config.unbind('gt', mode='normal')
config.bind('gt', 'tab-next')
config.bind('gT', 'tab-prev')
config.bind('gN', 'tab-close')
config.bind('gn', 'tab-clone')

# config.unbind('T', mode='normal')
# config.bind('T', 'set-cmd-text -s :buffer')

config.bind('\\t', 'set-cmd-text -s :buffer')
config.bind('\\b', 'set-cmd-text -s :bookmark-load')

c.scrolling.bar = 'always'
c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?hl=en&q={}"
}

dracula.draw.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    }
})
