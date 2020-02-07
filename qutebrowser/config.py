import os
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
config.bind('\\ww', ':open file:///home/pavel/MEGAsync/vimwiki_html/index.html')

c.scrolling.bar = 'always'
c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?hl=en&q={}"
}

c.zoom.levels = ['25%', '33%', '50%', '67%', '75%', '90%', '100%', '110%',
                 '125%', '133%', '150%', '175%', '200%', '250%', '300%',
                 '400%', '500%']

if os.uname().nodename == 'pavel-ntk':
    c.zoom.default = '133%'

dracula.draw.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    }
})
