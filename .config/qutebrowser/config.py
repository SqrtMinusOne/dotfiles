import os
import dracula.draw

from qutebrowser.api import interceptor

def filter_yt(info: interceptor.Request):
	"""Block the given request if necessary."""
	url = info.request_url
	if (url.host() == 'www.youtube.com' and
			url.path() == '/get_video_info' and
			'&adformat=' in url.query()):
		info.block()


interceptor.register(filter_yt)

config.load_autoconfig()

config.unbind('gt', mode='normal')
config.bind('gt', 'tab-next')
config.bind('gT', 'tab-prev')
config.bind('gN', 'tab-close')
config.bind('gn', 'tab-clone')

c.fonts.default_size = '10pt'
c.fonts.default_family = 'monospace'
c.fonts.web.size.default_fixed = 13

# config.unbind('T', mode='normal')
# config.bind('T', 'set-cmd-text -s :buffer')

# c.content.javascript.enabled = False

config.bind('\\t', 'set-cmd-text -s :buffer')
config.bind('\\b', 'set-cmd-text -s :bookmark-load')
config.bind('\\ww', ':open file:///home/pavel/Documents/org-mode/Bookmarks/bookmarks.html')

config.bind('\\z1', 'set zoom.default 100 ;; set fonts.default_size 10pt')
config.bind('\\z2', 'set zoom.default 125 ;; set fonts.default_size 12pt')

if c.colors.webpage.darkmode.enabled:
    config.bind('\\d', 'set colors.webpage.darkmode.enabled False ;; restart')
else:
    config.bind('\\d', 'set colors.webpage.darkmode.enabled True ;; restart')
    

# config.unbind('<Escape>', mode='insert')
config.bind('<Shift-Escape>', 'fake-key <Escape>', mode='insert')

RUSSIAN = 'йцукенгшщзхъфывапролджэячсмитьбю.'
ENGLISH = 'qwertyuiop[]asdfghjkl;\'zxcvbnm,./'

c.bindings.key_mappings = {
    **{r: e for r, e in zip(RUSSIAN, ENGLISH)},
    **{r.upper(): e.upper() for r, e in zip(RUSSIAN, ENGLISH)}
}

c.editor.command = [
    'nvim',
    '-f',
    '{file}',
    '-c',
    'normal {line}G{column0}l'
]

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
