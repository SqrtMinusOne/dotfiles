glide.keymaps.set("normal", "<A-w>", "tab_close")

glide.keymaps.set("normal", "<S-h>", "back")
glide.keymaps.set("normal", "<S-l>", "forward")

glide.keymaps.set("command", "<A-j>", "commandline_focus_next")
glide.keymaps.set("command", "<A-k>", "commandline_focus_back")

glide.o.native_tabs = "show"

async function updateTheme() {
  await browser.theme.update({
    colors: {
      frame: "#fff8f0",
      tab_background_text: "#222222",
      toolbar: "#f8cf8f",
      toolbar_text: "#222222",
      toolbar_field: "#fff8f0",
      toolbar_field_text: "#375cd8",
      tab_line: "#8a5d00",
      popup: "#f6ece8",
      popup_text: "#222222",
      tab_loading: "#8a5d00",
    },
  })
}

glide.autocmds.create("ConfigLoaded", {}, updateTheme)
updateTheme()
