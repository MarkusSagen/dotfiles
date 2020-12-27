var core = {
  "update": {
    "button": function (state) {
      app.button.title = "Current State: " + state.toUpperCase();
      app.button.icon = {
        "path": {
          "16": "../../data/icons/" + (state ? state + '/' : '') + "16.png",
          "32": "../../data/icons/" + (state ? state + '/' : '') + "32.png",
          "48": "../../data/icons/" + (state ? state + '/' : '') + "48.png",
          "64": "../../data/icons/" + (state ? state + '/' : '') + "64.png"
        }
      };
    },
    "page": function (e, reload) {
      app.content_script.send("storage", {
        "reload": reload,
        "top": e ? e.top : null,
        "uri": e ? e.uri : null,
        "storage": app.storage.global,
        "frameId": e ? e.frameId : null,
        "hostname": e ? e.hostname : null
      }, e ? e.tabId : null, e ? e.frameId : null);
    }
  }
};

app.button.clicked(function () {
  config.addon.state = config.addon.state === "dark" ? "light" : "dark";
  core.update.button(config.addon.state);
});

app.contextmenus.create({
  "contexts": ["page"],
  "id": "dark-mode-contextmenu",
  "title": "Exclude from dark mode"
});

app.contextmenus.clicked(function (e) {
  if (e.menuItemId === "dark-mode-contextmenu") {
    var pageUrl = e.pageUrl;
    chrome.storage.local.get({"whitelist": []}, function (storage) {
      var whitelist = storage.whitelist;
      whitelist.push(config.hostname(pageUrl));
      whitelist = whitelist.filter(function (element, index, array) {return element && array.indexOf(element) === index});
      chrome.storage.local.set({"whitelist": whitelist}, function () {});
    });
  }
});

app.content_script.receive("load", function (e) {core.update.page(e, false)});
app.content_script.receive("reload", function (e) {core.update.page(e, true)});

app.options.receive("dark-mode-item", function () {app.tab.open(app.homepage())});
app.options.receive("test-dark-mode", function () {app.tab.open(config.page.test)});
app.options.receive("open-support-page", function () {app.tab.open(app.homepage())});
app.options.receive("dark-theme-item", function () {app.tab.open(config.page.theme)});
app.options.receive("dark-new-tab-item", function () {app.tab.open(config.page.newtab)});
app.options.receive("make-a-donation", function () {app.tab.open(app.homepage() + "?reason=support")});

window.setTimeout(function () {core.update.button(config.addon.state)}, 300);