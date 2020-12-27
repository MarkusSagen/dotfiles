var background = (function () {
  var tmp = {};
  chrome.runtime.onMessage.addListener(function (request, sender, sendResponse) {
    for (var id in tmp) {
      if (tmp[id] && (typeof tmp[id] === "function")) {
        if (request.path === "background-to-page") {
          if (request.method === id) tmp[id](request.data);
        }
      }
    }
  });
  /*  */
  return {
    "receive": function (id, callback) {tmp[id] = callback},
    "send": function (id, data) {chrome.runtime.sendMessage({"path": "page-to-background", "method": id, "data": data})}
  }
})();

var config = {
  "general": {
    "link": document.getElementById("dark-mode-general-link")
  },
  "custom": {
    "link": document.getElementById("dark-mode-custom-link"),
    "style": document.getElementById("dark-mode-custom-style"),
  },
  "hostname": function (url) {
    url = url.replace("www.", '');
    var s = url.indexOf("//") + 2;
    if (s > 1) {
      var o = url.indexOf('/', s);
      if (o > 0) return url.substring(s, o);
      else {
        o = url.indexOf('?', s);
        if (o > 0) return url.substring(s, o);
        else return url.substring(s);
      }
    } else return url;
  },
  "apply": {
    "style": function (loc, href_a, href_b, txt, options) {
      if (options.frameId === 0) {
        if (options.reload === false) {
          if (href_a === '' && href_b === '' && txt === '') {
            config.temporarily.remove(0);
          } else {
            config.temporarily.add();
            config.temporarily.remove(200);
          }
        } else config.temporarily.remove(0);
      } else config.temporarily.remove(0);
      /*  */
      config.custom.style.textContent = txt;
      /*  */
      href_b ? config.custom.link.setAttribute("href", href_b) : config.custom.link.removeAttribute("href");
      href_a ? config.general.link.setAttribute("href", href_a) : config.general.link.removeAttribute("href");
    }
  },
  "temporarily": {
    "timeout": undefined,
    "id": "temporarily-dark-style",
    "add": function () {
      if (document.documentElement) {
        document.documentElement.setAttribute(config.temporarily.id, '');
      }
    },
    "remove": function (delay) {
      if (document.documentElement) {
        if (delay) {
          if (config.temporarily.timeout) window.clearTimeout(config.temporarily.timeout);
          config.temporarily.timeout = window.setTimeout(function () {
            document.documentElement.removeAttribute(config.temporarily.id);
          }, delay);
        } else {
          document.documentElement.removeAttribute(config.temporarily.id);
        }
      }
    }
  },
  "observer": {
    "storage": function () {
      chrome.storage.onChanged.addListener(function () {
        chrome.storage.local.get(null, function () {
          window.setTimeout(function () {
            background.send("reload");
          }, 0);
        });
      });
    },
    "head": new MutationObserver(function () {
      var tmp = {};
      /*  */
      if (document.documentElement) {
        tmp.a = document.getElementById("dark-mode-general-link");
        if (!tmp.a) document.documentElement.appendChild(config.general.link);
        /*  */
        tmp.b = document.getElementById("dark-mode-custom-link");
        if (!tmp.b) document.documentElement.appendChild(config.custom.link);
        /*  */
        tmp.c = document.getElementById("dark-mode-custom-style");
        if (!tmp.c) document.documentElement.appendChild(config.custom.style);
        /*  */
        config.observer.head.disconnect();
      }
    })
  },
  "load": function () {
    background.send("load");
    config.observer.storage();
    /*  */
    if (!config.general.link) {
      config.general.link = document.createElement("link");
      config.general.link.setAttribute("type", "text/css");
      config.general.link.setAttribute("rel", "stylesheet");
      config.general.link.setAttribute("id", "dark-mode-general-link");
    }
    /*  */
    if (!config.custom.link) {
      config.custom.link = document.createElement("link");
      config.custom.link.setAttribute("type", "text/css");
      config.custom.link.setAttribute("rel", "stylesheet");
      config.custom.link.setAttribute("id", "dark-mode-custom-link");
    }
    /*  */
    if (!config.custom.style) {
      config.custom.style = document.createElement("style");
      config.custom.style.setAttribute("lang", "en");
      config.custom.style.setAttribute("type", "text/css");
      config.custom.style.setAttribute("id", "dark-mode-custom-style");
    }
    /*  */
    if (document.documentElement) {
      document.documentElement.appendChild(config.general.link);
      document.documentElement.appendChild(config.custom.link);
      document.documentElement.appendChild(config.custom.style);
    } else {
      config.observer.head.observe(document, {
        "subtree": true,
        "childList": true,
      });
    }
  },
  "check": {
    "darkness": function (e) {
      try {
        if (e && e.length) {
          for (var i = 0; i < e.length; i++) {
            if (document.cookie) {
              if (document.cookie.indexOf(e[i]) !== -1) {
                return true;
              }
            }
            /*  */
            if (localStorage) {
              var keys = JSON.stringify(localStorage).replace(/\\/g, '').replace(/\"/g, '');
              if (keys) {
                if (keys.indexOf(e[i]) !== -1) {
                  return true;
                }
              }
            }
            /*  */
            if (sessionStorage) {
              var keys = JSON.stringify(sessionStorage).replace(/\\/g, '').replace(/\"/g, '');
              if (keys) {
                if (keys.indexOf(e[i]) !== -1) {
                  return true;
                }
              }
            }
          }
        }
      } catch (e) {
        return false;
      }
      /*  */
      return false;
    }
  },
  "update": function (e) {
    var id = null;
    var top = e.top ? e.top : document.location.href;
    var uri = e.uri ? e.uri : decodeURIComponent(top);
    var darkness = config.check.darkness(e.storage.cookie);
    var options = {"reload": e.reload, "frameId": e.frameId};
    var hostname = e.hostname ? e.hostname : config.hostname(top);
    /*  */
    if (darkness) return config.apply.style(0, '', '', '', options);
    if (top.indexOf("/chrome/newtab") !== -1) return config.apply.style(1, '', '', '', options);
    /*  */
    for (var i = 0; i < e.storage.whitelist.length; i++) {
      if (e.storage.whitelist[i] === hostname) {
        return config.apply.style(2, '', '', '', options);
      }
    }
    /*  */
    for (var i = 1; i <= website.total.themes.number; i++) {
      if (e.storage["dark_" + i]) {
        id = i;
        break;
      }
    }
    /*  */
    for (var name in website.custom.regex.rules) {
      if (e.storage[name]) {
        var rule = new RegExp(website.custom.regex.rules[name]);
        if (rule.test(uri)) {
          var href_a = e.storage.state === "dark" ? chrome.runtime.getURL("data/content_script/custom/dark.css") : '';
          var href_b = e.storage.state === "dark" ? chrome.runtime.getURL("data/content_script/custom/" + name + ".css") : '';
          /*  */
          href_a = website.exclude.from.custom.dark.mode.indexOf(name) === -1 ? href_a : '';
          config.apply.style(3, href_a, href_b, '', options);
          return;
        }
      }
    }
    /*  */
    if (e.storage.state === "dark") {
      if (id) {
        var path = chrome.runtime.getURL("data/content_script/general/dark_" + id + ".css");
        if (id === website.customized.theme.number) {
          config.apply.style(4, '', '', e.storage.custom, options);
        } else config.apply.style(5, path, '', '', options);
      } else config.apply.style(6, '', '', '', options);
    } else config.apply.style(7, '', '', '', options);
  }
};

config.load();
background.receive("storage", config.update);
//if (window === window.top) config.temporarily.add();
window.addEventListener("load", function () {config.observer.head.disconnect()}, false);