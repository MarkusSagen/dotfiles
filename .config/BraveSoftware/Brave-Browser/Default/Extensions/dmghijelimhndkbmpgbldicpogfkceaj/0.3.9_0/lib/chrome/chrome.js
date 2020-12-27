app.tab = {
  "open": function (url) {
    chrome.tabs.create({"url": url, "active": true});
  }
};

app.button = {
  set icon (e) {chrome.browserAction.setIcon(e)},
  set title (title) {chrome.browserAction.setTitle({"title": title})},
  "clicked": function (callback) {
    chrome.browserAction.onClicked.addListener(callback);
  }
};

app.contextmenus = {
  "create": function (e) {
    chrome.contextMenus.create(e);
  },
  "clicked": function (callback) {
    chrome.contextMenus.onClicked.addListener(callback);
  }
};

app.options = (function () {
  var tmp = {};
  chrome.runtime.onMessage.addListener(function (request, sender, sendResponse) {
    for (var id in tmp) {
      if (tmp[id] && (typeof tmp[id] === "function")) {
        if (request.path === "options-to-background") {
          if (request.method === id) tmp[id](request.data);
        }
      }
    }
  });
  /*  */
  return {
    "receive": function (id, callback) {tmp[id] = callback},
    "send": function (id, data) {
      chrome.runtime.sendMessage({"path": "background-to-options", "method": id, "data": data});
    }
  }
})();

app.storage = (function () {
  chrome.storage.onChanged.addListener(function () {
    chrome.storage.local.get(null, function (e) {
      app.storage.global = e;
    });
  });
  /*  */
  window.setTimeout(function () {
    chrome.storage.local.get(null, function (e) {
      app.storage.global = e;
      var script = document.createElement("script");
      script.src = "../common.js";
      document.body.appendChild(script);
    });
  }, 0);
  /*  */
  return {
    "global": {},
    "read": function (id) {return app.storage.global[id]},
    "write": function (id, data) {
      var tmp = {};
      tmp[id] = data;
      app.storage.global[id] = data;
      chrome.storage.local.set(tmp, function () {});
    }
  }
})();

app.content_script = (function () {
  var tmp = {};
  chrome.runtime.onMessage.addListener(function (request, sender, sendResponse) {
    for (var id in tmp) {
      if (tmp[id] && (typeof tmp[id] === "function")) {
        if (request.path === "page-to-background") {
          if (request.method === id) {
            var a = request.data || {};
            if (sender) {
              a.frameId = sender.frameId;
              /*  */
              if (sender.tab) {
                a.tabId = sender.tab.id;
                a.top = sender.tab.url ? sender.tab.url : '';
                a.uri = sender.tab.url ? decodeURIComponent(sender.tab.url) : '';
                a.hostname = sender.tab.url ? config.hostname(sender.tab.url) : '';
              }
            }
            /*  */
            tmp[id](a);
          }
        }
      }
    }
  });
  /*  */
  return {
    "receive": function (id, callback) {tmp[id] = callback},
    "send": function (id, data, tabId, frameId) {
      chrome.tabs.query({}, function (tabs) {
        tabs.forEach(function (tab) {
          if (tab) {
            if (tabId !== null) {
              if (tabId === tab.id) {
                if (frameId !== null) {
                  chrome.tabs.sendMessage(tabId, {
                    "method": id, 
                    "data": data,
                    "path": "background-to-page"
                  }, {"frameId": frameId});
                }
              }
            }
          }
        });
      });
    }
  }
})();