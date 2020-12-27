// Detecting popup opens
chrome.runtime.sendMessage({ popupOpen: true });

// Gumroad
const monthlyProductID = "AgEOO";
const yearlyProductID = "OOSyDN";

// Google Analytics
var _gaq = _gaq || [];
_gaq.push(["_setAccount", "UA-162757005-1"]);
_gaq.push(["_trackPageview"]);

(function () {
  var ga = document.createElement("script");
  ga.type = "text/javascript";
  ga.async = true;
  ga.src = "https://ssl.google-analytics.com/ga.js";
  var s = document.getElementsByTagName("script")[0];
  s.parentNode.insertBefore(ga, s);
})();

function trackOptionToggle(e) {
  _gaq.push(["_trackEvent", e.target.id, ` ${e.target.checked} - clicked`]);
}

// Features
const features = [
  "autoAdmit",
  "autoReject",
  "autoCaptions",
  "autoChat",
  "autoFullScreen",
  "autoJoin",
  "autoJoinParticipants",
  "hideCommentBubble",
  "hideComments",
  "keyCode",
  "borderColor",
  "muteMicrophone",
  "mutePopup",
  "muteVideo",
  "quickLeave",
  "smartUnmute",
  "transBar",
  "displayClock",
  "darkMode",
  "pictureInPicture",
  "noAddOthers",
  "mirrorVideos",
  "meetingTimer",
  "pinBottomBar",
  "toggleBottomBar",
  "hideNames",
  "showNames",
  "hideTalkIcons",
  "leavePrompt",
  "setBackgroundColor",
  "backgroundColor",
  "speakerBorder",
  "autoUnmute",
  "autoCopyURL",
  "hideTopBar",
];

const featureObject = features.reduce(
  (prev, current) => ({
    ...prev,
    [current]: false,
  }),
  {}
);

featureObject.keyCode = {
  keyCode: 32,
  ctrlKey: false,
  altKey: false,
  shiftKey: false,
  metaKey: false,
};

featureObject.backgroundColor = "#111111";
featureObject.borderColor = "#64ffda";

// Runs setup script
document.addEventListener("DOMContentLoaded", () => {
  restore_options();
  attachCheckboxHandlers();
});

// Saves options to chrome.storage
function save_options() {
  let featureState = featureObject;

  features.forEach((feature) => {
    if (feature == "backgroundColor") {
      featureState[feature] = document.getElementById(
        "backgroundColorPicker"
      ).value;
    } else if (feature == "borderColor") {
      featureState[feature] = document.getElementById("borderColor").value;
    } else if (feature == "keyCode") {
      delete featureState.keyCode;
    } else {
      featureState[feature] = document.getElementById(feature).checked;
    }
  });

  chrome.storage.sync.set(featureState);
}

// Restores select box and checkbox state using the preferences
// stored in chrome.storage.
function restore_options() {
  chrome.storage.sync.get(featureObject, function (items) {
    features.forEach((feature) => {
      if (feature === "keyCode") {
        const currentHotkey = new Hotkey(Hotkey.keysFromEvent(items[feature]));
        document.getElementById(
          "hotkey_edit"
        ).innerText = currentHotkey.display();
      } else if (feature === "backgroundColor") {
        document.getElementById("backgroundColorPicker").value = items[feature];
      } else if (feature === "borderColor") {
        document.getElementById("borderColor").value = items[feature];
      } else {
        document.getElementById(feature).checked = items[feature];
      }
    });
  });
}

// Adds a listener event to each checkbox element
// and saves the updated status when an checkbox changes states
function attachCheckboxHandlers() {
  const featureList = document.getElementById("featureList");

  Object.values(featureList.getElementsByTagName("input")).forEach(
    (element) => {
      if (element.type === "checkbox") {
        element.onchange = save_options;
        element.addEventListener("click", trackOptionToggle);
      }
    }
  );
}

// MIT Licensed - author: Andrew Grim, Casey Watts, adunkman, sparksm, callumcrae
const hotkeyContainer = document.getElementById("hotkey_edit");
const editButton = document.getElementById("editHotkey");
const saveButton = document.getElementById("saveHotkey");

let currentHotkey;

const keyCapture = (event) => {
  event.preventDefault();
  currentHotkey = new Hotkey(Hotkey.keysFromEvent(event));
  hotkeyContainer.innerText = currentHotkey.display();
};

editButton.addEventListener("click", () => {
  if (hotkeyContainer.getAttribute("data-active") === "false") {
    hotkeyContainer.setAttribute("data-active", "true");
    editButton.setAttribute("style", "display: none");
    saveButton.setAttribute("style", "display: block");
    document.body.addEventListener("keydown", keyCapture);
  }
});

saveButton.addEventListener("click", () => {
  if (hotkeyContainer.getAttribute("data-active") === "true") {
    hotkeyContainer.setAttribute("data-active", "false");
    editButton.setAttribute("style", "display: block");
    saveButton.setAttribute("style", "display: none");
    document.body.removeEventListener("keydown", keyCapture);
    chrome.storage.sync.set({ keyCode: currentHotkey.keys });
  }
});

document.addEventListener("click", (event) => {
  if (
    hotkeyContainer.getAttribute("data-active") === "true" &&
    currentHotkey != undefined &&
    event.target != saveButton &&
    event.target != editButton
  ) {
    hotkeyContainer.setAttribute("data-active", "false");
    editButton.setAttribute("style", "display: block");
    saveButton.setAttribute("style", "display: none");
    document.body.removeEventListener("keydown", keyCapture);
    chrome.storage.sync.set({ keyCode: currentHotkey.keys });
  }
});

class Hotkey {
  constructor({
    keyCode,
    ctrlKey = false,
    altKey = false,
    shiftKey = false,
    metaKey = false,
  }) {
    this.keys = { keyCode, ctrlKey, altKey, shiftKey, metaKey };
  }

  static keysFromEvent({ keyCode, ctrlKey, altKey, shiftKey, metaKey }) {
    const keys = { ctrlKey, altKey, shiftKey, metaKey };

    if (![16, 17, 18, 91].includes(keyCode)) {
      keys.keyCode = keyCode;
    }

    return keys;
  }

  fromEvent(event) {
    return new Hotkey(Hotkey.keysFromEvent(event));
  }

  keyCode(searchInput) {
    // Keyboard Events
    if (searchInput && "object" === typeof searchInput) {
      var hasKeyCode =
        searchInput.which || searchInput.keyCode || searchInput.charCode;
      if (hasKeyCode) searchInput = hasKeyCode;
    }

    // Numbers
    if ("number" === typeof searchInput) return names[searchInput];

    // Everything else (cast to string)
    var search = String(searchInput);

    // check codes
    var foundNamedKey = codes[search.toLowerCase()];
    if (foundNamedKey) return foundNamedKey;

    // check aliases
    var foundNamedKey = aliases[search.toLowerCase()];
    if (foundNamedKey) return foundNamedKey;

    // weird character?
    if (search.length === 1) return search.charCodeAt(0);

    return undefined;
  }

  keyStrings() {
    return [
      this.keys.ctrlKey && "Control",
      this.keys.altKey && "Alt",
      this.keys.shiftKey && "Shift",
      this.keys.metaKey && "Command",
      this.keys.keyCode && keyCode(this.keys.keyCode),
    ].filter((v) => v);
  }

  display() {
    return this.keyStrings()
      .map((key) => `${key}`)
      .join(" + ");
  }

  matchKeydown(event) {
    return (
      this.keys.ctrlKey == event.ctrlKey &&
      this.keys.altKey == event.altKey &&
      this.keys.shiftKey == event.shiftKey &&
      this.keys.metaKey == event.metaKey &&
      (this.keys.keyCode == event.keyCode ||
        ([16, 17, 18, 91].includes(event.keyCode) &&
          this.keys.keyCode === undefined))
    );
  }

  matchKeyup(event) {
    if (this.keys.keyCode && this.keys.keyCode == event.keyCode) {
      return true;
    }

    return (
      (this.keys.ctrlKey && !event.ctrlKey) ||
      (this.keys.altKey && !event.altKey) ||
      (this.keys.shiftKey && !event.shiftKey) ||
      (this.keys.metaKey && !event.metaKey)
    );
  }
}

// Adds listener for click on activate link
// when clicked it swaps the UI so a token can be entered
document.getElementById("activateLink").addEventListener("click", () => {
  document.getElementById("proLinks").style.display = "none";
  document.getElementById("activationForm").style.display = "contents";
});

const fetchSingleLicense = (options, licenseKey, error) => {
  fetch("https://api.gumroad.com/v2/licenses/verify", options)
    .then((response) => response.json())
    .then((data) => {
      if (data.success) {
        const nowInSeconds = new Date().getTime();
        const cancelledInSeconds = Date.parse(
          data.purchase.subscription_cancelled_at
        );
        const diffInSeconds = cancelledInSeconds - nowInSeconds;
        const diffInDays = diffInSeconds / (1000 * 3600 * 24);

        if (
          (data.purchase.email === "keyfermath@gmail.com" ||
            (data.purchase.variants === "(Single License)" &&
              data.uses <= 11) ||
            (data.purchase.variants === "(Ten Licenses)" && data.uses <= 15) ||
            (data.purchase.variants === "(Fifty Licenses)" &&
              data.uses <= 60) ||
            data.purchase.variants === "(Unlimited Licenses)") &&
          (diffInDays > 0 || !data.purchase.subscription_cancelled_at) &&
          !data.purchase.subscription_failed_at
        ) {
          licenseKey ? enableProFeatures(licenseKey) : enableProFeatures();
        } else if (error != undefined) {
          disableProFeatures("error");
        } else {
          disableProFeatures();
        }
      } else {
        fetchYearlyLicense(options, licenseKey, error);
      }
    })
    .catch(() => {
      return;
    });
};

const fetchYearlyLicense = (options, licenseKey, error) => {
  let newOptions = options;
  newOptions.body = JSON.parse(newOptions.body);
  newOptions.body.product_permalink = newOptions.body.product_permalink = yearlyProductID;
  newOptions.body = JSON.stringify(newOptions.body);

  fetch("https://api.gumroad.com/v2/licenses/verify", newOptions)
    .then((response) => response.json())
    .then((data) => {
      if (data.success) {
        const nowInSeconds = new Date().getTime();
        const cancelledInSeconds = Date.parse(
          data.purchase.subscription_cancelled_at
        );
        const diffInSeconds = cancelledInSeconds - nowInSeconds;
        let diffInDays = diffInSeconds / (1000 * 3600 * 24);

        if (
          (data.purchase.email === "keyfermath@gmail.com" ||
            (data.purchase.variants === "(Single License)" &&
              data.uses <= 11) ||
            (data.purchase.variants === "(Ten Licenses)" && data.uses <= 15) ||
            (data.purchase.variants === "(Fifty Licenses)" &&
              data.uses <= 60) ||
            data.purchase.variants === "(Unlimited Licenses)") &&
          (diffInDays > 0 || !data.purchase.subscription_cancelled_at) &&
          !data.purchase.subscription_failed_at
        ) {
          if (licenseKey != undefined) {
            enableProFeatures(licenseKey);
          } else {
            enableProFeatures();
          }
        } else {
          if (error != undefined) {
            disableProFeatures("error");
          } else {
            disableProFeatures();
          }
        }
      } else {
        if (error != undefined) {
          disableProFeatures("error");
        } else {
          disableProFeatures();
        }
      }
    })
    .catch(() => {
      return;
    });
};

chrome.storage.sync.get("licenseKey", (response) => {
  // License Key
  if (!response.licenseKey) {
    disableProFeatures();
  } else {
    const options = {
      method: "POST",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        product_permalink: monthlyProductID,
        license_key: response.licenseKey,
        increment_uses_count: "false",
      }),
    };

    fetchSingleLicense(options);
  }
});

document
  .getElementById("activationForm")
  .addEventListener("submit", (event) => {
    event.preventDefault();
    const licenseKey = document.getElementById("licenseKey").value;
    const options = {
      method: "POST",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        product_permalink: monthlyProductID,
        license_key: licenseKey,
        increment_uses_count: "true",
      }),
    };

    fetchSingleLicense(options, licenseKey, "error");
  });

function enableProFeatures(licenseKey = undefined) {
  if (licenseKey) {
    chrome.storage.sync.set({
      licenseKey: licenseKey,
    });
  }

  document.getElementById("proUpgrade").style.display = "none";
  document.getElementById("proFeatures").style.display = "block";
  document.getElementById("basicFeatureHeader").style.display = "block";
  document.getElementById("featureList").style.display = "inline";
  document.querySelector("body").style.width = "380px";
  document.getElementById("desktopFooter").style.width = "348px";
  document.getElementById("proFeatures").style.marginBottom = "90px";

  // Hotkey
  document.getElementById("hotkey_edit").disabled = false;
  document.getElementById("editHotkey").style.display = "block";
  document.getElementById("hotkeyTooltip").innerText =
    "Click to begin listening, click again to save.";
}

function disableProFeatures(message = undefined) {
  if (message === "error") {
    document.getElementById("licenseKey").style.border = "2px";
    document.getElementById("licenseKey").style.borderStyle = "solid";
    document.getElementById("licenseKey").style.borderColor = "red";
  }

  chrome.storage.sync.set({ licenseKey: undefined });
  document.getElementById("proUpgrade").style.display = "block";
  document.getElementById("proFeatures").style.display = "none";
  document.getElementById("desktopFooter").style.width = "768px";

  // Hotkey
  document.getElementById("hotkey_edit").disabled = true;
  document.getElementById("editHotkey").style.display = "none";
  document.getElementById("hotkeyTooltip").innerText =
    "Upgrade to Pro to set your own key binding.";
}

// MIT Licensed - author: Tim Oxley
function keyCode(searchInput) {
  // Keyboard Events
  if (searchInput && "object" === typeof searchInput) {
    var hasKeyCode =
      searchInput.which || searchInput.keyCode || searchInput.charCode;
    if (hasKeyCode) searchInput = hasKeyCode;
  }

  // Numbers
  if ("number" === typeof searchInput) return names[searchInput];

  // Everything else (cast to string)
  var search = String(searchInput);

  // check codes
  var foundNamedKey = codes[search.toLowerCase()];
  if (foundNamedKey) return foundNamedKey;

  // check aliases
  var foundNamedKey = aliases[search.toLowerCase()];
  if (foundNamedKey) return foundNamedKey;

  // weird character?
  if (search.length === 1) return search.charCodeAt(0);

  return undefined;
}

/**
 * Compares a keyboard event with a given keyCode or keyName.
 *
 * @param {Event} event Keyboard event that should be tested
 * @param {Mixed} keyCode {Number} or keyName {String}
 * @return {Boolean}
 * @api public
 */
keyCode.isEventKey = function isEventKey(event, nameOrCode) {
  if (event && "object" === typeof event) {
    var keyCode = event.which || event.keyCode || event.charCode;
    if (keyCode === null || keyCode === undefined) {
      return false;
    }
    if (typeof nameOrCode === "string") {
      // check codes
      var foundNamedKey = codes[nameOrCode.toLowerCase()];
      if (foundNamedKey) {
        return foundNamedKey === keyCode;
      }

      // check aliases
      var foundNamedKey = aliases[nameOrCode.toLowerCase()];
      if (foundNamedKey) {
        return foundNamedKey === keyCode;
      }
    } else if (typeof nameOrCode === "number") {
      return nameOrCode === keyCode;
    }
    return false;
  }
};

/**
 * Get by name
 *
 *   exports.code['enter'] // => 13
 */

var codes = {
  backspace: 8,
  tab: 9,
  enter: 13,
  shift: 16,
  ctrl: 17,
  alt: 18,
  "pause/break": 19,
  "caps lock": 20,
  esc: 27,
  space: 32,
  "page up": 33,
  "page down": 34,
  end: 35,
  home: 36,
  left: 37,
  up: 38,
  right: 39,
  down: 40,
  insert: 45,
  delete: 46,
  command: 91,
  "left command": 91,
  "right command": 93,
  "numpad *": 106,
  "numpad +": 107,
  "numpad -": 109,
  "numpad .": 110,
  "numpad /": 111,
  "num lock": 144,
  "scroll lock": 145,
  "my computer": 182,
  "my calculator": 183,
  ";": 186,
  "=": 187,
  ",": 188,
  "-": 189,
  ".": 190,
  "/": 191,
  "`": 192,
  "[": 219,
  "\\": 220,
  "]": 221,
  "'": 222,
};

// Helper aliases

var aliases = {
  windows: 91,
  "⇧": 16,
  "⌥": 18,
  "⌃": 17,
  "⌘": 91,
  ctl: 17,
  control: 17,
  option: 18,
  pause: 19,
  break: 19,
  caps: 20,
  return: 13,
  escape: 27,
  spc: 32,
  spacebar: 32,
  pgup: 33,
  pgdn: 34,
  ins: 45,
  del: 46,
  cmd: 91,
};

/*!
 * Programatically add the following
 */

// lower case chars
for (i = 97; i < 123; i++) codes[String.fromCharCode(i)] = i - 32;

// numbers
for (var i = 48; i < 58; i++) codes[i - 48] = i;

// function keys
for (i = 1; i < 13; i++) codes["f" + i] = i + 111;

// numpad keys
for (i = 0; i < 10; i++) codes["numpad " + i] = i + 96;

/**
 * Get by code
 *
 *   exports.name[13] // => 'Enter'
 */

var names = {}; // title for backward compat

// Create reverse mapping
for (i in codes) names[codes[i]] = i;

// Add aliases
for (var alias in aliases) {
  codes[alias] = aliases[alias];
}

// MIT Licensed - author: jwilson8767
function elementReady(selector) {
  return new Promise((resolve, reject) => {
    let el = document.querySelector(selector);
    if (el) {
      resolve(el);
    }
    new MutationObserver((mutationRecords, observer) => {
      // Query for elements matching the specified selector
      Array.from(document.querySelectorAll(selector)).forEach((element) => {
        resolve(element);
        //Once we have resolved we don't need the observer anymore.
        observer.disconnect();
      });
    }).observe(document.documentElement, {
      childList: true,
      subtree: true,
    });
  });
}

// Background Color Picker
let backgroundColorPicker = document.querySelector("#backgroundColorPicker");

backgroundColorPicker.addEventListener(
  "change",
  function updateBackgroundColor() {
    chrome.storage.sync.set({
      backgroundColor: backgroundColorPicker.value,
    });
  }
);

// Border Color Picker
let borderColorPicker = document.querySelector("#borderColor");
let defaultBorderValue = "#64ffda";

chrome.storage.sync.get("borderColor", (response) => {
  setTimeout(() => {
    if (response.borderColor == false) {
      chrome.storage.sync.set({ borderColor: defaultBorderValue });
      borderColorPicker.value = defaultBorderValue;
    }
  }, 500);
});

borderColorPicker.addEventListener("change", function updateBorderColor() {
  chrome.storage.sync.set({ borderColor: borderColorPicker.value });
});
