const baseURL = "https://www.meetenhancementsuite.com/";
const signupURL = `${baseURL}/signup`;
const changelogURL = `${baseURL}/changelog`;
const uninstallURL = `${baseURL}/goodbye`;
const featuresWithLicenseKey = [
  "licenseKey",
  "autoAdmit",
  "hideNames",
  "showNames",
  "hideTalkIcons",
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
  "hideJoinUpsell",
  "hideToolsUpsell",
  "leavePrompt",
  "setBackgroundColor",
  "backgroundColor",
  "speakerBorder",
  "autoUnmute",
  "autoCopyURL",
  "hideTopBar",
];

const featureObject = featuresWithLicenseKey.reduce(
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

chrome.runtime.onInstalled.addListener((details) => {
  if (details.reason === "install") {
    chrome.tabs.create({
      url: signupURL,
    });

    chrome.storage.sync.set(featureObject);
  }

  if (details.reason === "update") {
    chrome.tabs.create({
      url: changelogURL,
    });
    chrome.browserAction.setBadgeBackgroundColor({ color: [240, 104, 104, 1] });
    chrome.browserAction.setBadgeText({ text: "new" });
  }
});

chrome.runtime.setUninstallURL(uninstallURL);

chrome.runtime.onMessage.addListener((message) => {
  if (message.popupOpen) {
    chrome.browserAction.setBadgeText({ text: "" });
  }
});

chrome.storage.sync.get("licenseKey", (response) => {
  if (response.licenseKey != undefined) {
    chrome.runtime.onMessage.addListener(() => {
      let found = false;
      let tabId = undefined;
      let currentTabId = undefined;

      chrome.tabs.query(
        { active: true, windowType: "normal", currentWindow: true },
        (tab) => {
          currentTabId = tab[0].id;
        }
      );

      chrome.tabs.query({}, (tabs) => {
        for (var i = 0; i < tabs.length; i++) {
          if (tabs[i].audible) {
            found = true;
            tabId = tabs[i].id;
            winId = tabs[i].windowId;
          }
        }

        if (found == true && tabId != currentTabId) {
          chrome.tabs.update(tabId, { active: true });
          setTimeout(() => {
            chrome.windows.update(winId, { focused: true });
          }, 200);
        }
      });
    });
  }
});
