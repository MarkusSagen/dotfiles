window.addEventListener('message', e => {
  if (e.data && e.data.type === 'openNewTab') {
    let url = e.data.url;
    if (!url.startsWith('http')) {
      url = 'http://' + url;
    }
    chrome.tabs.create({
      active: true,
      url,
    });
  }
});
