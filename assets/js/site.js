(function () {
  "use strict";

  // ---- Theme toggle -------------------------------------------------------
  // No saved choice = follow the OS. Clicking pins the opposite of what's showing.
  var root = document.documentElement;
  var toggle = document.querySelector(".theme-toggle");
  var media = window.matchMedia("(prefers-color-scheme: dark)");

  function isDark() {
    var t = root.dataset.theme;
    return t ? t === "dark" : media.matches;
  }

  function syncLabel() {
    if (!toggle) return;
    var label = isDark() ? "Switch to light mode" : "Switch to dark mode";
    toggle.setAttribute("aria-label", label);
    toggle.title = label;
  }

  if (toggle) {
    toggle.addEventListener("click", function () {
      var next = isDark() ? "light" : "dark";
      root.dataset.theme = next;
      try { localStorage.setItem("theme", next); } catch (e) {}
      syncLabel();
    });
    media.addEventListener("change", syncLabel);
    syncLabel();
  }
})();
