/* ================================================================
   theme.js  –  Dark / Light theme toggle
================================================================ */

(function () {
  "use strict";

  var STORAGE_KEY = "openaq_theme";
  var DARK_LABEL  = "&#9790; Dark";   // crescent moon
  var LIGHT_LABEL = "&#9788; Light";  // sun

  /* ---- helpers ------------------------------------------------ */
  function isLight() {
    return document.body.classList.contains("theme-light");
  }

  function applyTheme(light) {
    if (light) {
      document.body.classList.add("theme-light");
    } else {
      document.body.classList.remove("theme-light");
    }
    updateButton(light);
    try { localStorage.setItem(STORAGE_KEY, light ? "light" : "dark"); }
    catch(e) {}
  }

  function updateButton(light) {
    var btn = document.getElementById("theme-toggle-btn");
    if (!btn) return;
    btn.innerHTML = light ? DARK_LABEL : LIGHT_LABEL;
    btn.title     = light ? "Switch to Dark theme" : "Switch to Light theme";
  }

  /* ---- toggle handler ----------------------------------------- */
  window.toggleTheme = function () {
    applyTheme(!isLight());
  };

  /* ---- initialise on DOM ready -------------------------------- */
  document.addEventListener("DOMContentLoaded", function () {
    var saved = "dark";
    try { saved = localStorage.getItem(STORAGE_KEY) || "dark"; } catch(e) {}
    applyTheme(saved === "light");
  });

  /* ---- also re-apply after Shiny re-renders the page ----------
     (Shiny sometimes re-injects body attributes) */
  $(document).on("shiny:connected", function () {
    var saved = "dark";
    try { saved = localStorage.getItem(STORAGE_KEY) || "dark"; } catch(e) {}
    applyTheme(saved === "light");
  });

})();
