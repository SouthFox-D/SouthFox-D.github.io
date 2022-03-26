id="local.search.active"
var inputArea       = document.querySelector("#local-search-input");
inputArea.onclick   = function(){ getSearchFile("search.xml"); this.onclick = null }
inputArea.onkeydown = function(){ if(event.keyCode == 13) return false }
