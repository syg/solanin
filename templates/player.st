<html>
<head>
 <title>Solanin</title>

 <script type="text/javascript" src="/_s/js/md5.js"></script>
 <script type="text/javascript" src="/_s/js/jquery.js"></script>
 <script type="text/javascript" src="/_s/js/jquery.event.drag.js"></script>
 <script type="text/javascript" src="/_s/js/soundmanager2.js"></script>
 <script type="text/javascript" src="/_s/js/player.js"></script>

 <link href="/_s/css/player.css" rel="stylesheet" type="text/css"/>
 <link href="/_s/css/config.css" rel="stylesheet" type="text/css"/>
</head>
<body>
 <div id="keyhelp">
  <div>
   <h1>solanin keybinds help</h1>
   <dl>
    <dt><span class="cmd">p</span></dt>
    <dd>play/pause the current song</dd>
    <dt><span class="cmd">u</span></dt>
    <dd>stop</dd>
    <dt><span class="cmd">j</span>/<span class="cmd">k</span></dt>
    <dd>next/previous song</dd>
    <br/>
    <dt><span class="cmd">[</span>/<span class="cmd">]</span></dt>
    <dd>decrease/increase volume</dd>
    <dt><span class="cmd">m</span></dt>
    <dd>mute/unmute</dd>
    <br/>
    <dt><span class="cmd">?</span></dt>
    <dd>toggle this help</dd>
   </dl>
  </div>
 </div>
 <div id="header">
  <div id="top">
   <div id="now-playing">
    <span>
     <img alt="welcome to solanin" src="_s/img/logo.png" />
    </span>
   </div>
   <div id="timing">
    <span id="timing-position">-:--</span>
    <span id="timing-total">-:--</span>
   </div>
   <div id="controls">
    <div id="statusbar">
     <div id="loading"></div>
     <div id="position"></div>
    </div>
   </div>
  </div>
  <form id="search" action="/" method="POST">
   <input class="search" type="text" name="q" value="" />
   <input class="hidden" type="submit" value="login" />
  </form>
  <div id="hack-header"></div>
 </div>
 <div id="content">
  <div id="playlist">
   $playlist()$
  </div>
 </div>
 <div id="volume">
  <div>
   <div id="volumebar">
    <div id="volumebar-position"></div>
   </div>
  </div>
 </div>
 <div id="footer">
  <div id="footer-content"></div>
  <div id="footer-status">
   press <span class="cmd">?</span> for keyboard shortcuts
  </div>
  <div class="tabs">
   <a href="/_b">Rebuild Index</a><a href="/_c">Configure</a><a href="/_p">Change Password</a><a href="/_x">Logout</a>
  </div>
 </div>
</body>
</html>
