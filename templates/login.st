<html>
<head>
 <title>Solanin - Login</title>

 <script type="text/javascript" src="/_s/js/jquery.js"></script>
 <script type="text/javascript" src="/_s/js/jquery.color.js"></script>
 <script type="text/javascript" src="/_s/js/login.js"></script>

 <link href="/_s/css/login.css" rel="stylesheet" type="text/css"/>
</head>
<body>
 <div id="login">
  <img alt="login" src="/_s/img/logo.png" />
  <form id="login-form" action="/_l" method="post">
   <input id="password" class="password $if(failed)$ error $endif$" type="password" name="password" />
   <input id="submit" class="submit" type="submit" value="login" />
  </form>
 </div>
</body>
</html>
