<html>
<head>
 <title>Solanin - Configure</title>
 <link href="/_s/css/setup.css" rel="stylesheet" type="text/css"/>
 <link href="/_s/css/config.css" rel="stylesheet" type="text/css"/>
</head>
<body>
 <div id="config">
  $if(!config.setup)$
   <p>You have successfully installed Solanin.</p>
   <p>Please take a few minutes to configure it.</p>
  $endif$
  $configForm()$
 </div>
</body>
</html>
