<html>
<head>
 <title>Solanin - Configure</title>
 <link href="/_s/css/setup.css" rel="stylesheet" type="text/css"/>
 <link href="/_s/css/config.css" rel="stylesheet" type="text/css"/>
</head>
<body>
 <div id="config">
  $if(!config.setup)$
   $if(config.ffmpeg)$
    <p>Based on your FFmpeg binary, the following formats are supported:</p>
    <p><b style="color: white;">$config.exts; separator=" "$</b></p>
   $else$
    <p>It looks like you did not point Solanin to a FFmpeg binary.</p>
    <p>You will only be able to stream <b>.mp3</b>s.</p>
   $endif$
   <p>Enter a password that you will be using to log in to complete the configuration.</p>
  $endif$
  $passwordForm()$
 </div>
</body>
</html>
